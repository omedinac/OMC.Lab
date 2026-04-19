# Generate member pages and publications from Google Sheets and BibTeX.
# Uses: tidyverse, stringi, googlesheets4, readr

if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
}

pacman::p_load(tidyverse, stringi, googlesheets4, readr, glue)

#' Create a URL-safe slug based on first and last names.
#' @param first_name Character scalar first name.
#' @param last_name Character scalar surname.
#' @return Character scalar slug.
make_member_slug <- function(first_name, last_name) {
  raw_slug <- str_trim(paste(first_name, last_name))
  raw_slug |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("(^-+|-+$)", "")
}

#' Squish and normalize character vectors.
#' @param value Character vector.
#' @return Character vector with whitespace normalized.
normalize_text <- function(value) {
  value |>
    tidyr::replace_na("") |>
    str_squish()
}

#' Normalize person names for robust matching.
#' @param value Character vector with person names.
#' @return Character vector normalized for comparisons.
normalize_person_name <- function(value) {
  value |>
    normalize_text() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

#' Parse BibTeX/BibLaTeX author strings into normalized components.
#'
#' Supports both classic name format ("Surname, Given") and key-value
#' author fragments such as:
#' "family = Vale, given = Adriano, prefix = do, useprefix = true".
#'
#' @param author_value Character scalar raw author value.
#' @return Named list with `author`, `surname`, and `firstname`.
parse_bibliography_author <- function(author_value) {
  author_value <- normalize_text(author_value)

  extract_bib_field <- function(text, field_name) {
    # Capture until next comma+key or end of string to support values with
    # internal punctuation wrapped in braces/quotes.
    pattern <- regex(
      as.character(glue(
        "(?:^|,\\s*){field_name}\\s*=\\s*(.+?)(?=,\\s*[a-z_]+\\s*=|$)"
      )),
      ignore_case = TRUE
    )

    value <- str_match(text, pattern)[, 2]
    value |>
      normalize_text() |>
      str_remove_all('^"|"$') |>
      str_remove_all("^\\{|\\}$")
  }

  has_key_value_format <- str_detect(
    author_value,
    regex("(?:^|,\\s*)(family|given|prefix|suffix)\\s*=", ignore_case = TRUE)
  )

  if (has_key_value_format) {
    family <- extract_bib_field(author_value, "family")
    given <- extract_bib_field(author_value, "given")
    prefix <- extract_bib_field(author_value, "prefix")
    suffix <- extract_bib_field(author_value, "suffix")
    useprefix_raw <- extract_bib_field(author_value, "useprefix")
    useprefix <- str_to_lower(normalize_text(useprefix_raw)) %in%
      c(
        "true",
        "t",
        "1",
        "yes"
      )

    surname <- if (!is.na(prefix) && prefix != "" && useprefix) {
      str_squish(paste(prefix, family))
    } else {
      family
    }

    firstname <- if (!is.na(prefix) && prefix != "" && !useprefix) {
      str_squish(paste(given, prefix))
    } else {
      given
    }

    author <- case_when(
      !is.na(surname) && surname != "" && !is.na(firstname) && firstname != "" ~
        glue("{surname}, {firstname}"),
      !is.na(surname) && surname != "" ~ surname,
      !is.na(firstname) && firstname != "" ~ firstname,
      TRUE ~ author_value
    )

    if (!is.na(suffix) && suffix != "") {
      author <- glue("{author}, {suffix}")
    }

    return(list(
      author = as.character(author),
      surname = as.character(surname),
      firstname = as.character(firstname)
    ))
  }

  if (str_detect(author_value, ",")) {
    surname <- str_extract(author_value, "^[^,]+") |> str_squish()
    firstname <- str_extract(author_value, "(?<=,).+$") |> str_squish()
  } else {
    parts <- str_split(author_value, "\\s+", simplify = TRUE)
    parts <- parts[parts != ""]

    if (length(parts) > 1) {
      surname <- parts[[length(parts)]]
      firstname <- str_squish(paste(parts[-length(parts)], collapse = " "))
    } else {
      surname <- author_value
      firstname <- ""
    }
  }

  list(
    author = as.character(str_squish(paste0(
      surname,
      ifelse(firstname != "", paste0(", ", firstname), "")
    ))),
    surname = as.character(surname),
    firstname = as.character(firstname)
  )
}

#' Remove markdown links while preserving visible text.
#' @param value Character vector.
#' @return Character vector without markdown link markup.
strip_markdown_links <- function(value) {
  value |>
    str_replace_all("\\[([^\\]]+)\\]\\([^\\)]+\\)", "\\1")
}

#' Extract YAML front matter lines from a QMD file.
#' @param file_path Character scalar path to a QMD file.
#' @return Character vector of front matter lines.
read_front_matter_lines <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  separators <- which(lines == "---")

  if (length(separators) < 2 || separators[[1]] != 1) {
    return(character())
  }

  lines[(separators[[1]] + 1):(separators[[2]] - 1)]
}

#' Read project metadata from project QMD files.
#' @param project_dir Character scalar project directory.
#' @return Tibble with one row per project.
read_projects_metadata <- function(project_dir) {
  project_files <- list.files(
    project_dir,
    pattern = "\\.qmd$",
    full.names = TRUE
  )

  if (length(project_files) == 0) {
    return(
      tibble(
        project_slug = character(),
        project_title = character(),
        project_link = character(),
        ongoing_project = logical(),
        authors = list()
      )
    )
  }

  purrr::map_dfr(project_files, function(project_path) {
    front_matter <- read_front_matter_lines(project_path)
    project_slug <- tools::file_path_sans_ext(basename(project_path))

    title_line <- front_matter[str_detect(front_matter, "^title:\\s*")]
    project_title <- if (length(title_line) > 0) {
      title_line[[1]] |>
        str_remove("^title:\\s*") |>
        str_remove_all('^"|"$') |>
        str_squish()
    } else {
      project_slug
    }

    author_lines <- front_matter[str_detect(
      front_matter,
      "^\\s*-\\s*name:\\s*"
    )]
    author_names <- if (length(author_lines) > 0) {
      author_lines |>
        str_remove("^\\s*-\\s*name:\\s*") |>
        str_remove_all('^"|"$') |>
        strip_markdown_links() |>
        str_squish()
    } else {
      character()
    }

    tibble(
      project_slug = project_slug,
      project_title = project_title,
      project_link = glue("/projects/{project_slug}.qmd"),
      ongoing_project = any(str_detect(
        front_matter,
        "^\\s*-\\s*Ongoing Project\\s*$"
      )),
      authors = list(author_names)
    )
  })
}

#' Check if a member is part of a project's author list.
#' @param project_authors Character vector of project author names.
#' @param member_name Character scalar full member name.
#' @return Logical scalar.
member_has_project <- function(project_authors, member_name) {
  if (length(project_authors) == 0 || is.na(member_name) || member_name == "") {
    return(FALSE)
  }

  member_name_norm <- normalize_person_name(member_name)
  project_authors_norm <- normalize_person_name(project_authors)

  any(project_authors_norm == member_name_norm)
}

#' Normalize URLs and prepend https:// when missing.
#' @param value Character vector with URLs.
#' @return Character vector with normalized URLs or NA.
normalize_url <- function(value) {
  value <- normalize_text(value)
  value <- if_else(value == "", NA_character_, value)
  if_else(
    !is.na(value) & !str_detect(value, "^[a-z]+://"),
    paste0("https://", value),
    value
  )
}

#' Convert a local file path to a site-root relative URL path.
#' @param path Character scalar file path.
#' @return Character scalar URL path starting with '/'.
to_site_path <- function(path) {
  paste0("/", str_replace_all(path, "\\\\", "/"))
}

#' Download a remote icon only if the destination file is missing.
#' @param icon_url Character scalar remote URL.
#' @param destination Character scalar local destination path.
#' @return Logical scalar indicating if the local icon is available.
download_icon_if_needed <- function(icon_url, destination) {
  if (!file.exists(destination)) {
    dir.create(dirname(destination), showWarnings = FALSE, recursive = TRUE)
    tryCatch(
      {
        utils::download.file(icon_url, destination, mode = "wb", quiet = TRUE)
      },
      error = function(e) NULL
    )
  }
  file.exists(destination)
}

#' Build social icon URLs, preferring local files saved in images/.
#' @param icons_dir Character scalar directory for social icon files.
#' @param theme_icon_color Character scalar hex color without '#'.
#' @return Named character vector of icon URLs by social label.
build_social_icon_urls <- function(icons_dir, theme_icon_color = "593196") {
  icon_specs <- tibble::tribble(
    ~label           , ~icon_name                   ,
    "Website"        , "bi:globe"                   ,
    "LinkedIn"       , "simple-icons:linkedin"      ,
    "Bluesky"        , "simple-icons:bluesky"       ,
    "Google Scholar" , "simple-icons:googlescholar" ,
    "Email"          , "bi:envelope"
  ) |>
    mutate(
      file_slug = label |>
        str_to_lower() |>
        str_replace_all("[^a-z0-9]+", "-") |>
        str_replace_all("(^-+|-+$)", ""),
      local_file = file.path(icons_dir, paste0(file_slug, ".svg")),
      remote_url = glue(
        "https://api.iconify.design/{icon_name}.svg?color=%23{theme_icon_color}"
      )
    )

  icon_available <- purrr::map2_lgl(
    icon_specs$remote_url,
    icon_specs$local_file,
    download_icon_if_needed
  )

  icon_urls <- if_else(
    icon_available,
    to_site_path(icon_specs$local_file),
    icon_specs$remote_url
  )

  stats::setNames(icon_urls, icon_specs$label)
}

#' Resolve a local member photo or fallback avatar.
#' @param slug Character scalar member slug.
#' @param full_name Character scalar full name.
#' @param image_dir Character scalar directory for member photos.
#' @return Character scalar path or URL to photo.
resolve_member_photo <- function(slug, full_name, image_dir) {
  candidates <- c(
    glue("{slug}.png"),
    glue("{slug}.jpg"),
    glue("{slug}.jpeg")
  )
  existing <- candidates[file.exists(file.path(image_dir, candidates))]
  if (length(existing) > 0) {
    file.path(image_dir, existing[[1]])
  } else {
    make_avatar_url(full_name)
  }
}

#' Create an avatar URL with initials for members without a photo.
#'
#' Uses the ui-avatars.com service to generate an image containing the
#' member's initials. Returns a fully-qualified URL which can be used
#' directly in markdown/image tags.
#'
#' @param full_name Character scalar full name (first and last name).
#' @param size Integer desired image size in pixels (default 256).
#' @return Character scalar URL to the generated avatar image.
make_avatar_url <- function(full_name, size = 256) {
  name <- normalize_text(full_name)
  if (is.na(name) || name == "") {
    name <- "?"
  }
  # URL-encode the name so spaces and special characters are safe in the URL
  name_enc <- utils::URLencode(name, reserved = TRUE)
  bg <- "593196"
  color <- "FFFFFF"
  paste0(
    "https://ui-avatars.com/api/?name=",
    name_enc,
    "&background=",
    bg,
    "&color=",
    color,
    "&size=",
    as.integer(size),
    "&rounded=true"
  )
}

#' Read BibTeX file into a tidy tibble.
#' @param bib_path Character scalar path to BibTeX file.
#' @return Tibble with parsed entries.
read_bibliography <- function(bib_path) {
  bib_data <- suppressMessages(bib2df::bib2df(bib_path)) %>%
    janitor::clean_names() %>%
    mutate(id = row_number())

  author_pub <- bib_data %>%
    unnest(author) %>%
    distinct(id, author) %>%
    mutate(parsed_author = purrr::map(author, parse_bibliography_author)) %>%
    mutate(
      author = purrr::map_chr(parsed_author, "author"),
      surname = purrr::map_chr(parsed_author, "surname"),
      firstname = purrr::map_chr(parsed_author, "firstname")
    ) %>%
    select(-parsed_author)

  # add id of articles to members
  member_articles <- author_pub %>%
    inner_join(
      select(members_raw, surname, firstname),
      by = c("surname", "firstname")
    ) %>%
    mutate(slug = make_member_slug(firstname, surname)) %>%
    select(slug, id) %>%
    left_join(bib_data, by = "id")

  member_articles_cleaned <- member_articles %>%
    mutate(
      journal = str_c("*", journaltitle, "*", sep = ""),
      journal = ifelse(category == "PHDTHESIS", school, journal),
      journal = ifelse(
        category == "BOOK",
        paste0(location, ": ", publisher),
        journal
      ),
      journal = ifelse(
        category %in% c("INCOLLECTION", "INBOOK"),
        paste0("*", booktitle, "*, ", location, ": ", publisher),
        journal
      ),
      journal = ifelse(category == "SOFTWARE", "R package", journal),
      journal = ifelse(category == "DATASET", publisher, journal),
      year = str_extract(date, "^\\d+") |> as.integer(),
      doi = ifelse(category == "SOFTWARE", annotation, doi),
      doi = ifelse(
        str_detect(doi, "^\\d+"),
        paste0("https://doi.org/", doi),
        doi
      ),
      pages = na_if(str_squish(pages), "")
    ) %>%
    select(
      slug,
      category,
      id,
      author,
      title,
      booktitle,
      chapter,
      journal,
      year,
      month,
      volume,
      number,
      pages,
      doi,
      url,
      annotation
    ) %>%
    unnest_longer(author) %>%
    mutate(
      author = purrr::map_chr(author, ~ parse_bibliography_author(.x)$author)
    ) %>%
    mutate(
      across(
        all_of(c("author", "title", "journal")),
        ~ str_remove_all(., "\\{|\\}")
      ),
      across(
        all_of(c("author", "title", "journal")),
        ~ str_replace_all(., "\\\\'e", "é")
      ),
      across(
        all_of(c("author", "title", "journal")),
        ~ str_replace_all(., "\\\\'o", "ó")
      ),
      across(
        all_of(c("author", "title", "journal")),
        ~ str_replace_all(., "\\\\`a", "à")
      ),
      title_linked = ifelse(
        !is.na(doi) & doi != "",
        paste0("[", title, "](", doi, ")"),
        title
      ),
      title_linked = ifelse(
        (is.na(doi) | doi == "") & (!is.na(url) & url != ""),
        paste0("[", title, "](", url, ")"),
        title_linked
      ),
      preprint = ifelse(
        annotation != "" & !category %in% c("SOFTWARE", "DATASET"),
        paste0("[Preprint](", annotation, ")"),
        NA
      )
    ) %>%
    mutate(
      first_name = str_extract(author, "(?<=, ).*"),
      surname = str_extract(author, ".*(?=,)"),
      .before = author
    ) %>%
    group_by(slug, id) %>%
    mutate(
      rownumber = row_number(),
      author = ifelse(
        rownumber > 1,
        paste(first_name, surname),
        author
      ),
      author = paste0(author, collapse = ", ")
    ) %>%
    filter(rownumber == max(rownumber)) %>%
    ungroup()

  references_complete <- member_articles_cleaned %>%
    mutate(
      info_pub = case_when(
        category %in% c("INCOLLECTION", "INBOOK") ~ pages,
        !is.na(number) & !is.na(pages) ~ paste0(
          volume,
          "(",
          number,
          "): ",
          pages
        ),
        !is.na(number) & is.na(pages) ~ paste0(volume, "(", number, ")"),
        TRUE ~ volume
      ),
      reference = paste0(
        author,
        ". (",
        year,
        "). ",
        title_linked,
        ". ",
        journal
      ),
      reference = ifelse(
        !is.na(info_pub) & info_pub != "",
        paste0(reference, ", ", info_pub, "."),
        paste0(reference, ".")
      ),
      reference = ifelse(
        !is.na(preprint),
        paste0(reference, " [", preprint, "]"),
        reference
      )
    ) %>%
    select(-c(title_linked, preprint, rownumber, info_pub))
}

#' Check whether a publication includes a member name.
#' @param author_field Character scalar author list.
#' @param first_name Character scalar first name.
#' @param last_name Character scalar surname.
#' @return Logical scalar.
matches_member_author <- function(author_field, first_name, last_name) {
  author_field <- normalize_text(author_field)
  pattern_one <- regex(
    as.character(glue("{last_name}.*{first_name}")),
    ignore_case = TRUE
  )
  pattern_two <- regex(
    as.character(glue("{first_name}.*{last_name}")),
    ignore_case = TRUE
  )
  valid <- author_field != ""
  valid &
    (str_detect(author_field, pattern_one) |
      str_detect(author_field, pattern_two))
}

#' Render a single member page.
#' @param member_row Single-row data frame with member information.
#' @param output_dir Output directory for member pages.
#' @param image_dir Directory for member photos.
#' @param bibliography_df Tibble of bibliography entries.
#' @param projects_df Tibble of project metadata.
#' @return Character scalar path to the generated file.
# Render a single member page.
# Adds email as a social icon (mailto:) and removes the separate Contact section.
write_member_page <- function(
  member_row,
  output_dir,
  image_dir,
  bibliography_df,
  projects_df,
  social_icon_urls
) {
  full_name <- str_trim(paste(member_row$firstname, member_row$surname))
  slug <- make_member_slug(member_row$firstname, member_row$surname)
  avatar_url <- resolve_member_photo(slug, full_name, image_dir)

  if (str_detect(avatar_url, "images\\/members")) {
    avatar_url <- file.path("/", avatar_url) # Ensure the URL is relative to the site root
  }

  email_raw <- normalize_text(member_row$email)
  email_url <- if (is.na(email_raw) || email_raw == "") {
    NA_character_
  } else {
    paste0("mailto:", email_raw)
  }

  link_items <- list(
    list(
      label = "Website",
      icon_url = social_icon_urls[["Website"]],
      url = normalize_url(member_row$personal_or_institutional_website)
    ),
    list(
      label = "LinkedIn",
      icon_url = social_icon_urls[["LinkedIn"]],
      url = normalize_url(member_row$linkedin)
    ),
    list(
      label = "Bluesky",
      icon_url = social_icon_urls[["Bluesky"]],
      url = normalize_url(member_row$bluesky)
    ),
    list(
      label = "Google Scholar",
      icon_url = social_icon_urls[["Google Scholar"]],
      url = normalize_url(member_row$google_scholar)
    ),
    list(
      label = "Email",
      icon_url = social_icon_urls[["Email"]],
      url = email_url
    )
  )

  icon_links <- purrr::keep(link_items, ~ !is.na(.x$url) && .x$url != "") |>
    purrr::map_chr(function(item) {
      glue(
        "<a class=\"social-link\" href=\"{item$url}\" target=\"_blank\" rel=\"noopener\">",
        "<img class=\"social-icon\" src=\"{item$icon_url}\" alt=\"{item$label}\" />",
        "</a>"
      )
    })

  links_block <- if (length(icon_links) > 0) {
    c("<div class=\"member-links\">", icon_links, "</div>")
  } else {
    ""
  }

  member_pubs <- bibliography_df |>
    filter(slug == !!slug) %>%
    arrange(author, desc(year))

  member_projects <- projects_df |>
    filter(purrr::map_lgl(
      authors,
      member_has_project,
      member_name = full_name
    )) |>
    arrange(desc(ongoing_project), project_title)

  has_projects <- nrow(member_projects) > 0

  listing_yaml <- if (has_projects) {
    c(
      "listing:",
      "  - id: projects",
      "    contents: ../projects/*.qmd",
      "    type: default",
      "    sort: \"date desc\"",
      "    date-format: long",
      "    fields: [image, title, author, reading-time]",
      "    include:",
      glue("      author: '{full_name}'")
    )
  } else {
    character()
  }

  projects_block <- if (has_projects) {
    c(
      "## Research Projects",
      "",
      "::: {#projects}",
      ":::",
      ""
    )
  } else {
    c("")
  }

  publications_block <- if (nrow(member_pubs) > 0) {
    c(
      "## Publications",
      "",
      paste0(member_pubs$reference, collapse = "\n\n")
    )
  } else {
    c("")
  }

  page_content <- c(
    "---",
    glue("title: \"{full_name}\""),
    listing_yaml,
    "---",
    "",
    "::: {.member-profile}",
    glue("![]({avatar_url}){{.member-photo}}"),
    links_block,
    ":::",
    "",
    glue("**Position:** {member_row$position}"),
    "",
    member_row$description,
    "",
    projects_block,
    publications_block
  )

  output_path <- file.path(output_dir, glue("{slug}.qmd"))
  writeLines(page_content, output_path)
  output_path
}

#' Render the members index page.
#' @param members_df Data frame of members.
#' @param output_file Path to members index QMD.
#' @param image_dir Directory for member photos.
#' @return Character scalar path to the generated file.
write_members_index <- function(members_df, output_file, image_dir) {
  cards <- members_df |>
    mutate(
      full_name = str_trim(paste(firstname, surname)),
      slug = make_member_slug(firstname, surname),
      avatar_url = map2_chr(
        slug,
        full_name,
        ~ resolve_member_photo(.x, .y, image_dir)
      )
    ) |>
    mutate(
      card = glue(
        "::: {{.member-card}}\n![]({avatar_url}){{.member-photo}}\n\n[**{full_name}**](members/{slug}.qmd)  \n{position}\n:::"
      )
    )

  content <- c(
    "---",
    "title: \"Members\"",
    "---",
    "",
    "::: {.member-grid}",
    cards$card,
    ":::",
    ""
  )

  writeLines(content, output_file)
  output_file
}

#' Render the publications page grouped by year.
#' @param bibliography_df Tibble of bibliography entries.
#' @param output_file Path to publications QMD.
#' @return Character scalar path to the generated file.
write_publications_page <- function(bibliography_df, output_file) {
  years <- sort(unique(bibliography_df$year), decreasing = TRUE)

  sections <- purrr::map_chr(years, function(year_label) {
    entries <- bibliography_df |>
      filter(year == year_label) |>
      arrange(reference) |>
      pull(reference)
    paste(c(glue("## {year_label}"), "", entries, ""), collapse = "\n\n")
  })

  writeLines(sections, output_file)
  output_file
}

output_dir <- "members"
index_file <- "members.qmd"
image_dir <- file.path("images", "members")
icons_dir <- file.path("images", "icons")
publications_file <- "publications.qmd"
bib_file <- "ecopo_working_group_publications.bib"
projects_dir <- "projects"

# Load members data from Google Sheets with caching based on modified time
sheet_id <- "1U_mY_fQYfd5RAXlsC96p_kP1B7bs-xa9qJtg-X6iSKA"

cache_dir <- "data"
members_cache_file <- file.path(cache_dir, "members_raw.csv")
members_meta_file <- file.path(cache_dir, "members_raw_meta.rds")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

remote_modified <- tryCatch(
  {
    googledrive::drive_get(id = sheet_id) |>
      googledrive::drive_reveal("modifiedTime") |>
      dplyr::pull(modified_time) |>
      as.POSIXct(tz = "UTC")
  },
  error = function(e) as.POSIXct(NA)
)

local_modified <- if (file.exists(members_meta_file)) {
  readRDS(members_meta_file)$modified_time
} else {
  as.POSIXct(NA)
}

needs_refresh <- !file.exists(members_cache_file) ||
  (!is.na(remote_modified) &&
    (is.na(local_modified) || remote_modified > local_modified))

if (needs_refresh) {
  members_raw <- googlesheets4::read_sheet(sheet_id, col_types = "c") |>
    arrange(surname, firstname)

  readr::write_csv(members_raw, members_cache_file)
  saveRDS(
    list(modified_time = remote_modified),
    members_meta_file
  )
} else {
  members_raw <- readr::read_csv(
    members_cache_file,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) |>
    arrange(surname, firstname)
}
bibliography_df <- read_bibliography(bib_file)
projects_df <- read_projects_metadata(projects_dir)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(image_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(icons_dir, showWarnings = FALSE, recursive = TRUE)

social_icon_urls <- build_social_icon_urls(icons_dir)

member_files <- members_raw |>
  rowwise() |>
  mutate(
    file_path = write_member_page(
      pick(everything()),
      "members",
      image_dir,
      bibliography_df,
      projects_df,
      social_icon_urls
    )
  ) |>
  ungroup() |>
  pull(file_path)

bibliography_without_duplicates <- bibliography_df %>%
  distinct(reference, .keep_all = TRUE) %>%
  distinct(id, .keep_all = TRUE)

index_path <- write_members_index(members_raw, index_file, image_dir)
publications_path <- write_publications_page(
  bibliography_without_duplicates,
  publications_file
)
