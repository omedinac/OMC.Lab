#show: doc => article(
$if(title)$
  title: [$title$],
  $if(subtitle)$
  subtitle: [$subtitle$],
  $endif$
$endif$

$if(by-author)$
  authors: (
    $for(by-author)$
      $if(it.name.literal)$
          ( name: [$it.name.literal$],
            affiliation: [$for(it.affiliations)$$it.name$$sep$, $endfor$],
            location: [$it.location$],
            role: [$for(it.roles)$$it.role$$sep$, $endfor$],
            email: [$it.email$] ),
      $endif$
    $endfor$    
  ),
$endif$         
$if(date)$
  date: "$date$",
$endif$

$if(lang)$
  lang: "$lang$",
$endif$
$if(region)$
  region: "$region$",
$endif$
$if(abstract)$
  abstract: [$abstract$],
  abstracttitle: "$labels.abstract$",
$endif$
$if(margin)$
  margin: ($for(margin/pairs)$$margin.key$: $margin.value$,$endfor$),
$endif$
$if(papersize)$
  paper: "$papersize$",
$endif$
$if(mainfont)$
  font: ("$mainfont$",),
$endif$
$if(fontsize)$
  fontsize: $fontsize$,
$endif$
$if(section-numbering)$
  sectionnumbering: "$section-numbering$",
$endif$
$if(toc)$
  toc: $toc$,
$endif$
$if(version)$
  version: [$version$],
$endif$
$if(first-page-footer)$
 first-page-footer: [$first-page-footer$],
$endif$
publisher: $if(publisher)$$publisher$$else$"Publisher"$endif$,
documenttype: $if(documenttype)$[$documenttype$]$else$""$endif$,
$if(toc-title)$
  toc_title: [$toc-title$],
$endif$
// $if(toc-indent)$
//   toc_indent: $toc-indent$,
// $endif$
//   toc_depth: $toc-depth$,
  // cols: $if(columns)$$columns$$else$1$endif$,
  doc,
)
