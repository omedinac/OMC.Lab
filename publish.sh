#!/bin/bash
# Publish the site to codeberg pages
quarto render
cp .domains _site/.domains

# Commit and push the contents
git add -A
read -r -p "Enter commit message: " commit_msg
commit_msg=${commit_msg:-"Publish site"}

git commit -m "$commit_msg"
git push origin main

# Wait some seconds to ensure the commit is registered
sleep 10

git subtree push --prefix _site origin pages