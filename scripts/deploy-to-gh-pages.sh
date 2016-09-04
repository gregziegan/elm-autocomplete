#!/bin/bash

set -e

git checkout gh-pages
git pull origin gh-pages
git merge master --no-edit
cd examples
make demo
cp site/* ..
cd ..
git add app.css logo.css autocomplete.css index.html index.js
git commit -m 'Update gh-pages files'
git push origin gh-pages
git checkout master
