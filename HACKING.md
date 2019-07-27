# Release procedure
* Documentation
  * Make sure the doc is up to date (See Documentation generation)
  * In the wikidoc branch, copy dev to VERSION
* update VERSION
* TBD

# Documentation generation
* Disable library wrapping
  ocamldoc doesn't play well with module aliases and library wrapping as
  done in dune. The short term solution is disable wrapping when
  generating the doc. The proper solution will be to switch to odoc.
  Replace (wrapped true) and (wrapped (transition ..))  by (wrapped
  false) in dune files.
* make doc
* git checkout wikidoc
* git reset --hard origin/wikidoc
* make installdoc
* cd _wikidoc
* git diff # review diff
* git commit -am "sync doc" && git push origin wikidoc
* cd ..
* git push origin wikidoc
