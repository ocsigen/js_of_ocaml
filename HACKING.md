# Release procedure
* Documentation
  * Make sure the doc is up to date (See Documentation generation)
  * In the wikidoc branch, copy dev to VERSION
* update VERSION
* dune-release tag $(cat VERSION)
* dune-release distrib -n js_of_ocaml
* dune-release publish distrib
* dune-release opam pkg
* dune-release opam submit

# Documentation generation
* make doc
* git checkout wikidoc && git reset --hard origin/wikidoc && git checkout master
* make installdoc
* cd _wikidoc
* git diff # review diff
* git commit -am "sync doc" && git push origin wikidoc
* cd ..
* git push origin wikidoc
