PYTHON27 = python2.7

build: gen/localstorage
	elm make Main.elm --yes --output elm.js

gen/localstorage:
	$(PYTHON27) ../elm-tools/localize.py ../localstorage gen/localstorage


