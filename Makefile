
main.js: src/*.elm
	elm make src/main.elm --output main.js

.PHONY: servephp
runphp:
	php -S localhost:8000
	open "http://localhost:8000/index.dev.html"
