

.PHONY: document
document:
	Rscript -e "devtools::document()"

.PHONY: install
install:
	make document
	Rscript -e "devtools::install()"

.PHONY: docs
docs:
	-rm -rf docs
	Rscript -e "pkgdown::build_site()"

test:
	Rscript -e "devtools::load_all(); tinytest::test_all()"

.PHONY: all
all:
	make install
	make docs
