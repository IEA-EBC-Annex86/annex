

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

check:
	Rscript -e "devtools::check()"

coverage: install
	Rscript -e 'covr::report(covr::package_coverage(), file = "annex_coverage.html")'

.PHONY: all
all:
	make document
	make install
	make check
	make docs
