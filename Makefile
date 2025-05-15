
# Reading version number from the package DESCRIPTION file
VERSION := $(shell grep '^Version:' DESCRIPTION | awk '{print $$2}')

.PHONY: document
document:
	Rscript -e "devtools::document()"

.PHONY: build install check
build: document
	@echo Building current version: $(VERSION)
	(cd ../ && R CMD build annex)
install: build
	@echo Installing current version: $(VERSION)
	(cd ../ && R CMD INSTALL annex_$(VERSION).tar.gz)
check: build
	@echo Checking current version: $(VERSION)
	(cd ../ && R CMD check --as-cran annex_$(VERSION).tar.gz)

.PHONY: docs
docs:
	-rm -rf docs
	Rscript -e "pkgdown::build_site()"

test:
	Rscript -e "devtools::load_all(); tinytest::test_all()"

devcheck:
	Rscript -e "devtools::check()"

coverage: install
	Rscript -e 'covr::report(covr::package_coverage(), file = "annex_coverage.html")'

.PHONY: all
all:
	make document
	make install
	make check
	make docs
