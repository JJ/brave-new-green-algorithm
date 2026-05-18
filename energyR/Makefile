# Makefile for the energyR R package
#
# Provides convenient targets for common development and CRAN-submission tasks.
# Run `make help` for a summary of all targets.
#
# Requirements
# ------------
# - R  (Rscript must be on PATH)
# - devtools, rcmdcheck, roxygen2, pkgdown (installed via `make deps`)
# - system tools: pandoc (for vignettes), qpdf (for PDF compression)
#   On Debian/Ubuntu: sudo apt-get install pandoc qpdf

PKG_NAME   := energyR
PKG_DIR    := .
BUILD_DIR  := /tmp/$(PKG_NAME)-build
RSCRIPT    := Rscript
R          := R

# ---------------------------------------------------------------------------
# Default
# ---------------------------------------------------------------------------
.DEFAULT_GOAL := help

.PHONY: help deps document check check-cran test build install pkgdown clean

help:            ## Show this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	  awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}'

# ---------------------------------------------------------------------------
# Development dependencies
# ---------------------------------------------------------------------------
deps:            ## Install all development dependencies from CRAN
	$(RSCRIPT) -e "pkgs <- c('devtools','roxygen2','rcmdcheck','pkgdown', \
	  'testthat','knitr','rmarkdown','kableExtra','reshape2','ggridges'); \
	  install.packages(pkgs[!pkgs %in% rownames(installed.packages())], \
	    repos='https://cloud.r-project.org')"

# ---------------------------------------------------------------------------
# Documentation (roxygen2 → NAMESPACE + man/)
# ---------------------------------------------------------------------------
document:        ## Regenerate NAMESPACE and man/ pages from roxygen2 tags
	$(RSCRIPT) -e "roxygen2::roxygenise('$(PKG_DIR)')"

# ---------------------------------------------------------------------------
# Testing
# ---------------------------------------------------------------------------
test:            ## Run the testthat test suite
	$(RSCRIPT) -e "devtools::test('$(PKG_DIR)')"

# ---------------------------------------------------------------------------
# R CMD check
# ---------------------------------------------------------------------------
check:           ## Run R CMD check (standard, not --as-cran)
	$(RSCRIPT) -e "rcmdcheck::rcmdcheck('$(PKG_DIR)', args='--no-manual')"

check-cran:      ## Run R CMD check with --as-cran (pre-submission gate)
	$(RSCRIPT) -e "rcmdcheck::rcmdcheck('$(PKG_DIR)', \
	  args=c('--no-manual','--as-cran'), \
	  error_on='warning')"

# ---------------------------------------------------------------------------
# Build source tarball and binary package
# ---------------------------------------------------------------------------
build:           ## Build source tarball in $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)
	$(R) CMD build --compact-vignettes=gs+qpdf $(PKG_DIR) \
	  --output-directory $(BUILD_DIR)
	@echo "Tarball written to $(BUILD_DIR)/"
	@ls -lh $(BUILD_DIR)/*.tar.gz

build-binary:    ## Build binary package in $(BUILD_DIR)  [platform-specific]
	mkdir -p $(BUILD_DIR)
	$(R) CMD INSTALL --build $(PKG_DIR) \
	  --library=$(BUILD_DIR)

# ---------------------------------------------------------------------------
# Install locally
# ---------------------------------------------------------------------------
install:         ## Install the package into the local R library
	$(R) CMD INSTALL $(PKG_DIR)

# ---------------------------------------------------------------------------
# pkgdown website
# ---------------------------------------------------------------------------
pkgdown:         ## Build pkgdown documentation website → docs/
	$(RSCRIPT) -e "pkgdown::build_site('$(PKG_DIR)')"

# ---------------------------------------------------------------------------
# Clean
# ---------------------------------------------------------------------------
clean:           ## Remove R CMD check artefacts and build output
	$(R) CMD check --clean $(PKG_NAME)
	rm -rf $(PKG_NAME).Rcheck
	rm -f $(PKG_NAME)_*.tar.gz
	rm -rf $(BUILD_DIR)
