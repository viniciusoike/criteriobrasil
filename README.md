# criteriobrasil

<!-- badges: start -->
[![R-CMD-check](https://github.com/viniciusoike/criteriobrasil/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/viniciusoike/criteriobrasil/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/viniciusoike/criteriobrasil/graph/badge.svg)](https://app.codecov.io/gh/viniciusoike/criteriobrasil)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`criteriobrasil` provides tidy tables from the Brazilian Economic
Classification Criterion (CCEB, or Critério Brasil), published by ABEP. It
includes the scoring rules, class cutoffs, income estimates, and geographic
distribution for the ingested editions, plus edition metadata in a consistent
R interface.

The package is currently **experimental**. The data tables are in place, but
the function API and column layout may still change.

## Installation

You can install the development version of `criteriobrasil` like so:

``` r
remotes::install_github("viniciusoike/criteriobrasil")
```

## Example

The package provides five tables: `editions`, `points`, `cutoffs`, `income`,
and `distribution`.

``` r
library(criteriobrasil)

# Tables and editions currently available
cceb_list_tables()

# Class cutoffs for the 2026 edition
cceb_get("cutoffs", edition = 2026)
```

## Data workflow

The source and build workflow in `data-raw/` downloads the ABEP PDFs without
versioning them, records their hashes in a manifest, extracts each CCEB
regime, validates the tables, and prepares the release assets:

1. download the ABEP PDFs and update `manifest.csv`;
2. extract each CCEB regime;
3. validate table integrity and compare every result with a source fingerprint;
4. write the internal table registry and prepare the `.rds` release assets.

Each package version downloads the tables from a fixed GitHub release tag. The
package itself contains only the table registry.

Downloaded PDFs are ignored by Git and are never committed to the package.
