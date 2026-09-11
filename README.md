
# criteriobrasil

<!-- badges: start -->
<!-- badges: end -->

The goal of `criteriobrasil` is to provide tidy tables from the Brazilian
Economic Classification Criterion (CCEB), published by ABEP. The package will
bring the scoring rules, class cutoffs, income estimates, geographic
distributions, and edition metadata into a consistent R interface.

This repository is currently at the package-scaffold stage. The data pipeline
will download source PDFs without versioning them, record their hashes in a
manifest, extract the three CCEB regimes, validate the resulting tables, and
publish package data and release assets.

## Installation

You can install the development version of criteriobrasil like so:

``` r
remotes::install_github("viniciusreginatto/criteriobrasil")
```

## Example

The planned API will use English function names with the `cceb_` prefix:

``` r
cceb_editions()
cceb_edition_for("2024-01-01")
cceb_points(edition = 2024)
```

## Data workflow

The source and build workflow lives in `data-raw/`:

1. download the ABEP PDFs and update `manifest.csv`;
2. extract each CCEB regime;
3. validate table integrity and compare every result with a source fingerprint;
4. write package data to `data/` and release assets.

Downloaded PDFs are ignored by Git and are never committed to the package.
