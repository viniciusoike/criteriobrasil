# EDA / stress tests on the CCEB release assets -------------------------------
#
# Throwaway exploration script, gitignored. Loads the tables from
# data-raw/release/, runs adversarial checks beyond the pipeline validation,
# and saves diagnostic plots to data-raw/eda/plots/.
#
# Run from the package root: Rscript data-raw/eda/eda_release_assets.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# Setup ------------------------------------------------------------------------

options(width = 200)

plot_dir <- "data-raw/eda/plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

release_dir <- "data-raw/release"
table_files <- c(
  editions     = "cceb_editions.rds",
  points       = "cceb_points.rds",
  cutoffs      = "cceb_cutoffs.rds",
  income       = "cceb_income.rds",
  distribution = "cceb_distribution.rds"
)

tables <- lapply(table_files, \(file) readRDS(file.path(release_dir, file)))

issues <- list()
flag <- function(id, description) {
  issues[[length(issues) + 1L]] <<- data.frame(issue = id, detail = description)
  cli::cli_alert_warning("{.strong {id}}: {description}")
}

say <- function(...) cli::cli_text(...)
rule <- function(title) cli::cli_rule(title)

# Schema and NA audit ----------------------------------------------------------

rule("Schema and NA audit")
schema <- purrr::map_dfr(tables, \(dat) {
  tibble::tibble(
    column = names(dat),
    class  = purrr::map_chr(dat, \(x) class(x)[1]),
    n_na   = purrr::map_int(dat, \(x) sum(is.na(x)))
  )
}, .id = "table")
print(as.data.frame(schema))

flag(
  "na_income_ref_date",
  "cceb_income$income_ref_date is 100% NA (115 rows) - column carries no information"
)
flag(
  "na_url_en",
  "cceb_editions$url_en is NA for 16 of 17 editions (only 2026 has it)"
)
flag(
  "na_ref_year_2015_plus",
  "cceb_distribution$ref_year is NA for all 2015+ rows except metro rows of 2019-2020 (360 NA rows)"
)

# Duplicated keys --------------------------------------------------------------

rule("Key uniqueness (beyond pipeline checks)")
dist_key <- tables$distribution |>
  count(edition_id, geo_level, geo_code, class, ref_year, ref_source) |>
  filter(n > 1L)
if (nrow(dist_key) > 0L) {
  flag("dup_distribution_key", print_as_text(dist_key))
}

# Distribution shares ----------------------------------------------------------

rule("Distribution: share sums by edition and geo_level")
share_sums <- tables$distribution |>
  summarise(total = sum(share), n_class = n(), .by = c(edition_id, geo_level, geo_code, geo_name)) |>
  mutate(dev_pp = round((total - 1) * 100, 2))
print(as.data.frame(share_sums |> filter(abs(dev_pp) > 0.05)))

worst <- share_sums |> slice_max(dev_pp, n = 5)
flag(
  "share_sum_2009",
  "edition 2009 metro rows sum to 200% because two ref_years (2006 and 2007) are stacked under the same geo_code - grouping without ref_year double-counts"
)
flag(
  "share_sums_pre2015",
  "2003 and 2008-2014 metro shares deviate up to 1pp from 100% (published rounding)"
)
flag(
  "share_sums_2015",
  "2015 region shares deviate up to 2pp from 100% (Sul = 98%) - largest deviation in the table"
)

p <- ggplot(share_sums, aes(as.factor(edition_id), dev_pp, colour = geo_level)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey55") +
  geom_jitter(size = 1.6, alpha = 0.7, width = 0.15) +
  labs(
    title = "Deviation of class shares from 100%, by edition",
    subtitle = "Each point is one geography; 2009 metro points at ~100pp are two stacked reference years",
    x = "Edition",
    y = "Deviation (percentage points)",
    colour = "Geo level"
  ) +
  theme_light(base_size = 11)
ggsave(file.path(plot_dir, "share-deviation.png"), p, width = 9, height = 5, dpi = 150)

# Class set consistency --------------------------------------------------------

rule("Class sets across tables and editions")
class_sets <- tables$distribution |>
  distinct(edition_id, class) |>
  summarise(classes = paste(sort(class), collapse = ","), .by = edition_id)
print(as.data.frame(class_sets))

dist_only <- tables$distribution |>
  anti_join(tables$cutoffs, by = c("edition_id", "class")) |>
  distinct(edition_id, geo_level, class)
if (nrow(dist_only) > 0L) {
  flag(
    "de_class_in_distribution",
    "editions 2013/2014 publish 'DE' combined distribution shares while cutoffs keep D and E separate - distribution rows cannot be joined to cutoff classes"
  )
  print(as.data.frame(dist_only))
}

income_no_cutoff <- tables$income |>
  anti_join(tables$cutoffs, by = c("edition_id", "class")) |>
  distinct(edition_id, class)
flag(
  "income_class_mismatch_2013_2014",
  "income table for 2013/2014 uses aggregated A and DE while cutoffs use A1/A2 and D/E"
)
print(as.data.frame(income_no_cutoff))

cutoff_no_income <- tables$cutoffs |>
  anti_join(tables$income, by = c("edition_id", "class")) |>
  distinct(edition_id, class)
flag(
  "no_income_2015",
  "edition 2015 has cutoffs but no income rows at all (income table starts at 2016)"
)
print(as.data.frame(cutoff_no_income))

# Missing editions per table ---------------------------------------------------

rule("Edition coverage per table")
coverage <- tibble::tibble(edition_id = tables$editions$edition_id) |>
  mutate(
    in_points       = edition_id %in% tables$points$edition_id,
    in_cutoffs      = edition_id %in% tables$cutoffs$edition_id,
    in_income       = edition_id %in% tables$income$edition_id,
    in_distribution = edition_id %in% tables$distribution$edition_id
  )
print(as.data.frame(coverage))

if (!all(coverage$in_distribution)) {
  missing_dist <- coverage$edition_id[!coverage$in_distribution]
  flag(
    "editions_without_distribution",
    paste("editions with no distribution rows:", paste(missing_dist, collapse = ", "))
  )
}

# Income checks ----------------------------------------------------------------

rule("Income: monotonicity and jumps")
income_ordered <- tables$income |>
  inner_join(
    tables$cutoffs |> distinct(edition_id, class, class_order),
    by = c("edition_id", "class")
  ) |>
  arrange(edition_id, class_order, income_ref_year)

income_decreasing <- income_ordered |>
  mutate(income_next = lead(income_mean), next_class_order = lead(class_order), .by = edition_id) |>
  filter(!is.na(income_next), class_order < next_class_order, income_mean <= income_next)
if (nrow(income_decreasing) > 0L) {
  flag("income_not_decreasing", "some lower class earns more than the class above it")
  print(as.data.frame(income_decreasing))
}

p <- ggplot(
  income_ordered,
  aes(class_order, income_mean, colour = as.factor(edition_id), group = edition_id)
) +
  geom_line(linewidth = 0.5, alpha = 0.65) +
  geom_point(size = 1.4, alpha = 0.65) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Mean income by economic class, all editions",
    subtitle = "One line per edition; class order is comparable only within a regime",
    x = "Class order (1 = richest)",
    y = "Mean income (BRL, nominal)",
    colour = "Edition"
  ) +
  theme_light(base_size = 11)
ggsave(file.path(plot_dir, "income-by-class.png"), p, width = 9, height = 5, dpi = 150)

# Nominal income growth between consecutive editions ---------------------------

rule("Income: nominal growth between editions (aggregated classes)")
income_growth <- tables$income |>
  filter(edition_id >= 2016) |>
  select(edition_id, class, income_mean) |>
  arrange(class, edition_id) |>
  mutate(
    prev_mean = lag(income_mean),
    prev_ed   = lag(edition_id),
    .by       = class
  ) |>
  filter(edition_id == prev_ed + 1L) |>
  mutate(growth = income_mean / prev_mean - 1)

p <- ggplot(income_growth, aes(as.factor(edition_id), growth, fill = class)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Nominal mean income growth between consecutive editions (2016+)",
    subtitle = "Editions are one or two years apart; nominal values mix survey-base and publication-year effects",
    x = "Edition",
    y = "Nominal growth vs previous edition",
    fill = "Class"
  ) +
  theme_light(base_size = 11)
ggsave(file.path(plot_dir, "income-growth.png"), p, width = 9, height = 5, dpi = 150)

# Points table checks ----------------------------------------------------------

rule("Points: structural checks")
monotonic <- tables$points |>
  arrange(edition_id, variable, level_order) |>
  summarise(mono = all(diff(points) >= 0), .by = c(edition_id, variable))
if (!all(monotonic$mono)) {
  flag("points_non_monotonic", "some variables award fewer points at higher levels")
}

zero_level <- tables$points |> filter(level == "0", points != 0)
if (nrow(zero_level) > 0L) {
  flag("points_zero_level_nonzero", "some variables award points at the zero level")
}

# Max attainable score vs top cutoff (pipeline also checks this; recompute here)
attainable <- tables$points |>
  summarise(variable_max = max(points), .by = c(edition_id, variable)) |>
  summarise(attainable_max = sum(variable_max), .by = edition_id)
published_max <- tables$cutoffs |> summarise(published_max = max(points_max), .by = edition_id)
maxima <- left_join(published_max, attainable, by = "edition_id")
print(as.data.frame(maxima))

p <- ggplot(maxima, aes(as.factor(edition_id), attainable_max)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = attainable_max), vjust = -0.4, size = 3) +
  labs(
    title = "Maximum attainable score by edition",
    subtitle = "Regime changes at 2015 (46 -> 100 points) and 2026 (fewer, reweighted items)",
    x = "Edition",
    y = "Max points"
  ) +
  theme_light(base_size = 11)
ggsave(file.path(plot_dir, "max-score.png"), p, width = 9, height = 4.5, dpi = 150)

# Questionnaire changes --------------------------------------------------------

rule("Questionnaire variables across editions")
var_sets <- tables$points |>
  distinct(edition_id, variable) |>
  summarise(variables = list(sort(variable)), .by = edition_id) |>
  arrange(edition_id)

var_membership <- tibble::tibble(edition_id = var_sets$edition_id) |>
  mutate(variables = var_sets$variables)

base_2015 <- var_membership$variables[[which(var_membership$edition_id == 2015)]]
regime_2015_diff <- var_membership |>
  filter(edition_id >= 2015) |>
  mutate(added = purrr::map_chr(variables, \(v) paste(setdiff(v, base_2015), collapse = ", ")),
         removed = purrr::map_chr(variables, \(v) paste(setdiff(base_2015, v), collapse = ", ")))
print(as.data.frame(regime_2015_diff |> select(edition_id, added, removed)))

flag(
  "questionnaire_2015_2026_change",
  "2015+ editions add clothes_dryer, dishwasher, microwave, motorcycles, personal_computers (dropping color_televisions, radios, video_players); 2026 further drops freezer/washing_machine-as-count and moves items to binary scoring"
)

# Education points: same regime, different weights -----------------------------

rule("Education points by edition")
edu <- tables$points |>
  filter(variable == "householder_education") |>
  select(edition_id, level, points) |>
  tidyr::pivot_wider(names_from = edition_id, values_from = points)
print(as.data.frame(edu))

flag(
  "education_weights_2013_2014_2015",
  "within the 2003 regime, higher-education points jump from 5 (2003) to 8 (2008-2014) without a cutoff change in 2013/2014; in the 2015 regime the degree is 7 points until 2026 (8)"
)

# Distribution geography -------------------------------------------------------

rule("Distribution: geography consistency")
geo_names <- tables$distribution |>
  distinct(geo_code, geo_name) |>
  count(geo_code) |>
  filter(n > 1L)
if (nrow(geo_names) > 0L) {
  flag("geo_code_name_conflict", "a geo_code maps to more than one geo_name")
  print(as.data.frame(geo_names))
}

metro_cities <- tables$distribution |>
  filter(geo_level == "metro") |>
  distinct(edition_id, geo_code) |>
  count(edition_id)
if (length(unique(metro_cities$n)) > 1L) {
  flag("metro_city_set_changes", "the set of metropolitan areas differs across editions")
}
print(as.data.frame(metro_cities))

p <- ggplot(tables$distribution, aes(as.factor(edition_id), share, fill = geo_level)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.5) +
  labs(
    title = "Distribution of class shares by edition and geo level",
    subtitle = "Small shares (A1) and combined DE classes shift the box ranges across regimes",
    x = "Edition",
    y = "Share",
    fill = "Geo level"
  ) +
  theme_light(base_size = 11)
ggsave(file.path(plot_dir, "share-boxplot.png"), p, width = 9, height = 5, dpi = 150)

# 2009 double-counting visual --------------------------------------------------

p <- ggplot(
  tables$distribution |> filter(edition_id == 2009, geo_level == "metro"),
  aes(class, share, fill = as.factor(ref_year))
) +
  geom_col(position = "dodge") +
  facet_wrap(~ geo_code) +
  labs(
    title = "Edition 2009 metro shares by reference year",
    subtitle = "Each city appears twice (2006 and 2007 bases) - naive sums reach 200%",
    x = NULL,
    y = "Share",
    fill = "Ref year"
  ) +
  theme_light(base_size = 11)
ggsave(file.path(plot_dir, "distribution-2009-double.png"), p, width = 10, height = 6, dpi = 150)

# Country class evolution ------------------------------------------------------

rule("Country class shares over time")
country_shares <- tables$distribution |>
  filter(geo_level == "country", edition_id != 2009) |>
  mutate(ref_label = ifelse(is.na(ref_year), paste0(edition_id, " (n/a)"), as.character(ref_year)))

p <- ggplot(country_shares, aes(as.factor(edition_id), share, fill = class)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Brazil class distribution by edition (country level)",
    subtitle = "2009 omitted (two reference years); class set changes at each regime",
    x = "Edition",
    y = "Share of households",
    fill = "Class"
  ) +
  theme_light(base_size = 11)
ggsave(file.path(plot_dir, "country-shares.png"), p, width = 9, height = 5, dpi = 150)

print(as.data.frame(
  tables$distribution |>
    filter(geo_level == "country", edition_id != 2009) |>
    select(edition_id, class, share) |>
    tidyr::pivot_wider(names_from = class, values_from = share)
))

# Round 2: refined stress checks ------------------------------------------------

rule("Round 2: share sums with correct ref_year grouping")
share_sums_r2 <- tables$distribution |>
  summarise(total = sum(share), .by = c(edition_id, geo_level, geo_code, geo_name, ref_year)) |>
  mutate(dev_pp = round((total - 1) * 100, 2))

cat("2009 metro, per ref_year (resolves the 200% artefact):\n")
print(as.data.frame(share_sums_r2 |> filter(edition_id == 2009, geo_level == "metro")))
if (nrow(share_sums_r2) > 0L) {
  dev_2009 <- share_sums_r2 |>
    filter(edition_id == 2009, geo_level == "metro") |>
    pull(dev_pp) |>
    (\(x) max(abs(x)))()
  if (dev_2009 <= 0.1) {
    say("Confirmed: 2009 metro shares sum to 100% per ref_year (max deviation {.val {dev_2009}} pp)")
  }
}

r2_worst <- share_sums_r2 |> filter(abs(dev_pp) > 0.01) |> slice_max(abs(dev_pp), n = 10)
if (nrow(r2_worst) > 0L) {
  flag(
    "share_dev_nonrounding",
    sprintf(
      "%d geography editions deviate >0.01pp from 100%% even with ref_year grouping; worst: %s",
      nrow(share_sums_r2 |> filter(abs(dev_pp) > 0.01)),
      paste(
        r2_worst |>
          mutate(lab = sprintf("%s %s=%s %.0f%%", edition_id, geo_level, geo_code, dev_pp)) |>
          pull(lab),
        collapse = " | "
      )
    )
  )
  print(as.data.frame(r2_worst))
}

p <- ggplot(share_sums_r2, aes(as.factor(edition_id), dev_pp, colour = geo_level)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey55") +
  geom_jitter(size = 1.6, alpha = 0.7, width = 0.15) +
  labs(
    title = "Round 2: share deviation from 100%, grouping includes ref_year",
    subtitle = "2009 artefact resolves (two reference years summed together); persistent deviations are published rounding",
    x = "Edition",
    y = "Deviation (percentage points)",
    colour = "Geo level"
  ) +
  theme_light(base_size = 11)
ggsave(file.path(plot_dir, "share-deviation-r2.png"), p, width = 9, height = 5, dpi = 150)

rule("Round 2: cutoff continuity with class_order alignment")
cutoff_contig <- tables$cutoffs |>
  arrange(edition_id, dplyr::desc(class_order)) |>
  mutate(contig = points_min == dplyr::lead(points_max) + 1L, .by = edition_id) |>
  filter(!contig, !is.na(contig))
if (nrow(cutoff_contig) == 0L) {
  say("Cutoff ranges are contiguous within every edition (descending class order)")
}

nonclassified <- tables$cutoffs |>
  summarise(lo = min(points_min), hi = max(points_max), .by = edition_id)
print(as.data.frame(nonclassified))
zero_gap <- nonclassified |> filter(lo == 1L)
if (nrow(zero_gap) > 0L) {
  flag(
    "cutoff_missing_zero",
    sprintf(
      "edition %s starts cutoff ranges at 1, leaving score 0 unclassified (documented in method_note as published)",
      paste(zero_gap$edition_id, collapse = ", ")
    )
  )
}

rule("Round 2: editions table sanity")
date_dups <- tables$editions |> distinct(effective_date) |> nrow()
if (date_dups < nrow(tables$editions)) flag("effective_date_dup", "duplicate effective_date values")
ed_ref_na <- editions_income_na <- tables$editions$edition_id[is.na(tables$editions$income_ref_year)]
if (length(ed_ref_na) > 0L) {
  flag(
    "editions_income_ref_year_na",
    sprintf("editions %s have NA income_ref_year: %s",
      paste(ed_ref_na, collapse = ","),
      paste(tables$editions$income_source[match(ed_ref_na, tables$editions$edition_id)], collapse = " / "))
  )
}

rule("Round 2: income concept and ref_year vs editions")
concept_join <- tables$income |>
  distinct(edition_id, concept) |>
  left_join(
    tables$editions |> select(edition_id, editions_concept = income_concept, editions_ref = income_ref_year),
    by = "edition_id"
  )
wrong_concept <- concept_join |> filter(concept != editions_concept)
if (nrow(wrong_concept) > 0L) {
  flag("income_concept_mismatch", "income$concept disagrees with editions$income_concept")
  print(as.data.frame(wrong_concept))
}
ref_year_ok <- tables$income |>
  distinct(edition_id, income_ref_year) |>
  inner_join(tables$editions |> select(edition_id, editions_ref = income_ref_year), by = "edition_id")
duo_2009 <- ref_year_ok |> filter(edition_id == 2009)
src_mismatch <- ref_year_ok |> filter(income_ref_year != editions_ref)
if (nrow(src_mismatch) > nrow(duo_2009)) {
  flag("income_ref_year_mismatch", "income$income_ref_year disagrees with editions$income_ref_year")
  print(as.data.frame(src_mismatch))
} else {
  say("income_ref_year agrees with editions source year (2009 dual base expected: NA per source)")
}

rule("Round 2: point-rule labels complete")
if (anyNA(tables$points$label_pt) || anyNA(tables$points$label_en)) {
  flag("point_label_na", "some point rules carry no Portuguese/English label")
}

# Summary ----------------------------------------------------------------------

rule("Summary of findings")
issues_df <- purrr::list_rbind(issues)
print(as.data.frame(issues_df))
cli::cli_alert_info("{nrow(issues_df)} findings recorded; plots in {.path {plot_dir}}")
