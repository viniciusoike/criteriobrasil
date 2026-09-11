# cceb_get validates its arguments before downloading

    Code
      cceb_get("unknown")
    Condition
      Error in `cceb_validate_table()`:
      ! `table` must be one available table or "all".
      i Available tables: editions, points, cutoffs, income, distribution, all.

---

    Code
      cceb_get(table = NA_character_)
    Condition
      Error in `cceb_validate_table()`:
      ! `table` must be one available table or "all".
      i Available tables: editions, points, cutoffs, income, distribution, all.

---

    Code
      cceb_get(edition = "2024")
    Condition
      Error in `cceb_validate_edition()`:
      ! `edition` must be `NULL`, one whole number, or "all".

---

    Code
      cceb_get(edition = c(2024, 2026))
    Condition
      Error in `cceb_validate_edition()`:
      ! `edition` must be `NULL`, one whole number, or "all".

---

    Code
      cceb_get("income", edition = 2015)
    Condition
      Error in `cceb_validate_availability()`:
      ! Edition 2015 is not available for table "income".
      i Available editions: 2003, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2016, 2018, 2019, 2020, 2021, 2022, 2024, 2026.

---

    Code
      cceb_get("all", edition = 2015)
    Condition
      Error in `cceb_validate_availability()`:
      ! Edition 2015 is not available for table "income".
      i Available editions: 2003, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2016, 2018, 2019, 2020, 2021, 2022, 2024, 2026.

# cceb_get reports download and asset errors

    Code
      cceb_get("points")
    Condition
      Error in `cceb_fetch_asset()`:
      ! Could not download CCEB table "points".
      i Release asset: https://github.com/viniciusoike/criteriobrasil/releases/download/data-2026-09-11/cceb_points.rds
      Caused by error in `.cceb_download_file()`:
      ! network unavailable

---

    Code
      cceb_get(table = "points")
    Condition
      Error in `cceb_validate_asset()`:
      ! Release asset for table "points" has an unexpected schema.
      i Expected columns: edition_id, block, variable, label_pt, label_en, level, level_order, points.
      i Found columns: wrong.

---

    Code
      cceb_get("points", edition = NULL)
    Condition
      Error in `cceb_validate_asset()`:
      ! Release asset for table "points" has unexpected editions.
      i Expected editions: 2003, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2020, 2021, 2022, 2024, 2026.
      i Found editions: 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2020, 2021, 2022, 2024, 2026, 9999.

