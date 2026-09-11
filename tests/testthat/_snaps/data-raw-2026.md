# a missing source point row fails loudly

    Code
      extract_2026_raw_points(pdf)
    Condition
      Error in `map()`:
      i In index: 4.
      Caused by error in `.f()`:
      ! Expected one row for "Banheiros" in count items; found 0.

# a changed distribution total fails validation

    Code
      validate_2026_data(data)
    Condition
      Error in `validate_cceb_distribution()`:
      ! Distribution shares must sum to 100% within 2.5 percentage points.

