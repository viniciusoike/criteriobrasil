# duplicate point keys fail validation

    Code
      validate_cceb_data(data)
    Condition
      Error in `.check_unique_key()`:
      ! cceb_points has duplicate rows for key edition_id, variable, level.

# cutoff gaps fail validation

    Code
      validate_cceb_data(data)
    Condition
      Error in `validate_cceb_cutoffs()`:
      ! The cutoff ranges for edition 2026 overlap or contain gaps.

# invalid income values fail validation

    Code
      validate_cceb_data(data)
    Condition
      Error in `validate_cceb_income()`:
      ! Income means must be complete, finite, and positive.

# invalid distribution totals fail validation

    Code
      validate_cceb_data(data)
    Condition
      Error in `validate_cceb_distribution()`:
      ! Distribution shares must sum to 100% within 2.5 percentage points.

# incomplete distribution class sets fail validation

    Code
      validate_cceb_data(data)
    Condition
      Error in `validate_cceb_distribution()`:
      ! Every geography within an edition must contain the same class set.

# orphan editions fail relational validation

    Code
      validate_cceb_data(data)
    Condition
      Error in `validate_cceb_relations()`:
      ! cceb_income contains unknown editions: 9999.

