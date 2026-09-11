# source verification rejects a changed PDF

    Code
      verify_source_files(manifest_path, directory, edition_ids = 2026L)
    Condition
      Error in `verify_source_files()`:
      ! Source PDF hash mismatch: 2026/pt (source.pdf).
      i Review the changed source and use the explicit refresh workflow if it is intentional.

# ordinary manifest updates reject changed source identity

    Code
      update_manifest(link, pdf_path, manifest_path)
    Condition
      Error in `update_manifest()`:
      ! The manifest entry for edition 2026/pt changed.
      i Use the explicit source-refresh workflow after reviewing the new PDF.

