# Data provenance: pinning imaging data

A Guix manifest pins the software of an experiment.  To reproduce a
paper you also need the *data* it used.  `(systole idc-download)` adds an
origin method, `idc-fetch`, that pins a cohort from the NCI Imaging Data
Commons (IDC) the way `git-fetch` pins a source tree:

| `git-fetch`            | `idc-fetch`                              |
|------------------------|------------------------------------------|
| repository URL         | IDC index release (`idc-index-data`)     |
| commit                 | SQL query over that release              |
| `sha256` of the tree   | `sha256` of the downloaded cohort        |

The query says *what you asked for* (provenance); the hash proves *what
you got* (reproducibility).  Both live in one ordinary `origin`.

## Example

```scheme
(use-modules (guix packages) (systole idc-download))

(define lidc-ct-cohort
  (origin
    (method idc-fetch)
    (uri (idc-reference
          (release "25.0.5")
          (query "SELECT SeriesInstanceUID FROM idc_index
                  WHERE collection_id = 'lidc_idri'
                    AND Modality = 'CT'
                    AND license_short_name LIKE 'CC BY %'
                  ORDER BY series_size_MB, SeriesInstanceUID
                  LIMIT 2")))
    (file-name "lidc-ct-cohort")
    (sha256 (base32 "0000000000000000000000000000000000000000000000000000"))))
```

Get the hash the usual Guix way: build once with a placeholder, then copy
the `actual hash` from the mismatch error.

```bash
guix build -L systole -f cohort.scm        # fails, prints the actual hash
# paste it into (sha256 (base32 "..."))
guix build -L systole -f cohort.scm        # downloads and verifies
guix build -L systole --check -f cohort.scm   # re-downloads, must match
```

The output contains one directory per series, named by its DICOM
`SeriesInstanceUID`, and `manifest.csv` with each series'
`SeriesInstanceUID`, `crdc_series_uuid`, `aws_bucket`, `collection_id`,
`license_short_name` and `source_DOI`.

## How it works

1. The release's `idc-index-data` wheel is fetched as a fixed-output
   origin, pinned by the SHA-256 that PyPI publishes
   (`%idc-index-releases`).
2. The query runs offline inside the build with DuckDB, against the
   wheel's `idc_index.parquet` (table `idc_index`).  It must return a
   `SeriesInstanceUID` column; any column of the index can filter the
   cohort (collection, modality, body part, age, sex, manufacturer,
   licence, ...).
3. Each selected series is copied from IDC's public bucket with
   `s5cmd --no-sign-request`.
4. Guix checks the recursive SHA-256 of the result.  Because IDC never
   edits a file in place (a change gets a new UUID, and old versions stay
   available), the same query on the same release yields the same bytes.

The download tools are build inputs of a fixed-output derivation, so
upgrading `duckdb` or `s5cmd` does not change the cohort's store path.

## Licences and substitutes

IDC data is public, but licences differ per collection (mostly CC BY;
some CC BY-NC).  `manifest.csv` carries each series' licence and DOI for
attribution, and the licence can be part of the query.

Outputs are **not substitutable** by default (`substitutable? #f`):
a substitute server would redistribute patient imaging, which matters
for non-commercial licences and for data IDC later retracts (for example
when protected health information is found).  Set
`(substitutable? #t)` in the `idc-reference` only for data you may
redistribute.

## Adding an IDC release

Add an entry to `%idc-index-releases` in
`systole/systole/idc-download.scm` with the wheel URL and the SHA-256
listed on <https://pypi.org/project/idc-index-data/>.  Existing cohorts
keep working: each one names its release.

## Limitations

- Only the columns of the offline index can be queried.  Finer DICOM
  attributes and clinical data live in IDC's BigQuery tables, which need
  a Google account and cannot run in the build sandbox.
- A cohort is one download: changing the query changes the whole hash.
  Large collections belong in several smaller cohorts.
- The data lives in `/gnu/store`; plan disk space and GC roots
  accordingly.
