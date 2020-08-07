### Tidy your data

This step does two main things:
1. Retains and renames required columns (e.g. PARAMETER to Variable)
1. Sets the timezone to PST
1. Filter out unwanted values of SAMPLE_CLASS and SAMPLE_STATE (e.g. blanks and replicates)

The underlying function being used in the `wqbc` R package is `tidy_ems_data`. The filtering is done separately.
