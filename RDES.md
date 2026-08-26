# RDES support

RDML7 supports the Real-time PCR Data Essential Spreadsheet Format (RDES) v1.0.

RDES is a UTF-8, tab-separated, LF-newline text format for exactly one qPCR
run. Amplification and melting data are stored in separate files.

## Import

Single amplification or melting file:

```r
x <- rdmlRead(
  "run_amplification.tsv",
  format = "rdes"
)
```

Import an amplification + melting pair into the same run:

```r
x <- rdmlRead(
  "run_amplification.tsv",
  format = "rdes",
  companionFile = "run_melting.tsv",
  expId = "experiment1",
  runId = "run1"
)
```

RDES is automatically detected for `.tsv`, and by header sniffing for `.csv`
or `.txt`. The content is still tab-separated even when the extension is
`.csv` or `.txt`, as specified by RDES.

## Export

If the selected run contains only one curve type:

```r
rdmlWrite(
  x,
  "run.tsv",
  format = "rdes"
)
```

Explicit amplification or melting export:

```r
rdmlWrite(
  x,
  "run.tsv",
  format = "rdes",
  expId = "experiment1",
  runId = "run1",
  rdesType = "adp"
)
```

Write both RDES files:

```r
paths <- rdmlWrite(
  x,
  "run.tsv",
  format = "rdes",
  expId = "experiment1",
  runId = "run1",
  rdesType = "both"
)
```

This creates:

```text
run_amplification.tsv
run_melting.tsv
```

## Multiple Tm values

RDES permits multiple Tm values separated by semicolons. The current RDML7
`dataType` model stores one numeric `meltTemp`; therefore import keeps the first
Tm and warns. The full melting fluorescence curve is retained.
