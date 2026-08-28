# RDML

[![Published in Bioinformatics](https://img.shields.io/badge/published%20in-Bioinformatics-ff69b4.svg?style=flat)](https://doi.org/10.1093/bioinformatics/btx528)

`RDML` is an R package for reading, manipulating, validating, converting, and
writing quantitative PCR data. The current implementation uses S7 value
objects, a camelCase functional API, and an extensible file-format registry.

The original RDML package is described in:

> Stefan Rödiger, Michał Burdukiewicz, Andrej-Nikolai Spiess, Konstantin
> Blagodatskikh. *Enabling reproducible real-time quantitative PCR research:
> the RDML package*. Bioinformatics. https://doi.org/10.1093/bioinformatics/btx528

## Installation

```r
install.packages("remotes")
remotes::install_github("kablag/RDML7")
```

## Basic workflow

```r
library(RDML)

x <- rdmlRead("experiment.rdml")

rdmlSummary(x)
rdmlValidate(x)

metadata <- asTable(x)

amp <- getFData(
  x,
  dpType = "adp",
  longTable = TRUE
)

rdmlWrite(
  x,
  "experiment-copy.rdml"
)
```

RDML uses value semantics:

```r
x <- setFData(
  x,
  fdata,
  description,
  fdataType = "adp"
)
```

## Formats

```r
rdmlFormats()
```

Additional readers/writers can be registered with `rdmlRegisterFormat()` or
loaded from module files with `rdmlLoadModule()`.

## RDES

RDES import and export are supported:

```r
x <- rdmlRead(
  "run_amplification.tsv",
  format = "rdes"
)

x <- rdmlRead(
  "run_amplification.tsv",
  format = "rdes",
  companionFile = "run_melting.tsv"
)

rdmlWrite(
  x,
  "run.tsv",
  format = "rdes",
  rdesType = "both"
)
```

See `vignette("RDES", package = "RDML")`.

## Legacy compatibility

Older names such as `AsTable()`, `GetFData()`, `SetFData()`, `MergeRDMLs()`,
and `rdml_read()` remain available for dependent packages. New code should use
the camelCase API.
