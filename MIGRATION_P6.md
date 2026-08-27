# RDML P6 — stability, importer contract and semantic validation

P6 deliberately does **not** add XSD validation and does **not** add round-trip
format tests.

## 1. Canonical importer intermediate representation

Vendor parsers may now return `rdmlImportData` instead of constructing the RDML
tree themselves. `rdmlRead()` recognizes the intermediate object and calls
`rdmlBuildImport()` centrally.

```r
series <- rdmlImportSeries(
  fdataType = "adp",
  fdata = fdata,
  description = description
)

rdmlImportData(
  series = list(series),
  publisher = "My instrument",
  format = "my-format"
)
```

CSV, Excel, ABI, Rotor-Gene, FQD, DTprime and RDES built-ins use this route.
Native RDML/XML remains a direct rich-object parser because it already is the
canonical full-fidelity representation.

## 2. Semantic validation

```r
issues <- rdmlValidate(x)
rdmlIsValid(x)
```

Validation checks duplicate keys, master/reference integrity, nested
experiment/run/react/data keys and basic curve consistency.

## 3. Plugin API contract

`rdmlRegisterFormat()` now accepts:

```r
apiVersion = 1L
capabilities = c("adp", "mdp", "cq", "multiTm")
```

`rdmlFormats()` exposes both fields. Existing modules that omit them continue
to register as API version 1 with no declared capabilities.

## 4. Explicit lossy-conversion policy

```r
rdmlRead(file, loss = "warn")
rdmlWrite(x, file, loss = "error")
```

Allowed values are `warn`, `error`, and `allow`. Loss conditions have structured
classes (`rdmlLossWarning`, `rdmlLossError`) and a machine-readable `code`.

## 5. Multiple RDES Tm values

`dataType` keeps standard `meltTemp` plus package-only `meltTemps` for the full
RDES vector. RDES export writes all values joined by semicolons. RDML/XML export
cannot represent more than one `meltTemp`, so it signals a lossy conversion
according to `loss=`. `meltTemps` is never serialized as an RDML XML element.

## 6. Compact summaries

```r
rdmlSummary(x)
print(x)
```

The summary reports experiments, runs, reactions, data entries, samples,
targets, amplification/melting curves and Cq counts, plus a per-run table.

## 7. CI

`.github/workflows/R-CMD-check.yaml` checks R release/devel on Linux and release
on Windows/macOS. It also runs `devtools::document()` and fails if generated
DESCRIPTION/NAMESPACE/man metadata differs from committed files.

## Apply

```bat
apply-p6.cmd "C:\path\to\RDML7"
```

Then restart R and run:

```r
devtools::load_all(".")
rdmlFormats()
devtools::test()
devtools::document()
```
