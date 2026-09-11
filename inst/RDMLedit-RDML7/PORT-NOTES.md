# RDMLedit → RDML7 port notes

This directory is a first native S7 port of the legacy application at:

https://github.com/PCRuniversum/RDML/tree/master/inst/RDMLedit

## Legacy → RDML7 API mapping

| Legacy RDMLedit | RDML7 port |
| --- | --- |
| `library(RDML)` | package namespace `RDML7::` |
| `RDML$new(file)` | `readRDML(file)` |
| `RDML$new()` | `rdmlType(version = "1.3")` |
| `$copy()` | ordinary R/S7 copy (`serialize` clone in editor state) |
| `MergeRDMLs()` | `mergeRDMLs()` |
| `$AsDendrogram()` | `asDendrogram()` |
| `$GetFData()` | `getFData()` |
| `$SetFData()` | `setFData()` |
| `$AsXML()` / old save | `writeRDML()` |
| `fooType$new(...)` | `fooType(...)` |
| R6 `$set()` extensions | pure helper functions |

## Why `rdml.extensions.R` was removed

The legacy file mutates the R6 `dataType` class at application start. This is
not a good fit for S7 and makes the package's object model depend on whether a
GUI happened to be launched.

For RDML7, preprocessing should have a shape like:

```r
processed <- preprocessAdp(curve, ...)
x <- setFData(x, processed$fdata, processed$description)
```

or use a dedicated RDML7 action generic if preprocessing becomes part of the
public package API.

## DESCRIPTION

At minimum add:

```text
Suggests:
    shiny,
    shinythemes
```

The core port intentionally avoids the large dependency set of the old editor.

## Next porting layer

The old UI also contains detailed editors for:

- sample xRef/annotation/quantity/cDNA/template quantity;
- target xRef/sequences/commercial assay;
- thermal cycling conditions and steps;
- experiment/run/react/data;
- qPCR preprocessing and Cq/hook analysis;
- melting preprocessing.

Those should be ported as small Shiny modules using S7 `set_props()` and
RDML7 public actions rather than by reintroducing mutable R6 behaviour.


## Full qPCR / melting port

The second port restores the analysis controls of the legacy app while keeping
analysis state outside the S7 classes.

Legacy mutable fields `rawAdp` and `rawMdp` are replaced by per-session backup
stores keyed by document and curve path. Preprocessing writes into the active
S7 curve but the original `fpoints` remain available to the Restore Raw buttons.

Algorithms map as follows:

| Legacy method | RDML7 editor |
| --- | --- |
| `data$PreprocessAdp()` | `editor_preprocess_adp()` + immutable S7 write-back |
| `data$CalcCq()` | `editor_calc_cq()` + `S7::set_props()` |
| `data$DetectHook()` | `editor_detect_hook()` |
| `data$PreprocessMdp()` | `editor_preprocess_mdp()` + immutable S7 write-back |
| `rawAdp` / `rawMdp` | Shiny session `rawCurves` store |

The old `shinyMolBio` PCR plate widget is intentionally not required. The port
uses position multi-selection, so the editor does not depend on a package that
is unrelated to RDML7 itself.
