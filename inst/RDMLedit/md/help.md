# RDML7 Editor

The RDML7 editor is the S7-native successor to the original `RDMLedit` Shiny
application.

## Files

The Files tab loads any format supported by `readRDML()`, keeps several
documents in memory, creates RDML 1.3 documents, merges objects and writes the
active object with `writeRDML()`.

## Metadata

The first metadata panels edit RDML ID, experimenter, documentation, dye,
sample and target master records.

### Experiment / Run / React / Data

The hierarchy editor follows the RDML structure directly:

`experiment в†’ run в†’ react в†’ data`

The Data panel edits `cq`, vector `meltTemp`, exclusion text, endpoint,
background fluorescence/slope and quantification fluorescence while preserving
existing amplification and melting datapoints.

Multiple melting temperatures can be entered as semicolon-separated values,
for example:

`87.8;82.2`

## qPCR

The qPCR tab restores the analysis controls of the original editor:

- optional preprocessing;
- LOWESS, moving average, Savitzky-Golay, spline, SuperSmoother and Whittaker
  smoothers;
- none/min-max/maximum/quantile/z-score normalization;
- Cq by threshold or SDM;
- automatic or per-target manual threshold;
- hook detection by `hookreg`, `hookregNL` or both;
- filtering by experiment, run, target and plate position;
- grouping by experiment, run, target, dye, sample, sample type or position;
- raw-curve restoration;
- per-curve and replicate Cq display.

Processed fluorescence values are written into the active RDML7 object, like
the original RDMLedit. The unprocessed curves are kept in session memory so
they can be restored before the application is closed.

## Melting curves

The melting tab restores the original controls:

- preprocessing on/off;
- background correction and temperature background range;
- min-max normalization;
- smoothing factor;
- experiment/run/target/position filtering;
- fluorescence and derivative plots;
- raw-curve restoration.

## Analysis packages

The editor uses the same analysis ecosystem as the legacy application, but
without modifying RDML7 classes:

- `chipPCR`: amplification smoothing/normalization and threshold Cq;
- `MBmca`: SDM and melting-curve preprocessing/derivatives;
- `PCRedux`: hook-effect detection;
- `plotly`: interactive curve plots;
- `DT`: interactive tables.

These packages are GUI dependencies, not part of the RDML7 object model.
