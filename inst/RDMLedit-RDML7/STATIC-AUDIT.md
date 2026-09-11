# Static audit — full RDML7 editor port

- OK: R6 runtime constructors removed
- OK: runtime class mutation removed
- OK: qPCR preprocessing controls
- OK: melting preprocessing controls
- OK: interactive shinyMolBio plate supported
- OK: plate fallback supported
- OK: Experiment/Run/React/Data editor
- OK: vector meltTemp editor

Delimiter counts:
- ui.R: (=139 )=139 {=0 }=0
- ui-full.R: (=187 )=187 {=3 }=3
- server.R: (=461 )=461 {=88 }=88
- server-full.R: (=757 )=757 {=200 }=200
- helpers.R: (=222 )=222 {=50 }=50
- analysis-helpers.R: (=508 )=508 {=132 }=132

R itself is not available in this execution environment, so final parse/load
testing must be performed in the RDML7 development checkout.


## fix2

- Empty qPCR catalogs now receive `character(0)` for `hook`, not a scalar
  `NA_character_`.
- S7 detection no longer uses `inherits(x, "S7_object")`.
- Property discovery uses `S7::props()` through `editor_s7_props()` /
  `editor_has_prop()`.
- This avoids silently treating valid RDML7 S7 objects as non-S7 values.

## fix3 — shinyMolBio

- OK: RDML7 pcrFormat adapter present
- OK: legacy RDML UI object created only in adapter
- OK: RDES plate inference present
- OK: qPCR adapter gets positions
- OK: melting adapter gets positions
- OK: no direct RDML7 pcrFormat to shinyMolBio
