# RDML7 public API naming convention

## Rules

- Actions start with a verb: `readRDML()`, `validateRDML()`.
- Conversion/coercion uses standard R `as...`: `asTable()`, `asDendrogram()`, `asXML()`.
- Predicates start with `is...`: `isValidRDML()`.
- Helper-object constructors start with `new...`.
- RDML schema/S7 type constructors keep the schema noun name:
  `rdmlType()`, `sampleType()`, `targetType()`, `dataType()`, etc.
- Previous public names may remain only in the compatibility layer.

## Canonical rename map

| Current main | Canonical |
|---|---|
| `rdmlRead()` | `readRDML()` |
| `rdmlWrite()` | `writeRDML()` |
| `rdmlRegisterFormat()` | `registerRDMLFormat()` |
| `rdmlUnregisterFormat()` | `unregisterRDMLFormat()` |
| `rdmlDetectFormat()` | `detectRDMLFormat()` |
| `rdmlFormats()` | `listRDMLFormats()` |
| `rdmlLoadModule()` | `loadRDMLModule()` |
| `rdmlBuildImport()` | `buildRDMLImport()` |
| `rdmlFromFData()` | `buildRDMLFromFData()` |
| `rdmlLossRecord()` | `newRDMLLossRecord()` |
| `rdmlValidate()` | `validateRDML()` |
| `rdmlIsValid()` | `isValidRDML()` |
| `rdmlEdit()` | `editRDML()` |
| `mergeRdmls()` | `mergeRDMLs()` |
| `asXml()` | `asXML()` |
| `rdmlSummary()` | `summary()` |
| `rdmlImportSeries()` constructor use | `newRDMLImportSeries()` |
| `rdmlImportData()` constructor use | `newRDMLImportData()` |

## Names that stay

These already follow the convention or are schema class constructors:

`asTable()`, `asDendrogram()`, `getFData()`, `setFData()`,
`rdmlType()`, `rdmlIdType()`, `idType()`, `idReferenceType()`,
`experimentType()`, `runType()`, `reactType()`, `dataType()`,
`sampleType()`, `targetType()` and the other `*Type()` constructors.

Legacy `AsTable()`, `GetFData()`, `SetFData()`, `AsDendrogram()`,
`AsXML()`, `MergeRDMLs()`, and snake_case wrappers are compatibility API,
not canonical naming.
