# NeAtlIBTS64 0.0.5

## New functions

* `SpeciesHLperYear()`: summarizes DATRAS HL data by species, with
  length ranges, number of measurements, scaled catches and number of
  hauls. Useful for exploring length-data availability by
  survey/year/quarter.
* `GetAlkDTR.NeAtl64()`: downloads DATRAS CA data for a species,
  survey, year and quarter, and formats it ready to build an ALK.
* `GrafAlk.NeAtl64()`: plots the age-length key as stacked bars of
  age proportions, fed with DATRAS data. Automatic title with the
  scientific name from WoRMS.

## Improvements

* `SpeciesCAperYear()`: new default output (`out = "summary"`) with a
  data.frame per species including total N, N aged, length range and
  age range. `out = "matrix"` option to keep the legacy behaviour.

## Cleanup

* Removed `MapLengths_nc()`: obsolete version that read local CSVs,
  replaced by `MapLengths()`, which pulls directly from DATRAS.
* `%>%` registered at package level via `usethis::use_pipe()`, no
  longer needs to be imported in each function.
* Examples for functions that require DATRAS access or local data
  wrapped in `\dontrun{}` (gearPlot*, qcHauls*, SplitLengths*,
  SurveyMap.IBTS*, MapLengths, getDatras2).

# NeAtlIBTS64 0.0.6

## New functions
* `AuditAlkCAMP_DATRAS()`: compares otoliths by length class between
  the local CAMP database (via GetAlk.camp64) and DATRAS, flagging
  discrepancies and marking length classes with imputed data (IEO
  flag 99/100).

## Improvements
* `GetAlkDTR.NeAtl64()`: stores n_por_talla as an attribute before
  computing proportions, allowing direct nested calls inside
  GrafAlk.NeAtl64 without losing the n shown above the bars.
* `GrafAlk.NeAtl64()`: now accepts two input formats — aggregated
  (talla, sexo, E0..Eplus+, the output of GetAlkDTR.NeAtl64) and
  individual (LngtClasscm, Age, raw CA records).

# NeAtlIBTS64 0.1.0

## New functions

* `TalAge.NeAtl64()`: length-at-age plot by year (several years) with
  `lattice::xyplot`, one panel per year, from DATRAS CA data.
  Complements `GrafAlk.NeAtl64()` (which summarizes as stacked bars for
  a single survey/year/quarter) by showing the length-age scatter and
  allowing years to be compared side by side in panels.

## Improvements

* `gearPlotHH.dodp/.wgdp/.nodp/.dowg/.wgdo()` (and their SH/NS
  variants): the dashed bands around the fitted curve are no longer
  calculated by incorrectly combining the `confint()` bounds of each
  coefficient separately, which did not correspond to any real
  confidence level for the curve. They are now calculated with
  `predict.lm(interval=)` (linear models) or the delta method (`nls`
  models), giving a statistically valid interval at the level
  requested in `c.int`/`c.inta`/`c.intb`.
* New `int.type` parameter in those same functions: `"prediction"`
  (the default, appropriate for flagging hauls with an abnormal gear
  geometry), `"confidence"` or `"both"`.
* Those functions now label the plot itself with the level and
  `int.type` actually used (a single label when `c.inta`/`c.intb`
  match, or one per curve, in that curve's color, when they differ).
* `qcHaulsDist()`: rewritten to support multi-country surveys such as
  NS-IBTS (one panel per country), with new `plots`, `esc.mult`
  parameters and the option to save the plot to a `.png` file.
* `IBTSSurveySummary()`: new `extra_surveys` parameter to add or force
  surveys outside the default IBTSWG list.
* `GetAlkDTR.NeAtl64()`: new `plus` (terminal age group) and `n.ots`
  (return otolith counts instead of proportions) parameters; supports
  direct nesting with `GrafAlk.NeAtl64()`.
