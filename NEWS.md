Known issues: <https://github.com/FOR-CAST/HSI_Caribou_MB/issues>

# HSI_Caribou_MB (development version)

* **Replaced the never-run test stub with metadata tests, and added testthat CI.** `tests/testthat/test-template.R` was the SpaDES boilerplate, unedited: paths from another machine, calls to `Event1`/`Event2` functions this module does not define, and assertions against placeholder strings. It had never been run and would have failed instantly, while making the module look tested. In its place, characterization tests over the module's public contract -- the input and output object names and classes, and the parameter names -- which is what a project binds to and what nothing checked until now. The expectations are GENERATED from the module's live metadata rather than transcribed, and were verified to fail when the contract changes.
* The contract test pins a **defect**: `outputObjects` has one row whose `objectName` is `NA`, i.e. `createsOutput()` was called without a name, so nothing downstream can bind to this module's output by name. The test asserts the current state rather than the intended one, so fixing it will fail the test and force the contract to be written down. The module also still takes `ml` (class `map`, the deprecated package) and `speciesLayers` as a `RasterStack`, so the terra migration has not reached it.

* Initial module: Manitoba caribou habitat-suitability-index (HSI) calculations over reporting polygons, including HSI plots.
* Fixed a partial-match warning by renaming the `rasterListByPoly()` argument `poly` to `polys` (2024-02).
* Documentation: use `full_width = FALSE` for tables in the PDF manual.
