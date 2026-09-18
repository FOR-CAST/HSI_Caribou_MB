## The module's metadata is its public contract: a project using this module binds to these
## object names and classes, and `reqdPkgs` states what it needs to run at all. These are
## CHARACTERIZATION tests -- they pin today's contract so a change to it has to be deliberate,
## rather than describing behaviour that did not exist before.
##
## GENERATED from the module's live metadata, then reviewed. When a change is intended, update
## this file in the same commit and bump the module version to match: removed, renamed or
## retyped is a MAJOR bump.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, moduleName)
})

test_that("inputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  inputs <- stats::setNames(md$inputObjects$objectClass, md$inputObjects$objectName)
  inputs <- inputs[!is.na(names(inputs))]
  expect_identical(
    inputs[order(tolower(names(inputs)))],
    c(
      "ml"            = "map",
      "speciesLayers" = "RasterStack",
      "sppEquiv"      = "data.table"
)
  )
})

test_that("the module declares no usable output objects", {
  ## Pinning a DEFECT, not a design: see the note in the module's NEWS. Every row of
  ## `outputObjects` has an NA `objectName`, i.e. `createsOutput()` was called without one,
  ## so nothing downstream can bind to this module's outputs by name.
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_equal(sum(!is.na(md$outputObjects$objectName)), 0L)
})

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_identical(
    sort(md$parameters$paramName),
    c(
      ".plotInitialTime", ".plotInterval", ".plots", ".saveInitialTime",
      ".saveInterval", ".seed", ".studyAreaName", ".useCache", "ageClassCutOffs",
      "ageClasses", "ageClassMaxAge", "disturbanceAgeCutoff", "reps", "sppEquivCol",
      "studyAreaNamesCol", "summaryInterval", "summaryPeriod", "upload", "uploadTo",
      "vegLeadingProportion"
)
  )
})

