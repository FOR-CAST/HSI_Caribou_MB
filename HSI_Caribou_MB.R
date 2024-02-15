defineModule(sim, list(
  name = "HSI_Caribou_MB",
  description = "Manitoba caribou habitat sutibility metrics from NRV simulation models",
  keywords = "",
  authors = c(
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(HSI_Caribou_MB = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "HSI_Caribou_MB.Rmd"), ## same file
  reqdPkgs = list(
    "crayon", "dplyr", "exactextractr", "fs", "future.apply", "future.callr", "ggplot2",
    "PredictiveEcology/LandR@development",
    "PredictiveEcology/LandWebUtils@development (>= 0.1.5)",
    "PredictiveEcology/map@development",
    "raster", "sf", "sp",
    "PredictiveEcology/SpaDES.core@development (>=1.1.0)"
  ),
  parameters = bindrows(
    defineParameter("ageClasses", "character", LandWebUtils:::.ageClasses, NA, NA,
                    "descriptions/labels for age classes (seral stages)"),
    defineParameter("ageClassCutOffs", "integer", LandWebUtils:::.ageClassCutOffs, NA, NA,
                    "defines the age boundaries between age classes"),
    defineParameter("ageClassMaxAge", "integer", 400L, NA, NA,
                    "maximum possible age"),
    defineParameter("disturbanceAgeCutoff", "integer", 10L, NA, NA,
                    "defines the age boundary between 'disturbed' and 'undisturbed' for HSI classification."),
    defineParameter("reps", "integer", 1L:10L, 1L, NA_integer_,
                    paste("number of replicates/runs per study area.")),
    defineParameter("sppEquivCol", "character", "LandR", NA, NA,
                    "The column in `sim$sppEquiv` data.table to use as a naming convention"),
    defineParameter("studyAreaNamesCol", "character", NA, NA, NA,
                    "column name used to identify names of subpolygons (features) the study area polygon."),
    defineParameter("summaryInterval", "integer", 100L, NA, NA,
                    "simulation time interval at which to take 'snapshots' used for summary analyses"),
    defineParameter("summaryPeriod", "integer", c(700L, 1000L), NA, NA,
                    "lower and upper end of the range of simulation times used for summary analyses"),
    defineParameter("upload", "logical", FALSE, NA, NA,
                    "if TRUE, uses the `googledrive` package to upload figures."),
    defineParameter("uploadTo", "character", NA, NA, NA,
                    paste("if `upload = TRUE`, a Google Drive folder id corresponding to `.studyAreaName`.")),
    defineParameter("vegLeadingProportion", "numeric", 0.8, 0.0, 1.0,
                    "a number that defines whether a species is leading for a given pixel"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput("ml", "map",
                 desc = "map list object from preamble module (e.g., LandWeb_preamble)."),
    expectsInput("speciesLayers", "RasterStack",
                 desc = "initial percent cover raster layers used for simulation."),
    expectsInput("sppEquiv", "data.table",
                 desc = "table of species equivalencies. See `LandR::sppEquivalencies_CA`.",
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.HSI_Caribou_MB = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, end(sim), "HSI_Caribou_MB", "postprocess", .last())
      sim <- scheduleEvent(sim, end(sim), "HSI_Caribou_MB", "plot", .last())
      sim <- scheduleEvent(sim, end(sim), "HSI_Caribou_MB", "save", .last())
      if (isTRUE(P(sim)$upload)) {
        sim <- scheduleEvent(sim, end(sim), "HSI_Caribou_MB", "upload", .last())
      }
    },
    postprocess = {
      sim <- postprocess(sim)
    },
    plot = {
      sim <- plotFun(sim)
    },
    save = {
      sim <- saveFun(sim)
    },
    upload = {
      # ! ----- EDIT BELOW ----- ! #
      browser() ## TODO
      mod$files2upload <- set_names(mod$files2upload, basename(mod$files2upload))

      gid <- as_id(sim$uploadTo[[P(sim)$.studyAreaName]])
      prevUploaded <- drive_ls(gid)
      toUpload <- mod$files2upload[!(basename(mod$files2upload) %in% prevUploaded$name)]
      uploaded <- map(toUpload, ~ drive_upload(.x, path = gid))
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  padL <- 4

  mod$analysesOutputsTimes <- analysesOutputsTimes(P(sim)$summaryPeriod, P(sim)$summaryInterval)

  mod$allouts <- fs::dir_ls(outputPath(sim), regexp = "vegType|TimeSince", recurse = 1, type = "file") %>%
    grep("gri|png|txt|xml", ., value = TRUE, invert = TRUE)
  mod$allouts2 <- grep(paste(paste0("year", paddedFloatToChar(
    setdiff(c(0, P(sim)$timeSeriesTimes), mod$analysesOutputsTimes), padL = padL)), collapse = "|"),
    mod$allouts, value = TRUE, invert = TRUE)

  filesUserHas <- mod$allouts2

  dirsExpected <- file.path(outputPath(sim), sprintf("rep%02d", P(sim)$reps))
  filesExpected <- as.character(sapply(dirsExpected, function(d) {
    c(
      file.path(d, sprintf("rstTimeSinceFire_year%04d.tif", mod$analysesOutputsTimes)),
      file.path(d, sprintf("vegTypeMap_year%04d.grd", mod$analysesOutputsTimes))
    )
  }))

  filesNeeded <- data.frame(file = filesExpected, exists = filesExpected %in% filesUserHas)

  if (!all(filesNeeded$exists)) {
    missing <- filesNeeded[filesNeeded$exists == FALSE, ]$file
    stop(sum(!filesNeeded$exists), " simulation files appear to be missing:\n", paste(missing, collapse = "\n"))
  }

  mod$layerName <- gsub(mod$allouts2, pattern = paste0(".*", outputPath(sim)), replacement = "")
  mod$layerName <- gsub(mod$layerName, pattern = "[/\\]", replacement = "_")
  mod$layerName <- gsub(mod$layerName, pattern = "^_", replacement = "")

  mod$tsf <- gsub(".*vegTypeMap.*", NA, mod$allouts2) %>%
    grep(paste(mod$analysesOutputsTimes, collapse = "|"), ., value = TRUE)
  mod$vtm <- gsub(".*TimeSinceFire.*", NA, mod$allouts2) %>%
    grep(paste(mod$analysesOutputsTimes, collapse = "|"), ., value = TRUE)

  mod$tsfTimeSeries <- gsub(".*vegTypeMap.*", NA, mod$allouts) %>%
    grep(paste(P(sim)$timeSeriesTimes, collapse = "|"), ., value = TRUE)
  mod$vtmTimeSeries <- gsub(".*TimeSinceFire.*", NA, mod$allouts) %>%
    grep(paste(P(sim)$timeSeriesTimes, collapse = "|"), ., value = TRUE)

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

calculateHSI <- function(summaryPolys, polyCol, maxAge, vtm, tsf) {
  if (!is(summaryPolys, "sf"))
    summaryPolys <- sf::st_as_sf(summaryPolys)

  polyNames <- unique(summaryPolys[[polyCol]])

  ## vegetation type maps
  message("|_ loading vegetation type maps...")
  vtmListByPoly <- rasterListByPoly(files = vtm, polys = summaryPolys, names = polyNames,
                                    col = polyCol, filter = "vegTypeMap_") ## TODO: cache this
  vtmReps <- attr(vtmListByPoly, "reps")
  vtmTimes <- attr(vtmListByPoly, "times")
  vtmStudyAreas <- attr(vtmListByPoly, "polyNames")

  ## time since fire maps
  message("|_ loading time since fire maps...")
  tsfListByPoly <- rasterListByPoly(files = tsf, polys = summaryPolys, names = polyNames,
                                    col = polyCol, filter = "rstTimeSinceFire_") ## TODO: cache this
  tsfReps <- attr(tsfListByPoly, "reps")
  tsfTimes <- attr(tsfListByPoly, "times")
  tsfStudyAreas <- attr(tsfListByPoly, "polyNames")
  tsfListByPoly <- lapply(tsfListByPoly, function(x) {
    x[] <- as.integer(pmin(maxAge, x[]))
    x
  })

  ## sanity checks
  stopifnot(
    all(names(tsfListByPoly) %in% gsub("vegTypeMap", "rstTimeSinceFire", names(vtmListByPoly))),
    all(tsfReps == vtmReps, na.rm = TRUE), ## CC vtm and tsf will have NA rep
    all(tsfTimes == vtmTimes),
    all(tsfStudyAreas == vtmStudyAreas)
  )

  message("|_ calculating HSI maps...")
  opt <- options(future.globals.maxSize = 5*1024^3) ## 5 GiB
  hsiListByPoly <- future_lapply(names(tsfListByPoly), function(tsf) {
    polyName <- rev(strsplit(tsf, "_")[[1]])[1]

    ## from 'Caribou Habitat Suitability' document / emails:
    ## https://docs.google.com/document/d/1qF1kLPmUWGFuKFEGL99BipqQlg9BO6AuSjrnavUb2vM/
    vtm <- gsub("rstTimeSinceFire", "vegTypeMap", tsf)
    vtmRas <- vtmListByPoly[[vtm]]
    vtmRAT <- raster::levels(vtmRas)[[1]]
    vtmTbl <- data.frame(leading = c("Pine", "Conifer", "Mixed", "Deciduous"), HSI = c(3L, 2L, 1L, 0L))

    ## TODO: how are we dealing with non-productive / wetland types?? we don't have suitable layers.
    ## other.high <- "Treed Bog"
    ## other.med <- c("Shrubby Bog", "Treed Fen", "Shrubby Fen", "Treed Swamp (Conifer)")
    ## other.low <- c("Bedrock", "Gramminoid Bog")

    hsi_vtm <- raster(vtmRas)
    vtmSpp <- vtmRAT[match(vtmRas[], vtmRAT$ID), ][["VALUE"]]
    mixedIDs <- which(vtmSpp == "Mixed")
    pineIDs <- which(grepl("Pinu_sp", vtmSpp))
    vtmLead <- equivalentName(vtmSpp, sppEquivalencies_CA, "Type", searchColumn = "LandR")
    vtmLead[mixedIDs] <- "Mixed"
    vtmLead[pineIDs] <- "Pine"
    hsi_vtm[] <- vtmTbl[match(vtmLead, vtmTbl$leading), ][["HSI"]]


    rcl_age <- matrix(c(c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 100L),
                        c(10L, 20L, 30L, 40L, 50L, 60L, 100L, maxAge + 1L),
                        c(0.0, 0.2, 0.4, 0.6, 0.8, 0.9, 1.0, 0.8)),
                      ncol = 3)
    hsi_tsf <- raster::reclassify(tsfListByPoly[[tsf]], rcl_age) ## multiplication factor for hsi_vtm

    ## the final HSI raster
    hsi <- hsi_tsf * hsi_vtm
    hsi[] <- as.integer(floor(0.5 + hsi[])) ## floor(0.5 + x) rounds 0.5 values as expected, unlike round(x)
    hsi
  }, future.packages = c("raster", "sp", "sf")) ## TODO: cache this
  names(hsiListByPoly) <- names(tsfListByPoly)
  options(opt)

  hsi_df <- bind_rows(lapply(seq_along(hsiListByPoly), function(i) {
    data.frame(id = 1L:ncell(hsiListByPoly[[i]]), hsi = hsiListByPoly[[i]][]) %>%
      na.omit(.) %>%
      count(., hsi) %>%
      mutate(., prop = n/sum(n), rep = tsfReps[i], time = tsfTimes[i], poly = tsfStudyAreas[i])
  }))

  hsi_df$hsi <- as.factor(hsi_df$hsi)
  hsi_df <- hsi_df %>%
    group_by(hsi, poly)

  return(hsi_df)
}

## postprocessing
postprocess <- function(sim) {
  .ncores <- pemisc::optimalClusterNum(5000, maxNumClusters = min(parallel::detectCores() / 2, 24L)) ## TODO: use module param
  options(future.availableCores.fallback = .ncores)

  ## current conditions
  vtmCC <- Cache(vegTypeMapGenerator,
                 x = sim$speciesLayers,
                 vegLeadingProportion = P(sim)$vegLeadingProportion,
                 mixedType = 2,
                 sppEquiv = sim$sppEquiv,
                 sppEquivCol = P(sim)$sppEquivCol,
                 colors = sim$sppColorVect,
                 doAssertion = FALSE)
  fname1 <- file.path(outputPath(sim), "vegTypeMap_year0000.grd")
  raster::writeRaster(vtmCC, fname1, datatype = "INT1U", overwrite = TRUE)

  fname2 <- file.path(outputPath(sim), "rstTimeSinceFire_year0000.tif")
  tsfCC <- sim$ml[["CC TSF"]]
  raster::writeRaster(tsfCC, fname2, datatype = "INT1U", overwrite = TRUE)

  ## HSI by polygons
  message(crayon::magenta("Calculating HSI by caribou unit...")) ## TODO: which polys?
  area_df = data.frame(
    poly = sim$ml[["MB Caribou Ranges"]][["RANGE_NAME"]],
    areaha = st_as_sf(sim$ml[["MB Caribou Ranges"]]) %>% st_area() %>% units::set_units(ha) %>% as.numeric()
  )
  mod$hsi_df_MB_CC <- suppressWarnings({
    calculateHSI(summaryPolys = sim$ml[["MB Caribou"]],
                 polyCol = "RANGE_NAME",
                 maxAge = P(sim)$ageClassMaxAge,
                 tsf = fname2, vtm = fname1) %>%
      left_join(area_df)
  })
  mod$hsi_df_MB <- calculateHSI(summaryPolys = sim$ml[["MB Caribou"]],
                                polyCol = "RANGE_NAME",
                                maxAge = P(sim)$ageClassMaxAge,
                                tsf = mod$tsf, vtm = mod$vtm) %>%
    left_join(area_df)

  return(invisible(sim))
}

## plotting
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  ## TODO: use Plots
  lbls <- unique(paste0(mod$hsi_df_MB$poly, " (", round(mod$hsi_df_MB$areaha), " ha)"))
  names(lbls) <- unique(mod$hsi_df_MB$poly)
  gg1 <- ggplot(mod$hsi_df_MB, aes(x = hsi, y = prop)) +
    facet_wrap(~poly, labeller = labeller(poly = lbls)) +
    geom_boxplot(outlier.colour = "grey4", outlier.shape = 21, outlier.size = 1.0) +
    coord_flip() +
    xlab("Simplified Caribou HSI") +
    ylab("Proportion of Polygon Area") +
    geom_point(data = mod$hsi_df_MB_CC, col = "darkred", size = 2.5) +
    theme_bw() +
    theme(legend.position = "none")
  ggsave(file.path(figurePath(sim), "Caribou_HSI_facet_by_caribou_unit.png"), gg1, height = 8, width = 12)

  mod$hsi_gg <- gg1

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

saveFun <- function(sim) {
  hsi_df <- rbind(mod$hsi_df_MB, mod$hsi_df_MB)
  write.csv(hsi_df, file.path(outputPath(sim), "HSI_caribou.csv"), row.names = FALSE)

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0") # LandWeb

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## older version of SpaDES.core used here doesn't have this function
if (packageVersion("SpaDES.core") < "2.0.2.9001") {
  figurePath <- function(sim) {
    file.path(outputPath(sim), "figures", current(sim)[["moduleName"]]) |>
      checkPath(create = TRUE)
  }
}
