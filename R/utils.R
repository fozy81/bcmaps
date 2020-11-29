# Modifications copyright (C) 2020 Tim Foster
# Copyright 2016 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' Check and fix polygons that self-intersect, and sometimes can fix orphan holes
#'
#' For `sf` objects, uses `sf::st_make_valid`.
#' Otherwise, uses the common method of buffering by zero.
#'
#' `fix_self_intersect` has been removed and will no longer work. Use
#' `fix_geo_problems` instead
#'
#' @param obj The SpatialPolygons* or sf object to check/fix
#' @param tries The maximum number of attempts to repair the geometry. Ignored for `sf` objects.
#'
#' @return The `SpatialPolygons*` or `sf` object, repaired if necessary
#' @export
fix_geo_problems <- function(obj, tries = 5) {
  UseMethod("fix_geo_problems")
}

#' @export
fix_geo_problems.Spatial <- function(obj, tries = 5) {
  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("Package rgeos required but not available", call. = FALSE)
  }

  is_valid <- suppressWarnings(rgeos::gIsValid(obj))

  if (is_valid) {
    message("Geometry is valid")
    return(obj)
  }

  ## If not valid, repair. Try max tries times
  i <- 1L
  message("Problems found - Attempting to repair...")
  while (i <= tries) {
    message("Attempt ", i, " of ", tries)
    obj <- rgeos::gBuffer(obj, byid = TRUE, width = 0)
    is_valid <- suppressWarnings(rgeos::gIsValid(obj))
    if (is_valid) {
      message("Geometry is valid")
      return(obj)
    } else {
      i <- i + 1
    }
  }
  warning("Tried ", tries, " times but could not repair geometry")
  obj
}

#' @export
fix_geo_problems.sf <- function(obj, tries = 5) {

  ## Check if the overall geomtry is valid, if it is, exit and return input
  is_valid <- suppressWarnings(suppressMessages(sf::st_is_valid(obj)))

  if (all(is_valid)) {
    message("Geometry is valid")
    return(obj)
  }

  message("Problems found - Attempting to repair...")

  make_valid(obj)
}

#' @export
fix_geo_problems.sfc <- fix_geo_problems.sf

#' Union a SpatialPolygons* object with itself to remove overlaps, while retaining attributes
#'
#' The IDs of source polygons are stored in a list-column called
#' `union_ids`, and original attributes (if present) are stored as nested
#' dataframes in a list-column called `union_df`
#'
#' @param x A `SpatialPolygons` or `SpatialPolygonsDataFrame` object
#'
#' @return A `SpatialPolygons` or `SpatialPolygonsDataFrame` object
#'
#' @examples
#' if (require(sp)) {
#'   p1 <- Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
#'   p2 <- Polygon(cbind(c(5,4,3,2,5),c(2,3,3,2,2)))
#'
#'   ps1 <- Polygons(list(p1), "s1")
#'   ps2 <- Polygons(list(p2), "s2")
#'
#'   spp <- SpatialPolygons(list(ps1,ps2), 1:2)
#'
#'   df <- data.frame(a = c("A", "B"), b = c("foo", "bar"),
#'                    stringsAsFactors = FALSE)
#'
#'   spdf <- SpatialPolygonsDataFrame(spp, df, match.ID = FALSE)
#'
#'   plot(spdf, col = c(rgb(1, 0, 0,0.5), rgb(0, 0, 1,0.5)))
#'
#'   unioned_spdf <- self_union(spdf)
#'   unioned_sp <- self_union(spp)
#' }
#' @export
self_union <- function(x) {
  if (!inherits(x, "SpatialPolygons")) {
    stop("x must be a SpatialPolygons or SpatialPolygonsDataFrame")
  }

  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Package raster could not be loaded", call. = FALSE)
  }

  unioned <- raster_union(x)
  unioned$union_ids <- get_unioned_ids(unioned)

  export_cols <- c("union_count", "union_ids")

  if (inherits(x, "SpatialPolygonsDataFrame")) {
    unioned$union_df <- lapply(unioned$union_ids, function(y) x@data[y, ])
    export_cols <- c(export_cols, "union_df")
  }

  names(unioned)[names(unioned) == "count"] <- "union_count"
  unioned[, export_cols]
}

#' Modified raster::union method for a single SpatialPolygons(DataFrame)
#'
#' Modify raster::union to remove the expression:
#'   if (!rgeos::gIntersects(x)) {
#'     return(x)
#'   }
#' As it throws an error:
#'   Error in RGEOSBinPredFunc(spgeom1, spgeom2, byid, func) :
#'    TopologyException: side location conflict
#'
#' @param x a single SpatialPolygons(DataFrame) object
#' @noRd
raster_union <- function(x) {
  # First get the function (method)
  f <- methods::getMethod("union", c("SpatialPolygons", "missing"))
  # Find the offending block in the body, and replace it with NULL
  the_prob <- which(grepl("!rgeos::gIntersects(x)", body(f), fixed = TRUE))
  if (length(the_prob)) {
    body(f)[[the_prob]] <- NULL
  }
  # Call the modified function with the input
  f(x)
}

## For each new polygon in a SpatialPolygonsDataFrame that has been unioned with
## itself (raster::union(SPDF, missing)), get the original polygon ids that
## compose it
get_unioned_ids <- function(unioned_sp) {
  id_cols <- grep("^ID\\.", names(unioned_sp@data))
  unioned_sp_data <- as.matrix(unioned_sp@data[, id_cols])
  colnames(unioned_sp_data) <- gsub("ID\\.", "", colnames(unioned_sp_data))

  unioned_ids <- apply(unioned_sp_data, 1, function(i) {
    as.numeric(colnames(unioned_sp_data)[i > 0])
  })

  names(unioned_ids) <- rownames(unioned_sp_data)
  unioned_ids
}

#' Get or calculate the attribute of a list-column containing nested dataframes.
#'
#' For example, `self_union` produces a `SpatialPolygonsDataFrame`
#' that has a column called `union_df`, which contains a `data.frame`
#' for each polygon with the attributes from the constituent polygons.
#'
#' @param x the list-column in the (SpatialPolygons)DataFrame that contains nested data.frames
#' @param col the column in the nested data frames from which to retrieve/calculate attributes
#' @param fun function to determine the resulting single attribute from overlapping polygons
#' @param ... other parameters passed on to `fun`
#'
#' @return An atomic vector of the same length as x
#' @export
#'
#' @examples
#' if (require(sp)) {
#'   p1 <- Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
#'   p2 <- Polygon(cbind(c(5,4,3,2,5),c(2,3,3,2,2)))
#'   ps1 <- Polygons(list(p1), "s1")
#'   ps2 <- Polygons(list(p2), "s2")
#'   spp <- SpatialPolygons(list(ps1,ps2), 1:2)
#'   df <- data.frame(a = c(1, 2), b = c("foo", "bar"),
#'                    c = factor(c("high", "low"), ordered = TRUE,
#'                               levels = c("low", "high")),
#'                    stringsAsFactors = FALSE)
#'   spdf <- SpatialPolygonsDataFrame(spp, df, match.ID = FALSE)
#'   plot(spdf, col = c(rgb(1, 0, 0,0.5), rgb(0, 0, 1,0.5)))
#'   unioned_spdf <- self_union(spdf)
#'   get_poly_attribute(unioned_spdf$union_df, "a", sum)
#'   get_poly_attribute(unioned_spdf$union_df, "c", max)
#' }
get_poly_attribute <- function(x, col, fun, ...) {
  if (!inherits(x, "list")) stop("x must be a list, or list-column in a data frame")
  if (!all(vapply(x, is.data.frame, logical(1)))) stop("x must be a list of data frames")
  if (!col %in% names(x[[1]])) stop(col, " is not a column in the data frames in x")
  if (!is.function(fun)) stop("fun must be a function")

  test_data <- x[[1]][[col]]

  return_type <- get_return_type(test_data)

  is_fac <- FALSE

  if (return_type == "factor") {
    is_fac <- TRUE
    lvls <- levels(test_data)
    ordered <- is.ordered(test_data)
    return_type <- "integer"
  }

  fun_value <- eval(call(return_type, 1))

  ret <- vapply(x, function(y) {
    fun(y[[col]], ...)
  }, FUN.VALUE = fun_value)

  if (is_fac) {
    ret <- factor(lvls[ret], ordered = ordered, levels = lvls)
  }

  ret
}

get_return_type <- function(x) {
  if (is.factor(x)) {
    return_type <- "factor"
  } else {
    return_type <- typeof(x)
  }
}

#' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cat(paste0(..., collapse = ""))
  utils::menu(choices) == which(choices == "Yes")
}


set_bc_albers <- function(x) {
  # Only try to set crs if it is sf/sfc, otherwise just return it.
  # This should always be sf/sfc except for in a test of `get_big_data()`
  if (!inherits(x, c("sf", "sfc"))) {
    return(x)
  }
  suppressWarnings(sf::st_set_crs(x, 27700))
}

make_valid <- function(x) {
  if (old_sf_geos() && !requireNamespace("lwgeom")) {
    stop("sf built with old GEOS, lwgeom package required.", call. = FALSE)
  }
  sf::st_make_valid(x)
}

old_sf_geos <- function() {
  geos_ver <- clean_geos_version()
  unname(numeric_version(geos_ver) < numeric_version("3.8"))
}

clean_geos_version <- function(geos_version = sf::sf_extSoftVersion()["GEOS"]) {
  # replace non-numeric version components with 9999 (eg. "dev")
  geos_version <- gsub("[-.]?([^0-9.-])+[-.]?", "-9999-", geos_version)
  # remove trailing "-"
  gsub("-$", "", geos_version)
}

make_scotdata_fn <- function(fn_title) {
  layers <- shortcut_layers()
  fn_meta <- layers[layers$title == fn_title$title,]
  glue::glue("bcdc_get_data(record = '{fn_meta$record}', resource = '{fn_meta$resource}')")
}


update_message_once <- function(...) {
  silence <- isTRUE(getOption("silence_update_message"))
  messaged <- scotmaps_env$scotmaps_update_message
  if (!silence && !messaged) {
    message(...)
    assign("scotmaps_update_message", TRUE, envir = scotmaps_env)
  }
}
