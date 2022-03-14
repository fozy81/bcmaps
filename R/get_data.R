# Modifications copyright (C) 2020 Tim Foster
# Copyright 2017 Province of British Columbia
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

#' Get a scotmaps spatial layer
#'
#' Layers are assessed directly from the [spatialdata.gov.scot](https://www.spatialdata.gov.scot/)
#' catalogue.
#'
#' @inheritParams community_councils
#' @param layer the name of the layer. The list of available layers can be
#' obtained by running `available_layers()`
#'
#' @return the layer requested
#' @export
#'
#' @examples
#' \dontrun{
#' get_layer("local_authorities")
#' }
get_layer <- function(layer, ask = TRUE, force = FALSE) {
  if (!is.character(layer)) {
    stop("You must refer to the map layer as a character string (in 'quotes')\n
         Use the function available_layers() to get a list of layers")
  }

  available <- available_layers()

  available_row <- available[available[["layer_name"]] == layer, ]

  if (nrow(available_row) != 1L && layer != "test") {
    stop(layer, " is not an available layer")
  }

  ret <- get_catalogue_data(layer, ask = ask, force = force)

  ret
}

rename_sf_col_to_geometry <- function(x) {
  geom_col_name <- attr(x, "sf_column")
  if (geom_col_name == "geometry") {
    return(x)
  }

  names(x)[names(x) == geom_col_name] <- "geometry"
  attr(x, "sf_column") <- "geometry"
  x
}

convert_to_sp <- function(sf_obj) {
  if (!requireNamespace("sf")) stop("The sf package is required to convert to sp")
  ret <- sf::st_zm(sf_obj, drop = TRUE)
  methods::as(ret, "Spatial")
}

#' List available data layers
#'
#' A data.frame of all available layers in the bcmaps package.
#'
#' @return A `data.frame` of layers, with titles, and a `shortcut_function` column
#' denoting whether or not a shortcut function exists that can be used to return the
#' layer. If `TRUE`, the name of the shortcut function is the same as the `layer_name`.
#' A value of `FALSE` in this column means the layer is available via `get_data()` but
#' there is no shortcut function for it.
#'
#' A value of `FALSE` in the `local` column means that the layer is not stored in the
#' scotmaps package but will be downloaded from the internet and cached
#' on your hard drive.
#'
#' @examples
#' \dontrun{
#' available_layers()
#' }
#' @export

available_layers <- function() {
  layers_df
  names(layers_df)[1:2] <- c("layer_name", "title")
  structure(layers_df, class = c("avail_layers", "tbl_df", "tbl", "data.frame"))
}

#' @export
print.avail_layers <- function(x, ...) {
  print(structure(x, class = setdiff(class(x), "avail_layers")))
  cat("\n------------------------\n")
  cat("All layers are downloaded from the internet and cached\n")
  cat(paste0("on your hard drive at ", data_dir(), "."))
}

shortcut_layers <- function() {
  al <- available_layers()
  al <- al[!is.na(al$layer_name), ]
  names(al)[1:2] <- c("layer_name", "title")
  # structure(al, class = c("avail_layers", "tbl_df", "tbl", "data.frame"))
  al
}




get_catalogue_data <- function(what, release = "latest", force = FALSE, ask = TRUE) {
  fname <- paste0(what, ".rds")
  dir <- data_dir()
  fpath <- file.path(dir, fname)
  layers_df <- shortcut_layers()
  if (!file.exists(fpath) | force) {
    check_write_to_data_dir(dir, ask)
    recordid <- layers_df$record[layers_df$layer_name == what]
    resourceid <- layers_df$resource[layers_df$layer_name == what]
    url <- layers_df$url[layers_df$layer_name == what]
    zip <- layers_df$zip[layers_df$layer_name == what]
    layer_type <- layers_df$layer[layers_df$layer_name == what]
    # Not all data on spatialhub...need better way to handle different data sources
    wd <- getwd()
    td <- tempdir()
    setwd(td)
    if (zip) {
      temp <- tempfile(fileext = ".zip")
      utils::download.file(url, temp)
      utils::unzip(temp)
      data <- dir(tempdir(), "*.shp$")
      layer <- sub(".shp$", "", data)
    }
    else if (!is.na(url)) {
      data <- readr::read_file(url)
    }
    else {
      data <- readr::read_file(paste0(
        "https://geo.spatialhub.scot/geoserver/",
        resourceid,
        "/wfs?authkey=b85aa063-d598-4582-8e45-e7e6048718fc&request=GetFeature&service=WFS&version=1.1.0&typeName=",
        recordid,
        "&outputFormat=json"
      ))
      layer <- NULL
    }
    if (is.na(layer_type)) {
      layer <- st_read(dsn = data, quiet = TRUE)
    } else {
      layer <- st_read(dsn = data, layer = layer_type, quiet = TRUE)
    }
    layer <- structure(layer, time_downloaded = Sys.time())
    saveRDS(layer, fpath)
    unlink(dir(td))
    setwd(wd)
  } else {
    layer <- readRDS(fpath)
    time <- attributes(layer)$time_downloaded
    update_message_once(paste0(what, " was updated on ", format(time, "%Y-%m-%d")))
  }

  layer
}
