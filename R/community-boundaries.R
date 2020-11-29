# Modifications copyright (C) 2020 Tim Foster
# Copyright 2017 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

#' Community Councils Boundaries
#'
#' @param ask Should the function ask the user before downloading the data to a
#'   cache? Defaults to the value of interactive().
#' @param force Should you force download the data?
#'
#' @return The spatial layer of `community_councils` in the desired class
#'
#' @export
#' @examples
#' \dontrun{
#' my_layer <- community_councils()
#' }
#'
community_councils <- function(ask = interactive(), force = FALSE) {
  dir <- data_dir()
  fpath <- file.path(dir, "scottish-community-council-boundaries.rds")

  if (!file.exists(fpath) | force) {
    check_write_to_data_dir(dir, ask)

    data <- readr::read_file(
      "https://geo.spatialhub.scot/geoserver/sh_commcnc/wfs?authkey=b85aa063-d598-4582-8e45-e7e6048718fc&request=GetFeature&service=WFS&version=1.1.0&typeName=pub_commcnc&outputFormat=json"
    )

    layer <- st_read(data, quiet = TRUE)
    layer <- structure(layer, time_downloaded = Sys.time())
    saveRDS(layer, fpath)
  } else {
    layer <- readRDS(fpath)
    time <- attributes(layer)$time_downloaded
    update_message_once(paste0("community_councils was updated on ", format(time, "%Y-%m-%d")))
  }
  layer
}

