
<!-- README.md is generated from README.Rmd. Please edit that file and re-knit-->
bcmaps
======

<a rel="Delivery" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>[![Travis-CI Build Status](https://travis-ci.org/bcgov/bcmaps.svg?branch=master)](https://travis-ci.org/bcgov/bcmaps)

Overview
--------

An [R](http://r-project.org) package of map layers for British Columbia.

Features
--------

Various layers of British Columbia, such as administrative boundaries, natural resource management boundaries, watercourses etc. All layers are available as [sp](http://cran.r-project.org/web/packages/sp/index.html) objects, and are in [BC Albers](http://spatialreference.org/ref/epsg/nad83-bc-albers/) projection, which is the [B.C. Government standard](https://www.for.gov.bc.ca/hts/risc/pubs/other/mappro/index.htm).

Installation
------------

The package is not available on CRAN, but can be installed from the bcgov R package repository:

``` r
install.packages("drat") # if not already installed
drat::addRepo("bcgov")
install.packages("bcmaps")
```

Usage
-----

To get full usage of the package, you will also need to install the [**bcmaps.rdata**](https://github.com/bcgov/bcmaps.rdata) package, which holds all of the datasets.

``` r
install.packages("bcmaps.rdata")
```

To see the layers that are available, fun the `avialable_layers()` function:

``` r
library(bcmaps)
available_layers()
#>                  Item                                               Title
#> 1            airzones                          British Columbia Air Zones
#> 2            bc_bound                                         BC Boundary
#> 3       bc_bound_hres                       BC Boundary - High Resolution
#> 4        ecoprovinces                       British Columbia Ecoprovinces
#> 5          ecoregions                         British Columbia Ecoregions
#> 6         gw_aquifers British Columbia's developed ground water aquifers.
#> 7      municipalities                         British Columbia Ecoregions
#> 8  regional_districts                         British Columbia Ecoregions
#> 9    watercourses_15M       British Columbia watercourses at 1:15M scale.
#> 10    watercourses_5M        British Columbia watercourses at 1:5M scale.
```

To load any of them, simply type `get_layer('layer_name')`, where `'layer_name'` is the name of the layer of interest. Then you can use the data as you would any `sp` object. Alternatively, there are shortcut functions for each of the layers:

For example:

``` r
bc <- bc_bound()
```

A couple of simple examples. By default, all layers are returned as `sf` spatial objects:

``` r
library(bcmaps)
library(sf)
#> Linking to GEOS 3.6.2, GDAL 2.2.2, proj.4 4.9.3, lwgeom 2.4.0 r15853

# Load and plot the boundaries of B.C.

bc <- bc_bound()
plot(st_geometry(bc))

## Next load the Regional Districts data, then extract and plot the Kootenays
rd <- regional_districts()
kootenays <- rd[rd$ADMIN_AREA_NAME == "Regional District of Central Kootenay", ]
plot(st_geometry(kootenays), col = "lightseagreen", add = TRUE)
```

![](README-plot-maps-1.png)

``` r
#text(st_coordinates(kootenays), 
 #    labels = kootenays$region_name, cex = 0.6)
```

### Plot watercourses in British Columbia at a course scale

``` r
library("sp")
# Load watercourse data and plot with boundaries of B.C.
plot(bc_bound(class = "sp"))
plot(watercourses_15M(class = "sp"), add = TRUE)
```

![](README-watercourses-1.png)

### Size of British Columbia

There is also a simple function that returns the size of B.C. in various units. You can choose total area, land area only, or freshwater area only:

``` r
bc_area("total", "ha")
#> total_ha 
#> 94473500

bc_area("land", "m2")
#>     land_m2 
#> 9.25186e+11

bc_area("freshwater", "km2")
#> freshwater_km2 
#>          19549
```

### Vignettes

We have written a short vignette on plotting points on one of the layers from `bcmaps`. You can view the vignette online [here](/vignettes/add_points.md) or if you installed the package using `devtools::install_github("bcgov/bcmaps", build_vignettes = TRUE)` you can open it using `browseVignettes("bcmaps")`.

Project Status
--------------

Inder active development

Getting Help or Reporting an Issue
----------------------------------

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/bcmaps/issues/).

How to Contribute
-----------------

Pull requests of new B.C. layers are welcome. If you would like to contribute to the package, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

Source Data
-----------

The source datasets used in this package come from various sources under open licences, including [DataBC](http://data.gov.bc.ca) ([Open Government Licence - British Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61)) and [Statistics Canada](http://www.statcan.gc.ca/start-debut-eng.html) ([Statistics Canada Open Licence Agreement](http://www.statcan.gc.ca/eng/reference/licence-eng)). See the `data-raw` folder for details on each source dataset.

Licence
-------

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

This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a complete list of our repositories on GitHub.
