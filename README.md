
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# shinyrems

`shinyrems` is an app that allows you to visualize and download data
from the EMS database.

#### Follow these instructions to run the app:

First let’s install the package\! Paste the following into the console:

``` r
if(!"drat" %in% installed.packages()[,1]) install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("shinyrems")
```

##### The app can be run in 4 modes: “demo”, “2yr”, “historic”, and “all”, which refer to the datasets used/required.

To use a demo
dataset:

    shinyrems::run_app(run_mode = "demo")

##### For the other modes, you will be prompted to download/update the data, which is stored on your computer for later use. Note it is not necessary to update the data.

For the ‘2yr’ dataset (data from 2018-01-01 to present):

    shinyrems::run_app(run_mode = "2yr")

For the ‘historic’ dataset (data from 1964-01-01 to 2018-01-01):  
\* note this requires download of a 4gb file\!

    shinyrems::run_app(run_mode = "historic")

For all data:

    shinyrems::run_app(run_mode = "all")

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/shinyrems/issues).

[Pull requests](https://github.com/poissonconsulting/shinyrems/pulls)
are always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms
