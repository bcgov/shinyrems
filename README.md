
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# shinyrems

`shinyrems` is an app that allows you to visualize and download data
from the EMS database.

#### To run the app, paste the following into an R console

``` r
# First let's install the package!
if(!"drat" %in% installed.packages()[,1]) install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("shinyrems")
# Now we can run the app. To launch the app with a demo dataset simply run:
shinyrems::run_app()
# To run with actual datasets you will need to select the appropriate run_mode and follow prompts to download or update data. Note it is not neccessary to update the data although you might want to get the most recent data.
# to access the '2 year' dataset (data from 2018-01-01 to present), use:
shinyrems::run_app(run_mode = "2yr")
# to access the 'historic' dataset (data from 1964-01-01 to 2018-01-01), which requires download of a 4gb file, use:
shinyrems::run_app(run_mode = "historic")
# to access all data (data from 1964-01-01 to present), use:
shinyrems::run_app(run_mode = "all")
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/shinyrems/issues).

[Pull requests](https://github.com/poissonconsulting/shinyrems/pulls)
are always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms
