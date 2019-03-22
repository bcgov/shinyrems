
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

# shinyrems

`shinyrems` provides an app which serves as an interface to the `rems` R
package and ems database.

#### To run the app, paste the following into an R console

``` r
# install rems R package
install_github("bcgov/rems")
# download the ems SQLite database from BC Data Catalogue
rems::download_historic_data(ask = FALSE)
# install shinyrems R package
install_github("poissonconsulting/shinyrems")
# run app
shinyrems::run_app()
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/shinyrems/issues).

[Pull requests](https://github.com/poissonconsulting/shinyrems/pulls)
are always welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms
