
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyrems

<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![Apache
license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

`shinyrems` is an app that allows you to download, clean, visualize and
analyze data from the Environmental Monitoring System (EMS) database.
Create plots and view water quality data with BC aquatic life water
quality guidelines.

## Installation

To install the latest development version from
[GitHub](https://github.com/bcgov/shinyrems)

``` r
# install.packages("remotes")
remotes::install_github("bcgov/rems")
remotes::install_github("bcgov/wqbc")
remotes::install_github("bcgov/shinyrems")
```

Please remember to restart your R session after installation.

## Usage

ShinyRems app can be run with various EMS datasets that must be
downloaded and stored on the user’s computer. Prior to app startup, you
will be prompted to download data if required and you will be notified
if data updates are available.

There are six possible run modes corresponding to different datasets:

-   **‘demo’** - uses a demo dataset that requires no download of EMS
    data  
-   **‘2yr’** - uses most recent 2 years of EMS data (from 2018-01-01 to
    present)
-   **‘4yr’** - uses most recent 4 years of EMS data from 2016-01-01 to
    present)  
-   **‘historic’** - uses historic EMS data (up to 2018-01-01)  
-   **‘all’** - uses combined “2yr” and “historic” EMS data
-   **‘upload’** - allows user to upload their own data following
    correct format

To select a run mode, provide the value (as it appears above) to
`shinyrems::run_ems_app()`. For example, type the following into your R
console:

``` r
# select historic run mode
shinyrems::run_ems_app("historic")

# select upload run mode
shinyrems::run_ems_app("upload")

# "2yr" run mode is the default and will be selected if no other value is provided
shinyrems::run_ems_app()
```

## Contribution

Please report any [issues](https://github.com/bcgov/shinyrems/issues).

[Pull requests](https://github.com/bcgov/shinyrems/pulls) are always
welcome.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

    Copyright 2019 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
