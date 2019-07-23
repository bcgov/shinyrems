
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyrems

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Apache
license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

`shinyrems` is an app that allows you to visualize and download data
from the EMS database.

## Installation

To install the latest development version from
[GitHub](https://github.com/bcgov/shinyrems)

``` r
# install.packages("remotes")
remotes::install_github("bcgov/shinyrems")
```

## Usage

The app has 4 run modes:

  - **“demo”**
  - **“2yr”**
  - **“historic”**
  - **“all”**

Each run mode has different data requirements.

  - If using **“2yr”**, **“historic”** or **“all”** run mode, you will
    be prompted to download or update the data onto your machine prior
    to the app being launched. This requires internet\!

  - If using **“historic”** and **“all”**, the data downloaded is in
    excess of 4gb\!

  - If you only require data since 2018-01-01, the **“2yr”** run mode
    works with a smaller dataset and the app will run a little faster.

To simply run the app without having to download any data first, use the
“demo” run mode:

    shinyrems::run_app(run_mode = "demo")

For the other run modes, simply substitute “demo” with one of the other
options, e.g.:

    shinyrems::run_app(run_mode = "2yr")

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
