# Copyright 2019 Province of British Columbia
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

# Building a Prod-Ready, Robust Shiny Application.
#
# Each step is optional.
#

# 2. All along your project

## 2.1 Add modules

golem::add_module(name = "ems") # Name of the module

## 2.2 Add dependencies

usethis::use_package("rems")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")

## 2.3 Add tests

usethis::use_test("app")

# 3. Documentation

## 3. Vignette
usethis::use_vignette("shinyrems")
devtools::build_vignettes()

## 3. Code coverage
usethis::use_travis()
usethis::use_appveyor()
usethis::use_coverage()
