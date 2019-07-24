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

context("test-data")

test_that("ems data functions work", {
  data <- ems_demo_data
  x <- site_to_emsid(data, c("SUN WAVE - MAIN OUTFALL",
                             "KITIMAT RIVER STATION 40"))
  expect_identical(x, c("0430022", "E103446"))

  x <- parameter_to_site(data)
  expect_is(x, "character")
  expect_length(x, 115)

  x <- parameter_to_location(data)
  expect_is(x, "data.frame")
  expect_identical(nrow(x), 115L)

  x <- parameter_to_date(data)
  expect_is(x, "Date")
  expect_identical(x, as.Date(c("1966-08-18", "2019-03-06")))

  x <- run_mode_data("demo")
  expect_identical(data, x)

  x <- demo_parameter()
  expect_identical(x, c("Temperature", "Turbidity"))
  expect_identical(run_mode_parameter("demo"), x)
})
