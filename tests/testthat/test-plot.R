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

context("test-plot")

test_that("ems plotting functions work", {
  data <- ems_demo_data
  x <- leaflet_labels(data[1, 1:2])
  expect_is(x, "list")
  expect_is(x[[1]], "character")

  x <- html_site_map("Temperature")
  expect_is(x, "shiny.tag")

  x <- ems_plot(data, "Temperature")
  expect_is(x, "ggplot")
  expect_identical(x$data, data)

})
