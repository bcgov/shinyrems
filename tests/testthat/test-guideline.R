# Copyright 2020 Province of British Columbia
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

test_that("add guideline to empty NULL guidelines table", {
  # make "fake" data set
  data = data.frame(
    EMS_ID = c("5844124", "5844124"),
    Station = c("DAM MINE", "DAM MINE"),
    Variable = c("Aluminum Total", "Aluminum Total"),
    Code = c("AL-T", "AL-T"),
    Value = c(0.159, 0.014),
    Units = c("mg/L", "mg/L"),
    DetectionLimit = c(0.003, 0.003),
    ResultLetter = c(NA_character_, NA_character_),
    Date = c(as.Date("2021-02-25"), as.Date("2021-06-01")),
    Outlier = c(FALSE, FALSE)
  )

  guidelines <- add_manual_guideline(
    x = NULL,
    data = data,
    limit = 0.2,
    name = "first_test",
    id = "manual_1"
  )
  expect_identical(
    colnames(guidelines),
    c("UpperLimit", "id", "calculated", "Guideline", "Variable", "Date")
  )
  expect_identical(nrow(guidelines), 4L)
  expect_equal(guidelines$UpperLimit, c(0.2, 0.2, 0.2, 0.2))
  expect_identical(min(guidelines$Date), as.Date("2021-02-25"))
  expect_identical(max(guidelines$Date), as.Date("2021-06-01"))
  expect_identical(unique(guidelines$Variable), "Aluminum Total")
})

test_that("add second guideline to guidelines table", {
  # make "fake" data set
  data <- data.frame(
    EMS_ID = c("5844124", "5844124"),
    Station = c("DAM MINE", "DAM MINE"),
    Variable = c("Aluminum Total", "Aluminum Total"),
    Code = c("AL-T", "AL-T"),
    Value = c(0.159, 0.014),
    Units = c("mg/L", "mg/L"),
    DetectionLimit = c(0.003, 0.003),
    ResultLetter = c(NA_character_, NA_character_),
    Date = c(as.Date("2021-02-25"), as.Date("2021-06-01")),
    Outlier = c(FALSE, FALSE)
  )
  # make "fake" initial guideline table
  x <- data.frame(
    UpperLimit = rep(0.2, 4),
    id = rep("manual_1", 4),
    calculated = rep(FALSE, 4),
    Guideline = rep("first_test", 4),
    Variable = rep("Aluminum Total"),
    Date = c(
      rep(as.Date("2021-02-25"), 2),
      rep(as.Date("2021-06-01"), 2)
    )
  )
  guidelines <- add_manual_guideline(
    x = x,
    data = data,
    limit = 0.001,
    name = "second_test",
    id = "manual_2"
  )

  expect_identical(
    colnames(guidelines),
    c("UpperLimit", "id", "calculated", "Guideline", "Variable", "Date")
  )
  expect_identical(nrow(guidelines), 8L)
  expect_equal(
    guidelines$UpperLimit,
    c(0.2, 0.2, 0.2, 0.2, 0.001, 0.001, 0.001, 0.001)
  )
  expect_identical(
    guidelines$id,
    c(rep("manual_1", 4), rep("manual_2", 4))
  )
  expect_identical(
    guidelines$Guideline,
    c(rep("first_test", 4), rep("second_test", 4))
  )
})
