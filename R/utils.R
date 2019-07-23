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

selectInputX <- function(..., label = "Select sites:", choices, selected = choices[1]) {
  selectizeInput(..., multiple = TRUE, label = label,
                 choices = choices,
                 selected = selected,
                 options = list(
    'plugins' = list('remove_button'),
    'create' = TRUE,
    'persist' = FALSE))
}

emsTableOutput <- function(...){
  wellPanel(dataTableOutput(...), class = "wellpanel")
}

emsDownload <- function(..., label = "Download Data (csv)", br = TRUE){
  if(br){
    return(tagList(
      br(),
      downloadButton(..., class = 'small-dl',
                     label = label),
      br(), br()
    ))
  }
  downloadButton(..., class = 'small-dl',  label = label)
}

