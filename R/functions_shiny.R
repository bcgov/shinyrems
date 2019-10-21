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

click_js <- function(id){
  glue::glue("document.getElementById('{id}').click();")
}

br2 <- function() tagList(br(), br())
br3 <- function() tagList(br(), br(), br())

help_text <- function(x){
  p(x, style = "font-size: 11px; color: grey;")
}

button <- function(id, label = "Get Data (csv)", icon = "download", status = "primary"){
  tags$button(id = id,
              type = "button",
              class = glue::glue("btn action-button btn-sm btn-{status}"),
              HTML(as.character(icon(icon)), label))
}

select_input_x <- function(..., label = "Select sites:", choices, selected = choices[1]) {
  selectizeInput(..., multiple = TRUE, label = label,
                 choices = choices,
                 selected = selected,
                 options = list(
                   'plugins' = list('remove_button'),
                   'create' = TRUE,
                   'persist' = FALSE))
}

ems_table_output <- function(...){
  wellPanel(DT::DTOutput(...), style = "font-size:87%", class = "wellpanel")
}

ems_data_table <- function(data){
  if(!is.data.frame(data)) return()
  DT::datatable(data, escape = FALSE, rownames = FALSE,  class = 'cell-border compact',
                options = list(ordering = TRUE,
                               autowidth = TRUE, scrollX = TRUE,
                               columnDefs = list(list(className = 'dt-center',
                                                      targets = "_all"))))
}

hide <- function(id){
  shinyjs::hide(id, anim = TRUE)
}

show <- function(id){
  shinyjs::show(id, anim = TRUE)
}


