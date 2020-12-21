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
inline <- function (x) {
  div(style = "display: inline-block;vertical-align:top;", x)
}

title <- function(x) {
  div(h4(x), style = "border-bottom: 1px solid #494949;")
}

subtitle <- function(x) {
  tags$label(x)
}

dl_group <- function(x, ns) {
  fillRow(
    height = "90%", width = 300, flex = c(2, 3),
    dl_button(ns(paste0("dl_", x)), "Download"),
    textInput(ns(paste0("file_", x)), label = NULL, value = "", placeholder = "file name")
  )
}

waiter_html <- function(x) {
  tagList(
    waiter::spin_chasing_dots(),
    br2(),
    h3(x)
  )
}

site_map <- function(ns) {
  tagList(
    help_text("Click a circle marker to add to selected sites. Selected sites
    are shown in red. Click a watershed polygon (or select watershed from dropdown in sidebar)
              to zoom to that watershed and filter site choices.
              Only watershed groups with sites are shown."),
    uiOutput(ns("ui_wsgroup")),
    shinycssloaders::withSpinner(leaflet::leafletOutput(ns("leaf"))),
    br()
  )
}

error_modal <- function(x) {
  modalDialog(
    title = "Please fix the following error and resubmit:",
    footer = modalButton(label = "Got it"),
    tagList(
      p(x)
    )
  )
}

help_output <- function(id) {
  div(textOutput(id),
    style = "color: grey;"
  )
}

plot_outputs <- function(x, ns, height) {
  tagList(
    plotOutput(ns(paste0("plot_", x)), height = height),
    br()
  )
}

rename_inputs <- function(site, ns) {
  tagList(
    textInput(ns(site), label = paste("rename", site, "to"))
  )
}
