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

# Module UI

#' @title   mod_data_ui and mod_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data
#'
#' @keywords internal
mod_data_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      uiOutput(ns("ui_dataset")),
      br(),
        tags$label("Select site(s) or"),
        actionLink(ns("search_map"), label = "find sites on map"),
        checkboxInput(ns("check_permit"),
          label = "Filter by Permit Number",
          value = FALSE
        ),
        uiOutput(ns("ui_permit")),
      checkboxInput(ns("check_wshedgroup"),
                    label = "Filter by Watershed Group",
                    value = FALSE
      ),
      uiOutput(ns("ui_wshedgroup")),
        radioButtons(ns("site_type"),
          label = NULL,
          choices = c("Monitoring Location", "EMS ID"),
          selected = "Monitoring Location", inline = TRUE
        ),
        uiOutput(ns("ui_site")),
        tags$label("Select Parameter"),
        radioButtons(ns("param_strict"),
          label = NULL,
          choices = c(
            "in ANY of selected sites",
            "in ALL of selected sites"
          ),
          selected = "in ANY of selected sites"
        ),
        uiOutput(ns("ui_parameter")),
        uiOutput(ns("ui_date")),
      inline(uiOutput(ns("ui_get"))),
      inline(uiOutput(ns("ui_reset")))
      ),
    mainPanel(
      tabsetPanel(
        selected = "Data",
        id = ns("tabset_data"),
        tabPanel(
          title = "Data",
          br(),
          dl_group("raw", ns),
          br2(),
          uiOutput(ns("ui_table_raw"))
        ),
        tabPanel(
          title = "Site Map",
          wellPanel(site_map(ns), class = "wellpanel")
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_data
#' @export
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns

  dataset <- getShinyOption("dataset", "demo")
  lookup <- getShinyOption("lookup", NULL)
  lookup_location <- getShinyOption("lookup_location", NULL)
  ems_data <- getShinyOption("ems_data", NULL)
  watershed_groups <- getShinyOption("watershed_groups", NULL)

  output$ui_dataset <- renderUI({
    title(paste("Dataset:", pretty_dataset(dataset)))
  })

  all_data <- reactive({
    ems_data
  })

  ########## ---------- dataset ---------- ##########
  output$ui_get <- renderUI({
    req(input$site)
    req(input$parameter)
    button(ns("get"), "Get/Update Data")
  })

  output$ui_reset <- renderUI({
    button(ns("reset"), "Reset Fields")
  })

  observeEvent(input$get, {
    waiter::waiter_show(html = waiter_html("Fetching requested data ..."))
    emsid <- translate_sites()
    rv$data <- ems_data(
      dataset = dataset,
      parameter = input$parameter,
      emsid = emsid,
      from_date = as.character(input$date_range[1]),
      to_date = as.character(input$date_range[2]),
      data = all_data()
    )
    waiter::waiter_hide()
  })

  observeEvent(input$reset, {
    updateSelectInput(session, "parameter", selected = "")
    updateSelectInput(session, "permit", selected = "")
    updateSelectInput(session, "wshedgroup", selected = "")
    updateSelectInput(session, "site", selected = "")
    updateCheckboxInput(session, "check_permit", value = FALSE)
    updateCheckboxInput(session, "check_wshedgroup", value = FALSE)

  })

  rv <- reactiveValues(
    data = empty_raw,
    cols = character(0),
    check_data = NULL,
    site_selected = NULL,
    site_choices = NULL
  )

  observe({
    if (!all_depth_na(rv$data)) {
      rv$cols <- c("UPPER_DEPTH", "LOWER_DEPTH")
    }
  })

  get_permits <- reactive({
    req(input$check_permit)
    permits(lookup_location, input$wshedgroups)
  })

  get_wshedgroups <- reactive({
    req(input$check_wshedgroup)
    wshedgroups(lookup_location, input$permit)
  })

  get_sites <- reactive({
    filter_sites(input$permit, input$wshedgroup, lookup_location, input$site_type)
  })

  get_site_locations <- reactive({
    x <- site_col(input$site_type)
    lookup_location[lookup_location[[x]] %in% get_sites(), ]
  })

  get_parameters <- reactive({
    site_parameters(
      input$site,
      lookup,
      input$site_type,
      input$param_strict
    )
  })

  translate_sites <- reactive({
    unique(translate_site(input$site, lookup, input$site_type))
  })

  output$ui_wshedgroup <- renderUI({
    req(input$check_wshedgroup)
    selectInput(ns("wshedgroup"),
                label = NULL,
                choices = c(get_wshedgroups(), ""),
                selected = ""
    )
  })

  output$ui_permit <- renderUI({
    select_input_x(ns("permit"),
      label = NULL,
      choices = c(get_permits(), ""),
      selected = ""
    )
  })

  output$ui_site <- renderUI({
    select_input_x(ns("site"),
      label = NULL,
      choices = c(get_sites(), ""),
      selected = ""
    )
  })

  output$ui_parameter <- renderUI({
    selectInput(ns("parameter"),
      label = NULL,
      choices = c(get_parameters(), ""),
      selected = "", multiple = FALSE
    )
  })

  output$ui_date <- renderUI({
    req(input$parameter)
    req(input$site)
    dates <- date_range(
      input$site, input$parameter,
      lookup, input$site_type
    )
    dateRangeInput(ns("date_range"),
      label = "Get any available data between dates:",
      start = dates[1], end = dates[2],
      min = dates[1], max = dates[2]
    )
  })

  observeEvent(input$search_map, {
    updateTabsetPanel(session, "tabset_data", selected = "Site Map")
  })

  output$leaf <- leaflet::renderLeaflet({
    ems_leaflet(watershed_groups, get_site_locations(), input$site_type)
  })

  observe({
    req(input$tabset_data == "Site Map")
    sites <- get_site_locations()
    id <- site_col(input$site_type)

    if (is.null(input$site)) {
      return(
        leafletProxy("leaf") %>%
          leaflet::removeShape("Sites") %>%
          addAwesomeMarkers(
            data = sites,
            icon = icon_blue,
            lng = ~LONGITUDE,
            lat = ~LATITUDE,
            group = "Sites",
            layerId = sites[[id]],
            label = sites[[id]]
          )
      )
    }

    selected <- lookup_location[lookup_location[[id]] %in% input$site, ]

    leafletProxy("leaf") %>%
      leaflet::removeShape("Sites") %>%
      addAwesomeMarkers(
        data = sites,
        icon = icon_blue,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        group = "Sites",
        layerId = sites[[id]],
        label = sites[[id]]
      ) %>%
      addAwesomeMarkers(
        data = selected,
        icon = icon_red,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        group = "Sites",
        layerId = selected[[id]],
        label = selected[[id]]
      )
  })

  observe({
    req(input$wshedgroup)
    zoom_to("leaf", input$wshedgroup)
  })

  observeEvent(input$leaf_marker_click, {
    sites <- c(input$leaf_marker_click$id, input$site)
    updateSelectizeInput(session, "site", selected = sites)
  })

  observeEvent(input$leaf_shape_click, {
    ws <- input$leaf_shape_click$id
    updateSelectInput(session, "wshedgroup", selected = ws)
  })

  output$ui_table_raw <- renderUI({
    ems_table_output(ns("table_raw"))
  })

  output$table_raw <- DT::renderDT({
    ems_data_table(rv$data)
  })

  output$dl_raw <- downloadHandler(
    filename = function() {
      paste0(input$file_raw, ".csv")
    },
    content = function(file) {
      readr::write_csv(rv$data, file)
    }
  )

  return(
    list(
      dataset = reactive({
        dataset
      }),
      data = reactive({
        rv$data
      }),
      all_data = all_data,
      cols = reactive({
        rv$cols
      }),
      data_type = reactive({
        input$data_type
      }),

      date = reactive({
        input$date_range
      }),
      lookup = reactive({
        lookup
      })
    )
  )
}
