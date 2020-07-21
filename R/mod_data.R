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
      shinyjs::hidden(div(
        id = ns("div_data_find"),
        tags$label("Select site(s) or"),
        actionLink(ns("search_map"), label = "find sites on map"),
        checkboxInput(ns("check_permit"),
          label = "Filter by Permit Number",
          value = FALSE
        ),
        uiOutput(ns("ui_permit")),
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
        uiOutput(ns("ui_get")),
        br()
      )),
      shinyjs::hidden(div(
        id = ns("div_data_upload"),
        # radioButtons(ns("data_type"),
        #   label = "Data format",
        #   choices = c(
        #     "Tidied EMS Data" = "tidy",
        #     "Raw EMS Data" = "raw"
        #   ),
        #   selected = "tidy"
        # ),
        fileInput(ns("upload_data"),
          buttonLabel = span(tagList(icon("upload"), "csv")),
          label = "",
          placeholder = "Upload your own dataset",
          accept = c(".csv")
        ),
        uiOutput(ns("ui_upload_parameter")),
        dl_button(ns("dl_template"), label = "Download Template")
      ))
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
  ems_data <- getShinyOption("ems_data", NULL)

  output$ui_dataset <- renderUI({
    title(paste("Dataset:", pretty_dataset(dataset)))
  })


  all_data <- reactive({
    ems_data
  })

  ########## ---------- dataset ---------- ##########
  observe({
    raw_rv$data <- empty_raw
    hide("div_data_find")
    hide("div_data_upload")
    showTab("tabset_data", target = "Site Map", session = session)
    updateTabsetPanel(session, "tabset_data", selected = "Data")
    if (dataset == "upload") {
      return({
        raw_rv$data <- data.frame()
        show("div_data_upload")
        hideTab("tabset_data", target = "Site Map", session = session)
      })
    }
    show("div_data_find")
  })

  output$ui_get <- renderUI({
    req(input$site)
    req(input$parameter)
    button(ns("get"), "Get/Update Data")
  })

  raw_rv <- reactiveValues(
    data = empty_raw,
    cols = character(0),
    check_data = NULL
  )
  observe({
    if (!all_depth_na(raw_rv$data)) {
      raw_rv$cols <- c("UPPER_DEPTH", "LOWER_DEPTH")
    }
  })

  observeEvent(input$get, {
    waiter::waiter_show(html = waiter_html("Fetching requested data ..."))
    emsid <- translate_sites()
    raw_rv$data <- ems_data(
      dataset = dataset,
      parameter = input$parameter,
      emsid = emsid,
      from_date = as.character(input$date_range[1]),
      to_date = as.character(input$date_range[2]),
      data = all_data()
    )
    waiter::waiter_hide()
  })

  output$ui_upload_parameter <- renderUI({
    req(raw_rv$check_data)
    selectInput(ns("upload_parameter"), "Select parameter",
                choices = raw_rv$check_data$Variable)
  })

  observeEvent(input$upload_data, {
    data <- readr::read_csv(input$upload_data$datapath)
    check <- try(check_data_upload(data), silent = TRUE)
    if (is_try_error(check)) {
      raw_rv$check_data <- NULL
      return(showModal(error_modal(check)))
    } else {
      raw_rv$check_data <- check
    }
    # req(input$upload_parameter)

  })

  observe({
    req(input$upload_parameter)
    print(input$upload_parameter)
    processed <- process_data_upload(raw_rv$check_data, input$upload_parameter)
    print(processed)
    raw_rv$data <- processed
  })
#
#   observe({
#     if (dataset == "upload") {
#       req(input$upload_data)
#
#     }
#   })



  lookup_location <- reactive({
    req(lookup)
    get_lookup_location(lookup)
  })

  get_permits <- reactive({
    req(input$check_permit)
    permits(lookup)
  })

  get_sites <- reactive({
    permit_sites(input$permit, lookup, input$site_type)
  })

  get_site_locations <- reactive({
    req(lookup_location())
    lookup_location <- lookup_location()
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

  # observe({
  #   req(input$data_type)
  #   if (input$data_type == "raw") {
  #     raw_rv$template <- template_raw
  #     raw_rv$template_df <- template_raw_df
  #   } else {
  #     raw_rv$template <- template_tidy
  #     raw_rv$template_df <- template_tidy_df
  #   }
  # })

  output$ui_wsgroup <- renderUI({
    selectInput(ns("wsgroup"),
      label = "Zoom to watershed group",
      choices = c(sort(watershed_groups$WATERSHED_GROUP_NAME), ""),
      selected = ""
    )
  })

  output$ui_permit <- renderUI({
    select_input_x(ns("permit"),
      label = "Permit number:",
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

    lookup_location <- lookup_location()
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
    req(input$wsgroup)
    zoom_to("leaf", input$wsgroup)
  })

  observeEvent(input$leaf_marker_click, {
    sites <- c(input$leaf_marker_click$id, input$site)
    updateSelectizeInput(session, "site", selected = sites)
  })

  observeEvent(input$leaf_shape_click, {
    ws <- input$leaf_shape_click$id
    updateSelectInput(session, "wsgroup", selected = ws)
  })

  output$ui_table_raw <- renderUI({
    ems_table_output(ns("table_raw"))
  })

  output$table_raw <- DT::renderDT({
    ems_data_table(raw_rv$data)
  })

  output$dl_raw <- downloadHandler(
    filename = function() {
      paste0(input$file_raw, ".csv")
    },
    content = function(file) {
      readr::write_csv(raw_rv$data, file)
    }
  )

  output$dl_template <- downloadHandler(
    filename = function() "ems_template.csv",
    content = function(file) {
      readr::write_csv(raw_rv$template_df, file)
    }
  )

  rcode <- reactive({
    rcode_data(dataset,
      emsid = translate_sites(),
      parameter = input$parameter,
      date = input$date_range, file = input$upload_data$name
    )
  })

  output$rcode <- renderUI({
    rcode()
  })

  return(
    list(
      dataset = reactive({
        dataset
      }),
      data = reactive({
        raw_rv$data
      }),
      all_data = all_data,
      cols = reactive({
        raw_rv$cols
      }),
      data_type = reactive({
        input$data_type
      }),
      rcode = rcode,
      emsid = translate_sites,
      parameter = reactive({
        input$parameter
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
