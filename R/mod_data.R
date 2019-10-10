#' # Module UI
#'
#' #' @title   mod_data_ui and mod_data_server
#' #' @description  A shiny Module.
#' #'
#' #' @param id shiny id
#' #' @param input internal
#' #' @param output internal
#' #' @param session internal
#' #'
#' #' @rdname mod_data
#' #'
#' #' @keywords internal
#' #' @export
#' mod_data_ui <- function(id){
#'   ns <- NS(id)
#'   run_mode <- getShinyOption("run_mode", "demo")
#'   ui_sidebar <- mod_data_find_ui("data_find_ui_1")
#'   if(run_mode == "upload")
#'     ui_sidebar <- mod_data_upload_ui("data_upload_ui_1")
#'
#'   tagList(
#'     sidebarLayout(
#'       sidebarPanel(width = 4, class = 'sidebar',
#'                    ui_sidebar),
#'       mainPanel(width = 8,
#'                 # invisible download handlers so can use bootstrap buttons
#'                 uiOutput(ns("preview_table")),
#'                 downloadButton(ns("dl_data_handler"), label = NULL,
#'                                style = "visibility: hidden;"))
#'   ))
#' }
#'
#' # Module Server
#'
#' #' @rdname mod_data
#' #' @export
#' #' @keywords internal
#'
#' mod_data_server <- function(input, output, session){
#'   ns <- session$ns
#'   run_mode <- getShinyOption("run_mode", "demo")
#'
#'   if(run_mode == "upload"){
#'     get_data <- callModule(mod_data_upload_server, "data_upload_ui_1")
#'   } else {
#'     get_data <- callModule(mod_data_find_server, "data_find_ui_1")
#'   }
#'
#'   observe({
#'     req(get_data())
#'     print(get_data())
#'   })
#'   ########## ---------- Preview Data tab ---------- ##########
#'   # preview_data <- reactive({
#'   #   req(input$upload_data)
#'   #   preview(input$data_type, get_data(), input$upload_data)
#'   # })
#'
#'   output$preview_table <- renderUI({
#'     data <- get_data()
#'     req(data)
#'     # print(data)
#'     if(is.character(data)){
#'       return(error_text(data))
#'     }
#'     tagList(
#'       button(ns('dl_data'), label = "Download Raw Data"),
#'       br(),
#'       ems_table_output(ns('data_table'))
#'     )
#'   })
#'
#'   output$data_table <- DT::renderDT({
#'     ems_data_table(preview_data())
#'   })
#'
#'   observeEvent(input$dl_data, {
#'     shinyjs::runjs(click_js(ns("dl_data_handler")))
#'   })
#'
#'   output$dl_data_handler <- downloadHandler(
#'     filename = function() "ems_data.csv",
#'     content = function(file) {
#'       readr::write_csv(preview_data(), file)
#'     })
#' }
#'
#'
