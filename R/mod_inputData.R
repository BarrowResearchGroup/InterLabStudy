# Module UI
  
#' @title   mod_inputData_ui and mod_inputData_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_inputData
#'
#' @keywords internal
#' @export 
#' @import shinydashboard shinyWidgets
#' @importFrom magrittr %>%
#' @importFrom shiny NS tagList validate
mod_inputData_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
  
  tabItem(tabName = "inputData",
          fluidRow(column(width = 12,
                          h1("Welcome!"),
                          tags$div(
                            tags$p("Welcome to this online tool compare your data with standard results."),
                            tags$p("This tool is still under active development")
                          ) 
                          )
                   ),
          fluidRow(
            column(width = 6,
                   box(width = NULL,
                       height = NULL,
                       selectInput(inputId = ns("sample_type"),
                                   label = "Which sample",
                                   choices = c("ESFA" = "ESFA", "PLFA" = "PLFA",
                                               "SRFA" = "SRFA", "SRNOM" = "SRNOM"), 
                                   multiple = FALSE,
                                   selected = "SRNOM"),
                       selectInput(inputId = ns("ion_mode"),
                                   label = "Ionisation mode",
                                   choices = c("Positive mode" = "pos", "Negative mode" = "neg" ), 
                                   multiple = FALSE,
                                   selected = "neg"),
                       fileInput(inputId = ns("masslists"),
                                 label = "Choose data",
                                 multiple= FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       hr(),
                       h5("Options"),
                       awesomeCheckbox(inputId = ns("remove_iso"),"Remove isotopic formulae", value = TRUE)
                   )),
            column(width = 6, 
                   box(width = NULL,
                       height = NULL,
                       h5("File Details"),
                       htmlOutput(ns("masslist_info"))
                   ))
          ),
          fluidRow(
            column(width = 12,
                   htmlOutput(ns("package_version")))
          )
  )
  
}
    
# Module Server
    
#' @rdname mod_inputData
#' @export
#' @keywords internal
    
mod_inputData_server <- function(input, output, session, common_data){
  ns <- session$ns
  
  output$package_version<-renderUI({
    shiny::HTML(paste("InterLabStudy package version:",packageVersion("InterLabStudy")))
  })
  
  # Read masslist input
  masslist<-reactive({
    if(is.null(input$masslists))return(NULL)
    
    data<-readr::read_csv(input$masslists$datapath,col_types = "dic") %>%
      magrittr::set_colnames(c("mz","int","formula")) %>%
      dplyr::filter({if(input$remove_iso) !stringr::str_detect(formula,'\\s[:digit:]{1,2}[:alpha:]{1,2}[:digit:]{1,2}') else TRUE})
    #validate main check if any problems during read
    shiny::validate(check_file(data))
    return(data)
    
  })
  
  output$masslist_info<-renderUI({
    req(masslist())
    masslist() %>%
      glue::glue_data("File length: {nrow(.)} lines") %>%
      shiny::HTML()
  })
  
 
  for_return <- reactive({
    req(input$ion_mode)
    req(input$sample_type)
    
    list(
      ion_mode = input$ion_mode,
      sample_type = input$sample_type,
      masslist_file = masslist()
      )
  })
  
  #return
  return(for_return)
  
}
    
## To be copied in the UI
# mod_inputData_ui("inputData_ui_1")
    
## To be copied in the server
# callModule(mod_inputData_server, "inputData_ui_1")
 
