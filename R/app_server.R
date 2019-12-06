#' @import shiny
app_server <- function(input, output,session) {
  
    ggplot2::theme_set(InterLabStudy::theme_custom())
    common_data<-InterLabStudy::common_data
    detected_data<-InterLabStudy::detected_data
    sample_metrics<-InterLabStudy::sample_metrics
    
  # List the first level callModules here
    inputData <- shiny::callModule(module = mod_inputData_server, id = "inputData_ui_1")
    
    shiny::callModule(module = mod_Report_server, id = "Report_ui_1", common_data = common_data, detected_data = detected_data, sample_metrics = sample_metrics, inputData = inputData)
    shiny::callModule(module = mod_Help_server, id = "Help_ui_1")
}
