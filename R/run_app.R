#' Run the Shiny Application
#' @param ... additional options to pass to golem options
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
    options(shiny.maxRequestSize=4000*1024^2) 
    with_golem_options(
        app = shinyApp(ui = app_ui, server = app_server), 
        golem_opts = list(...)
    )
}