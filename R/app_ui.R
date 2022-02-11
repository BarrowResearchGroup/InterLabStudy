#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    # from inst/app/www
    golem_add_external_resources(),
    #golem::js(),
    #golem::favicon(),
    # List the first level UI elements here
    fluidPage(
      h1("InterLabStudy")
    )
  )


# Sidebar -------------------------------------------------------------------------------------


  sidebar <- shinydashboard::dashboardSidebar(
    width = 200,
    hr(),
    shinydashboard::sidebarMenu(id="tabs",
                shinydashboard::menuItem(text = "Import", tabName = "inputData", icon = icon("upload")),
                shinydashboard::menuItem(text = "Report", tabName = "report", icon = icon("chart-bar")),
                shinydashboard::menuItem(text = "Help", tabName = "help", icon = icon("info"))
    )
  )


# Body ----------------------------------------------------------------------------------------


  body <- shinydashboard::dashboardBody(

    shinybusy::add_busy_bar(color = "#FF0000"),

    shinydashboard::tabItems(
    mod_inputData_ui("inputData_ui_1"),
    mod_Report_ui("Report_ui_1"),
    mod_Help_ui("Help_ui_1")
    )
  )


# Page ----------------------------------------------------------------------------------------


  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "InterLabStudy",
                    titleWidth = 200),
    sidebar,
    body,
    skin = "black"
  )
}

golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'InterLabStudy')
  )

  tagList(
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
