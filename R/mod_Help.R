# Module UI
  
#' @title   mod_Help_ui and mod_mod_Help_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_Help
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_Help_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
  
  tabItem(tabName = "help",
          fluidRow(column(width = 12,
                          h2("App Help"),
                          tags$div(
                            tags$h3("Import tab"),
                            HTML("<p>
                            This tab enables the uploading of your data set. </br>
                            First select the correct <b>Sample</b> and <b>Ionisation mode</b>. </br>
                            Assignments can then be uploaded as a <b>.csv</b> file with the following columns:
                            <b>m/z</b>, <b>intensity</b>, and <b>formula</b>. </br>
                            The <b>formula</b> string should be in the form: 
                            C<sub>C</sub>H<sub>H</sub>N<sub>N</sub>Na<sub>Na</sub>O<sub>O</sub>S<sub>S</sub> </br>
                            For example: C<sub>6</sub>H<sub>12</sub>O<sub>6</sub> would be written as C6H12O6  </br>
                            Isotopes, if included, should be written in the form &quotC6H8O2 13C1&quot. </br>
                            Isotopes are by default removed, to keep them, uncheck the <b>Remove isotopic formulae</b> checkbox.</br>
                                 </p>"),
                            tags$h3("Report tab"),
                            HTML("<p>This section is split into three further tabs; <b>Matched data</b>, <b>Metrics</b>, and <b>Data tables</b><p>"),
                            HTML("<h4><b>Matched data</b></h4>"),
                            HTML("<p>
                            This section compares the uploaded data to two other datasets; <b><em>common</em></b> and <b><em>detected</em></b>. </br>
                            The <b><em>common</em></b> data set is comprised of formulae that are common in all assigned data or all but one dataset, the <b><em>detected</em></b> data set 
                            is comprised of formulae that were present in &#8805 3 assigned data sets. </br>
                            These data sets can be found in the 
                            <a href='https://www.github.com/BarrowResearchGroup/InterLabStudy'target='_blank'><em>InterLabStudy</em></a>
                            package, and are accessed by 
                            <code>InterLabStudy::common_data</code> and <code>InterLabStudy::detected_data</code> respectively.
                                      <p>"),
                            HTML("<h4><b>Metrics</b></h4>"),
                            HTML("<p>This section sample metrics such as <b>H/C</b>, <b>O/C</b>, <b>AI<sub>mod</sub></b> and <b>MW</b> 
                                 are plotted as boxplots. </br>
                                 The metrics for the currently uploaded data is plotted as an red <span style='color:red;'>X</span>  </br>
                                 metrics can be accessed by <code>InterLabStudy::sample_metrics</code> </p>"),
                            HTML("<h4><b>Data tables</b></h4>"),
                            HTML("<p>This section displays the underlying data tables<p>")
                          ) 
                          ))
          )
}
    
# Module Server
    
#' @rdname mod_Help
#' @export
#' @keywords internal
    
mod_Help_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_Help_ui("Help_ui_1")
    
## To be copied in the server
# callModule(mod_Help_server, "Help_ui_1")
 
