# Module UI
  
#' @title   mod_Report_ui and mod_Report_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_Report
#'
#' @keywords internal
#' @export 
#' @import shinydashboard shinyWidgets ggplot2
#' @importFrom magrittr %>%
#' @importFrom shiny NS tagList 
#' @importFrom plotly plotlyOutput ggplotly layout
#' @importFrom ggplot2 ggplot
#' @importFrom glue glue
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom purrr pluck map2_df
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na gather
#' @importFrom dplyr if_else

mod_Report_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
  tabItem(tabName = "report",
          fluidRow(column(width = 12,
                          tabBox(width = NULL,
                                 tabPanel(
                                   h5("Matched data"),
                                   splitLayout(
                                     cellWidths = c("30%","70%"),
                                     box(width = NULL,htmlOutput(ns("matching_stats")),
                                         collapsible = TRUE, title = "Matching Stats", status = "primary", solidHeader = TRUE),
                                     box(width = NULL,div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("matching_table"))),
                                         collapsible = TRUE, title = "Matching Table", status = "primary", solidHeader = TRUE)
                                     ),
                                   splitLayout(
                                     cellWidths = c("85%","15%"),
                                     box(width = NULL, plotly::plotlyOutput(ns("VK_matched_data"),height = "600px") , collapsible = TRUE,
                                         title = "Van Krevelen", status = "primary", solidHeader = TRUE),
                                     box(width = NULL,
                                         selectizeInput(ns("vk_mode"),"Data to plot",
                                                        choices = c("Matched data only" = "matched",
                                                                    "Matched and unmatched" = "both",
                                                                    "Sample coverage" = "coverage")),
                                         awesomeCheckbox(ns("vk_limit_axes"),label = "Limit axes",value = FALSE),
                                         conditionalPanel(condition = "input.vk_limit_axes", ns = ns,
                                                          hr(),
                                                          splitLayout(
                                                            numericInput(ns("vk_xlim_min"), "x axis min", value = 0, min = 0, step = 0.1),
                                                            numericInput(ns("vk_xlim_max"), "x axis max", value = 2, min = 0, step = 0.1)
                                                          ),
                                                          splitLayout(
                                                            numericInput(ns("vk_ylim_min"), "y axis min", value = 0, min = 0, step = 0.1),
                                                            numericInput(ns("vk_ylim_max"), "y axis max", value = 2, min = 0, step = 0.1)
                                                          )
                                         ),
                                         hr(),
                                         radioButtons(ns("dl_plot_format"), label = "Select the file type", choices = list("png", "pdf","eps","tiff")),
                                         selectizeInput(ns("plot_units"), label = "Select units", choices = list("in", "cm", "mm")),
                                         numericInput(ns("plot_width"), label = "Plot width", value = 12),
                                         numericInput(ns("plot_height"), label = "Plot height", value = 8),
                                         numericInput(ns("plot_dpi"), label = "Plot resolution (dpi)", value = 300),
                                         downloadButton(ns("download_VK_matched"),"Download van Krevelen plot"),
                                         collapsible = TRUE, title = "Plot Settings", status = "primary", solidHeader = TRUE)
                                     ),
                                   splitLayout(
                                     cellWidths = c("85%","15%"),
                                     box(width = NULL, plotly::plotlyOutput(ns("ms_matched_data"),height = "600px") , collapsible = TRUE,
                                         title = "Mass Spectra", status = "primary", solidHeader = TRUE),
                                     box(width = NULL,
                                         radioButtons(ns("dl_plot_format_ms"), label = "Select the file type", choices = list("png", "pdf","eps","tiff")),
                                         selectizeInput(ns("plot_units_ms"), label = "Select units", choices = list("in", "cm", "mm")),
                                         numericInput(ns("plot_width_ms"), label = "Plot width", value = 12),
                                         numericInput(ns("plot_height_ms"), label = "Plot height", value = 8),
                                         numericInput(ns("plot_dpi_ms"), label = "Plot resolution (dpi)", value = 300),
                                         downloadButton(ns("download_ms_matched"),"Download mass spectra plot"),
                                         collapsible = TRUE, title = "Plot Settings", status = "primary", solidHeader = TRUE)
                                   ),
                                   box(width = NULL, div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("matched_data"))),
                                       collapsible = TRUE, title = "Matched Data", status = "primary", solidHeader = TRUE)
                                   ),
                                 tabPanel(
                                   h5("Metrics"),
                                   splitLayout(
                                     cellWidths = c("40%","40%","20%"),
                                     box(width = NULL, plotly::plotlyOutput(ns("HC_plot"),height = "600px") , collapsible = TRUE,
                                         title = "HC Metrics", status = "primary", solidHeader = TRUE),
                                     box(width = NULL, plotly::plotlyOutput(ns("OC_plot"),height = "600px") , collapsible = TRUE,
                                         title = "OC Metrics", status = "primary", solidHeader = TRUE),
                                     box(width = NULL,
                                         awesomeCheckbox(ns("add_jitter"), "Add jitter to labels", value = TRUE),
                                         radioButtons(ns("dl_plot_format2"), label = "Select the file type", choices = list("png", "pdf","eps","tiff")),
                                         selectizeInput(ns("plot_units2"), label = "Select units", choices = list("in", "cm", "mm")),
                                         numericInput(ns("plot_width2"), label = "Plot width", value = 12),
                                         numericInput(ns("plot_height2"), label = "Plot height", value = 8),
                                         numericInput(ns("plot_dpi2"), label = "Plot resolution (dpi)", value = 300),
                                         splitLayout(
                                           downloadButton(ns("download_HC"),"Download HC plot"),
                                           downloadButton(ns("download_OC"),"Download OC plot")
                                         ),
                                         splitLayout(
                                           downloadButton(ns("download_AI_mod"),"Download AI plot"),
                                           downloadButton(ns("download_MW"),"Download MW plot")
                                         ),
                                         collapsible = TRUE, title = "Plot Settings", status = "primary", solidHeader = TRUE)
                                   ),
                                   splitLayout(
                                     cellWidths = c("40%","40%"),
                                     box(width = NULL, plotly::plotlyOutput(ns("AI_mod_plot"),height = "600px") , collapsible = TRUE,
                                         title = "AImod Metrics", status = "primary", solidHeader = TRUE),
                                     box(width = NULL, plotly::plotlyOutput(ns("MW_plot"),height = "600px") , collapsible = TRUE,
                                         title = "MW Metrics", status = "primary", solidHeader = TRUE)
                                   )
                                   
                                   
                                 ),
                                 tabPanel(
                                   h5("Data tables"),
                                   box(width = NULL, div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("selected_common_data"))),
                                       collapsible = TRUE, title = "Selected Common Data", status = "primary", solidHeader = TRUE),
                                   box(width = NULL, div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("selected_detected_data"))),
                                       collapsible = TRUE, title = "Selected Detected Data", status = "primary", solidHeader = TRUE),
                                   box(width = NULL, div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("input_data"))),
                                       collapsible = TRUE, title = "Input Data", status = "primary", solidHeader = TRUE)
                                   )
                                 )
                          )))
  
}
    
# Module Server
    
#' @rdname mod_Report
#' @export
#' @keywords internal
    
mod_Report_server <- function(input, output, session, common_data, detected_data, sample_metrics, inputData){
  
  #inputData is a list object with 3 levels:
  #masslist_file : masslist tibble
  #ion_mode : pos / neg
  #sample_type : ESFA, PLFA, SRFA, SRNOM
  
  ns <- session$ns

# Generate reactives ------------------------------------------------------

  #retrieve correct table
  selected_common_data<- reactive({
    req(common_data, inputData())
    common_data %>% 
      purrr::pluck(paste0(inputData()$sample_type, "_", inputData()$ion_mode))
  })
  
  output$selected_common_data <- DT::renderDataTable({
    req(selected_common_data())
    return(selected_common_data())
  })
  
  #retrieve correct table
  selected_detected_data<- reactive({
    req(detected_data, inputData())
    detected_data %>% 
      purrr::pluck(paste0(inputData()$sample_type, "_", inputData()$ion_mode))
  })
  
  output$selected_detected_data <- DT::renderDataTable({
    req(selected_detected_data())
    return(selected_detected_data())
  })
  
  #generate needed data columns 
  input_data <- reactive({
    req(!is.null(inputData()$masslist_file))
    inputData()$masslist_file %>%
      dplyr::mutate(
        C = InterLabStudy::count_atoms(formula = formula, element = "C"),
        H = InterLabStudy::count_atoms(formula = formula, element = "H"),
        N = InterLabStudy::count_atoms(formula = formula, element = "N"),
        O = InterLabStudy::count_atoms(formula = formula, element = "O"),
        S = InterLabStudy::count_atoms(formula = formula, element = "S"),
        Na = InterLabStudy::count_atoms(formula = formula, element = "Na")
      ) %>%
      dplyr::mutate(H_C = H/C,
             O_C = O/C,
             dbe = C - H/2 + N/2 + 1 ,
             AI_mod = (1 + C - (0.5 * O) - S - 0.5 *(N + H))/(C - (0.5 * O) - N - S))
    })
  
  output$input_data <- DT::renderDataTable({
    req(input_data())
    return(input_data())
  })
  

# Perform data matching ---------------------------------------------------

matched_data <- reactive({
  req(selected_common_data(), selected_detected_data() ,input_data())
  
  common_formulae<-selected_common_data()$formula
  detected_formulae<-selected_detected_data()$formula %>% dplyr::setdiff(.,common_formulae) #remove common from detected data
  input_formulae<-input_data()$formula
  
  input_data() %>%
    dplyr::mutate(status = dplyr::case_when(
      formula %in% common_formulae ~ "common matched",
      formula %in% detected_formulae ~ "detected matched",
      TRUE ~ "unique"
      ),
      mode = inputData()$ion_mode
      ) 
    
  })  
  
  output$matched_data <- DT::renderDataTable({
    req(matched_data())
    return(matched_data())
  }, colnames = c("m/z","intensity","formula","C","H","N","O","S","Na","H/C","O/C","DBE","AImod","match status","ionization mode"))
  

# Matched van Krevelen ----------------------------------------------------

  
  VK_matched_data <- reactive({
    req(matched_data(),selected_common_data(),selected_detected_data(),input_data())
    
    #calculate unmatched data
    unmatched_common<-selected_common_data() %>%
      dplyr::filter(!formula %in% matched_data()$formula) %>%
      dplyr::mutate(status = "common unmatched")
    
    unmatched_detected<-selected_detected_data() %>%
      dplyr::filter(!formula %in% c(matched_data()$formula, unmatched_common$formula)) %>%
      dplyr::mutate(status = "detected unmatched")
    
   data<-switch(input$vk_mode,
                "matched" = matched_data(),
                "both" = dplyr::bind_rows(matched_data(),unmatched_detected, unmatched_common),
                "coverage" = purrr::map2_df(list(input_data(), selected_common_data(), selected_detected_data()),
                                     c("Sample","Common","Detected"),
                                     ~dplyr::mutate(.x,status = .y))
                )
   #limit axes
   limit_axes<-input$vk_limit_axes
   xlim_min<-input$vk_xlim_min
   xlim_max<-input$vk_xlim_max
   ylim_min<-input$vk_ylim_min
   ylim_max<-input$vk_ylim_max
   
   #set markers to use
   markers<-switch(input$vk_mode,
                   "matched" = c("common matched" = 16,"detected matched" = 15,"unique" = 17),
                   "both" = c("common matched" = 16,"detected matched" = 15,"unique" = 17,
                              "common unmatched" = 16, "detected unmatched" = 15),
                   "coverage" = c("Sample" = 17, "Common" = 16, "Detected" = 15)
                   )
   
   
   return({
     data %>%
       dplyr::select(formula,H_C,O_C,status) %>%
       ggplot2::ggplot(aes(O_C,H_C,col = status, shape = status)) +
       geom_point(alpha = 0.5) +
       labs(x = "O/C",
            y = "H/C") +
       scale_colour_viridis_d()+
       theme(legend.title = element_blank())+
       scale_shape_manual(values = markers)+
       {if(limit_axes == TRUE)xlim(c(xlim_min,xlim_max))}+
       {if(limit_axes == TRUE)ylim(c(ylim_min,ylim_max))}
     })
    
  })
  
  output$VK_matched_data <- plotly::renderPlotly({
    VK_matched_data() %>%
      plotly::ggplotly(.)
    
  })
  
  output$download_VK_matched <- downloadHandler(
    filename =  function() {paste("VK_matched", input$dl_plot_format, sep=".")},
    content = function(file) {
      VK_matched_data() %>% 
        multi_plot_save(file, plot =  . ,
                        type = input$dl_plot_format,
                        width = input$plot_width,
                        height = input$plot_height,
                        units = input$plot_units,
                        dpi = input$plot_dpi)
    }
  )
  

# Mass spectrum -----------------------------------------------------------
   ms_matched_data <- reactive({
     req(matched_data(),selected_common_data(),selected_detected_data(),input_data())
     
     list(selected_common_data(),selected_detected_data()) %>%
       purrr::map2_df(.,c("common","detected"),~dplyr::mutate(.x,origin = .y)) %>%
       dplyr::left_join(.,{matched_data() %>% dplyr::select(formula,status)}, by = "formula") %>%
       dplyr::mutate(int = 100) %>%
       tidyr::replace_na(list(status = "unmatched")) %>%
       dplyr::bind_rows(.,{matched_data() %>% dplyr::mutate(origin = "sample")}) %>%
       ggplot(aes(label = formula, label2 = mz, label3 = int,label4 = status))+
       geom_segment(aes(x = mz, xend = mz, y = 0, yend = int, col = status), size = 0.2)+
       labs(x = "m/z",
            y = "Intensity")+
       facet_grid(rows = vars(factor(origin,levels = c("sample","common","detected"), ordered = TRUE)),
                  scales = "free_y")+
       theme(legend.title = element_blank())
   })

  output$ms_matched_data <- plotly::renderPlotly({
    ms_matched_data() %>%
      plotly::ggplotly(.,tooltip = c("label","label2","label3","label4"), dynamicTicks = TRUE) %>%
      plotly::layout(margin = list(l=100))
  })
  
  output$download_ms_matched <- downloadHandler(
    filename =  function() {paste("ms_matched", input$dl_plot_format_ms, sep=".")},
    content = function(file) {
      ms_matched_data() %>% 
        InterLabStudy::multi_plot_save(file, plot =  . ,
                        type = input$dl_plot_format_ms,
                        width = input$plot_width_ms,
                        height = input$plot_height_ms,
                        units = input$plot_units_ms,
                        dpi = input$plot_dpi_ms)
    }
  )
  
# Matching Statistics -----------------------------------------------------

  matching_table <- reactive({
    req(matched_data())
    tibble::tibble(
      dataset = c("Common","Detected","Sample Unique"),
      n = c(
        {matched_data() %>% dplyr::filter(status == "common matched") %>% nrow()},
        {matched_data() %>% dplyr::filter(status == "detected matched") %>% nrow()},
        {matched_data() %>% dplyr::filter(status == "unique") %>% nrow()}
        ),
      total = c(
        {nrow(selected_common_data())},
        {nrow(selected_detected_data()) - nrow(selected_common_data())},
        {nrow(input_data())}
      )
    ) %>%
      dplyr::mutate(
        perc_sample = round(100 * n/nrow(input_data()), digits = 1),
        perc_dataset = round( 100 * n/total, digits = 1)
        ) %>%
      dplyr::select(dataset, n, perc_sample, total, perc_dataset)
      
  })
  
  output$matching_table <- DT::renderDataTable({
    req(matching_table())
    return(matching_table())
  }, colnames = c("Dataset", "No. in Sample", "% of Sample", "Total Dataset Size", "% of Dataset"))
  
  output$matching_stats<-renderUI({
    req(matching_table())
    
    perc_com<-dplyr::filter(matching_table(),dataset == "Common")$perc_dataset/100
    perc_det<-dplyr::filter(matching_table(),dataset == "Detected")$perc_dataset/100
    
    HTML(glue::glue(dplyr::if_else(perc_com >= 0.90,"<span style=\"color:green\">","<span style=\"color:blue\">"),
                    "{scales::percent(perc_com)} of common peaks matched </span> <br/>",
                    "{scales::percent(perc_det)} of detected peaks matched"
                    )
    )
  })
  

# Sample Metrics ----------------------------------------------------------

current_metrics <- reactive({
  req(input_data(),selected_common_data())
  
  common_formula<-selected_common_data()$formula
  
  output<-input_data() %>%
    dplyr::ungroup() %>%
    dplyr::filter(formula %in% common_formula) %>%
    dplyr::summarise(
      HC_metric = sum(H_C * int, na.rm = TRUE)/sum(int,na.rm = TRUE),
      AI_mod_metric = sum(AI_mod * int,na.rm = TRUE)/sum(int,na.rm = TRUE),
      MW_metric = sum(mz * int, na.rm = TRUE)/sum(int, na.rm = TRUE),
      OC_metric = sum(O_C * int, na.rm = TRUE)/sum(int, na.rm = TRUE)
    ) 
 
  return(output)
  
})  
  

 #HC 
 HC_plot <- reactive({
   req(inputData(), current_metrics())
   data<- sample_metrics %>%
     purrr::pluck(paste0("HC",inputData()$ion_mode)) %>% 
     tidyr::gather("Sample","Value",-c(Instrument)) %>%
     dplyr::add_row(.,Instrument = "X", Sample = inputData()$sample_type, Value = current_metrics()$HC_metric[[1]]) 
   data %>%
     ggplot(aes(Sample,Value, group = Sample)) +
     stat_boxplot(geom = "errorbar", width = 0.2)+
     geom_boxplot(aes(fill = Sample),alpha = 0.5,colour = "black")+
     {if(!input$add_jitter) geom_text(aes(label = Instrument, colour = Instrument))}+
     {if(input$add_jitter) geom_text(aes(label = Instrument, colour = Instrument), position = position_jitter(seed = 2000))}+
     scale_colour_manual(values = expand_palette(data$Instrument, "black", c("X" = "red")))+
     labs(y = "H/C metric")+
     theme(legend.title = element_blank(),
           legend.position = "none")
 }) 
 
 output$HC_plot <- plotly::renderPlotly({
   HC_plot() %>%
     plotly::ggplotly(.)
 })
 
 output$download_HC <- downloadHandler(
   filename =  function() {paste("HC_plot", input$dl_plot_format2, sep=".")},
   content = function(file) {
     HC_plot() %>% 
       InterLabStudy::multi_plot_save(file, plot =  . ,
                       type = input$dl_plot_format2,
                       width = input$plot_width2,
                       height = input$plot_height2,
                       units = input$plot_units2,
                       dpi = input$plot_dpi2)
   }
 )
 
 #OC
 OC_plot <- reactive({
   req(inputData(), current_metrics())
   data <- sample_metrics %>%
     purrr::pluck(paste0("OC",inputData()$ion_mode)) %>% 
     tidyr::gather("Sample","Value",-c(Instrument)) %>%
     dplyr::add_row(.,Instrument = "X", Sample = inputData()$sample_type, Value = current_metrics()$OC_metric[[1]]) 
   data %>%
     ggplot(aes(Sample,Value, group = Sample, fill = Sample)) +
     stat_boxplot(geom = "errorbar", width = 0.2)+
     geom_boxplot(alpha = 0.5)+
     {if(!input$add_jitter) geom_text(aes(label = Instrument, colour = Instrument))}+
     {if(input$add_jitter) geom_text(aes(label = Instrument, colour = Instrument), position = position_jitter(seed = 2000))}+
     scale_colour_manual(values = expand_palette(data$Instrument, "black", c("X" = "red")))+
     labs(y = "O/C metric")+
     theme(legend.title = element_blank(),
           legend.position = "none")
 }) 
 
 output$OC_plot <- plotly::renderPlotly({
   OC_plot() %>%
     plotly::ggplotly(.)
 })
 
 output$download_OC <- downloadHandler(
   filename =  function() {paste("OC_plot", input$dl_plot_format2, sep=".")},
   content = function(file) {
     OC_plot() %>% 
       InterLabStudy::multi_plot_save(file, plot =  . ,
                       type = input$dl_plot_format2,
                       width = input$plot_width2,
                       height = input$plot_height2,
                       units = input$plot_units2,
                       dpi = input$plot_dpi2)
   }
 )
 
 #AImod
 AI_mod_plot <- reactive({
   req(inputData(), current_metrics())
   data <- sample_metrics %>%
     purrr::pluck(paste0("AI",inputData()$ion_mode)) %>% 
     tidyr::gather("Sample","Value",-c(Instrument)) %>%
     dplyr::add_row(.,Instrument = "X", Sample = inputData()$sample_type, Value = current_metrics()$AI_mod_metric[[1]]) 
   data %>%
     ggplot(aes(Sample,Value, group = Sample, fill = Sample)) +
     stat_boxplot(geom = "errorbar", width = 0.2)+
     geom_boxplot(alpha = 0.5)+
     {if(!input$add_jitter) geom_text(aes(label = Instrument, colour = Instrument))}+
     {if(input$add_jitter) geom_text(aes(label = Instrument, colour = Instrument), position = position_jitter(seed = 2000))}+
     scale_colour_manual(values = expand_palette(data$Instrument, "black", c("X" = "red")))+
     labs(y = "AImod metric")+
     theme(legend.title = element_blank(),
           legend.position = "none")
 }) 
 
 output$AI_mod_plot <- plotly::renderPlotly({
   AI_mod_plot() %>%
     plotly::ggplotly(.)
 })
 
 output$download_AI_mod <- downloadHandler(
   filename =  function() {paste("AI_mod_plot", input$dl_plot_format2, sep=".")},
   content = function(file) {
     AI_mod_plot() %>% 
       InterLabStudy::multi_plot_save(file, plot =  . ,
                       type = input$dl_plot_format2,
                       width = input$plot_width2,
                       height = input$plot_height2,
                       units = input$plot_units2,
                       dpi = input$plot_dpi2)
   }
 )
 
 #MW 
 MW_plot <- reactive({
   req(inputData(), current_metrics())
   data <- sample_metrics %>%
     purrr::pluck(paste0("MW",inputData()$ion_mode)) %>% 
     tidyr::gather("Sample","Value",-c(Instrument)) %>%
     dplyr::add_row(.,Instrument = "X", Sample = inputData()$sample_type, Value = current_metrics()$MW_metric[[1]]) 
   data %>%
     ggplot(aes(Sample,Value, group = Sample, fill = Sample)) +
     stat_boxplot(geom = "errorbar", width = 0.2)+
     geom_boxplot(alpha = 0.5)+
     {if(!input$add_jitter) geom_text(aes(label = Instrument, colour = Instrument))}+
     {if(input$add_jitter) geom_text(aes(label = Instrument, colour = Instrument), position = position_jitter(seed = 2000))}+
     scale_colour_manual(values = expand_palette(data$Instrument, "black", c("X" = "red")))+
     labs(y = "MW metric")+
     theme(legend.title = element_blank(),
            legend.position = "none")
 }) 
 
 output$MW_plot <- plotly::renderPlotly({
   MW_plot() %>%
     plotly::ggplotly(.)
 })
 
 output$download_MW <- downloadHandler(
   filename =  function() {paste("MW_plot", input$dl_plot_format2, sep=".")},
   content = function(file) {
     MW_plot() %>% 
       InterLabStudy::multi_plot_save(file, plot =  . ,
                       type = input$dl_plot_format2,
                       width = input$plot_width2,
                       height = input$plot_height2,
                       units = input$plot_units2,
                       dpi = input$plot_dpi2)
   }
 )
    
}
    
## To be copied in the UI
# mod_Report_ui("Report_ui_1")
    
## To be copied in the server
# callModule(mod_Report_server, "Report_ui_1")

