#' Save a ggplot object
#'
#' 
#'
#' @param file Filepath to save plot to
#' @param plot A ggplot object
#' @param type One of "png", "pdf", "eps", or "tiff"
#' @param width Plot width
#' @param height Plot height
#' @param units One of "in", "cm", or "mm"
#' @param dpi Plot resolution in dpi
#'
#'
#' @export
#' @examples \dontrun{ multi_plot_save("test.png",test_plot,"png",6,6,"in",600) }
#' @author Hugh Jones \email{hj.hujones.hugh@googlemail.com}

multi_plot_save<-function(file,plot,type,width,height,units,dpi){
  switch(type,
         "png" = ggplot2::ggsave(file = file, plot = plot, width = width, height = height, units = units, device = ragg::agg_png, res = dpi),
         "pdf" = ggplot2::ggsave(file = file, plot = plot, width = width, height = height, units = units, device = grDevices::cairo_pdf, dpi = dpi),
         "eps" = ggplot2::ggsave(file = file, plot = plot, width = width, height = height, units = units, device = grDevices::cairo_ps, dpi = dpi, fallback_resolution = 600),
         "tiff" = ggplot2::ggsave(file = file, plot = plot, width = width, height = height, units = units, device = ragg::agg_tiff, res = dpi)
  )
  }