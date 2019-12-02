#' Custom ggplot2 theme as used by the Barrow group at Warwick
#' 
#' @param base_size The base size, default is 11
#' @param base_family The base family, default is ""
#' @param base_line_size The base line size, default is \code{base_size}/22
#' @param base_rect_size The base rectangle size, default is \code{base_size}/22
#' 
#' @return an ggplot2 theme
#' @export
#' @author Hugh Jones \email{hj.hujones.hugh@googlemail.com}

theme_custom <- function (
  base_size = 11, 
  base_family = "",
  base_line_size = base_size/22, 
  base_rect_size = base_size/22
) {
  half_line <- base_size/2
  ggplot2::theme_bw(base_size = base_size,
           base_family = base_family, 
           base_line_size = base_line_size,
           base_rect_size = base_rect_size) %+replace% 
    ggplot2::theme(
      # legend.position = "none",
      # legend.title=element_blank(),
      strip.background =element_rect(fill= "#4169E1"),
      strip.text = element_text(colour = "white", 
                                size = rel(0.8),
                                face = "bold",
                                margin = margin(0.8 * half_line,
                                                0.8 * half_line,
                                                0.8 * half_line,
                                                0.8 * half_line)),
      # axis.text = element_text(size = 10),
      axis.title.y = element_text(face = "bold", angle = 90, vjust = 2),
      axis.title.x = element_text(face = "bold"))}
