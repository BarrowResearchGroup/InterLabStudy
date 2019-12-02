#' Expand colour palette 
#'
#' @param vector vector to generate colours for
#' @param default_colour default colour to use
#' @param named_colours named vector for colours to use
#'
#' @return an vector of colours
#'
#' @export
#' @examples
#' expand_palette(c("a","b","c"), "black", c("c" = "red"))
#' @author Hugh Jones \email{hj.hujones.hugh@googlemail.com}

expand_palette<-function(vector, default_colour = "black", named_colours){
  if(missing(named_colours)){
    named_colours<-NULL
  }
  number <- dplyr::n_distinct(vector)
  rep_palette <- rep(c(default_colour), length.out = number - length(named_colours))
  names(rep_palette) <- unique(vector)[ !unique(vector) %in% names(named_colours)]
  palette<-c(named_colours, rep_palette)
  return(palette)
}
