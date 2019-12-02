#' Count atoms in a formula
#'
#' 
#'
#' @param formula A chemical formula
#' @param element A chemical element
#'
#' @return an integer
#'
#' @importFrom magrittr %>%
#' @export
#' @examples
#' count_atoms("C6H12O6","C")
#' @author Hugh Jones \email{hj.hujones.hugh@googlemail.com}

count_atoms<-function(formula,element){
  
  
  if(!is.character(formula)) {
    stop("formula is not of class character", call. = FALSE)
  }
  
  if(!is.character(element)) {
    stop("element is not of class character", call. = FALSE)
  }
  
  if(any(stringr::str_count(element) > stringr::str_count(formula))) {
    stop("element longer than formula, did you invert the position of the 2?", call. = FALSE)
  }
  
  if(any(stringr::str_detect(element, "^[:alpha:]{1,2}$", negate = TRUE))) {
    stop("element is not valid", call. = FALSE)
  }
  
  formula %>% 
    stringr::str_extract_all(.,paste0('(?<=',element,')[:digit:]+')) %>%
    purrr::map(as.integer) %>%
    purrr::map(sum) %>%
    tidyr::replace_na(0) %>%
    unlist() 
}
