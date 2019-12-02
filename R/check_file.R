#' Check file type and structure is correct
#' @return NULL or a character vector#' 
#' @param data file to check for problems
#' @export
#' @author Hugh Jones \email{hj.hujones.hugh@googlemail.com}
#' @importFrom readr problems
check_file <- function(data) {
  if (any({
    ncol(data) != 3
  },
  {
    !is.numeric(data[[1]])
  },
  {
    !is.numeric(data[[2]])
  },
  {
    !is.character(data[[3]])
  },
  {
    nrow(readr::problems(data)) > 0
  }))
  {
    "WARNING: Incorrect file format, problems detected"
  }
  else
  {
    NULL
  }
}

