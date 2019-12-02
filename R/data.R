#' Collated sample metrics 
#' 
#' A list of 8 data frames 
#' 
#' @format A list with 8 levels:
#' \describe{
#' \item{AIneg}{Data table holding collated negative mode AI mod sample metrics}
#' \item{AIpos}{Data table holding collated positive mode AI mod sample metrics}
#' \item{HCneg}{Data table holding collated negative mode H/C sample metrics}
#' \item{HCpos}{Data table holding collated positive mode H/C sample metrics}
#' \item{MWneg}{Data table holding collated negative mode MW sample metrics}
#' \item{MWpos}{Data table holding collated positive mode MW sample metrics}
#' \item{OCneg}{Data table holding collated negative mode O/C sample metrics}
#' \item{OCpos}{Data table holding collated positive mode O/C sample metrics}
#' }
#' Each data frame has 5 variables:
#' \describe{
#' \item{Blank}{Lab identifier}
#' \item{ESFA}{Value of metric for ESFA sample}
#' \item{PLFA}{Value of metric for PLFA sample} 
#' \item{SRFA}{Value of metric for SRFA sample}
#' \item{SRNOM}{Value of metric for SRNOM sample}
#' }
#' @seealso Other study data can be found in \code{common_data} and \code{detected_data}
"sample_metrics"

#' Molecular formula assignments found in all datasets
#' 
#' A list of 8 data frames
#' 
#' @format A list with 8 levels
#' \describe{
#' \item{ESFA_neg}{Data table holding negative mode assignments for ESFA}
#' \item{ESFA_pos}{Data table holding positive mode assignments for ESFA}
#' \item{PLFA_neg}{Data table holding negative mode assignments for PLFA}
#' \item{PLFA_pos}{Data table holding positive mode assignments for PLFA}
#' \item{SRFA_neg}{Data table holding negative mode assignments for SRFA}
#' \item{SRFA_pos}{Data table holding positive mode assignments for SRFA}
#' \item{SRNOM_neg}{Data table holding negative mode assignments for SRNOM}
#' \item{SRNOM_pos}{Data table holding positive mode assignments for SRNOM}
#' }
#' Each data frame has 12 variables:
#' \describe{
#' \item{C}{Number of carbon atoms in the formula}
#' \item{H}{Number of hydrogen atoms in the formula}
#' \item{O}{Number of oxygen atoms in the formula}
#' \item{N}{Number of nitrogen atoms in the formula}
#' \item{S}{Number of sulfur atoms in the formula}
#' \item{Na}{Number of sodium atoms in the formula}
#' \item{mz}{theoretical calculated m/z of the formula}
#' \item{formula}{assigned formula}
#' \item{H_C}{H/C ratio}
#' \item{O_C}{O/C ratio}
#' \item{dbe}{Double Bond Equivalent}
#' }
#' @seealso Other study data can be found in \code{detected_data} and \code{sample_metrics}
"common_data"

#' Molecular formula assignments found at least three datasets
#' 
#' A list of 8 data frames
#' 
#' @format A list with 8 levels
#' \describe{
#' \item{ESFA_neg}{Data table holding negative mode assignments for ESFA}
#' \item{ESFA_pos}{Data table holding positive mode assignments for ESFA}
#' \item{PLFA_neg}{Data table holding negative mode assignments for PLFA}
#' \item{PLFA_pos}{Data table holding positive mode assignments for PLFA}
#' \item{SRFA_neg}{Data table holding negative mode assignments for SRFA}
#' \item{SRFA_pos}{Data table holding positive mode assignments for SRFA}
#' \item{SRNOM_neg}{Data table holding negative mode assignments for SRNOM}
#' \item{SRNOM_pos}{Data table holding positive mode assignments for SRNOM}
#' }
#' Each data frame has 12 variables:
#' \describe{
#' \item{C}{Number of carbon atoms in the formula}
#' \item{H}{Number of hydrogen atoms in the formula}
#' \item{O}{Number of oxygen atoms in the formula}
#' \item{N}{Number of nitrogen atoms in the formula}
#' \item{S}{Number of sulfur atoms in the formula}
#' \item{Na}{Number of sodium atoms in the formula}
#' \item{mz}{theoretical calculated m/z of the formula}
#' \item{formula}{assigned formula}
#' \item{H_C}{H/C ratio}
#' \item{O_C}{O/C ratio}
#' \item{dbe}{Double Bond Equivalent}
#' }
#' @seealso Other study data can be found in \code{common_data} and \code{sample_metrics}
"detected_data"
