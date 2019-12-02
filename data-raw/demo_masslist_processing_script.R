library(tidyverse)
library(magrittr)


#generate demo masslist

masslist<- read_csv(.)

mono_atom_to1<-function(string){
 
  raw_formula <- string %>%
    str_remove(.,pattern =  '(\\[[:alpha:]+\\])') %>%
    str_trim("both")
  raw_formula %>%
    str_replace_all(pattern = '([:lower:]{1})(?!\\d)',replacement = '\\11') %>%
    str_replace_all(pattern = '([:upper:])(?![:lower:])(?!\\d)',replacement = '\\11') %>%
    str_trim("both") %>%
    return()
  
}


#utilise m/z, int, formula format
# composer utilses [] for adducts, simply removing them should bring formulas 
# inline with master-file
masslist %>%
  select(`Peak m/z`,Intensity,Formula) %>%
  set_colnames(c("mz","int","formula")) %>%
  mutate(formula = mono_atom_to1(formula)) %>%
  write_csv(.,path = "data-raw/demo_masslist.csv")
  




