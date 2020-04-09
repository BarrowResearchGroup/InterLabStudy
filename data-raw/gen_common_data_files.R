
library(tidyverse)
library(magrittr)



#path2<-"" #set path here

#composer style representation with []
gen_formula_composer<-function(data){
  mode<-data$mode[[1]]
  switch(mode,
         "neg" = mutate(data, formula = glue::glue("C{C}H{H-1}N{N}O{O}S{S} [H]") %>% str_remove_all(.,'[:alpha:]{1,2}0')),
         "pos" = mutate(data, formula = glue::glue("C{C}H{H}N{N}O{O}S{S} [Na]") %>% str_remove_all(.,'[:alpha:]{1,2}0')))
}
#Compacts character strings together
gen_formula_generic<-function(data){
  mode<-data$mode[[1]]
  switch(mode,
         "neg" = mutate(data, formula = glue::glue("C{C}H{H}N{N}Na{Na}O{O}S{S}") %>% str_remove_all(.,'[:alpha:]{1,2}0')),
         "pos" = mutate(data, formula = glue::glue("C{C}H{H}N{N}Na{Na}O{O}S{S}") %>% str_remove_all(.,'[:alpha:]{1,2}0')))
}


#generate common data
common_data<-list.files(path2,full.names = TRUE) %>%
  stringr::str_subset(.,'\\/common\\_') %>%
  {paths<- .
  paths %>%
    purrr::map(~readr::read_csv(.x, col_types = 'iiiiiid')) %>%
    magrittr::set_names(.,basename(paths) %>% stringr::str_remove_all(.,'\\..*') %>% stringr::str_remove(.,'common_'))
  } %>%
  map(~rename(.x, mz = `m/z`)) %>%
  map2(.,names(.),~mutate(.x, mode = str_extract(.y,'(?<=\\_).+'))) %>%
  map(~gen_formula_generic(.x)) %>%
  map(~mutate(.x,
              H_C = H/C,
              O_C = O/C,
              dbe = C - H/2 + N/2 + 1)
      )



save(common_data, file = "data-raw/common_data.RData")
  

#generate detected data
detected_data<-list.files(path2,full.names = TRUE) %>%
  stringr::str_subset(.,'\\/detected\\_') %>%
  {paths<- .
  paths %>%
    purrr::map(~readr::read_csv(.x, col_types = 'iiiiiid')) %>%
    magrittr::set_names(.,basename(paths) %>% stringr::str_remove_all(.,'\\..*') %>% stringr::str_remove(.,'detected_'))
  } %>%
  map(~rename(.x, mz = `m/z`)) %>%
  map2(.,names(.),~mutate(.x, mode = str_extract(.y,'(?<=\\_).+'))) %>%
  map(~gen_formula_generic(.x)) %>%
  map(~mutate(.x,
              H_C = H/C,
              O_C = O/C,
              dbe = C - H/2 + N/2 + 1)
  )



save(detected_data, file = "data-raw/detected_data.RData")

path3 <- "/Supplementary/Metrics" #set full path here
#generate sample metrics
sample_metrics<-list.files(path3,full.names = TRUE) %>% 
  {paths<- .
  paths %>%
    purrr::map(~readxl::read_xlsx(.x)) %>%
    magrittr::set_names(.,basename(paths) %>% stringr::str_remove_all(.,'\\..*'))
  }

save(sample_metrics, file = "data-raw/sample_metrics.RData")

