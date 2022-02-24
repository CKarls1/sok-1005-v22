setwd("~/Desktop/GitHub/sok-1005_v22/Mappeoppgave 4")

library(tidyverse)
library(rvest)
library(rlist)



# insperert av 
# https://datacornering.com/use-data-frame-row-as-a-column-names-in-r/



scrape <- function(url, Emnekode) {
  return(read_html(url) %>% 
           html_nodes(.,'table') %>% 
           html_table(., fill = TRUE) %>% 
           list.stack(.) %>% 
           janitor::row_to_names(., 1) %>% 
           separate(Dato,
                    into = c("Dag", "Dato"),
                    sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
           .[-length(.$Dag),] %>%
           filter(!str_detect(Dato, "Dato"), 
                  !str_detect(Beskrivelse, "international|(WISEFLOW)| Alta")) %>% 
           zoo::na.locf(.) %>% 
           mutate(Dato=as.Date(Dato, format = "%d.%m.%Y"), 
                  Uke = strftime(Dato, format = "%V"), 
                  Dag = strftime(Dato, format = "%A") )%>% 
           select(Dag,Dato,Uke,Tid,Rom, Lærer,Beskrivelse))
}


SOK_1005 <- scrape("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list", "SOK-1005")
BED_2032 <- scrape("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=BED-2032-1&View=list", "BED-2031")
BED_2021 <- scrape("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=BED-2021-1&View=list", "BED-2021")

timeplan <- bind_rows(SOK_1005, BED_2032, BED_2021)

