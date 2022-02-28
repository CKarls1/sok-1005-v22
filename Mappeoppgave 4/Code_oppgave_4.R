setwd("~/Desktop/GitHub/sok-1005_v22/Mappeoppgave 4")

library(tidyverse)
library(rvest)
library(rlist)

# samarbeidet med Rudi Hansen, Marcus Edvardsen og Yves Sebazungu
# alle leverer individuelle besvarelser 

# inspirert av 
# https://datacornering.com/use-data-frame-row-as-a-column-names-in-r/
# utgitte forelesninger i kurset



#lager en liste med alle fagene vi har. SOK-1005, BED-2021, BED-2032
# benytter de fag kodene jeg selv har da jeg har tre valgfag og ikke samme kursplan.
Timeplan_URL <- list("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list", 
                     "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=BED-2032-1&View=list",
                     "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=BED-2021-1&View=list")






Skrape_func <- function(url) {
  return(read_html(url) %>% 
           html_nodes(.,'table') %>% 
           html_table(., fill = TRUE) %>% 
           list.stack(.) %>% # slår sammen listene til et datasett
           janitor::row_to_names(., 1) %>% # setter første rad som header
           separate(Dato,
                    into = c("Dag", "Dato"),
                    sep = "(?<=[A-Za-z])(?=[0-9])") %>% # separerer dag og dato
           .[-length(.$Dag),] %>%
           filter(!str_detect(Dato, "Dato"), 
                  !str_detect(Beskrivelse, "international|(WISEFLOW)| Alta")) %>% # filtrerer de variablene jeg ikke ønsker 
           zoo::na.locf(.) %>%  # Fyller inn NA med veridien over, da enkelte emner har samme dato på to forskjellige steder
           mutate(Dato=as.Date(Dato, format = "%d.%m.%Y"), 
                  Uke = strftime(Dato, format = "%V"), 
                  Dag = strftime(Dato, format = "%A") )%>% # lager en ny variabel for dag så de tomme dataene blir fylt med riktig dag
           select(Dag,Dato,Uke,Tid,Rom, Lærer)) # velger det som skal inn i listen
}


timeplan <- map(Timeplan_URL, Skrape_func) 
timeplan
