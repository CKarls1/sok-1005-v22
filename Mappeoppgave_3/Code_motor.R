setwd("~/Desktop/GitHub/sok-1005_v22/Mappeoppgave_3")


library(rvest)
library(tidyverse)
library(janitor)
library(dplyr)


raw_data <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"
Rekkeviddetall <- raw_data %>% 
  read_html() %>% 
  html_nodes(xpath = '/html/body/article/section/div[4]/div[4]/div[1]/table') %>% 
    html_table(header = 1, fill = TRUE) %>% 
  as.data.frame() 
 


sapply(Rekkeviddetall, class)  


