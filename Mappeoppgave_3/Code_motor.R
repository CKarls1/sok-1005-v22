setwd("~/Desktop/GitHub/sok-1005_v22/Mappeoppgave_3")

# Har sammarbeidet med Rudi Hansen, Marcus Edvardsen og levert individuele besvarelser.
library(tidyverse)
library(rvest)
library(dplyr)
library(janitor)
library(plotly)

# inspirert av 
# https://stackoverflow.com/questions/52722846/how-can-i-remove-non-numeric-characters-from-strings-using-gsub-in-r?fbclid=IwAR0pKhomymoThSLBaWiVTaDGudQOTg4WxOc3A6mkvmhNICA-0hi8VZfkZSQ
# https://www.opencodez.com/how-to-guide/how-to-use-xpath-for-web-scraping-with-r.htm
Rekkeviddetall <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"  %>% 
  read_html() %>% 
  html_nodes(xpath = '/html/body/article/section/div[4]/div[4]/div[1]/table') %>% # henter inn tabellen ved å kopiere xpathen fra sidekildenen
  html_table(header = 1, fill = TRUE) %>% # setter at første rad blir header for hver kolonne 
  as.data.frame() %>% 
  filter(!str_detect(Avvik, "x")) %>% # filterer bort de modellene uten data 
  separate(`WLTP.tall`, sep = "/", into=c("WLTP","kWh")) %>% # Separerer WLTP-tall til to kolonner for å forenkle og at det ser ryddigere ut
  mutate(STOPP = as.numeric(gsub("km", "", STOPP))) %>% #fjerner km for å kunne gjøre det numeric
  mutate(WLTP = as.numeric(gsub("km", "", WLTP)))  %>% #fjerner km for å kunne gjøre det numeric
  rename("Modell" = Modell..temp..varierte.fra.0..til..10..) %>% # endrer navn på titel for å få ggplotly til å bli penere
  ggplot(., aes(x = WLTP, y = STOPP, label = Modell)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "red")  +
  scale_y_continuous(limits = c(200, 700)) + 
  scale_x_continuous(limits = c(200, 700)) +
  ggtitle("Kjøreavstand til elbiler. \nTemp. varierte fra 0° til -10°") +
  theme_minimal()


ggplotly(Rekkeviddetall) #henter tabellen gjennom ggplotly for å kunne se hvilke data hver enkelt plott har. 



 
lm(STOPP ~ WLTP, data = Rekkeviddetall$data) 

  ggplotly(Rekkeviddetall + geom_smooth(method = lm))
  
# Ved å se på koeffisient intervallet ser vi at for hver økninning av stopp på en vil wltp ha en økning på 0.8671. 
# Ser vi fra den røde linja som viser et perfekt forhold mellom stopp og wltp er det en posetiv korrelasjon, men den ligger litt lavere ned 
# som indikerer avviket forresaket av kulden. Vi ser også at det er ikke så mye avvik mellom wltp og stopp heller. 





 

