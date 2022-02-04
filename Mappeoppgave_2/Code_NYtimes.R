setwd("~/Desktop/GitHub/sok-1005_v22/Mappeoppgave_2")

library(jsonlite)
library(tidyverse)
library(ggrepel)
library(rvest)
library(countrycode)


#Henter data fra sidekilder
Raw_data <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

Raw_data <- Raw_data %>% 
  mutate(countryCode = state.abb[match(name,state.name)])

Raw_data[is.na(Raw_data)] = "D.C."


ggplot(Raw_data, aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k)) + 
  geom_point(colour = "palegreen3") + 
  geom_text(aes(label = countryCode),hjust=0, vjust=-1.5, size = 3) +
  labs(title = "Andel av befolkning som er fullvaksinert","Antall døde per 100.000") +
  ggtitle("20 måndelige døde i gjennomsnitt pr 100.000") + 
  scale_x_continuous(labels = scales::percent, limits=c(0.45, 0.80), breaks=seq(0.45, 0.80, by = 0.05)) + 
  theme_bw() + 
  annotate("text", x=0.50, y=15, 
           label= "Lavere vaksinasjonsrate,\n høyere dødsrate") +
  annotate("segment", x = 0.49, 
           xend = 0.48, y = 15.5, 
           yend = 17, colour = "black", arrow = arrow())+
  annotate("text", x=0.75, y=9, 
           label= "Høyere vaksinasjonsrate,\n lavere dødsrate") +
  annotate("segment", x = 0.75, 
           xend = 0.78, y = 8, 
           yend = 6, colour = "black", arrow = arrow())



## oppgave 2


lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = Raw_data)


ggplot(Raw_data, aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k)) + 
  geom_point(colour = "palegreen3") + 
  geom_text(aes(label = countryCode),hjust=0, vjust=0, size = 3) +
  geom_smooth(method = lm)+
  xlab("Andel av befolkning som er fullvaksinert") + 
  ylab("Antall døde per 100.000") +
  ggtitle("20 måndelige døde i gjennomsnitt pr 100.000") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 10)) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 10)) +
  scale_x_continuous(labels = scales::percent, limits=c(0.45, 0.80), breaks=seq(0.45, 0.80, by = 0.05)) + 
  theme_bw() + 
  annotate("text", x=0.50, y=15, 
           label= "Lavere vaksinasjonsrate,\n høyere dødsrate") +
  annotate("segment", x = 0.49, 
           xend = 0.45, y = 15.5, 
           yend = 17, colour = "black", arrow = arrow())+
  annotate("text", x=0.75, y=9, 
           label= "Høyere vaksinasjonsrate,\n lavere dødsrate") +
  annotate("segment", x = 0.75, 
           xend = 0.78, y = 8, 
           yend = 6, colour = "black", arrow = arrow())









