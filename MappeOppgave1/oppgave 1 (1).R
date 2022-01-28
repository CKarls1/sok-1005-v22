library(readr)
library(ggplot2)
library(zoo)
library(tidyverse)
library(data.table)

### Oppgave 1

lowertrop <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

lowertrop <- subset(lowertrop, select = -c(Land:AUST)) %>%
  filter(Year != max(Year), Year != min(Year))

lowertrop$Globe <- as.numeric(lowertrop$Globe)

lowertrop = lowertrop %>%
  mutate(tempmean = rollmean(Globe, k=13, fill=NA, align ='right'),
         Dato = as.yearmon(paste(lowertrop$Year, lowertrop$Mo), "%Y %m"))
lowertrop$Dato <- as.Date(as.yearmon(lowertrop$Dato))

ggplot(lowertrop, aes(x = Dato, y = Globe))+
  geom_point(color = "blue", size = 1.5) + 
  geom_line(color = "blue", size = 0.5) +
  theme_bw()+
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("År") + 
  ylab("Månedlig gjennomsnittsendring i temperatur") +
  ggtitle("Siste globale gjennomsnittlige lavere troposfæriske temperaturer") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 6)) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 10)) +
  geom_line(color ="red", aes(y = lowertrop$tempmean)) +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
  scale_y_continuous(breaks=scales::breaks_pretty(n=20),
                     expand = expansion(add = 0.1))

### Oppgave 2

Lower_Troposphere <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
Mid_Troposphere <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
Tropopause <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
Lower_Stratosphere <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

Lower_Troposphere <- subset(Lower_Troposphere, select =c(Year,Mo,NoPol)) %>%
  filter(Year != max(Year), Year != min(Year))%>%
  mutate(Atmosfæren = "Nedre troposfære")

Mid_Troposphere <- subset(Mid_Troposphere, select =c(Year,Mo,NoPol)) %>%
  filter(Year != max(Year), Year != min(Year)) %>%
  mutate(Atmosfæren = "Midt troposfæren")

Tropopause <- subset(Tropopause, select =c(Year,Mo,NoPol)) %>%
  filter(Year != max(Year), Year != min(Year)) %>%
  mutate(Atmosfæren = "Troposfæren")

Lower_Stratosphere <- subset(Lower_Stratosphere, select =c(Year,Mo,NoPol)) %>%
  mutate(Atmosfæren = "Nedre stratosfære") %>%
  mutate_all(as.character)

Tabell <- bind_rows(Lower_Troposphere, Mid_Troposphere, Tropopause, Lower_Stratosphere)

Tabell$NoPol <- as.numeric(Tabell$NoPol)

Tabell <- Tabell %>%
  mutate('Rullerende Gjennomsnitt' = rollmean(NoPol, k=13, fill=NA, align ='right'), 
         Dato = as.yearmon(paste(Tabell$Year, Tabell$Mo), "%Y %m")) 

Tabell$Dato <- as.Date(Tabell$Dato)

ggplot(Tabell,aes(x = Dato, y = NoPol, color = Atmosfæren)) +
  geom_path(size = 0.5) +
  geom_hline(yintercept = 0) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("År") + 
  ylab("Tempraturer") + 
  ggtitle("Varierende tempraturer fra 60 grader til 90 grader Nord") +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 6)) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 11)) +
  geom_line(aes(y=Tabell$'Rullerende Gjennomsnitt', color = 'Rullerende Gjennomsnitt' )) +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
  scale_y_continuous(breaks=scales::breaks_pretty(n=20), expand = expansion(add = 1))

