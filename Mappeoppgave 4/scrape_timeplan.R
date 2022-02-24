rm(list=ls())

library(rvest)
library(tidyverse)
library(rlist)

# The URL
browseURL("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list")

url <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list"

page <- read_html(url)

table <- html_nodes(page, 'table') # one table per week
table <- html_table(table, fill=TRUE) # force them into a list

table[[1]]
dframe <- list.stack(table) # stack the list into a data frame
dframe

# define first row as variable name
colnames(dframe) <- dframe[1,]
dframe

# remove the rows with Dato in it
dframe <- dframe %>% filter(!Dato=="Dato")

str(dframe)

# Separate the Dato into two columns:
dframe <- dframe %>% separate(Dato, 
                              into = c("Dag", "Dato"), 
                              sep = "(?<=[A-Za-z])(?=[0-9])")

# remove last observation (duplicate)
dframe <- dframe[-length(dframe$Dag),]

# code into date format
dframe$Dato <- as.Date(dframe$Dato, format="%d.%m.%Y")
# generate a week variable
dframe$Uke <- strftime(dframe$Dato, format = "%V")

# select
dframe <- dframe %>% select(Dag,Dato,Uke,Tid,Rom)
dframe
str(dframe)

# task: build a procedure that can scrape "many" courses, e.g. the 3 courses you have this semester, from a list
