library(usethis)
library(devtools)
library(dplyr)
library(ggplot2)

db1 <- read.csv2("data-raw/scenarios_db.csv")

count_ind<- function(x,t){
  
  db <- x %>%
    group_by(.data[[t]]) %>%
    count()
  ggplot(data= db) + geom_bar(aes(x= .data[[t]], y=n), stat="identity")+
    theme_bw()+
    theme(axis.text = element_text(angle = 90))

  }

count_ind(x=db1, t="NCP")


