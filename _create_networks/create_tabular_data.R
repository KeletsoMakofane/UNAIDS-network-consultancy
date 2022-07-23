
library(tidyverse) 
library(lubridate)


table_hiv        <- read_csv("/Users/keletsomakofane/Documents/_data/AHRI/hiv_surveillance_2020/RD05-99 ACDIS HIV All.csv")
table_episodes   <- read_csv("/Users/keletsomakofane/Documents/_data/AHRI/ahri1/_memberstatusobservationsdownload/RD01-11 ACDIS Member Status Observations/RD01-11 ACDIS Member Status Observations.csv")

master_table <- table_episodes %>%
  filter(lubridate::year(VisitDate) %in% c(2018,2019)) %>%
  left_join(table_hiv, by=c("IIntId", "VisitDate")) %>%
  arrange(IIntId, VisitDate)
