library(tidyverse)
library(readxl)
library(sqldf)
library(lubridate)

all <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/KnownAddresses.csv")


homeless <- all %>%  
  filter(str_detect(STREET,'HOMELESS|HMLS'))

homeless1_1 <- sqldf("select * 
                      from homeless 
                      group by LNAME, FNAME, DOB
                      order by LNAME, FNAME, DOB")

write.csv(homeless1_1[1:40,], "~/SWB Projects/Chicago 400 Alliance/Homeless_example.csv")

write.csv(homeless1_1, "~/SWB Projects/Chicago 400 Alliance/Homeless.csv")
