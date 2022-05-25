library(tidyverse)
library(readxl)
library(sqldf)
library(lubridate)


#when on home comp
setwd("~/SWB Projects/Chicago 400 Alliance/Gun Registry")

df <- read_excel("Gun Registry Feb 20 (all wards).xlsx",skip=1,trim_ws=TRUE,
                 col_types=c("text", "text", "text", "text", "text", "text",
                             "text", "text", "text", "text", "text","text","text",
                             "text"),
                 col_names=c("WARD","PHOTO","LNAME","FNAME","BLOCK",
                             "SEX","RACE","DOB","AGE","HEIGHT", "WEIGHT", "CONVICDT",
                             "STATUTE","STATDESC")
)


df$DOB <- mdy(df$DOB)
df$CONVICDT <- mdy(df$CONVICDT)
View(df)

GUN <- df %>% arrange(LNAME,FNAME,DOB) %>%
          select(LNAME,FNAME,SEX,RACE,DOB,AGE,HEIGHT,WEIGHT,BLOCK,CONVICDT,STATUTE,STATDESC,WARD,PHOTO)

GUN <- sqldf("select * 
                      from GUN 
                      group by LNAME, FNAME, DOB
                      order by LNAME, FNAME, DOB")

write.csv(GUN[1:40,], "~/SWB Projects/Chicago 400 Alliance/Gun Registry/Gun_Registry_example.csv")

GUN$GUNREGFL <- "Y"

write.csv(GUN, "~/SWB Projects/Chicago 400 Alliance/Gun Registry/Gun_Registry.csv")

##Notes about GUN records, the following people have two records, data seems the same just written differently for 
### the statute

#CROSBY, JEWELL
#DANIEL, JOWIL
#DAVENPORT, DEREK
