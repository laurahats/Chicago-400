library(tidyverse)
library(readxl)
library(sqldf)
library(lubridate)


#when on home comp
setwd("~/SWB Projects/Chicago 400 Alliance/Chicago Police Dept")

df <- read_excel("CPD_Arrests2017-20 (1).xlsx",skip=1,trim_ws=TRUE,
                 col_types=c("text","text", "text", "text", "text", "text", "text",
                             "text", "text", "text", "text", "text","text","text",
                             "text","text","text","text","text","text","text","text",
                             "text","text","text","text","text","text","text","text",
                             "text","text","text","text","text","text","text"),
                 col_names=c("DROP","ARRESTDT","CB_NO","RD_NO","FNAME","MNAME","LNAME",
                             "SEX","RACE","AGE","LOCKRELD", "DISTRICT", "ARRBEAT",
                             "ARRADD","CHG1FBI","CHG1STAT","CHG1DESC","CHG1TYPE","CHG1CLAS",
                             "CHG2FBI","CHG2STAT","CHG2DESC","CHG2TYPE","CHG2CLAS",
                             "CHG3FBI","CHG3STAT","CHG3DESC","CHG3TYPE","CHG3CLAS",
                             "CHG4FBI","CHG4STAT","CHG4DESC","CHG4TYPE","CHG4CLAS",
                             "BONDAMT","BONDDT","BONDTYPE")
)


df$ARRESTDT <- ymd(df$ARRESTDT)
df$LOCKRELD <- ymd(df$LOCKRELD)

VARIABLES <- c("LNAME", "MNAME", "FNAME", "ARRESTDT", "CB_NO", "RD_NO", "SEX", "RACE",
            "LOCKRELD", "DISTRICT", "ARRBEAT", "ARRADD", "CHG1FBI", "CHG1STAT", "CHG1DESC",
            "CHG1TYPE", "CHG1CLAS", "CHG2FBI", "CHG2STAT", "CHG2DESC", "CHG2TYPE", "CHG2CLAS",
            "CHG3FBI", "CHG3STAT", "CHG3DESC", "CHG3TYPE", "CHG3CLAS", "CHG4FBI", "CHG4STAT",
            "CHG4DESC", "CHG4TYPE", "CHG4CLAS", "BONDAMT", "BONDDT", "BONDTYPE")

finaldf <- df %>% select(VARIABLES)
View(finaldf)

#some will be in multiple files, get rid of duplicates by the identifiers
CPD <- finaldf %>% arrange(LNAME,FNAME,SEX,RACE,ARRESTDT)

CPD_1 <- sqldf("select *
                      from CPD 
                      group by LNAME,FNAME,SEX, RACE, ARRESTDT
                      order by LNAME,FNAME,SEX, RACE, ARRESTDT")

write.csv(CPD_1[1:40,], "~/SWB Projects/Chicago 400 Alliance/Chicago Police Dept/Arrests_example.csv")

CPD_1$CPDFL <- "Y"

write.csv(CPD_1, "~/SWB Projects/Chicago 400 Alliance/Chicago Police Dept/Arrests.csv")

