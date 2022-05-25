library(tidyverse)
library(readxl)
library(lubridate)
# TO WORK ON NEXT!!!

# create a function that reads in all the datasets with text format on all date
# fields.
#then write a function that will check if characters exist in the date field
# if so then use dmy function
# use nchars to see length and if less than 8 add leading zero
# for mmddyyyy dates use mdy function on them

setwd("~/SWB Projects/Chicago 400 Alliance/IL Prison Population2")












#When on work comp
#setwd("~/My Documents/SWB/Chicago 400 Alliance/IL Prison Population Repository")

#when on home comp
setwd("~/SWB Projects/Chicago 400 Alliance/IL Prision Population")

files <- (Sys.glob("*.xls"))

listOfFiles <- lapply(files, function(x) 
  read_excel(path=x,skip=6,trim_ws=TRUE,
             col_types=c("text","text", "date", "text", "text", "text", "date",
                         "text", "text", "date", "date", "date", "date",
                         "text", "text", "text", "text", "text", "text"),
             col_names=c("IDOC","Name","DOB","SEX","RACE","VETSTAT",
                         "CURADMDT","ADMTYPE","PARENTINT",
                         "PRJMSRDT","PRJDISDT","CUSTDT","SENTCDT",
                         "CRIMCLAS","HOLDOFF","SENTCYRS","SENTCMHS",
                         "TRTHSENT","SENTCCNY")
  ))

listOfFiles <- setNames(listOfFiles, files)

df <- bind_rows(listOfFiles, .id = "SOURCE")

df$DOB <- ymd(df$DOB)
df$CURADMDT <- ymd(df$CURADMDT)
df$PRJMSRDT <- ymd(df$PRJMSRDT)
df$PRJDISDT <- ymd(df$PRJDISDT)
df$CUSTDT <- ymd(df$CUSTDT)
df$SENTCDT <- ymd(df$SENTCDT)

setwd("~/SWB Projects/Chicago 400 Alliance/IL Prision Population/otherdateformat")

files2 <- (Sys.glob("*.xls"))

listOfFiles2 <- lapply(files2, function(x) 
  read_excel(path=x,skip=6,trim_ws=TRUE,
             col_types=c("text","text", "text", "text", "text", "text", "text",
                         "text", "text", "text", "text", "text", "text",
                         "text", "text", "text", "text", "text", "text"),
             col_names=c("IDOC","Name","DOB","SEX","RACE","VETSTAT",
                         "CURADMDT","ADMTYPE","PARENTINT",
                         "PRJMSRDT","PRJDISDT","CUSTDT","SENTCDT",
                         "CRIMCLAS","HOLDOFF","SENTCYRS","SENTCMHS",
                         "TRTHSENT","SENTCCNY")
  ))

listOfFiles2 <- setNames(listOfFiles2, files2)

df2 <- bind_rows(listOfFiles2, .id = "SOURCE")

df2$DOB <- mdy(df2$DOB)
df2$CURADMDT <- mdy(df2$CURADMDT)
df2$PRJMSRDT <- mdy(df2$PRJMSRDT)
df2$PRJDISDT <- mdy(df2$PRJDISDT)
df2$CUSTDT <- mdy(df2$CUSTDT)
df2$SENTCDT <- mdy(df2$SENTCDT)


fulldf <- bind_rows(df,df2)
fulldf2 <- fulldf[nchar(fulldf$IDOC) == 6 & !is.na(fulldf$IDOC) & !is.na(fulldf$Name),]
fulldf2$Name <- str_replace(fulldf2$Name, ",.", ", ")

#write code to find, who doesn't have a "comma" in the Name field
check <- fulldf2 %>%  
  filter(str_detect(Name,', ',negate = TRUE))


#Split 
Names <- strsplit(fulldf2$Name, ", ")
#any elements in this list that have only 1 data point??
check <- Names[lapply(Names, length) == 1]

mat <- matrix(unlist(Names), ncol=2, byrow=TRUE)
matLast <- as_tibble(mat)

First_Name <- gsub("\\ .*", "", matLast$V2)
matFirst <- as_tibble(First_Name)
colnames(matFirst) <- "FNAME"
colnames(matLast) <- c("LNAME","drop")

#need to investigate why the Name split has less records, missing Name??
df3 <- cbind(matLast, matFirst, fulldf2)

finaldf <- df3 %>% select(-drop,-Name)


#some will be in multiple files, get rid of duplicates by the identifiers
prison <- finaldf %>% arrange(IDOC,LNAME,FNAME,DOB,CUSTDT)

prison_1 <- sqldf("select *
                      from prison 
                      group by IDOC,LNAME,FNAME,DOB,CUSTDT
                      order by IDOC,LNAME,FNAME,DOB,CUSTDT")

write.csv(prison_1[1:40,-3], "~/SWB Projects/Chicago 400 Alliance/IL Prision Population/prison_example.csv")

prison_1$ILPRISFL <- "Y"

write.csv(prison_1, "~/SWB Projects/Chicago 400 Alliance/IL Prision Population/prison.csv")





prison2 <- fulldf %>% 
          group_by(Name, DOB, Sex, Race) %>%
          mutate(count=n()) %>%
          select(count,everything())

View(prison2)


prison3 <- prison2 %>% filter(count>2)
View(prison3)

prison4 <- prison2 %>% filter(Name=="ADAMS, ROBERT")

View(prison4)





