library(tidyverse)
library(readxl)
library(sqldf)

#when on home comp
setwd("~/SWB Projects/Chicago 400 Alliance/VAD")

df <- read_excel("VAD.VAI_Dec2019-Mar2020.xlsx",
                 skip=1,
                 trim_ws=TRUE,
      col_types=c("text","text", "text", "text", "text", "text", "date",
                  "text", "date", "text", "text", "text"),
      col_names=c("IDOC","Offender","HOLDOFF","COMMITCNTY",
                  "AGE","LOCATION","GATEVIDT","DEST",
                  "PRMSRDT", "DISCHRDT","OFFSTAT","SEXOFF")
  )


#Remove the footnotes that were read in and empty records
df2 <- df[!grepl("*Violate",df$Offender) & !is.na(df$Offender),]

#Cut first 7 characters out of the Name. then split by comma to get first 
#and last name then remove middle initial
#from last name.
Offender2 <- substring(df2$Offender, 8)

#write code to find, who doesn't have a "comma" in the Name field
check <- df2 %>%  
  filter(str_detect(Offender,',',negate = TRUE))

Names <- strsplit(Offender2, ",")
#any elements in this list that have only 1 data point??
check <- Names[lapply(Names, length) == 1]

mat <- matrix(unlist(Names), ncol=2, byrow=TRUE)
matLast <- as_tibble(mat)

First_split <- strsplit(matLast$V2, " ")
First_Name <- gsub("\\ .*", "", matLast$V2)
matFirst <- as_tibble(First_Name)
colnames(matFirst) <- "FNAME"
colnames(matLast) <- c("LNAME","drop")


df3 <- cbind(matLast, matFirst, df2)
finaldf <- df3 %>% select(-drop,-Offender)


#We only want the individuals that are homeless
table(finaldf$HOLDOFF)
check <- unique(finaldf$HOLDOFF)
sort(check)
datac <- table(check)

#violators <- finaldf %>%
 # filter(str_detect(HOLDOFF,'Sex Off Fail to Report'))


#data finalization
violators_1 <- finaldf %>% arrange(IDOC,LNAME,FNAME)

violators_2 <- sqldf("select *
                      from violators_1 
                      group by IDOC, LNAME, FNAME
                      order by IDOC, LNAME, FNAME")

write.csv(violators_2[1:40,], "~/SWB Projects/Chicago 400 Alliance/VAD/VAD_example.csv")

violators_2$VADFL <- "Y"

write.csv(violators_2, "~/SWB Projects/Chicago 400 Alliance/VAD/VAD.csv")
