library(tidyverse)
library(readxl)
library(sqldf)
library(lubridate)

#when on home comp
setwd("~/SWB Projects/Chicago 400 Alliance/SOR_contacts")

files <- (Sys.glob("*.xls"))

listOfFiles <- lapply(files, function(x) 
  read_excel(path=x,skip=1,trim_ws=TRUE,
             col_types=c("text","text", "text", "text", "text", "text", "text",
                         "text", "date"),
             col_names=c("LNAME","FNAME","MNAME","STREET","CITY","STATE",
                         "ZIPCODE","COUNTY","DOB")
  ))

df <- bind_rows(listOfFiles, .id = "id")

#In the XLSX files most of them have 9 columns, import these files:
files2 <- (Sys.glob("*.xlsx"))

listOfFiles2 <- lapply(files2, function(x) 
  read_excel(path=x,skip=1,trim_ws=TRUE,
             col_types=c("text","text", "text", "text", "text", "text", "text",
                         "text", "date"),
             col_names=c("LNAME","FNAME","MNAME","STREET","CITY","STATE",
                         "ZIPCODE","COUNTY","DOB")
  ))

df2 <- bind_rows(listOfFiles2, .id = "id")

#to make the above code work the file  "Copy of xSOR.20131005.xlsx" had to be edited to 
# remove a record that was a repeat of column names inside the file
#to make the above code work "Copy of xSOR.20120627.xlsx" had to be edited to 
# remove additional empty columns
#to make the above code work "Copy of xSOR.20130511.xlsx" and "Copy of xSOR.20130615.xlsx" had 
# to be edited to remove additional columns of Conviction County and Conviction State


fulldf <- bind_rows(df,df2)
fulldf2 <- fulldf[!is.na(fulldf$LNAME),]

#We only want the individuals that are homeless
#table(fulldf2$Address)
#check <- unique(fulldf2$Address)
#View(check)
#sort(check)
#datac <- table(check)
#View(datac)

#Search these terms for homeless:
#HOMELESS
#HMLS
#NONE

#possible terms to search listed below to be confirmed
#Shelters
# 5313 S INDIANA AVE Apt. SHLT	
# 5313 S INDIANA AVE	
# 5313 S INDIANA Apt. SHLT	
# 5313 S INDIANA Apt. 1406	
# 5313 S INDIANA	
# 3551 W ROOSEVELT RD Apt. SHLT
# 3551 W ROOSEVELT RD Apt. 12
# 2715 W HARRISON ST Apt. SHLT
# 2715 W HARRISON ST
# 200 S SACRAMENTO BLVD Apt. SHLT
# 200 S SACRAMENTO BLVD
# 11321 S WENTWORTH AVE Apt. SHLT
# 11321 S WENTWORTH AVE

#no information:
# 000000 000
# 000000 00000
# 000000 0000000 Apt. 000000
# TO BE DETERMINED
# CHICAGO
# AWOL NO ADDRESS
# Address Unknown


#homeless1 <- fulldf2 %>% select(-id) %>% 
#  filter(str_detect(Address,'HOMELESS|HMLS'))

#homeless1_1 <- sqldf("select * 
#                      from homeless1 
#                      group by LNAME, FNAME, DOB
#                      order by LNAME, FNAME, DOB")

#write.csv(homeless1_1, "homeless.csv")

setwd("~/SWB Projects/Chicago 400 Alliance/SexOffenders")

#In the XLSX files they have 22 columns, import these files: column 15 is date
files <- (Sys.glob("*.xlsx"))

listOfFiles <- lapply(files, function(x) 
  read_excel(path=x,skip=1,trim_ws=TRUE,
             col_types=c("text","text", "text", "text", "text", "text", "text",
                         "text", "text", "text", "text", "text", "text", "text",
                         "date", "text", "text", "text", "text", "text", "text",
                         "text"),
             col_names=c("LNAME","FNAME","MNAME","STREET","CITY","STATE",
                         "ZIPCODE","COUNTY","X", "Y", "HEIGHT", "WEIGHT", "RACE", "GENDER", "DOB",
                         "STATUS", "CRIMECLAS", "CONVCOUN", "CONVSTAT", "VICTMAGE",
                         "OFFAGE", "CRIMES")
  ))


dfx <- bind_rows(listOfFiles, .id = "id")

#In the XLSX files they have 22 columns, import these files: column 15 is date
setwd("~/SWB Projects/Chicago 400 Alliance/SexOffenders/SplitCrimes")

dfx2 <- read_excel(path="Copy of 20161015144228 SO-1.xlsx",skip=1,trim_ws=TRUE,
                  col_types=c("text","text", "text", "text", "text", "text", "text",
                              "text", "text", "text", "text", "text", "text", "text",
                              "date", "text", "text", "text", "text", "text", "text",
                              "text","text","text","text","text","text","text","text",
                              "text","text"),
                  col_names=c("LNAME","FNAME","MNAME","STREET","CITY","STATE","ZIPCODE",
                              "COUNTY","X", "Y", "HEIGHT", "WEIGHT", "RACE", "GENDER", 
                              "DOB","STATUS","CRIMECLAS","CONVCOUN","CONVSTAT","VICTMAGE","OFFAGE",
                              "CRIMES1","CRIMES2","CRIMES3","CRIMES4","CRIMES5","CRIMES6","CRIMES7",
                              "CRIMES8","CRIMES9","CRIMES10")
)


skipNApaste <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

dfx2$CRIMES <- skipNApaste(dfx2$CRIMES1,dfx2$CRIMES2,dfx2$CRIMES3,dfx2$CRIMES4,dfx2$CRIMES5,dfx2$CRIMES6,dfx2$CRIMES7,
                          dfx2$CRIMES8,dfx2$CRIMES9,dfx2$CRIMES10, sep = ", ", na.rm = T)

dfx2x <- dfx2 %>% select(-CRIMES1,-CRIMES2,-CRIMES3,-CRIMES4,-CRIMES5,-CRIMES6,-CRIMES7,-CRIMES8,-CRIMES9,-CRIMES10)

fulldfx <- bind_rows(dfx,dfx2x)
fulldfx2 <- fulldfx[!is.na(fulldfx$LNAME),]




#hmls_violators <- hmls %>%
#  filter(str_detect(Crimes,'FAILURE TO REPORT ANNUALLY'))

#hmls_violators_1 <- sqldf("select *, count(*) as cnt 
#                      from hmls_violators 
#                      group by Last_Name, First_Name, DOB
#                      order by Last_Name, First_Name, DOB")

#write.csv(hmls_violators_1, "hmls_violators_nodups.csv")






#some will be in multiple files, get rid of duplicates by the identifiers
fulldf2$DOB <- as.Date(fulldf2$DOB)
fulldfx2$DOB <- as.Date(fulldfx2$DOB)

SOR1 <- fulldf2 %>% select(-id) %>% arrange(LNAME,FNAME,DOB)
SOR2 <- fulldfx2 %>% select(-id) %>% arrange(LNAME,FNAME,DOB)

SOR1_1 <- sqldf("select *
                      from SOR1 
                      group by LNAME,FNAME,DOB
                      order by LNAME,FNAME,DOB")

SOR2_1 <- sqldf("select *
                  from SOR2
                  group by LNAME, FNAME, DOB
                  order by LNAME, FNAME, DOB")

# put the datasets together with a full join
fullSOR <- full_join(SOR1_1, SOR2_1, by = c("LNAME", "FNAME", "DOB")) 

fullSOR2 <- fullSOR %>%
        select(LNAME,FNAME,DOB,STATUS,CRIMECLAS,
            CONVCOUN,CONVSTAT,VICTMAGE,OFFAGE)


write.csv(fullSOR2[1:40,], "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/SOR_example.csv")

fullSOR2$SORFL <- "Y"

write.csv(fullSOR2, "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/SOR.csv")


