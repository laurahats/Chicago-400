library(tidyverse)
library(readxl)
library(sqldf)

#when on home comp
setwd("~/SWB Projects/Chicago 400 Alliance/SexOffenders")

#In the XLSX files they have 22 columns, import these files: column 15 is date
files <- (Sys.glob("*.xlsx"))

listOfFiles <- lapply(files, function(x) 
  read_excel(path=x,skip=1,trim_ws=TRUE,
             col_types=c("text","text", "text", "text", "text", "text", "text",
                         "text", "text", "text", "text", "text", "text", "text",
                         "date", "text", "text", "text", "text", "text", "text",
                         "text"),
             col_names=c("Last_Name","First_Name","Middle_Name","Address","City","State",
                         "Zip","County","X", "Y", "Height", "Weight", "Race", "Gender", "DOB",
                         "Status", "Classification", "Convict County", "Convict State", "Victim Age",
                         "Offender Age", "Crimes")
  ))



df <- bind_rows(listOfFiles, .id = "id")

#In the XLSX files they have 22 columns, import these files: column 15 is date
setwd("~/SWB Projects/Chicago 400 Alliance/SexOffenders/SplitCrimes")

df2 <- read_excel(path="Copy of 20161015144228 SO-1.xlsx",skip=1,trim_ws=TRUE,
             col_types=c("text","text", "text", "text", "text", "text", "text",
                         "text", "text", "text", "text", "text", "text", "text",
                         "date", "text", "text", "text", "text", "text", "text",
                         "text","text","text","text","text","text","text","text",
                         "text","text"),
             col_names=c("Last_Name","First_Name","Middle_Name","Address","City","State","Zip",
                         "County","X", "Y", "Height", "Weight", "Race", "Gender", 
                         "DOB","Status","Classification","Convict_County","Convict State","Victim Age","Offender Age",
                         "Crimes1","Crimes2","Crimes3","Crimes4","Crimes5","Crimes6","Crimes7",
                         "Crimes8","Crimes9","Crimes10")
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

df2$Crimes <- skipNApaste(df2$Crimes1,df2$Crimes2,df2$Crimes3,df2$Crimes4,df2$Crimes5,df2$Crimes6,df2$Crimes7,df2$Crimes8,
                 df2$Crimes9,df2$Crimes10, sep = ", ", na.rm = T)

df2x <- df2 %>% select(-Crimes1,-Crimes2,-Crimes3,-Crimes4,-Crimes5,-Crimes6,-Crimes7,-Crimes8,-Crimes9,-Crimes10)

fulldf <- bind_rows(df,df2x)
fulldf2 <- fulldf[!is.na(fulldf$Last_Name),]

hmls <- fulldf2 %>% 
  filter(str_detect(Address,'HOMELESS|HMLS'))
        
View(hmls)

#We only want the individuals that are homeless
table(hmls$Crimes)
check <- unique(hmls$Crimes)
View(check)
sort(check)
datac <- table(check)
View(datac)

hmls_violators <- hmls %>%
    filter(str_detect(Crimes,'FAILURE TO REPORT ANNUALLY'))

hmls_violators_1 <- sqldf("select *, count(*) as cnt 
                      from hmls_violators 
                      group by Last_Name, First_Name, DOB
                      order by Last_Name, First_Name, DOB")

write.csv(hmls_violators_1, "hmls_violators_nodups.csv")


