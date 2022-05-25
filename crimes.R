library(tidyverse)
library(readxl)
library(sqldf)
library(lubridate)

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




#some will be in multiple files, get rid of duplicates by the identifiers
fulldfx2$DOB <- as.Date(fulldfx2$DOB)

SOR2 <- fulldfx2 %>% select(-id) %>% arrange(LNAME,FNAME,DOB,CRIMES)

SOR2_1 <- sqldf("select *
                  from SOR2
                  group by LNAME, FNAME, DOB,CRIMES
                  order by LNAME, FNAME, DOB,CRIMES")

# put the datasets together with a full join
fullSOR2 <- SOR2_1 %>%
    filter(!is.na(CRIMES)) %>%
  select(LNAME,FNAME,DOB,CRIMECLAS,
         CONVCOUN,CONVSTAT,VICTMAGE,OFFAGE,CRIMES)


write.csv(fullSOR2[1:40,], "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/Crimes_example.csv")

write.csv(fullSOR2, "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/Crimes.csv")
