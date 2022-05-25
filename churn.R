
library(tidyverse)
library(readxl)
library(sqldf)
library(lubridate)


# FIRST GET PRISION DATA

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
df$CUSTDT <- ymd(df$CUSTDT)

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
df2$CUSTDT <- mdy(df2$CUSTDT)

df22 <- df2 %>% select(-CURADMDT,-PRJMSRDT, -PRJDISDT,-SENTCDT)
df1 <- df %>% select(-CURADMDT,-PRJMSRDT,-PRJDISDT,-SENTCDT)

fulldf <- bind_rows(df1,df22)

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

finaldf$LNAME <- toupper(finaldf$LNAME)
finaldf$FNAME <- toupper(finaldf$FNAME)

#some will be in multiple files, get rid of duplicates by the identifiers
prison <- finaldf %>% arrange(IDOC,LNAME,FNAME,DOB,SOURCE)

prison_1 <- sqldf("select *
                      from prison 
                      group by IDOC,LNAME,FNAME,DOB,SOURCE
                      order by IDOC,LNAME,FNAME,DOB,SOURCE")
prison_1$EVENT <- "PRISON"

p2 <- prison_1 %>%
  mutate(
    TIMECHECK_MTH = case_when(str_detect(toupper(SOURCE), "DECEMBER|DEC |DEC_") ~ "DEC",
                              str_detect(toupper(SOURCE), "JANUARY|JAN |JAN_") ~ "JAN",
                              str_detect(toupper(SOURCE), "FEBUARY|FEB |FEB_") ~ "FEB",
                              str_detect(toupper(SOURCE), "MARCH|MAR |MAR_") ~ "MAR",
                              str_detect(toupper(SOURCE), "APRIL|APR |APR_") ~ "APR",
                              str_detect(toupper(SOURCE), "MAY|MAY |MAY_") ~ "MAY",
                              str_detect(toupper(SOURCE), "JUNE|JUN |JUN_") ~ "JUN",
                              str_detect(toupper(SOURCE), "JULY|JUL |JUL_") ~ "JUL",
                              str_detect(toupper(SOURCE), "AUGUST|AUG |AUG_") ~ "AUG",
                              str_detect(toupper(SOURCE), "SEPTEMBER|SEPT |SEPT_") ~ "SEPT",
                              str_detect(toupper(SOURCE), "OCTOBER|OCT |OCT_") ~ "OCT",
                              str_detect(toupper(SOURCE), "NOVEMBER|NOV |NOV_") ~ "NOV",
                              TRUE ~ " "
    ),
    TIMECHECK_YR = case_when(str_detect(SOURCE, "2021") ~ "2021",
                             str_detect(SOURCE, "2020") ~ "2020",
                             str_detect(SOURCE, "2019") ~ "2019",
                             str_detect(SOURCE, "2018") ~ "2018",
                             str_detect(SOURCE, "2017") ~ "2017",
                             str_detect(SOURCE, "2016") ~ "2016",
                             str_detect(SOURCE, "2015") ~ "2015",
                             str_detect(SOURCE, "2014") ~ "2014",
                             str_detect(SOURCE, "2013") ~ "2013",
                             str_detect(SOURCE, "2012") ~ "2012",
                             str_detect(SOURCE, "2011") ~ "2011",
                             str_detect(SOURCE, "2010") ~ "2010",
                             str_detect(SOURCE, "2009") ~ "2009",
                             str_detect(SOURCE, "2008") ~ "2008",
                             str_detect(SOURCE, "2007") ~ "2007",
                             str_detect(SOURCE, "2006") ~ "2006",
                             str_detect(SOURCE, "2005") ~ "2005",
                             str_detect(SOURCE, "2004") ~ "2004",
                             TRUE ~ " "
    )
  )
    

check <- p2 %>% filter(is.na(TIMECHECK_YR))

p2$TIMECHECK_DAY <- as.numeric(NA)
p2$TIMECHECK_YR <- as.numeric(p2$TIMECHECK_YR)

churn1 <- p2 %>%
  select(LNAME,FNAME,DOB,EVENT,TIMECHECK_YR, TIMECHECK_MTH, TIMECHECK_DAY, CUSTDT, SOURCE) %>%
  arrange(LNAME, FNAME, DOB, TIMECHECK_YR, TIMECHECK_MTH, TIMECHECK_DAY, EVENT)


#NOW ADD LOCATION CHECK POINTS

setwd("~/SWB Projects/Chicago 400 Alliance/SOR_contacts")

files <- (Sys.glob("*.xls"))

listOfFiles <- lapply(files, function(x) 
  read_excel(path=x,skip=1,trim_ws=TRUE,
             col_types=c("text","text", "text", "text", "text", "text", "text",
                         "text", "date"),
             col_names=c("LNAME","FNAME","MNAME","STREET","CITY","STATE",
                         "ZIPCODE","COUNTY","DOB")
  ))

listOfFiles <- setNames(listOfFiles, files)

df <- bind_rows(listOfFiles, .id = "SOURCE")

#In the XLSX files most of them have 9 columns, import these files:
files2 <- (Sys.glob("*.xlsx"))

listOfFiles2 <- lapply(files2, function(x) 
  read_excel(path=x,skip=1,trim_ws=TRUE,
             col_types=c("text","text", "text", "text", "text", "text", "text",
                         "text", "date"),
             col_names=c("LNAME","FNAME","MNAME","STREET","CITY","STATE",
                         "ZIPCODE","COUNTY","DOB")
  ))
listOfFiles2 <- setNames(listOfFiles2, files2)

df2 <- bind_rows(listOfFiles2, .id = "SOURCE")

#to make the above code work the file  "Copy of xSOR.20131005.xlsx" had to be edited to 
# remove a record that was a repeat of column names inside the file
#to make the above code work "Copy of xSOR.20120627.xlsx" had to be edited to 
# remove additional empty columns
#to make the above code work "Copy of xSOR.20130511.xlsx" and "Copy of xSOR.20130615.xlsx" had 
# to be edited to remove additional columns of Conviction County and Conviction State


fulldf <- bind_rows(df,df2)
fulldf2 <- fulldf[!is.na(fulldf$LNAME),]


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

listOfFiles <- setNames(listOfFiles, files)

dfx <- bind_rows(listOfFiles, .id = "SOURCE")

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
                               "DOB","STATUS","CRIMCLAS","CONVCOUN","CONVSTAT","VICTMAGE","OFFAGE",
                               "CRIMES1","CRIMES2","CRIMES3","CRIMES4","CRIMES5","CRIMES6","CRIMES7",
                               "CRIMES8","CRIMES9","CRIMES10")
)
dfx2$SOURCE <- "Copy of 20161015144228 SO-1.xlsx"

fulldfx <- bind_rows(dfx,dfx2)
fulldfx2 <- fulldfx[!is.na(fulldfx$LNAME),]


#some will be in multiple files, get rid of duplicates by the identifiers
fulldf2$DOB <- as.Date(fulldf2$DOB)
fulldfx2$DOB <- as.Date(fulldfx2$DOB)

fulldfx2$LNAME <- toupper(fulldfx2$LNAME)
fulldfx2$FNAME <- toupper(fulldfx2$FNAME)
fulldf2$LNAME <- toupper(fulldf2$LNAME)
fulldf2$FNAME <- toupper(fulldf2$FNAME)

SOR1 <- fulldf2 %>%  arrange(LNAME,FNAME,DOB,SOURCE)
SOR2 <- fulldfx2 %>% arrange(LNAME,FNAME,DOB,SOURCE)

SOR1_1 <- sqldf("select *
                      from SOR1 
                      group by LNAME,FNAME,DOB, STREET, CITY, STATE, SOURCE
                      order by LNAME,FNAME,DOB, STREET, CITY, STATE, SOURCE")

SOR2_1 <- sqldf("select *
                  from SOR2
                  group by LNAME, FNAME, DOB, STREET, CITY, STATE, SOURCE
                  order by LNAME, FNAME, DOB, STREET, CITY, STATE, SOURCE")

# put the datasets together with a full join
fullSOR <- full_join(SOR1_1, SOR2_1, by = c("LNAME", "FNAME", "DOB", "STREET", "CITY", "STATE", "SOURCE")) 

homeless <- fullSOR %>%  
  mutate(
    EVENT = case_when(str_detect(toupper(STREET), "HOMELESS|HMLS") ~ "HOMELESS",
                              str_detect(toupper(STREET), "CORRECTIONS|JAIL") ~ "PRISON",
                              TRUE ~ STREET
    )
  )

#next extract the numbers and break down in YEAR, MONTH, and DAY
homeless$SOURCE <- str_remove(homeless$SOURCE, "SO-1|SO-1")

homeless$SOURCE_numbers <- regmatches(homeless$SOURCE, gregexpr("[[:digit:]]+", homeless$SOURCE))

#read only at most 8numbers, this will elimate time in the file name.
homeless$SOURCE_numbers <- substr(homeless$SOURCE_numbers, 1, 8)

homeless$SOURCE_numbers2 <- ymd(homeless$SOURCE_numbers)
check <- homeless[is.na(homeless$SOURCE_numbers2),]


homeless$TIMECHECK_YR <- year(homeless$SOURCE_numbers2)
homeless$TIMECHECK_MTHx <- as.character(month(homeless$SOURCE_numbers2))
homeless$TIMECHECK_DAY <- day(homeless$SOURCE_numbers2)

oldvals <- as.character(1:12)
newvals <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT",
             "OCT", "NOV", "DEC")
homeless$TIMECHECK_MTH <- newvals[match(homeless$TIMECHECK_MTHx, oldvals)]

check <- homeless %>% filter(is.na(TIMECHECK_YR))

churn2 <- homeless %>%
  select(LNAME,FNAME,DOB,EVENT,TIMECHECK_YR, TIMECHECK_DAY, TIMECHECK_MTH, SOURCE) %>%
  arrange(LNAME, FNAME, DOB, TIMECHECK_YR, TIMECHECK_MTH, TIMECHECK_DAY, EVENT)


# Sex Offender Registry Violation EVENT

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

listOfFiles <- setNames(listOfFiles, files)

dfx <- bind_rows(listOfFiles, .id = "SOURCE")

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
                               "DOB","STATUS","CRIMCLAS","CONVCOUN","CONVSTAT","VICTMAGE","OFFAGE",
                               "CRIMES1","CRIMES2","CRIMES3","CRIMES4","CRIMES5","CRIMES6","CRIMES7",
                               "CRIMES8","CRIMES9","CRIMES10")
)

dfx2$SOURCE <- "Copy of 20161015144228 SO-1.xlsx"


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

SOR2 <- fulldfx2 %>% arrange(LNAME,FNAME,DOB,SOURCE,CRIMES)

SOR2_1 <- sqldf("select *
                  from SOR2
                  group by LNAME, FNAME, DOB,SOURCE,CRIMES
                  order by LNAME, FNAME, DOB,SOURCE,CRIMES")

# put the datasets together with a full join
fullSOR2 <- SOR2_1 %>%
  filter(!is.na(CRIMES)) %>%
  select(LNAME,FNAME,DOB,SOURCE,CRIMECLAS,
         CONVCOUN,CONVSTAT,VICTMAGE,OFFAGE,CRIMES)

fullSOR2$newCRIMES <- str_replace_all(toupper(fullSOR2$CRIMES), " ", "")


failreg <- fullSOR2 %>%
  mutate(
    FAIL_TO_REGISTER = case_when(
      str_detect(newCRIMES, "FAILURETOREPORT") | 
        str_detect(newCRIMES, "FAILURETORPT") |
        str_detect(newCRIMES, "FAILTOREPORT") |
        str_detect(newCRIMES, "FAILTORPT") |
        str_detect(newCRIMES, "FAILURETOREGISTER") |
        str_detect(newCRIMES, "FAILTOREGISTER") |
        str_detect(newCRIMES, "FAILURETONOTIFY") |
        str_detect(newCRIMES, "FAILTONOTIFY") |
        str_detect(newCRIMES, "VIOLATESEXOFFENDER") |
        str_detect(newCRIMES, "FALSEINFO") |
        str_detect(newCRIMES, "ADDRESS\\(2COUNTS") |
        str_detect(newCRIMES, "EMPLOYMENT\\(2COUNTS") |
        str_detect(newCRIMES, "SCHOOL\\(2COUNTS") |
        str_detect(newCRIMES, "2ND\\(2COUNTS") |
        str_detect(newCRIMES, "REGISTRATION\\(2COUNTS") |
        str_detect(newCRIMES, "ANNUALLY\\(2COUNTS") |
        str_detect(newCRIMES, "EMPLOYMENT\\(3COUNTS") |
        str_detect(newCRIMES, "NOTIFICATION/NO FIXED ADDRESS") ~ "Y",
      TRUE ~ "N"
    ),
    EVENT="SEX OFFENDER REGISTRY VIOLATION"
  ) %>%
  filter(FAIL_TO_REGISTER == 'Y')


failreg2 <- failreg %>% arrange(LNAME,FNAME,DOB,SOURCE)

failreg3 <- sqldf("select *
                      from failreg2 
                      group by LNAME,FNAME,DOB,SOURCE
                      order by LNAME,FNAME,DOB,SOURCE")

failreg4 <- failreg3 %>%
  select(LNAME,FNAME,DOB,SOURCE,EVENT)

#next extract the numbers and break down in YEAR, MONTH, and DAY
failreg4$SOURCE <- str_remove(failreg4$SOURCE, "SO-1|SO-1")

failreg4$SOURCE_numbers <- regmatches(failreg4$SOURCE, gregexpr("[[:digit:]]+", failreg4$SOURCE))

#read only at most 8numbers, this will elimate time in the file name.
failreg4$SOURCE_numbers <- substr(failreg4$SOURCE_numbers, 1, 8)

failreg4$SOURCE_numbers2 <- ymd(failreg4$SOURCE_numbers)
check <- failreg4[is.na(failreg4$SOURCE_numbers2),]


failreg4$TIMECHECK_YR <- year(failreg4$SOURCE_numbers2)
failreg4$TIMECHECK_MTHx <- as.character(month(failreg4$SOURCE_numbers2))
failreg4$TIMECHECK_DAY <- day(failreg4$SOURCE_numbers2)

oldvals <- as.character(1:12)
newvals <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEPT",
             "OCT", "NOV", "DEC")
failreg4$TIMECHECK_MTH <- newvals[match(failreg4$TIMECHECK_MTHx, oldvals)]

check <- failreg4 %>% filter(is.na(TIMECHECK_YR))

churn3 <- failreg4 %>%
  select(LNAME,FNAME,DOB,EVENT,TIMECHECK_YR, TIMECHECK_DAY, TIMECHECK_MTH, SOURCE) %>%
  arrange(LNAME, FNAME, DOB, TIMECHECK_YR, TIMECHECK_MTH, TIMECHECK_DAY, EVENT)


#from churn1 we only want sex offenders.
churn22 <- churn2 %>% select(LNAME, FNAME, DOB) %>% arrange(LNAME, FNAME, DOB)
churn222 <- sqldf("select *
                      from churn22 
                      group by LNAME,FNAME,DOB
                      order by LNAME,FNAME,DOB")

churn1x <- right_join(churn1, churn222, by = c("LNAME","FNAME","DOB"))
churn1xx <- churn1x %>% filter(!is.na(EVENT))

churn2$CUSTDT <- as.Date(NA)

churn3$CUSTDT <- as.Date(NA)
  
final_churn <- rbind(churn3,churn1xx,churn2)

final_churn2 <- final_churn %>% arrange(LNAME, FNAME, DOB, TIMECHECK_YR, TIMECHECK_MTH, TIMECHECK_DAY, EVENT)


write.csv(final_churn2, "~/SWB Projects/Chicago 400 Alliance/final_churn_v2.csv")


