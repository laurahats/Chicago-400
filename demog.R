library(tidyverse)
library(readxl)
library(sqldf)
library(lubridate)

homeless <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/Homeless.csv")
failreg <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/Failure_to_register.csv")
sor <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/SOR.csv")
prison <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/IL Prision Population/prison.csv")
#vad <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/VAD/VAD.csv")
gunreg <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/Gun Registry/Gun_Registry.csv")
#cpd <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/Chicago Police Dept/Arrests.csv")

#cut CPD down to one record per individual and same for Prison
prison2 <- prison %>% select(LNAME,FNAME,DOB,SEX,RACE,VETSTAT) %>% arrange(LNAME,FNAME,DOB)

prison2x <- sqldf("select distinct *
                      from prison2 
                      group by LNAME,FNAME,DOB
                      order by LNAME,FNAME,DOB")

#cpd2 <- cpd %>% select(LNAME,FNAME,DOB) %>% arrange(LNAME,FNAME)

#cpd2x <- sqldf("select distinct *
#                      from cpd2 
#                      group by LNAME,FNAME,DOB
#                      order by LNAME,FNAME,DOB")

homeless$HMLESS <- 'Y'
failreg$FAILREG <- 'Y'
sor$SORFL <- 'Y'
prison2x$ILPRISFL <- 'Y'
gunreg$GUNFL <- 'Y'

# put the datasets together with a full join
full <- full_join(homeless, failreg, by = c("LNAME", "FNAME","DOB")) 
full2 <- full_join(full, sor, by = c("LNAME", "FNAME","DOB"))
full3 <- full_join(full2, prison2x, by = c("LNAME", "FNAME","DOB"))
final <- full_join(full3, gunreg, by = c("LNAME", "FNAME","DOB"))

final <- full4 %>% select(LNAME,FNAME,DOB,SEX.x,SEX.y,RACE.x,RACE.y,VETSTAT,HEIGHT,WEIGHT,SORFL,
                          FAILREG,HMLESS,ILPRISFL,GUNFL)


final$SEX.x[is.na(final$SEX.x)] <- final$SEX.y[is.na(final$SEX.x)]
final$RACE.x[is.na(final$RACE.x)] <- final$RACE.y[is.na(final$RACE.x)]

final$SEX <- final$SEX.x
final$RACE <- final$RACE.x

demog <- final %>% select(LNAME,FNAME,DOB,SEX,RACE,VETSTAT,HEIGHT,WEIGHT,SORFL,
                          FAILREG,HMLESS,ILPRISFL,GUNFL)

write.csv(demog[1:40,], "~/SWB Projects/Chicago 400 Alliance/demog_example.csv")

write.csv(demog, "~/SWB Projects/Chicago 400 Alliance/demog.csv")

