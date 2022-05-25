library(tidyverse)
library(readxl)
library(sqldf)

crimes_data <- read.csv(file = "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/Crimes.csv")

crimes_data$newCRIMES <- str_replace_all(toupper(crimes_data$CRIMES), " ", "")


failreg <- crimes_data %>%
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
    )
  ) %>%
  filter(FAIL_TO_REGISTER == 'Y')


failreg2 <- failreg %>% arrange(LNAME,FNAME,DOB)

failreg3 <- sqldf("select *
                      from failreg2 
                      group by LNAME,FNAME,DOB
                      order by LNAME,FNAME,DOB")

failreg4 <- failreg3 %>%
  select(LNAME,FNAME,DOB,CRIMES,FAIL_TO_REGISTER)


write.csv(failreg4[1:40,], "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/Failure_to_register_example.csv")

write.csv(failreg4, "~/SWB Projects/Chicago 400 Alliance/SOR_contacts/Failure_to_register.csv")



