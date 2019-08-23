library(readxl)
library(dplyr)
setwd('C:/Users/563525/Desktop/Athena Health')

sheet1 <- read_excel('athena_interview_data.xlsx',sheet = 'Claim Actions Data')
sheet2 <- read_xlsx('athena_interview_data.xlsx',sheet = 'Client Details Data')

View(sheet1)
View(sheet2_unique)
View(athena)

sheet2_unique <- unique(sheet2)

athena <- merge(sheet1, sheet2_unique, by = 'CLIENTID')
summary(athena)
str(athena)

attach(athena)
athena$CLIENTID <- as.factor(athena$CLIENTID)
athena$OVERALLSTATUS <- as.factor(athena$OVERALLSTATUS)
athena$CLIENTSPECIALTY <- as.factor(athena$CLIENTSPECIALTY)
athena$CLIENTSIZE <- as.factor(athena$CLIENTSIZE)


levels(athena$weeknum)

# No NA values in dataset
athena[rowSums(is.na(athena) == 1), ]

# write.csv(athena,"Athena.csv")

athena$weeknum <- paste('Week', strftime(athena$WEEK , format = "%V"), sep = " ")
athena$weeknum <- as.factor(athena$weeknum)

athena_newdata<- aggregate(athena$LEGACYCODINGACTIONS,athena$NEWCODINGACTIONS,
                           athena$LEGACYAPPEALACTIONS,athena$NEWAPPEALACTIONS,
                           by = list(athena$CLIENTID,athena$weeknum),
                           FUN = sum)
athena_sumdata <- athena %>% 
  group_by(CLIENTID,weeknum) %>%
  summarize(sum_LEGACYCODINGACTIONS = sum(LEGACYCODINGACTIONS),
            sum_NEWCODINGACTIONS = sum(NEWCODINGACTIONS),
            sum_LEGACYAPPEALACTIONS = sum(LEGACYAPPEALACTIONS),
            sum_NEWAPPEALACTIONS = sum(NEWAPPEALACTIONS))

#########################################################
##### First Quest

#	What % of clients are using this new functionality? 

athena_firstques <- athena %>% 
  group_by(CLIENTID) %>%
  summarize(sum_NEWCODINGACTIONS = sum(NEWCODINGACTIONS),
            sum_NEWAPPEALACTIONS = sum(NEWAPPEALACTIONS))

View(athena_firstques)
attach(athena_firstques)

nrow(athena_firstques[(sum_NEWCODINGACTIONS != 0 | sum_NEWAPPEALACTIONS != 0),])/nrow(sheet2_unique) 

# Answer for First quest
# Out of 1200 clients 761 clients uses new functionality
# i.e) 63.41 % of Clients are using the new functionality

##### First Quest
#########################################################

#########################################################
##### Second Quest
# What types and sizes of clients are using the functionality the least/the most?
athena_secondques <- athena %>% 
  group_by(CLIENTSPECIALTY) %>%
  summarize(sum_NEWCODINGACTIONS = sum(NEWCODINGACTIONS),
            sum_NEWAPPEALACTIONS = sum(NEWAPPEALACTIONS))

View(athena_secondques)
attach(athena_secondques)

# Pediatricians uses the new functionality more than others. 
# And the Family medicine uses the new functionality lowest.

athena_secondques_1 <- athena %>% 
  group_by(CLIENTSIZE) %>%
  summarize(sum_NEWCODINGACTIONS = sum(NEWCODINGACTIONS),
            sum_NEWAPPEALACTIONS = sum(NEWAPPEALACTIONS))


View(athena_secondques_1)

# Interstingly medium size clients are using the new functionality more than
# large size clients

##### Second Quest
#########################################################

#########################################################

##### Third Quest
# Do larger clients tend to correlate with higher usage rates?

# No, the medium sized clients uses the new feature more than the 
# higher sized clients. 
##### Third Quest

#########################################################

# Create 2 metrics (with visualizations) that can be utilized to track the success of this new functionality. Why did you choose these metrics?


# Has this new functionality proven to be successful? Are there any significant trends?
# https://stepupanalytics.com/ab-testing-with-r/


# Would you recommend that this functionality gets expanded to the entire client base? Or are there certain clients that should be expanded to first?
# We have access to a product education team at athena that creates training materials and walkthroughs for our clients. Where would you recommend they focus their efforts?
# Are there any pieces of information that would be useful to have in the next version of this data set?
  



