# The data wrangling of chapter 6.

BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)

RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = T)

names(BPRS)
str(BPRS)
summary(BPRS)

names(RATS)
str(RATS)
summary(RATS)


# There are 40 observations of 11 variables in the BPRS dataset. 9 of these variables consist of measures of repeated experiments.
# There are 16 observations of 13 variables in the RATS dataset.

library(dplyr)
library(tidyr)


# Converting the categorical variables of both data sets to factors.

BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

RATSL$ID <- factor(RATSL$ID)
RATSL$Group <- factor(RATSL$Group)


#Converting the data sets to long form. Adding a week variable to BPRS and a Time variable to RATS.

BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
glimpse(BPRSL)
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))
glimpse(BPRSL)

RATSL <- RATS %>% gather(key = times, value = rats, -ID, -Group)
glimpse(RATSL)
RATSL <- RATSL %>% mutate(Time = as.integer(substr(times,3,4)))
glimpse(RATSL)

BPRSL
RATSL

# After wrangling, BPRSL-dataset contains 360 onservations of 4 variables and RATSL-dataset 176 observations of 5 variables. This means, that every observation in time dooes not form an own variable, but an own observation unit instead. 

write.csv(BPRSL, file = "BPRSL.sav")
write.csv(RATSL, file = "RATSL.sav")