title: "Linear regression"
"Joonas Luukkonen"
"11.11.2019"
# This script is used to perform the exercices of lesson _regression and model validation_.



lrn14 <- read.table("https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
dim(lrn14)
str(lrn14)

#The data consists of 183 observations of 60 variables. Variables seem to be mainly integer, Likert-scaled variables, despite of four last variables (Age, Attitude, Points and gender,which are not Likert-scaled. In addition, "gender" is a factor variable.)


deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
library(dplyr)

deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

str(lrn14)
keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(lrn14, one_of(keep_columns))
learning2014 <- filter(learning2014, Points > "0")
str(learning2014)

setwd("~/IODS-project")
setwd("~/IODS-project/data")
write.csv(learning2014, file = "learning2014.csv")

read.csv("learning2014.csv")

head(learning2014)

str(learning2014)
summary(learning2014)
dim(learning2014)

library(ggplot2)
hist(learning2014$Age)
hist(learning2014$Attitude)
hist(learning2014$deep)
hist(learning2014$stra)
hist(learning2014$surf)
hist(learning2014$Points)

pairs(learning2014)

library(GGally)
library(ggplot2)

install.packages("GGally")

p <- ggpairs(learning2014, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p

my_model <- lm(Points ~ Attitude + stra + surf ,data = learning2014)
summary(my_model)

my_model2 <- lm(Points ~ Attitude ,data = learning2014)
summary(my_model2)


plot(my_model2, which = c(1))
plot(my_model2, which = c(2))
plot(my_model2, which = c(5))
write.csv(learning2014, file = "learning2014.csv")