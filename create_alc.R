#Joonas Luukkonen
#14.11.
#This scrit includes codes of the data wrangling exercise of lesson 3. https://archive.ics.uci.edu/ml/machine-learning-databases/00320/


url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets"

# web address for math class data

url_math <- paste(url, "student-mat.csv", sep = "/")


# read the math class questionaire data into memory

math <- read.table(url_math, sep = ";" , header=TRUE)

math <- read.table("student-mat.csv", sep = ";", header=TRUE)

por <- read.table("student-por.csv", sep = ";", header = TRUE)

str(math)
str(por)
dim(math)
dim(por)

# There are 395 osbservations of 33 variables in the student.mat.csv and 649 observations of 33 variables in the other one.
#Both datasets consist of the same Factor and integer variables, which is why it is possible to combine them.


# Joining the two datasets using following variables as (student) identifiers. Only students present in both datasets are included.

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
library(dplyr)

math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))

str(math_por)
dim(math_por)

# After combining the datasets, there are 382 observations and 53 variables in the data.

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns


# for every column name not used for joining...
for(column_name in notjoined_columns) {
  two_columns <- select(math_por, starts_with(column_name))
  first_column <- select(two_columns, 1)[[1]]
  if(is.numeric(first_column)) {
    alc[column_name] <- round(rowMeans(two_columns))
  } else {
    alc[column_name] <- first_column
  }
}

#creating a new alc-variable by averaging
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
alc <- mutate(alc, high_use = alc_use > 2)

# glimpse at the new combined data
str(alc)

write.csv(alc, file = "alc.csv.")