title: "Dimensionality reduction techniques"
"Joonas Luukkonen"
"1.12.2019"

# Load the "human" data into R and exlopre the structure and the dimensions of the data.

human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)
str(human)
names(human)

# There are 195 observations of 19 varaibles in the data, including integer, numerical and factor variables. The data consists of measures related to calculating the human development indices and it combines several indicators from most countries in the world.

