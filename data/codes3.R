alc <- read.csv("alc.csv")
library(ggplot2)
library(dplyr)
library(tidyr)
str(alc)
dim(alc)
glimpse(alc)
g1 <- ggplot(alc, aes(x = high_use, y = G3))
read.csv("alc.csv")

head(alc)
str(alc)
colnames(alc)
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()


g1 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))
g2 <- ggplot(alc, aes(x = high_use, y = goout, col = sex))
g1 + geom_boxplot()
g2 + geom_boxplot()


g11 <- ggplot
g11 <- ggplot(data = alc, aes(y =absences, x = high_use , col = sex))
g12 <- ggplot(data = alc, aes(y =high_use, x = Fjob, col = sex))

g11 + geom_col()
g12 + geom_col()


attach(alc)
mytable0 <- table(high_use)
mytable0
prop.table(mytable0)

mytable <- table(Fjob, high_use)
mytable
prop.table(mytable, 1)

mytable2 <- table(Medu, high_use)
mytable2
prop.table(mytable2, 1)

chisq.test(mytable)
chisq.test(mytable2)

prop.table(mytable0, 1)
prop.table(mytable0, 2)


m <- glm(high_use ~ Fjob + Medu + goout + absences, data = alc, family = "binomial")
summary(m)

OR <- coef(m) %>% exp
CI <- confint(m) %>% exp
cbind(OR, CI)

m1 <- glm(high_use ~ Fjob + goout + absences, data = alc, family = "binomial")
summary(m1)

OR <- coef(m1) %>% exp
CI <- confint(m1) %>% exp
cbind(OR, CI)

m2 <- glm(high_use ~ goout + absences, data = alc, family = "binomial")
summary(m2)

OR <- coef(m2) %>% exp
CI <- confint(m2) %>% exp
cbind(OR, CI)


probabilities <- predict(m2, type = "response")
alc <- mutate(alc, probability = probabilities)
alc <- mutate(alc, prediction = probability > 0.5)
table(high_use = alc$high_use, prediction = alc$prediction)

g6 <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))
g6 + geom_point()
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table %>% addmargins

loss_func <- function(class, prob) {
n_wrong <- abs(class - prob) > 0.5
mean(n_wrong)
}

loss_func(class = alc$high_use, prob = 1)
loss_func(class = alc$high_use, prob = 0)
loss_func(class = alc$high_use, prob = alc$probability)


library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)
cv$delta[1]



library(MASS)
data("Boston")

str(Boston)
summary(Boston)
pairs(Boston)

library(GGally)
library(ggplot2)
library(tidyverse)
library(dplyr)


p <- ggpairs(Boston, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p

install.packages("corrplot")
library(corrplot)

cor_matrix<-cor(Boston) %>% round(digits = 2)
cor_matrix
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex = 0.6)

boston_scaled <- scale(Boston)
summary(boston_scaled)
class(boston_scaled)
boston_scaled <- as.data.frame(boston_scaled)

summary(boston_scaled$crim)
bins <- quantile(boston_scaled$crim)
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
table(crime)
boston_scaled <- dplyr::select(boston_scaled, -crim)
boston_scaled <- data.frame(boston_scaled, crime)

n <- nrow(boston_scaled)
ind <- sample(n,  size = n * 0.8)
train <- boston_scaled[ind,]
test <- boston_scaled[-ind,]
correct_classes <- test$crime
test <- dplyr::select(test, -crime)




lda.fit <- lda(crime ~ ., data = train)
lda.fit
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "orange", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

classes <- as.numeric(train$crime)

plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)

lda.pred <- predict(lda.fit, newdata = test)
table(correct = correct_classes, predicted = lda.pred$class)
crime




data("Boston")
boston_scaled2 <- scale(Boston)
summary(boston_scaled2)
class(boston_scaled2)
boston_scaled2 <- as.data.frame(boston_scaled2)

dist_eu <- dist(boston_scaled2)
summary(dist_eu)
dist_man <- dist(boston_scaled2, method = 'manhattan')
summary(dist_man)

km <-kmeans(boston_scaled2, centers = 4)
km

km <-kmeans(boston_scaled2, centers = 3)
km

km2 <-kmeans(boston_scaled2, centers = 2)
km2



pairs(boston_scaled2[], col = km2$cluster)