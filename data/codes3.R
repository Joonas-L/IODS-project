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
