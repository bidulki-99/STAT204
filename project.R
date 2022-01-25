setwd("C:/Users/윤민서/OneDrive/Desktop/통계계산프로그래밍")
library(dplyr)
library(ggplot2)

car <- read.csv("data/cardio_train.csv", header = T, sep = ';')
car <- car[ ,-1]
sum(is.na(car))

car <- car %>% mutate(bmi = weight / (height^2 / 10000)) %>% arrange(bmi)
boxplot(car$bmi)
car <- car[101:69900,]
ggplot(car, aes(x = bmi)) + geom_histogram(binwidth = 1)
car.0 <- car %>% filter(cardio == 0)
car.1 <- car %>% filter(cardio == 1)
var.test(car.1$bmi, car.0$bmi)
t.test(car.1$bmi, car.0$bmi, alternative = "greater")

ggplot(car, aes(x = cholesterol)) + geom_bar()
chol.1 <- car %>% filter(cholesterol == 1)
sum(chol.1$cardio)
sum(chol.1$cholesterol) / 1
chol.2 <- car %>% filter(cholesterol == 2)
sum(chol.2$cardio)
sum(chol.2$cholesterol) / 2
chol.3 <- car %>% filter(cholesterol == 3)
sum(chol.3$cardio)
sum(chol.3$cholesterol) / 3
prop.test(c(22995, 5735, 6162), c(52226, 9522, 8052))

ggplot(car, aes(x = smoke)) + geom_bar()
smoke.0 <- car %>% filter(smoke == 0)
sum(smoke.0$cardio)
69800 - sum(car$smoke)
smoke.1 <- car %>% filter(smoke == 1)
sum(smoke.1$cardio)
sum(car$smoke)
prop.test(c(31968, 2924), c(63645, 6155), alternative = "less")