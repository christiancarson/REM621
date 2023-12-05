#Assignment 3
#load data
wd <- getwd()
data <-
#Variables cleaned
#GDP per capita (ppp-$)
#Environmental Performance Index (EPI)
#Human Development Index (HDI) - Averarge years of schooling, life expectancy at birth, and GNI per capita
#Population
#Territory

model <- lm(Price ~ Mileage + Type + Country, data = cars)
summary(model)

ssr <- sum((fitted(model) - mean(model$model))^2)
sst <- sum((model$model - mean(model$model))^2)
r_squared <- 1 - (ssr/sst)

r_squared


