#Christian Carson
#REM 621
#Assignment 2
#10/31/2023


#####Part 1 ######
#install packages if you dont have them
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("viridis")
#install.packages("ggthemes")

#Libs
library(reshape2)
library(ggplot2)
library(scales)
library(viridis)
library(ggthemes)
source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")



#Setup
#Parameters for sheet
#Carrying capacity
K <- 120000
#growth rate
r <- 0.21
#price per unit
p <- 100
#100 year value at 3% discount rate
 <- 0
#

#Year
year <- 0:100

#year 1
x <- 10000

#Stand Volume
#stand volume for each year
logistic_growth <- function(x, r, K) {
  return(r * x * (1 - (x/K)))
}
#stand volume for each year
V <- logistic_growth(x, r, K)

#volume for each year in the 100 year period
for (i in 1:100) {
  V[i+1] <- logistic_growth(V[i], r, K)
}

V
#harvest rate


#harvest

#revenue

