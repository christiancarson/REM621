#Assignment 3
#You've been asked by an intergovernmental organization to help make an economic
# case for environmental protection. One question is whether there's a relation
# between environmental protection indicators for countries and their economic
# performance, measured by GDP per capita. It's worth exploring with the dataset
# 1. Provide a critique (2-3 paragraphs) of the indicators for environmental
#    protection, social equity, and GDP per capita. Discuss their limitations in
#    meaningfully representing national trends or relationships between social
#    well-being and environmental sustainability (include references).
# 2. Use regression models to test the correlation between GDP per capita and
#    environmental protection. Also, check the correlation between the social
#    equity indicator and GDP per capita. This can be done using multiple
#    regression or a second regression analysis. Present your results in graphs
#    and include a brief summary (1-2 paragraphs) of your findings.

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")


#Users/critty/Desktop/GitHub/REM621/Assignment 3/ Assignment 3 REM621.csv
wd <- setwd("/Users/critty/Desktop/GitHub/REM621/Assignment 3/")
data <- read.csv("Assignment 3 REM621.csv", header = TRUE, sep = ",")

#numeric
data$GDP.per.capita.PPP <- as.numeric(gsub(",", "", data$GDP.per.capita.PPP))

#LMs for EPI and SE individually
EPI <- lm(Environmental.Performance.Index ~ GDP.per.capita.PPP, data = data)
SE <- lm(Social.Equity ~ GDP.per.capita.PPP, data = data)
#multiple regression
EPI_SE <- lm(Environmental.Performance.Index + Social.Equity ~ GDP.per.capita.PPP, data = data)

#summary stats
summary(EPI) -> EPIsummary
summary(SE) -> SEsummary
summary(EPI_SE) -> EPI_SEsummary

#summary text for plots
EPIsummary_text <- paste("EPI ~ PPP:\n",
                       "R-squared = ", round(EPIsummary$r.squared, 2))
SEsummary_text <- paste("SE ~ PPP:\n",
                       "R-squared = ", round(SEsummary$r.squared, 2))
EPI_SEsummary_text <- paste("(EPI + SE) ~ PPP:\n",
                       "R-squared = ", round(EPI_SEsummary$r.squared, 2))

#fitted values
data$Fitted_EPI <- fitted(EPI)
data$Fitted_SE <- fitted(SE)
data$Fitted_EPI_SE <- fitted(EPI_SE)

#plot
ggplot(data, aes(x = GDP.per.capita.PPP)) +
  geom_line(aes(y = Fitted_EPI, color = "Environmental Performance Index (EPI)"), size = 1) +
  geom_line(aes(y = Fitted_SE, color = "Social Equity (SE)"), size = 1) +
  geom_line(aes(y = Fitted_EPI_SE, color = "EPI & SE"), size = 1) +
  geom_text(aes(x = max(GDP.per.capita.PPP), y = max(Fitted_EPI)+1, label = EPIsummary_text),
            hjust = 1, vjust = 0, size = 5, color = "blue") +
  geom_text(aes(x = max(GDP.per.capita.PPP), y = max(Fitted_SE)+4, label = SEsummary_text),
            hjust = 1, vjust = 1, size = 5, color = "red") +
  geom_text(aes(x = max(GDP.per.capita.PPP), y = max(Fitted_EPI_SE)+6, label = EPI_SEsummary_text),
            hjust = 1, vjust = 2, size = 5, color = "purple") +
  scale_color_manual(values = c("Environmental Performance Index (EPI)" = "blue", "Social Equity (SE)" = "red", "EPI & SE" = "purple" )) +
  labs(x = "GDP per Capita (PPP)", y = "Index Value", color = "Index Type") +
theme_Publication()

library(kableExtra)

#combined table of summary stats for EPI, SE, and EPI+SE via kable
