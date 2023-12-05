# Fishers in an area have been complaining that they just can't make ends meet anymore because catch is declining, and they ask for help in the form of a subsidy to offset 50% of their costs.
# Using the parameters we adopted in class, what will happen to total catch for the fishery?
# What is the maximum sustainable yield for this fishery, and at what level of effort can we achieve it?
# What is the maximum economic yield for the fishery, and at what level of effort can we achieve that?
# What combination of parameters would you need to have MSY happen at a lower effort level than MEY?


library(ggplot2)
library(tableHTML)
library(tidyverse)
library(dplyr)
#catchability
Q <- 0.005
#growth rate 
r <- 0.2 
#carrying capacity
K <- 100 
#price per fish 
p <- 5 
#regular cost per fish
c <- 0.3 
#subsidy cost per fish
sc <- 0.15 


effort <- seq(0, 50, by = 1)
biomass <- K * (1 - (Q * effort) / r)
yield <- Q * effort * biomass
revenue <- p * yield
cost <- c * effort
subsidized_cost <- sc * effort
profit <- revenue - cost
subsidized_profit <- revenue - subsidized_cost

######## Fishery at MSY#####
# Biomass MSY
B_MSY <- K / 2
#Yield at MSY 
Y_MSY <- (r * K) / 4 
#Effort at MSY
E_MSY <- r / (2 * Q)
#Profit at MSY
Profit_MSY <- (p * Y_MSY) - (c * E_MSY)

#Standard MEY
#Effort at standard MEY
E_MEY <- ((r)/(2*Q)*((1)-(c/(p*K*Q))))
#𝑴𝑬𝒀 Standard 
MEY <- Q * E_MEY * K * (1 - Q * E_MEY / r)
#Biomass at standard MEY
B_MEY <- MEY / (Q * E_MEY) 
#Profit at standard MEY
Profit_MEY <- (p * MEY) - (c * E_MEY)

#Standard Open Access Equilibrium (OAE)
#Biomass at standard OAE
B_OA <- c / (p * Q) 
#Effort at standard OAE
E_OA <- (r / Q) * (1 - c / (p * Q * K))
#Yield at standard OAE
Y_OA <- Q * E_OA * (c / (p * Q))
#Profit at standard OAE
Profit_OA <- (p * Y_OA) - (c * E_OA)

######Subsidized Fishery at MSY####
#Subsidized MEY
#Effort at Subsidized MEY
#E_MEY Subsidized
E_MEY_Subsidized <- ((r)/(2*Q))*((1)-(sc/(p*K*Q)))
#𝑴𝑬𝒀 Subsidized = 𝒒𝑬𝑴𝑬𝒀𝑲 ∙ 𝟏 − 𝒒𝑬𝑴𝑬𝒀𝒓 
MEY_Subsidized <- Q * E_MEY_Subsidized * K * (1 - Q * E_MEY_Subsidized / r)
#Biomass at Subsidized MEY
B_MEY_Subsidized <- MEY_Subsidized / (Q * E_MEY_Subsidized)
#Profit at Subsidized MEY
Profit_MEY_Subsidized <- (p * MEY_Subsidized) - (sc * E_MEY_Subsidized)

#Subsidized Open Access Equilibrium (OAE)
#Biomass at Subsidized OAE
B_OA_Subsidized <- sc / (p * Q)
#Effort at Subsidized OAE
E_OA_Subsidized <- (r / Q) * (1 - sc / (p * Q * K))
#Yield at Subsidized OAE
Y_OA_Subsidized <- Q * E_OA_Subsidized * (sc / (p * Q))
#Profit at Subsidized OAE
Profit_OA_Subsidized <- (p * Y_OA_Subsidized) - (sc * E_OA_Subsidized)

#MATA table
MATA <- data.frame(
  Category = c('Effort', 'Biomass', 'Yield', 'Profit'), 
  MSY = c(E_MSY, B_MSY, Y_MSY, Profit_MSY),
  OA = c(E_OA, B_OA, Y_OA, Profit_OA),
  OA_Subsidized = c(E_OA_Subsidized, B_OA_Subsidized, Y_OA_Subsidized, Profit_OA_Subsidized),
  MEY = c(E_MEY, B_MEY, MEY, Profit_MEY),
  MEY_Subsidized = c(E_MEY_Subsidized, B_MEY_Subsidized, MEY_Subsidized, Profit_MEY_Subsidized))
#transpose but keep names
MATA <- t(MATA)
rownames(MATA) <- c('Category', 'MSY', 'OA', 'OA_Subsidized', 'MEY', 'MEY_Subsidized')
colnames(MATA) <- c('Effort', 'Biomass', 'Yield', 'Profit')
#remove first row
MATA <- MATA[-1,]

tableHTML(MATA)

#Plotting
library(ggplot2)

#select vars and create df with all above 0
df <- data.frame(
  revenue = pmax(revenue, 0),
  effort = effort, 
  cost = cost, 
  subsidized_cost = subsidized_cost,
  profit = pmax(profit, 0),
  subsidized_profit = pmax(subsidized_profit, 0),
  profit_MSY = Profit_MSY,
  profit_MEY = Profit_MEY,
  profit_OA = Profit_OA,
  profit_MEY_Subsidized = Profit_MEY_Subsidized,
  profit_OA_Subsidized = Profit_OA_Subsidized,
  effort_MSY = E_MSY,
  effort_MEY = E_MEY,
  effort_OA = E_OA,
  effort_MEY_Subsidized = E_MEY_Subsidized,
  effort_OA_Subsidized = E_OA_Subsidized
)
source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")
#plot
p <- ggplot(df, aes(x = effort)) +
  geom_line(aes(y = revenue, color = "Revenue"), size = 1) +
  geom_line(aes(y = cost, color = "Cost"), size = 1) +
  geom_line(aes(y = subsidized_cost, color = "Subsidized Cost"), size = 1, linetype = "dashed") +
  geom_line(aes(y = profit, color = "Profit"), size = 1) +
  geom_line(aes(y = subsidized_profit, color = "Subsidized Profit"), size = 1, linetype = "dashed") +
  
  geom_point(aes(x = effort_MSY, y = profit_MSY), color = "forestgreen", size = 4) +
  geom_text(aes(x = effort_MSY, y = profit_MSY, label = "MSY"), color = "forestgreen", nudge_y = 1, size = 4) +
  
  geom_point(aes(x = effort_MEY, y = profit_MEY), color = "forestgreen", size = 4) +
  geom_text(aes(x = effort_MEY, y = profit_MEY, label = "MEY"), color = "forestgreen", nudge_y = 1, size = 4) +
  
  geom_point(aes(x = effort_OA, y = profit_OA), color = "forestgreen", size = 4) +
  geom_text(aes(x = effort_OA, y = profit_OA, label = "OA"), color = "forestgreen", nudge_y = 0, nudge_x = -2, size = 4) +
  geom_point(aes(x = effort_MEY_Subsidized, y = profit_MEY_Subsidized), color = "darkgreen", size = 4, shape = 17) +
  geom_text(aes(x = effort_MEY_Subsidized, y = profit_MEY_Subsidized, label = "MEY Sub."), color = "darkgreen", nudge_y = 1, size = 4) +
  
  geom_point(aes(x = effort_OA_Subsidized, y = profit_OA_Subsidized), color = "darkgreen", size = 4, shape = 17) +
  geom_text(aes(x = effort_OA_Subsidized, y = profit_OA_Subsidized, label = "OA Sub."), color = "darkgreen", nudge_y = -.5 ,nudge_x = 3, size = 4) +
  
  scale_color_manual(values = c("Revenue" = "blue", "Cost" = "red","Subsidized Cost" = "red", "Profit" = "forestgreen", "Subsidized Profit" = "darkgreen")) +
  
  labs(x = "Effort", y = "$", color = "Line Type") + 
   theme_Publication() + 
   theme(legend.position = "bottom")

print(p)
