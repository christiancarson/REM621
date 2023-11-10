# Fishers in an area have been complaining that they just can't make ends meet anymore because catch is declining, and they ask for help in the form of a subsidy to offset 50% of their costs.
# Using the parameters we adopted in class, what will happen to total catch for the fishery?
# What is the maximum sustainable yield for this fishery, and at what level of effort can we achieve it?
# What is the maximum economic yield for the fishery, and at what level of effort can we achieve that?
# What combination of parameters would you need to have MSY happen at a lower effort level than MEY?


library(ggplot2)
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

########Standard Fishery at MSY#####
#Standard MSY
B_MSY <- K / 2
#Biomass at MSY 
Y_MSY <- (r * K) / 4 
#Effort at MSY
E_MSY <- r / (2 * Q)

#Standard MEY
#Effort at standard MEY
E_MEY <- ((r)/(2*Q)*((1)-(c/(p*K*Q))))
#𝑴𝑬𝒀 Standard 
MEY <- Q * E_MEY * K * (1 - Q * E_MEY / r)
#Biomass at standard MEY
B_MEY <- MEY / (Q * E_MEY) 

#Standard Open Access Equilibrium (OAE)
#Biomass at standard OAE
B_OA <- c / (p * Q) 
#Effort at standard OAE
E_OA <- (r / Q) * (1 - c / (p * Q * K))
#Yield at standard OAE
Y_OA <- Q * E_OA * (c / (p * Q))

######Subsidized Fishery at MSY####
#Subsidized MEY
#Effort at Subsidized MEY
#E_MEY Subsidized
E_MEY_Subsidized <- ((r)/(2*Q))*((1)-(sc/(p*K*Q)))
#𝑴𝑬𝒀 Subsidized = 𝒒𝑬𝑴𝑬𝒀𝑲 ∙ 𝟏 − 𝒒𝑬𝑴𝑬𝒀𝒓 
MEY_Subsidized <- Q * E_MEY_Subsidized * K * (1 - Q * E_MEY_Subsidized / r)
#Biomass at Subsidized MEY
B_MEY_Subsidized <- MEY_Subsidized / (Q * E_MEY_Subsidized)

#Subsidized Open Access Equilibrium (OAE)
#Biomass at Subsidized OAE
B_OA_Subsidized <- sc / (p * Q)
#Effort at Subsidized OAE
E_OA_Subsidized <- (r / Q) * (1 - sc / (p * Q * K))
#Yield at Subsidized OAE
Y_OA_Subsidized <- Q * E_OA_Subsidized * (sc / (p * Q))

#Create a MATA table
MATA <- data.frame(
  Category = c('Effort', 'Biomass', 'Yield'), 
  MSY = c(E_MSY, B_MSY, Y_MSY),
  OA = c(E_OA, B_OA, Y_OA),
  OA_Subsidized = c(E_OA_Subsidized, B_OA_Subsidized, Y_OA_Subsidized),
  MEY = c(E_MEY, B_MEY, MEY),
  MEY_Subsidized = c(E_MEY_Subsidized, B_MEY_Subsidized, MEY_Subsidized)
)
#transpose but keep names
MATA <- t(MATA)
rownames(MATA) <- c('Category', 'MSY', 'OA', 'OA_Subsidized', 'MEY', 'MEY_Subsidized')
colnames(MATA) <- c('Effort', 'Biomass', 'Yield')
#remove first row
MATA <- MATA[-1,]

tableHTML(MATA)

#setup
df <- data.frame(effort, biomass, yield, revenue, cost, profit, subsidized_profit, MSY, B_MSY, E_MSY, MEY, B_MEY, E_MEY, B_OA, E_OA, Y_OA, B_OA_Subsidized, E_OA_Subsidized, Y_OA_Subsidized, MEY_Subsidized, B_MEY_Subsidized, E_MEY_Subsidized)

#table
library(tableHTML)
tableHTML(df, rownames = FALSE)
