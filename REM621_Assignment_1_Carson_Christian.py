#NPV calculation

yrs <- 1:20  #Vector of years

yrrev <- 1000000  #Yearly revenue (assume constant)

totcost <- 10000000  #Total cost (assume only first year costs)

dr <- c(0.01,0.05,0.1,0.2) #Vector of discount rates 

pv <- function(fv, disrt, year)  #Present value function given the future value, discount rate, and year 
{fv / (1 + disrt)^year}          

pvs <- matrix(NA, ncol = length(dr), nrow = length(yrs))  #Empty matrix for present values 
colnames(pvs) <- dr                                       

#Calculating present values over the different discount rates and years
#This is called a nested loop (for-loop within a for-loop), not very efficient but ok for now

for(j in 1:length(dr))
{
  for(i in 1:length(yrs))
  {
    pvs[i,j] <-   pv(yrrev,dr[j],yrs[i])
  }
}

npv <- colSums(pvs) - totcost  #NPVs
bcr <- colSums(pvs) / totcost  #BCRs

#Simple plot of results with 'base' package, there are many more options with 'ggplot2' package
matplot(yrs, pvs/1000000, type = "l", 
        xlab = "Years", ylab = "Present value (millions)", lty= 1, lwd = 2)