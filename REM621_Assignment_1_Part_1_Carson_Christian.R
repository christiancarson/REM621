#Christian Carson
#REM 621
#Assignment 1
#09/22/2023
#############################################################################
#You’ve been asked to evaluate a proposal for a new dam to be
#built along a remote river that is currently used for subsistence fisheries.
#Construction is expected to cost $10M in the first year (pretend this is a fast build),
#with additional operating costs of $100K/year over the following 19 years.
#It is expected that this dam will generate electricity worth
#$1M/year starting in year 3 increasing by 2% per year from then on due to higher expected demand
#Proponents have also indicated an additional $50K/year from local fishing
#tourism starting in year 5 when the new lake is stable and fully stocked with fish.
#The planning and public consultation process is in very early stages,
#so there is a lot of uncertainty about what discount rate and
#decision rule to use for the project. The main questions are:

#####Part 1 ######

#1. What is the NPV and BCR of the project at a 3, 5, 10, and 20% discount rate?
#Question 1

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
#years vec
yrs <- 1:20
#Total cost year 1
totcost <- 10000000 
#cost every year following 1
annualcost <- rep(100000, length(yrs))

#electrical increasing 2% every year after year 3, 0 for first three years
yrrev <- c(rep(0, 2), 1000000, rep(1000000, length(yrs) - 3))
yrrev[4:length(yrs)] <- yrrev[4:length(yrs)] * 1.02^(1:(length(yrs)-3))

#fishing after year 5
fishrev <- rep(0, length(yrs))
fishrev[6:length(yrs)] <- rep(50000, length(yrs)-5)

#discount rates vec 
dr <- c(0.03, 0.05, 0.1, 0.2)  # Changed from 0.01, 0.05, 0.1, 0.2 based on your requirement

#storeage vector for the dr forloop for both npv and bcr
npv_results <- numeric(length(dr))
bcr_results <- numeric(length(dr))

#loop over each discount rate to calculate NPV and BCR -> each rows calc broken down below
for (i in 1:length(dr)) {
  #iterate dr to get the current discount rate
  rate <- dr[i]
  #calculate pv of annual costs for each year with future value/(1 + dr)^(year-1);'-1' since the first year's costs aren't discounted. # nolint
  discounted_costs <- sum(annualcost / ((1 + rate)^(yrs - 1)))
  #calculate the pv of elec revenues, from year 3 on (same formula)
  discounted_elec_rev <- sum(yrrev / ((1 + rate)^(yrs - 2)))
  #calculate the pv of fish revenues, from year 5 on (same formula)
  discounted_fish_rev <- sum(fishrev / ((1 + rate)^(yrs - 4)))
  #npv calculation across all dr; total revenue - total cost, resulting in the net benefit or loss at given dr
  npv_results[i] <- (discounted_elec_rev + discounted_fish_rev) - (totcost + discounted_costs)
  #bcr ration; >1, benefits outweigh costs, <1 costs outweigh benefits
  bcr_results[i] <- (discounted_elec_rev + discounted_fish_rev) / (totcost + discounted_costs)
}

######plot the npv and bcr at each dr to see what is best
results <- data.frame(DiscountRate = dr, NPV = npv_results, BCR = bcr_results)
long_results <- melt(results, id.vars = "DiscountRate", variable.name = "Metric", value.name = "Value")
long_results$Metric <- as.character(long_results$Metric)
long_results$Value[long_results$Metric == "BCR"] <- (long_results$Value[long_results$Metric == "BCR"] - 1) * 100
long_results$Metric[long_results$Metric == "BCR"] <- "BCR (% Deviation from 1)" 
#tried many times to get the axes to conform and have one for $ and one for % for bcr but just had to go with discrete axes->
long_results$FormattedValue <- ifelse(long_results$Metric == "NPV", 
                                      scales::dollar(long_results$Value), 
                                      scales::percent(long_results$Value/100))

colors <- scale_fill_viridis(discrete = TRUE)
npv_color <- viridis(2)[1]
bcr_color <- viridis(2)[2]

#plot
p <- ggplot(long_results, aes(x = as.factor(DiscountRate), y = FormattedValue)) + 
  geom_bar(data = subset(long_results, Metric == "NPV"), aes(fill = "NPV"), 
           stat = "identity", position = "dodge") +
  geom_point(data = subset(long_results, Metric == "BCR (% Deviation from 1)"), 
             aes(color = "BCR"), size = 3) +
  geom_line(data = subset(long_results, Metric == "BCR (% Deviation from 1)"), 
            aes(group = Metric, color = "BCR", linetype = "BCR"), linewidth = 1) +
  facet_wrap(~Metric, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = c("NPV" = npv_color)) +
  scale_color_manual(values = c("BCR" = bcr_color)) +
  scale_linetype_manual(values = c("BCR" = "solid")) +
  labs(x = NULL, y = NULL, 
       fill = NULL, color = NULL) +
  theme_Publication() +
  theme(legend.position = "bottom")


# Print the plot
print(p)

#####Part 2#### -> COMPLETED IN EXCEL FILE
#What are other costs and benefits that we could consider
#as part of this project? Build a MATA table and include 
#a qualitative impact scale to help convey this.

#######Part 3 ########## -> completed in comment on file submission 
#Based on the table from the previous questions, 
#which includes both quantitative (e.g., NPV, BCR)
# and qualitative impacts, write a short paragraph to
# convince someone that 1) this project should be built,
# and 2) this project should not be built.

