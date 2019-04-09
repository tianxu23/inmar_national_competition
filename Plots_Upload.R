############################## Rahul's Code Below ##############################
#####
### Summary ###

# Clustered data containing information on customer zone to see what the major differentiations between customer zones were.
# The results confirm the intuition that the major customer segments are related to location, perhaps prompting according
# distribution strategies. Three clusters are optimal and these correspond to customers in the Eastern US, Central US, and Western US.
### R Markdown Initiate ###

knitr::opts_chunk$set(echo = TRUE)
### Load Libraries ###

library(readxl)
library(corrplot)
library(d3heatmap)
library(NbClust)
library(cluster)
library(mclust)
### Data Read & Clean ###
# Set Directory
#setwd("C:/Users/Rahul Narula/Desktop/Duke/Wake Forest Case Competition")

# Load Data
demand <- read_xlsx("2019 WFU Business Analytics Case Competition - CASEDATA.xlsx", sheet = 1)
inbound.cost <- read_xlsx("2019 WFU Business Analytics Case Competition - CASEDATA.xlsx", sheet = 3, skip = 4, n_max = 15)
handling.charges <- read_xlsx("2019 WFU Business Analytics Case Competition - CASEDATA.xlsx", sheet = 4)
outbound.cost <- read_xlsx("2019 WFU Business Analytics Case Competition - CASEDATA.xlsx", sheet = 5, skip = 1)
transit.time <- read_xlsx("2019 WFU Business Analytics Case Competition - CASEDATA.xlsx", sheet = 6, skip = 1)
air.cost <- read_xlsx("2019 WFU Business Analytics Case Competition - CASEDATA.xlsx", sheet = 7, skip = 1)
# Fix Variable Names
colnames(demand)[1]           <- "Customer"
colnames(air.cost)[1]         <- "Customer"
colnames(outbound.cost)[1]    <- "Customer"
colnames(transit.time)[1]     <- "Customer"
colnames(inbound.cost)[1]     <- "DC"
colnames(handling.charges)[1] <- "DC"
# Join Data
customer <- merge(demand, outbound.cost, by = "Customer")
customer <- merge(customer, transit.time, by = "Customer")
customer <- merge(customer, air.cost, by = "Customer")

# Create Standardized Dataset
customer.std <- customer %>% 
                mutate_at(c(seq(1,ncol(customer))), 
                funs(c(scale(.))))
### Correlation Matrix ###
customer.mat <- as.matrix(customer)
corrplot(cor(customer.mat))
### Clustering ###
# Find Optimal Number of Clusters 
## (3 for unstandardized data, 2/3 tie for standardized. Went with 3)
nbclust <- NbClust(customer.std, method="kmeans", index="all")
# K-Means Clustering
set.seed(98)
best.k <- 3              
km <- kmeans(customer.std, centers=best.k)
############################ Tiansheng's code Below ############################
#################### CASE Competition ##################################
library(tidyverse)
library(readxl)
#setwd("C:/Users/31221/Desktop/wake forest")
if (1==2){
forecasted_demand <- read_xlsx("2019 case.xlsx", sheet = 1)

annual_plant_capacity <- read_xlsx("2019 case.xlsx", sheet = 2)

inbound_freight_costs <- read_xlsx("2019 case.xlsx", sheet = 3, skip = 4, n_max = 15)

handling_charges <- read_xlsx("2019 case.xlsx", sheet = 4)

outbound_ground_cost <- read_xlsx("2019 case.xlsx", sheet = 5, skip = 1)

transit_time_ground <- read_xlsx("2019 case.xlsx", sheet = 6, skip = 1)

next_day_air_cost <- read_xlsx("2019 case.xlsx", sheet = 7, skip = 1)
}
#install.packages("lpSolveAPI")
library(lpSolveAPI)  

# set up the constraints for capacity,supply and demand
# built three vectors for constraints
capacity = pull(annual_plant_capacity, 2)
capacity
capacity = rbind(c('camden','modesto'),capacity)
capacity
capacity_constraint = t(capacity)
capacity_constraint
Supply_constraint = rep(0, 15)
Supply_constraint = cbind(inbound_freight_costs[,1],Supply_constraint) #inbound_freight_costs[,1] used to add DC names to the vector
Supply_constraint
Demand_constraint = as.data.frame(forecasted_demand)
Demand_constraint

# create objective coefficient vector: 
cost_coefficient_camden = pull(inbound_freight_costs, 2) + pull(handling_charges, 2)
cost_coefficient_modesto = pull(inbound_freight_costs,3) + pull(handling_charges, 2)
outbound_ground_cost = as.data.frame(outbound_ground_cost)

cost_coefficient_inbound = numeric()
for(i in 1:505) {
  for(j in 2:16) {
    cost_coefficient_inbound = c(cost_coefficient_inbound,outbound_ground_cost[i,j])
  }
}
length(cost_coefficient_inbound)

# Decision Matrix (plant)
plant_allocation = matrix(rep(0, len=2*7605), nrow = 2)
for(i in 1:2) {
  for(j in 1:15) {
    if(i == 1) {
      plant_allocation[i,j] = 1
    }
    else if (i == 2) {
      plant_allocation[i,j+15] = 1
    }
  }
}

dim(plant_allocation)

# Decision Matrix (supply)
dc_supply = matrix(rep(0, len=15*7605), nrow = 15)
for(i in 1:15) {
 for(j in 0:506) {
   if(j <= 1) {
    dc_supply[i,j*15+1*i] = 1
   }
   else {
    dc_supply[i,j*15+1*i] = -1
   }
 }
}
dim(dc_supply)

# Decision Matrix (supply)
customer_zone = matrix(rep(0, len=505*7605), nrow = 505)
for(i in 1:505) {
  for(j in 1:15) {
    customer_zone[i,30+(15*(i-1))+j] = 1
  }
}
dim(customer_zone)
###################
###### Solver ######
# create model with 7605 variables
lps.model <- make.lp(0, 7605)

# add constraints
# Plant capacity
capacity_constraint_num = as.numeric(capacity_constraint[,2])
for (i in 1:2) {
  add.constraint(lps.model, plant_allocation[i,], "<=", capacity_constraint_num[i])
}

# supply constraints
Supply_constraint_num = as.numeric(Supply_constraint[,2])
for (i in 1:15){
  add.constraint(lps.model, dc_supply[i,], "=", Supply_constraint_num[i])
}

# Demend constraints
Demand_constraint_num = as.numeric(Demand_constraint[,2])
for (i in 1:505){
  add.constraint(lps.model, customer_zone[i,],'=',Demand_constraint_num[i])
}


# set objective function (default: find minimum)
set.objfn(lps.model, c(cost_coefficient_camden,cost_coefficient_modesto,cost_coefficient_inbound))

# force variables to be integers
set.type(lps.model, 1:7605, type=c("integer"))

# write model to a file
write.lp(lps.model,'model.lp',type='lp')

# solve
solve(lps.model)

# Retrieve the var values from a solved linear program model 
vars <- get.variables(lps.model)
vars
# make a table to display results
results_table <- tibble(pull(inbound_freight_costs, 1))
results_table <- results_table %>%
  add_column(vars[1:15]) %>%
  add_column(vars[16:30])

results_table <- results_table %>%
  rename(DC = 1, 'From Camden' = 2, 'From Modesto' = 3)

results_table_no_days_dcs <- results_table

# for each demand zone, add a column representing
# the amount coming from each DC to that zone
for (i in 1:505) {
  customer_zone_temp <- c()
  for (j in 1:15){
    index_temp <- 30 + (i-1)*15 + j
    customer_zone_temp <- c(customer_zone_temp, vars[index_temp])
  }
  
  results_table <- results_table %>%
    add_column(customer_zone_temp) %>%
    rename(new = i+3)
}
dim(results_table)
# fix the colum names
name_vec <- c()
for (i in 1:505){
  name_temp <- toString(outbound_ground_cost[i,1])
  name_vec <- c(name_vec, name_temp)
}
names(results_table)[4:508] <- name_vec
sum(results_table[,3])


dc_customer_table = data.frame()
i = 1
while (i <= 7605) {
  dc_customer_table = dc_customer_table %>% rbind(vars[i:((i-1)+15)])
  i = i+15
}
results_table_no_days_customers <- results_table


# To answer question 1: no, they do not have the correct number.
# Based on the data provided, the optimal situation would be to 
# build Distribution Centers at 12 of the proposed locations.  
# The DC at Indianapolis would receive shipments from both
# plants.  The DCs at Chicago, Dallas and Denver Memphis would 
# receive from only the Modesto plant.  The other DCs would 
# receive from only the Camden plant.  The data for how much is 
# sent from each DC to each demand area is also available, if 
# desired.


# check to make sure no factory is asked to make too much
print(sum(results_table[2]))
print(sum(results_table[3]))

forecasted_demand

# check to make sure demand is satisfied:
demand_temp <- c()
for (i in 1:505) {
  sum_temp <- 0
  for (j in 1:15){
    index_temp <- 30 + (i-1)*15 + j
    sum_temp <- sum_temp + vars[index_temp]
  }
  
  demand_temp <- c(demand_temp, sum_temp)
}
demand_check <- forecasted_demand %>%
  add_column(demand_temp) %>%
  filter(Demand != demand_temp)

# if this prints zero, then it works correctly
print(nrow(demand_check))
                                          #
############################################################
############ force two days shipping ################################
dim(outbound_ground_cost)
dim(next_day_air_cost)
dim(transit_time_ground)

# if transit time is larger than 2, we choost next day air
two_days_shipping_cost = outbound_ground_cost
for (i in 1:505){
  for (j in 2:16){
    if(transit_time_ground[i,j]>2){
      two_days_shipping_cost[i,j] = next_day_air_cost[i,j]
    }
  }
}
head(two_days_shipping_cost)

# rerun solver to get the optimal solution
# only change the outbound cost
capacity = pull(annual_plant_capacity, 2)
capacity = rbind(c('camden','modesto'),capacity)
capacity_constraint = t(capacity)
Supply_constraint = rep(0, 15)
Supply_constraint = cbind(inbound_freight_costs[,1],Supply_constraint) #inbound_freight_costs[,1] used to add DC names to the vector
Demand_constraint = as.data.frame(forecasted_demand)

# create objective coefficient vector: 
cost_coefficient_camden = pull(inbound_freight_costs, 2) + pull(handling_charges, 2)
cost_coefficient_modesto = pull(inbound_freight_costs,3) + pull(handling_charges, 2)

cost_coefficient_inbound = numeric()
for(i in 1:505) {
  for(j in 2:16) {
    cost_coefficient_inbound = c(cost_coefficient_inbound,two_days_shipping_cost[i,j])
  }
}

# Decision Matrix (plant)
plant_allocation = matrix(rep(0, len=2*7605), nrow = 2)
for(i in 1:2) {
  for(j in 1:15) {
    if(i == 1) {
      plant_allocation[i,j] = 1
    }
    else if (i == 2) {
      plant_allocation[i,j+15] = 1
    }
  }
}

# Decision Matrix (supply)
dc_supply = matrix(rep(0, len=15*7605), nrow = 15)
for(i in 1:15) {
  for(j in 0:506) {
    if(j <= 1) {
      dc_supply[i,j*15+1*i] = 1
    }
    else {
      dc_supply[i,j*15+1*i] = -1
    }
  }
}

# Decision Matrix (supply)
customer_zone = matrix(rep(0, len=505*7605), nrow = 505)
for(i in 1:505) {
  for(j in 1:15) {
    customer_zone[i,30+(15*(i-1))+j] = 1
  }
}####################
##### Solver ######
# create model with 7605 variables
lps.model2 <- make.lp(0, 7605)

# add constraints
# Plant capacity
capacity_constraint_num = as.numeric(capacity_constraint[,2])
for (i in 1:2) {
  add.constraint(lps.model2, plant_allocation[i,], "<=", capacity_constraint_num[i])
}

# supply constraints
Supply_constraint_num = as.numeric(Supply_constraint[,2])
for (i in 1:15){
  add.constraint(lps.model2, dc_supply[i,], "=", Supply_constraint_num[i])
}

# Demend constraints
Demand_constraint_num = as.numeric(Demand_constraint[,2])
for (i in 1:505){
  add.constraint(lps.model2, customer_zone[i,],'=',Demand_constraint_num[i])
}

# set objective function (default: find minimum)
set.objfn(lps.model2, c(cost_coefficient_camden,cost_coefficient_modesto,cost_coefficient_inbound))

# force variables to be integers
set.type(lps.model2, 1:7605, type=c("integer"))

# write model to a file
write.lp(lps.model2,'model.lp',type='lp')

# solve
solve(lps.model2)

# Retrieve the var values from a solved linear program model 
vars_two_days <- get.variables(lps.model2)

# make a table to display results
results_table_two_days <- tibble(pull(inbound_freight_costs, 1))
results_table_two_days <- results_table_two_days %>%
  add_column(vars_two_days[1:15]) %>%
  add_column(vars_two_days[16:30])

results_table_two_days <- results_table_two_days %>%
  rename(DC = 1, 'From Camden' = 2, 'From Modesto' = 3)

results_table_two_days_dcs <- results_table_two_days

# for each demand zone, add a column representing
# the amount coming from each DC to that zone
for (i in 1:505) {
  customer_zone_temp <- c()
  for (j in 1:15){
    index_temp <- 30 + (i-1)*15 + j
    customer_zone_temp <- c(customer_zone_temp, vars_two_days[index_temp])
  }
  
  results_table_two_days <- results_table_two_days %>%
    add_column(customer_zone_temp) %>%
    rename(new = i+3)
}
# fix the colum names
name_vec <- c()
for (i in 1:505){
  name_temp <- toString(two_days_shipping_cost[i,1])
  name_vec <- c(name_vec, name_temp)
}
names(results_table_two_days)[4:508] <- name_vec
sum(results_table_two_days[,3])


dc_customer_table = data.frame()
i = 1
while (i <= 7605) {
  dc_customer_table = dc_customer_table %>% rbind(vars[i:((i-1)+15)])
  i = i+15
}
results_table_two_days_customers <- results_table_two_days


                                           #
##### calculate the total cost per weight ###############
# the total cost without two-days shipping
total_cost_inbound_1 = sum(results_table[,2:3]*inbound_freight_costs[,2:3])
total_handling_cost_1 = sum(results_table[,2]*handling_charges[,2])+sum(results_table[,3]*handling_charges[,2])
total_cost_outbound_1 = sum(results_table[,4:508]*t(outbound_ground_cost)[2:16,2:505])
total_cost_1 = total_cost_inbound_1+total_handling_cost_1+total_cost_outbound_1
total_cost_1

# the total cost forced two-days shipping
total_cost_inbound_2 = sum(results_table_two_days[,2:3]*inbound_freight_costs[,2:3])
total_handling_cost_2 = sum(results_table_two_days[,2]*handling_charges[,2])+sum(results_table[,3]*handling_charges[,2])
total_cost_outbound_2 = sum(results_table_two_days[,4:508]*t(two_days_shipping_cost)[2:16,2:505])
total_cost_2 = total_cost_inbound_2+total_handling_cost_2+total_cost_outbound_2
total_cost_2

# the difference 
dif_two_days = total_cost_2-total_cost_1
dif_two_days              #
############################################################
############ force three days shipping ################################
dim(outbound_ground_cost)
dim(next_day_air_cost)
dim(transit_time_ground)

# if transit time is larger than 2, we choost next day air
three_days_shipping_cost = outbound_ground_cost
for (i in 1:505){
  for (j in 2:16){
    if(transit_time_ground[i,j]>3){
      three_days_shipping_cost[i,j] = next_day_air_cost[i,j]
    }
  }
}
head(three_days_shipping_cost)

# rerun solver to get the optimal solution
# only change the outbound cost
capacity = pull(annual_plant_capacity, 2)
capacity = rbind(c('camden','modesto'),capacity)
capacity_constraint = t(capacity)
Supply_constraint = rep(0, 15)
Supply_constraint = cbind(inbound_freight_costs[,1],Supply_constraint) #inbound_freight_costs[,1] used to add DC names to the vector
Demand_constraint = as.data.frame(forecasted_demand)

# create objective coefficient vector: 
cost_coefficient_camden = pull(inbound_freight_costs, 2) + pull(handling_charges, 2)
cost_coefficient_modesto = pull(inbound_freight_costs,3) + pull(handling_charges, 2)

cost_coefficient_inbound = numeric()
for(i in 1:505) {
  for(j in 2:16) {
    cost_coefficient_inbound = c(cost_coefficient_inbound,three_days_shipping_cost[i,j])
  }
}

# Decision Matrix (plant)
plant_allocation = matrix(rep(0, len=2*7605), nrow = 2)
for(i in 1:2) {
  for(j in 1:15) {
    if(i == 1) {
      plant_allocation[i,j] = 1
    }
    else if (i == 2) {
      plant_allocation[i,j+15] = 1
    }
  }
}

# Decision Matrix (supply)
dc_supply = matrix(rep(0, len=15*7605), nrow = 15)
for(i in 1:15) {
  for(j in 0:506) {
    if(j <= 1) {
      dc_supply[i,j*15+1*i] = 1
    }
    else {
      dc_supply[i,j*15+1*i] = -1
    }
  }
}

# Decision Matrix (supply)
customer_zone = matrix(rep(0, len=505*7605), nrow = 505)
for(i in 1:505) {
  for(j in 1:15) {
    customer_zone[i,30+(15*(i-1))+j] = 1
  }
}
##################
###### Solver ######
# create model with 7605 variables
lps.model2 <- make.lp(0, 7605)

# add constraints
# Plant capacity
capacity_constraint_num = as.numeric(capacity_constraint[,2])
for (i in 1:2) {
  add.constraint(lps.model2, plant_allocation[i,], "<=", capacity_constraint_num[i])
}

# supply constraints
Supply_constraint_num = as.numeric(Supply_constraint[,2])
for (i in 1:15){
  add.constraint(lps.model2, dc_supply[i,], "=", Supply_constraint_num[i])
}

# Demend constraints
Demand_constraint_num = as.numeric(Demand_constraint[,2])
for (i in 1:505){
  add.constraint(lps.model2, customer_zone[i,],'=',Demand_constraint_num[i])
}

# set objective function (default: find minimum)
set.objfn(lps.model2, c(cost_coefficient_camden,cost_coefficient_modesto,cost_coefficient_inbound))

# force variables to be integers
set.type(lps.model2, 1:7605, type=c("integer"))

# write model to a file
write.lp(lps.model2,'model.lp',type='lp')

# solve
solve(lps.model2)

# Retrieve the var values from a solved linear program model 
vars_three_days <- get.variables(lps.model2)

# make a table to display results
results_table_three_days <- tibble(pull(inbound_freight_costs, 1))
results_table_three_days <- results_table_three_days %>%
  add_column(vars_three_days[1:15]) %>%
  add_column(vars_three_days[16:30])

results_table_three_days <- results_table_three_days %>%
  rename(DC = 1, 'From Camden' = 2, 'From Modesto' = 3)

results_table_three_days_dcs <- results_table_three_days

# for each demand zone, add a column representing
# the amount coming from each DC to that zone
for (i in 1:505) {
  customer_zone_temp <- c()
  for (j in 1:15){
    index_temp <- 30 + (i-1)*15 + j
    customer_zone_temp <- c(customer_zone_temp, vars_three_days[index_temp])
  }
  
  results_table_three_days <- results_table_three_days %>%
    add_column(customer_zone_temp) %>%
    rename(new = i+3)
}
# fix the colum names
name_vec <- c()
for (i in 1:505){
  name_temp <- toString(three_days_shipping_cost[i,1])
  name_vec <- c(name_vec, name_temp)
}
names(results_table_three_days)[4:508] <- name_vec
sum(results_table_three_days[,3])


dc_customer_table = data.frame()
i = 1
while (i <= 7605) {
  dc_customer_table = dc_customer_table %>% rbind(vars[i:((i-1)+15)])
  i = i+15
}
results_table_three_days_customers <- results_table_three_days


                                          #
###### calculate the total cost per weight ###############
# the total cost without three-days shipping
total_cost_inbound_1 = sum(results_table[,2:3]*inbound_freight_costs[,2:3])
total_handling_cost_1 = sum(results_table[,2]*handling_charges[,2])+sum(results_table[,3]*handling_charges[,2])
total_cost_outbound_1 = sum(results_table[,4:508]*t(outbound_ground_cost)[2:16,2:505])
total_cost_1 = total_cost_inbound_1+total_handling_cost_1+total_cost_outbound_1
total_cost_1

# the total cost forced three-days shipping
total_cost_inbound_2 = sum(results_table_three_days[,2:3]*inbound_freight_costs[,2:3])
total_handling_cost_2 = sum(results_table_three_days[,2]*handling_charges[,2])+sum(results_table[,3]*handling_charges[,2])
total_cost_outbound_2 = sum(results_table_three_days[,4:508]*t(three_days_shipping_cost)[2:16,2:505])
total_cost_2 = total_cost_inbound_2+total_handling_cost_2+total_cost_outbound_2
total_cost_2

# the difference 
dif_three_days = total_cost_2-total_cost_1
dif_three_days
             #
############################################################
############### force four days shipping ################################
dim(outbound_ground_cost)
dim(next_day_air_cost)
dim(transit_time_ground)

# if transit time is larger than 2, we choost next day air
four_days_shipping_cost = outbound_ground_cost
for (i in 1:505){
  for (j in 2:16){
    if(transit_time_ground[i,j]>4){
      four_days_shipping_cost[i,j] = next_day_air_cost[i,j]
    }
  }
}
head(four_days_shipping_cost)

# rerun solver to get the optimal solution
# only change the outbound cost
capacity = pull(annual_plant_capacity, 2)
capacity = rbind(c('camden','modesto'),capacity)
capacity_constraint = t(capacity)
Supply_constraint = rep(0, 15)
Supply_constraint = cbind(inbound_freight_costs[,1],Supply_constraint) #inbound_freight_costs[,1] used to add DC names to the vector
Demand_constraint = as.data.frame(forecasted_demand)

# create objective coefficient vector: 
cost_coefficient_camden = pull(inbound_freight_costs, 2) + pull(handling_charges, 2)
cost_coefficient_modesto = pull(inbound_freight_costs,3) + pull(handling_charges, 2)

cost_coefficient_inbound = numeric()
for(i in 1:505) {
  for(j in 2:16) {
    cost_coefficient_inbound = c(cost_coefficient_inbound,four_days_shipping_cost[i,j])
  }
}

# Decision Matrix (plant)
plant_allocation = matrix(rep(0, len=2*7605), nrow = 2)
for(i in 1:2) {
  for(j in 1:15) {
    if(i == 1) {
      plant_allocation[i,j] = 1
    }
    else if (i == 2) {
      plant_allocation[i,j+15] = 1
    }
  }
}

# Decision Matrix (supply)
dc_supply = matrix(rep(0, len=15*7605), nrow = 15)
for(i in 1:15) {
  for(j in 0:506) {
    if(j <= 1) {
      dc_supply[i,j*15+1*i] = 1
    }
    else {
      dc_supply[i,j*15+1*i] = -1
    }
  }
}

# Decision Matrix (supply)
customer_zone = matrix(rep(0, len=505*7605), nrow = 505)
for(i in 1:505) {
  for(j in 1:15) {
    customer_zone[i,30+(15*(i-1))+j] = 1
  }
}
 ###############
###### Solver ######
# create model with 7605 variables
lps.model2 <- make.lp(0, 7605)

# add constraints
# Plant capacity
capacity_constraint_num = as.numeric(capacity_constraint[,2])
for (i in 1:2) {
  add.constraint(lps.model2, plant_allocation[i,], "<=", capacity_constraint_num[i])
}

# supply constraints
Supply_constraint_num = as.numeric(Supply_constraint[,2])
for (i in 1:15){
  add.constraint(lps.model2, dc_supply[i,], "=", Supply_constraint_num[i])
}

# Demend constraints
Demand_constraint_num = as.numeric(Demand_constraint[,2])
for (i in 1:505){
  add.constraint(lps.model2, customer_zone[i,],'=',Demand_constraint_num[i])
}

# set objective function (default: find minimum)
set.objfn(lps.model2, c(cost_coefficient_camden,cost_coefficient_modesto,cost_coefficient_inbound))

# force variables to be integers
set.type(lps.model2, 1:7605, type=c("integer"))

# write model to a file
write.lp(lps.model2,'model.lp',type='lp')

# solve
solve(lps.model2)

# Retrieve the var values from a solved linear program model 
vars_four_days <- get.variables(lps.model2)

# make a table to display results
results_table_four_days <- tibble(pull(inbound_freight_costs, 1))
results_table_four_days <- results_table_four_days %>%
  add_column(vars_four_days[1:15]) %>%
  add_column(vars_four_days[16:30])

results_table_four_days <- results_table_four_days %>%
  rename(DC = 1, 'From Camden' = 2, 'From Modesto' = 3)

results_table_four_days_dcs <- results_table_four_days

# for each demand zone, add a column representing
# the amount coming from each DC to that zone
for (i in 1:505) {
  customer_zone_temp <- c()
  for (j in 1:15){
    index_temp <- 30 + (i-1)*15 + j
    customer_zone_temp <- c(customer_zone_temp, vars_four_days[index_temp])
  }
  
  results_table_four_days <- results_table_four_days %>%
    add_column(customer_zone_temp) %>%
    rename(new = i+3)
}
# fix the colum names
name_vec <- c()
for (i in 1:505){
  name_temp <- toString(four_days_shipping_cost[i,1])
  name_vec <- c(name_vec, name_temp)
}
names(results_table_four_days)[4:508] <- name_vec
sum(results_table_four_days[,3])


dc_customer_table = data.frame()
i = 1
while (i <= 7605) {
  dc_customer_table = dc_customer_table %>% rbind(vars[i:((i-1)+15)])
  i = i+15
}
results_table_four_days_customers <- results_table_four_days


                                          #
###### calculate the total cost per weight ###############
# the total cost without four-days shipping
total_cost_inbound_1 = sum(results_table[,2:3]*inbound_freight_costs[,2:3])
total_handling_cost_1 = sum(results_table[,2]*handling_charges[,2])+sum(results_table[,3]*handling_charges[,2])
total_cost_outbound_1 = sum(results_table[,4:508]*t(outbound_ground_cost)[2:16,2:505])
total_cost_1 = total_cost_inbound_1+total_handling_cost_1+total_cost_outbound_1
total_cost_1

# the total cost forced four-days shipping
total_cost_inbound_2 = sum(results_table_four_days[,2:3]*inbound_freight_costs[,2:3])
total_handling_cost_2 = sum(results_table_four_days[,2]*handling_charges[,2])+sum(results_table[,3]*handling_charges[,2])
total_cost_outbound_2 = sum(results_table_four_days[,4:508]*t(four_days_shipping_cost)[2:16,2:505])
total_cost_2 = total_cost_inbound_2+total_handling_cost_2+total_cost_outbound_2
total_cost_2

# the difference 
dif_four_days = total_cost_2-total_cost_1
dif_four_days             #
############################### Avi's Code Below ###############################
########################### Cost Difference Analysis ###########################
##### Libraries #####
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
##### Filter Table
### Creating table to filter data of all possible costs to the costs that we
# would be incurring from the original optimal solution

filter_table <- as.data.frame(t(results_table_no_days_customers[,c(1,4:ncol(results_table_no_days_customers))]))
filter_table$Customer <- rownames(filter_table)
names(filter_table) <- c(t(filter_table[1,]))
filter_table <- filter_table[2:nrow(filter_table),]
names(filter_table)[16] <- "Customer"
head(filter_table) 
molten_filter <- melt(filter_table, id.vars = "Customer", value.name = "Demand", variable.name = "DC") %>%
    filter(as.numeric(Demand) != 0) %>%
    select(-Demand)

class(molten_filter$Demand)
##### Melting Frames #####
OBG_Melt <- melt(outbound_ground_cost, 
                 id.vars = "Customer",
                 variable.name = "City",
                 value.name = c("Ground_Cost")
                 )
NDAC_Melt <- melt(next_day_air_cost, 
                 id.vars = "Customer",
                 variable.name = "City",
                 value.name = c("Air_Cost")
                 )
TTG_Melt <- melt(transit_time_ground, 
                 id.vars = "Customer",
                 variable.name = "City",
                 value.name = c("Ground_Time")
                 )
FD_Melt <- melt(forecasted_demand, 
                 id.vars = "Customer",
                 value.name = c("Demand")
                 ) %>%
    select(Customer, Demand)
##### Creating table of all costs and demand
Customer_Clusters <- data.frame(cbind(as.character(forecasted_demand$Customer),km$cluster))
names(Customer_Clusters) <- c("Customer","Cluster")
Cost_Data <- full_join(OBG_Melt, NDAC_Melt, by = c("Customer","City")) %>%
    full_join(TTG_Melt, by = c("Customer","City")) %>%
    left_join(FD_Melt, by = c("Customer")) %>%
    # Add Clusters from Rahul's Analysis to add color to plot
    #left_join(tibble(km$cluster)), by = ""
    mutate(Cost_Diff = Air_Cost - Ground_Cost,
           Customer = as.character(Customer)) %>%
    # Inner joining to molten filter
    inner_join(molten_filter, by = c("Customer", "City"="DC")) %>%
    left_join(Customer_Clusters, by = "Customer")
head(Cost_Data)

cor(Cost_Data$Ground_Cost, Cost_Data$Ground_Time) # Same as Aiko's

### For the clients with really high demand, the Air Unit Cost is rarely more
# than $1.5 more than the Ground Unit Cost.
### For most of these, the ground time is <2 days, but around Demand =  100k,
# there seems to be a cluster where the ground time is really high, closer
# to five days. These are probably the customers worth going for next day air

ggplot(data = Cost_Data,
       mapping = aes(x=Ground_Time,y=Cost_Diff)) +
    #turn violin on or off to get a better sense of the distribution
    geom_violin(aes(group=Ground_Time)) + 
    geom_point(aes(size=Demand, colour = Cluster), alpha = 0.1) +
        theme(
        plot.background = element_rect(fill="#F8F0F0"),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill="#F8F0F0"),
        legend.position = "bottom",
        panel.background = element_rect(fill="#F8F0F0"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "#94ADBB"),
        text = element_text(color = "#044061",family = "mono"),#, face="bold"),
        axis.text = element_text(color = "#044061",family = "mono")
        ) +
    ylab("Difference Between Air and Ground Shipping Cost by Customer") +
    xlab("Days to Ship by Ground, by Customer") +
    ggtitle("Air and Ground Shipping Cost Difference\nAgainst Forecasted Demand") +
    guides(colour = guide_legend(title = "Customer\nCluster"),
           size = guide_legend(title = "Forecasted Demand\nby Customer (lbs)")
           )

ggplot(data = Cost_Data,
       mapping = aes(x=Demand,y=Cost_Diff)) +
    geom_point(aes(size=Ground_Time, colour = Cluster), alpha = 0.25) +
    theme(
        plot.background = element_rect(fill="#F8F0F0"),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill="#F8F0F0"),
        legend.position = "bottom",
        panel.background = element_rect(fill="#F8F0F0"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "#94ADBB"),
        text = element_text(color = "#044061",family = "mono"),#, face="bold"),
        axis.text = element_text(color = "#044061",family = "mono")
        ) +
    guides(colour = guide_legend(title = "Customer\nCluster"),
           size = guide_legend(title = "Ship Time\nby Ground")
           ) +
    ylab("Difference Between Air and Ground Shipping Cost by Customer") +
    xlab("Forecasted Demand by Customer (lbs)") +
    ggtitle("Air and Ground Shipping Cost Difference\nAgainst Forecasted Demand")
