################################## CASE Competition ##################################
library(tidyverse)
library(readxl)
setwd("C:/Users/31221/Desktop/wake forest")
forecasted_demand <- read_xlsx("2019 case.xlsx", sheet = 1)

annual_plant_capacity <- read_xlsx("2019 case.xlsx", sheet = 2)

inbound_freight_costs <- read_xlsx("2019 case.xlsx", sheet = 3, skip = 4, n_max = 15)

handling_charges <- read_xlsx("2019 case.xlsx", sheet = 4)

outbound_ground_cost <- read_xlsx("2019 case.xlsx", sheet = 5, skip = 1)

transit_time_ground <- read_xlsx("2019 case.xlsx", sheet = 6, skip = 1)

next_day_air_cost <- read_xlsx("2019 case.xlsx", sheet = 7, skip = 1)

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
dc_supply [1,]
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
Demand_constraint_num = as.integer(Demand_constraint_num*0.95)

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

results_table

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
results_table


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

senstvt_constraint_1$duals[525]
senstvt_report_1$objfrom[1]
senstvt_report_1$objtill[3:17]
