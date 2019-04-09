################################## CASE Competition ##################################
library(tidyverse)
library(readxl)
library(lpSolveAPI) 
setwd("C:/Users/31221/Desktop/wake forest")
forecasted_demand <- read_xlsx("2019 case.xlsx", sheet = 1)

annual_plant_capacity <- read_xlsx("2019 case.xlsx", sheet = 2)

inbound_freight_costs <- read_xlsx("2019 case.xlsx", sheet = 3, skip = 4, n_max = 15)

handling_charges <- read_xlsx("2019 case.xlsx", sheet = 4)

outbound_ground_cost <- read_xlsx("2019 case.xlsx", sheet = 5, skip = 1)

transit_time_ground <- read_xlsx("2019 case.xlsx", sheet = 6, skip = 1)

next_day_air_cost <- read_xlsx("2019 case.xlsx", sheet = 7, skip = 1)

#install.packages("lpSolveAPI")
 

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

get.objective(lps.model)
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

############################################################
### force two days shipping ################################
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
}

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
vars_two_days <- get.variables(lps.model2)

# make a table to display results
results_table_two_days <- tibble(pull(inbound_freight_costs, 1))
results_table_two_days <- results_table_two_days %>%
  add_column(vars_two_days[1:15]) %>%
  add_column(vars_two_days[16:30])

results_table_two_days <- results_table_two_days %>%
  rename(DC = 1, 'From Camden' = 2, 'From Modesto' = 3)

results_table_two_days

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
results_table_two_days


############## calculate the total cost per weight ###############
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
dif_two_days



############################################################
### force three days shipping ################################
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

###### Solver ######
# create model with 7605 variables
lps.model3 <- make.lp(0, 7605)

# add constraints
# Plant capacity
capacity_constraint_num = as.numeric(capacity_constraint[,2])
for (i in 1:2) {
  add.constraint(lps.model3, plant_allocation[i,], "<=", capacity_constraint_num[i])
}

# supply constraints
Supply_constraint_num = as.numeric(Supply_constraint[,2])
for (i in 1:15){
  add.constraint(lps.model3, dc_supply[i,], "=", Supply_constraint_num[i])
}

# Demend constraints
Demand_constraint_num = as.numeric(Demand_constraint[,2])
for (i in 1:505){
  add.constraint(lps.model3, customer_zone[i,],'=',Demand_constraint_num[i])
}

# set objective function (default: find minimum)
set.objfn(lps.model3, c(cost_coefficient_camden,cost_coefficient_modesto,cost_coefficient_inbound))

# force variables to be integers
set.type(lps.model3, 1:7605, type=c("integer"))

# write model to a file
write.lp(lps.model3,'model.lp',type='lp')

# solve
solve(lps.model3)

# Retrieve the var values from a solved linear program model 
vars_three_days <- get.variables(lps.model3)

# make a table to display results
results_table_three_days <- tibble(pull(inbound_freight_costs, 1))
results_table_three_days <- results_table_three_days %>%
  add_column(vars_three_days[1:15]) %>%
  add_column(vars_three_days[16:30])

results_table_three_days <- results_table_three_days %>%
  rename(DC = 1, 'From Camden' = 2, 'From Modesto' = 3)

results_table_three_days

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

dc_customer_table = data.frame()
i = 1
while (i <= 7605) {
  dc_customer_table = dc_customer_table %>% rbind(vars[i:((i-1)+15)])
  i = i+15
}
results_table_three_days


############## calculate the total cost per weight ###############
# the total cost without two-days shipping
total_cost_inbound_1 = sum(results_table[,2:3]*inbound_freight_costs[,2:3])
total_handling_cost_1 = sum(results_table[,2]*handling_charges[,2])+sum(results_table[,3]*handling_charges[,2])
total_cost_outbound_1 = sum(results_table[,4:508]*t(outbound_ground_cost)[2:16,2:505])
total_cost_1 = total_cost_inbound_1+total_handling_cost_1+total_cost_outbound_1
total_cost_1

# the total cost forced two-days shipping
total_cost_inbound_3 = sum(results_table_three_days[,2:3]*inbound_freight_costs[,2:3])
total_handling_cost_3 = sum(results_table_three_days[,2]*handling_charges[,2])+sum(results_table[,3]*handling_charges[,2])
total_cost_outbound_3 = sum(results_table_three_days[,4:508]*t(three_days_shipping_cost)[2:16,2:505])
total_cost_3 = total_cost_inbound_3+total_handling_cost_3+total_cost_outbound_3
total_cost_3

# the difference 
dif_three_days = total_cost_3-total_cost_1
dif_three_days


#########################sensitivity report ######################

senstvt_report_1 =get.sensitivity.obj(lps.model)
senstvt_constraint_1 =get.sensitivity.rhs(lps.model)
senstvt_report_1[1]
length(senstvt_report_1[1])
senstvt_constraint_1$duals[1:2]
senstvt_constraint_1$dualsfrom[1:2]
senstvt_constraint_1$dualstill[1:2]
get.constraints(lps.model)


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

