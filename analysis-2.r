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


library(lpSolveAPI)  

# create b vector
b <- pull(annual_plant_capacity, 2)
b <- c(b, rep(0, 15))
b <- c(b, pull(forecasted_demand, 2))


# create c vector
c <- pull(inbound_freight_costs, 2) + pull(handling_charges, 2)
c <- c(c, pull(inbound_freight_costs, 3) + pull(handling_charges, 2))
for (i in 1:505) {
  temp <- outbound_ground_cost %>% 
    slice(i) %>%
    unlist(., use.names=FALSE)
  temp2 <- temp[2:16]
  c <- c(c, temp2)
}

# create A matrix

A <- matrix(rep(0, len=552*7605), nrow = 552)

# first two rows: plant capacity
for (i in 1:15) {
  A[1,i] <- 1
  A[2, i + 15] <- 1
}

# make sure DC in and out are equal
for (i in 1:15) {
  row_num <- i + 2
  # in amounts
  A[row_num, i] <- 1
  A[row_num, i + 15] <- 1
  
  # out amounts: every 15th element
  for (j in 1:505){
    index <- 30 + i + (j-1) * 15
    A[row_num, index] <- -1
  }
}

# ensure demand zones get proper amount
for (i in 1:505) {
  row <- i + 17
  for (j in 1:15){
    col_num <- 30 + i + (j-1)*505
    A[row, col_num] <- 1
  }
  print(row)
}
            

# Solve LP

# create model with 7605 variables
lps.model <- make.lp(0, 7605)

# add constraints
for (i in 1:2) {
  add.constraint(lps.model, A[i,], "<=", b[i])
}

for (i in 3:552){
  add.constraint(lps.model, A[i,], "=", b[i])
}

# set objective function (default: find minimum)
set.objfn(lps.model, c)  

# force variables to be integers
set.type(lps.model, 1:7605, type=c("integer"))

# write model to a file
write.lp(lps.model,'model.lp',type='lp')

# solve
solve(lps.model)

# Retrieve the var values from a solved linear program model 
vars <- get.variables(lps.model)

# make a table to display results
# pull in the list of DC names
results_table <- tibble(pull(inbound_freight_costs, 1))
# pull in the amounts going from each of the two plants
# to each of the 15 DCs
results_table <- results_table %>%
  add_column(vars[1:15]) %>%
  add_column(vars[16:30])

# add column names
results_table <- results_table %>%
  rename(DC = 1, 'From Camden' = 2, 'From Modesto' = 3)


results_table
# for each demand zone, add a column representing
# the amount coming from each DC to that zone
for (i in 1:505) {
  col_temp <- c()
  for (j in 1:15){
    index_temp <- 30 + i + (j-1)*505
    col_temp <- c(col_temp, vars[index_temp])
  }
  
  results_table <- results_table %>%
    add_column(col_temp) %>%
    rename(new = i+3)
}

# fix the colum names
name_vec <- c()
for (i in 1:505){
  name_temp <- toString(outbound_ground_cost[i,1])
  name_vec <- c(name_vec, name_temp)
}
names(results_table)[4:508] <- name_vec


# To answer question 1: no, they do not have the correct number.
# Based on the data provided, the optimal situation would be to 
# Distribution Centers at all the proposed locations.  
# The DC at Knoxville would receive shipments from both
# plants.  The DCs at Chicago, Dallas, Denver, Indianapolis,
# Louisville, Memphis, and Nashville would receive from only
# the Modesto plant.  The other DCs would receive from only the
# Camden plant.  The data for how much is sent from each DC to
# each demand area is also available, if desired.

# check to make sure no factory is asked to make too much
print(sum(results_table[2]))
print(sum(results_table[3]))

# check to make sure demand is satisfied:
demand_temp <- c()
for (i in 1:505) {
  sum_temp <- 0
  for (j in 1:15){
    index_temp <- 30 + i + (j-1)*505
    sum_temp <- sum_temp + vars[index_temp]
  }
  
  demand_temp <- c(demand_temp, sum_temp)
}
demand_check <- forecasted_demand %>%
  add_column(demand_temp) %>%
  filter(Demand != demand_temp)

# if this prints zero, then it works correctly
print(nrow(demand_check))

# Questions 2-4:

# "Does it ever make sense to use Next Day Air?" 
# "What is the tradeoff between customer service and cost?"
# These two questions are essentially the same thing: 
# since the ground freight costs are always less than the 
# Next Day Air, from a pure cost perspective, it never
# makes sense to use Next Day Air.  However, customer
# service should also be figured in.  

# take results table and limit it to the results
# regarding DC to demand zone shipping
shipping_table <- results_table[4:508]
shipping_matrix <- data.matrix(shipping_table)

# convert the shipping time table to a matrix
time_matrix <- t(data.matrix(transit_time_ground[2:16]))

# multiply, element-wise, the amount shipped and the
# time it takes to ship, for each origin-destination pair
shipping_days_matrix <- shipping_matrix * time_matrix

# sum over the entire matrix to find the 
# total days of shipping and total amount shipped
total_shipping_days <- sum(shipping_days_matrix)
total_amount_shipped <- sum(shipping_matrix)

# calculate the average days it takes to ship with ground
avg_shipping_days <- total_shipping_days / total_amount_shipped

# it takes 2.499864 days on avg
print(avg_shipping_days)

# next, calculate the total cost of the ground shipping
ground_cost <- sum(vars[31:7605] * c[31:7605])
print(ground_cost)
# so using ground freight only, it costs $6,301,038 

# to calculate cost of if only air shipping is used
# convert air rates to a matrix
air_rates_matrix <- t(data.matrix(next_day_air_cost[2:16]))
# multiply, element wise, rates * pounds shipped, and sum
air_cost <- sum(shipping_matrix * air_rates_matrix)
print(air_cost)
# so using air only, it would cost $29,775,865
# and, of course, take 1 day by assumption

# So, the tradeoff is, on one extreme, using only ground,
# which would cost $6.3 million, and take 2.5 days on average.
# Or, on the other extreme, using next day air only, which
# would cost $29.8 million and take 1 day, by assumption.
# One option would be to simply default to free ground shipping,
# and give the customer the option to pay for next day air. Or, lower
# prices overall and just ask the customer to choose which option 
# they want to pay for. However, if the company has decided it wants
# to offer 'free' shipping and pay the costs themselves, it can 
# decide between the two extremes, or find a better middle ground.

# For example, what if the company determined that if the ground
# shipping would take more than 2 days, it would use air freight instead?

# use this matrix to select entries only for the shipments the
# optimal plan found above actually makes.
shipping_matrix[shipping_matrix >0] <- 1

# select the ground days info for shipments actually made
ground_days_matrix <- shipping_matrix * time_matrix
# if they are more than 2 days, the company will use 
# air shipment, so change thes entries to zero
ground_days_matrix[ground_days_matrix > 2] <- 0 

# now do the opposite: select only shipments where the 
# ground shipment takes more than two days
air_days_matrix <- shipping_matrix * time_matrix
air_days_matrix[air_days_matrix <= 2] <- 0
air_days_matrix[air_days_matrix > 0] <- 1

# Now, for this plan, we want to find the total cost
# and the avg shipping time.

# reload this matrix, as it has been modified
shipping_matrix <- data.matrix(shipping_table)

# calculate total days for each shipping method, then find avg
custom_ground_time <- sum(ground_days_matrix * shipping_matrix)
custom_air_time <- sum(air_days_matrix * shipping_matrix)
custom_avg_time <- (custom_ground_time + custom_air_time) / sum(shipping_matrix)
print(custom_avg_time)

# calculate total cost

# create the rates matrix for ground shipping
ground_rates_matrix <- t(data.matrix(outbound_ground_cost[2:16]))

# convert ground_days_matrix to only indicator variables
ground_days_matrix[ground_days_matrix >0] <- 1

# multiply indicator matrices * shipping amount matrix * cost matrices
custom_ground_cost <- sum(ground_days_matrix * shipping_matrix * ground_rates_matrix)
custom_air_cost <- sum(air_days_matrix * shipping_matrix * air_rates_matrix)

# sum the two methods
custom_total_cost <- custom_ground_cost + custom_air_cost
print(custom_total_cost)

# So using this method, it would cost $20,213,360 to reduce the
# average shipping days to 1.4, and guarantee no shipping of 
# more than 2 days.  This is an in-between plan of only ground or 
# only air, and the company could choose where it wants to fall 
# exactly, but this shows the trade off.

