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

# create b vector
b <- pull(annual_plant_capacity, 2)
b
b <- c(b, rep(0, 15))
b
b <- c(b, pull(forecasted_demand, 2))
b
length(b)
length(pull(forecasted_demand, 2))


# create c vector
c <- pull(inbound_freight_costs, 2) + pull(handling_charges, 2)
c
c <- c(c, pull(inbound_freight_costs, 3) + pull(handling_charges, 2))
c



for (i in 1:505) {
  temp <- outbound_ground_cost %>% 
    slice(i+1) %>%
    unlist(., use.names=FALSE)
  temp2 <- temp[2:16]
  c <- c(c, temp2)
}

# create A matrix

A <- matrix(rep(0, len=552*7605), nrow = 552)
dim(A)

for (i in 1:15) {
  A[1,i] <- 1
  A[2, i + 15] <- 1
}

for (i in 1:15) {
  row_num <- i + 2
  A[row_num, i] <- 1
  A[row_num, i + 15] <- 1
  start <- 30 + ((i-1)*505)+1
  end <- start + 504
  for (j in start:end){
    A[row_num, j] <- -1
  }
}

# this is the problem area
for (i in 1:505) {
  row <- i + 17
  for (j in 1:15){
    col_num <- 30 + i + (j-1)*505
    A[row, col_num] <- 1
  }
  print(row)
}
           
b
# Solve LP

# create model with 7605 variables
lps.model <- make.lp(0, 7605)

# add constraints
for (i in 1:2) {
  add.constraint(lps.model, A[i,], "<=", b[i])
}
mode(b)

for (i in 3:552){
  add.constraint(lps.model, A[i,], "=", b[i])
}
b[522]
# set objective function (default: find minimum)
set.objfn(lps.model, c)  
length(c)
# force variables to be integers
set.type(lps.model, 1:7605, type=c("integer"))

# write model to a file
write.lp(lps.model,'model.lp',type='lp')

# solve
solve(lps.model)

# Retrieve the var values from a solved linear program model 
vars <- get.variables(lps.model)

# make a table to display results
results_table <- tibble(pull(inbound_freight_costs, 1))
results_table <- results_table %>%
  add_column(vars[1:15]) %>%
  add_column(vars[16:30])

results_table <- results_table %>%
  rename(DC = 1, 'From Camden' = 2, 'From Modesto' = 3)
results_table


# To answer question 1: no, they do not have the correct number.
# Based on the data provided, the optimal situation would be to 
# Distribution Centers at all the proposed locations except for
# Nashville.  The DC at Chicago would receive shipments from both
# plants.  The DCs at Dallas and Denver would receive from only
# the Modesto plant.  The other DCs would receive from only the
# Camden plant.  The data for how much is sent from each DC to
# each demand area is also available, if desired.

# check to make sure no factory is asked to make too much
print(sum(results_table[2]))
print(sum(results_table[3]))



