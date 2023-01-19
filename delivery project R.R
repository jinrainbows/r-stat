# Hungerville has undergone rapid development from a 26 block square to 36 blocks.
# The goal of these questions is to estimate the number of cyclists (level of effort) that need to be available in order 
# to meet a pre-specified level of service in terms of how long an order would have 
# to wait at a restaurant before being picked up. We will assume that there are at least as many 
# cyclists as orders waiting to be picked up.
# A larger restaurant list is available in the assignments folder on Canvas.
# It is called restaurants.updated.Rdata and contains 40 restaurants.
# The pre-specified level of service is that the average wait time for an order to be picked up is no longer than 5 minutes.


# The town of Hungerville has come onto the radar of a company that offers meal delivery from restaurants. 
# It uses cyclists for these deliveries.
# Hungerville is an interesting town because it is laid out in a perfect city grid and has exactly 
# 36 blocks in both the north and east directions. Roads going north-south are called Avenues,
# and roads east-west are called streets.
# Meal pickups and deliveries can be viewed as only happening at street intersections.
# This means that every cyclist's, restaurant's and customer's location can be represented as a pair (i,j),
# i for the street they are on, and j for the avenue, where both i and j take integer values between 1 and 36.
# The city planners used the convention that the intersection at the south west corner of the grid is (1,1)
# and the intersection in the north east corner is (36,36).


#Q1. (49 pts) Write a function called "run.sim", that runs a simulation, where the function takes the following four arguments
#    (no default values required, but you can add them if you want). 
#    
#    1. num.cyclists --- the number of cyclists initially available.
#    2. num.orders --- the number of orders initially waiting for pick-up.
#    3. grid.size --- the size of the Hungerville city grid.
#    4. num.its --- the number of iterations in the Monte Carlo simulation.

# The function should return a one column matrix, with the number of rows equal to num.its. 
# The elements in this matrix should be the average time that orders wait to be picked up (where the average
# is taken over all orders (not cyclists) within an iteration).

# Because grid.size is now a variable you will want to add an extra argument to your "newcyclist" and "neworder" 
# functions from Assignment 5, to reflect this fact. Grid size can default to 36.
# Make sure you are randomly drawing a restaurant from the new list of 40, not the original list of 20. 

install.packages("lpSolve")
library(lpSolve)
load("~/Downloads/restaurants.updated.Rdata")

newcyclist <- function(BID = paste(sample(c(LETTERS,0:9),8),sep="",collapse=""),
                       Location = sample(36, 2, replace = TRUE),
                       Status = "Waiting", Destination = NULL,
                       OID = NULL, Speed = 1) {
  return(list(BID = BID, Location = Location,
              Status = Status, Destination = Destination,
              OID = OID, Speed = Speed))
}

neworder <- function(OID = paste(sample(c(LETTERS,0:9), 8), sep="", collapse=""),
                     WaitTime = 0, Restaurant = restaurant.list[[sample(length(restaurant.list), 1)]],
                     Destination = sample(36, 2, replace = TRUE)) {
  return(list(OID = OID, WaitTime = WaitTime, 
              Restaurant = Restaurant, Destination = Destination))
}

distance <- function(loc1, loc2) {
  return(sum(abs(loc1 - loc2)))
}

# The problem you will face in this question is that in Assignment 5, the number of cyclists equalled the number of orders,
# whereas now that is not necessarily true. If the number of orders equals the number of cyclists then the
# distance matrix is square (the same number of rows as columns).
# The optimization function, "lp.assign" in fact requires a square matrix as input and will fail to converge otherwise.

# The trick here, when the number of cyclists does not equal the number of orders, is to pad the distance matrix
# with *zeroes* (not NAs), so that it becomes square. If we have cyclists as rows and orders as columns in the distance matrix,
# and there were 20 cyclists and 10 orders, then you would need to add 10 columns of 20 rows each to make the distance
# matrix square. Because these extra columns (phantom orders) are constructed with all zero entries they don't change
# the solution to the optimization problem. 
# Equivalently, you could create a distance matrix as square and full of zeroes, and then just compute the elements
# that correspond to actual orders. 

# Bottom line: you have to add in an extra step as compared to Assignment 5, where you ensure that the 
# distance matrix is square if necessary, before calling "lp.assign".

#a. Check all the arguments to make sure that they are non-negative numerics. The functions "stopifnot" and "is.numeric"
# will help you do this.

#b. The other thing to check, and stop if it is not true,
# is that the number of cyclists is greater than or equal to the number of orders.

#c. Paste the code for your run.sim function. It will implicitly include your answers to parts a and b.
run.sim <- function(num.cyclists, num.orders, grid.size, num.its) {
  #answer to a
  stopifnot(is.numeric(num.cyclists), is.numeric(num.orders), is.numeric(grid.size), is.numeric(num.its))
  stopifnot(num.cyclists >= 0, num.orders >= 0, grid.size >= 0, num.its >= 0)
  
  #answer to b
  stopifnot(num.cyclists >= num.orders)
  
  #rest of function
  avgtime <- matrix(data = NA, nrow = num.its, ncol = 1)
  
  for (x in 1:num.its) {
    newcyclist()
    neworder()
    
    cyclists <- replicate(num.cyclists, newcyclist(), simplify = F)
    orders <- replicate(num.orders, neworder(), simplify = F)
    
    if (length(cyclists) >= length(orders)) {
      dist_mat <- matrix(0, length(cyclists), length(cyclists))
    } else (length(orders) > length(cyclists)) {
      dist_mat <- matrix(0, length(orders), length(orders))
    }
    
    for (i in 1:length(cyclists)) {
      for (j in 1:length(orders)) {
        dist_mat[i, j] = distance(cyclists[[i]]$Location,
                                  orders[[j]]$Restaurant$Location)
      }
    }
    fm <- lp.assign(dist_mat)
    avgtime[x,1] <- (fm$objval / num.orders) 
  }
  return(avgtime)
}

# Run the function with these arguments and report the estimated mean waiting time for each:

#d. run.sim(num.cyclists = 30, num.orders = 30, grid.size = 36, num.its = 100)
apply(X = run.sim(num.cyclists = 30, num.orders = 30, grid.size = 36, num.its = 100), 
      MARGIN = 2,
      FUN = mean)
#estimated mean waiting time: 9.256667

#e. run.sim(num.cyclists = 40, num.orders = 30, grid.size = 36, num.its = 100)
apply(X = run.sim(num.cyclists = 40, num.orders = 30, grid.size = 36, num.its = 100),
      MARGIN = 2, 
      FUN = mean)
#estimated mean waiting time: 5.887333

#f. run.sim(num.cyclists = 60, num.orders = 20, grid.size = 36, num.its = 100)
apply(X = run.sim(num.cyclists = 60, num.orders = 20, grid.size = 36, num.its = 100),
      MARGIN = 2,
      FUN = mean)
#estimated mean waiting time: 3.6355

#g. run.sim(num.cyclists = 15, num.orders = 20, grid.size = 36, num.its = 100)
run.sim(num.cyclists = 15, num.orders = 20, grid.size = 36, num.its = 100)
#Error in run.sim(num.cyclists = 15, num.orders = 20, grid.size = 36, num.its = 100) : 
#num.cyclists >= num.orders is not TRUE

#h. run.sim(num.cyclists = 20, num.orders = 20, grid.size = 36, num.its = TRUE)
run.sim(num.cyclists = 20, num.orders = 20, grid.size = 36, num.its = TRUE)
#Error in run.sim(num.cyclists = 20, num.orders = 20, grid.size = 36, num.its = TRUE) : 
#is.numeric(num.its) is not TRUE


#Q2 (16 pts) We now assume that there are 30 orders waiting, the grid size is 36 and num.its = 100. 
#   In this question you will explore the average order wait time as the 
#   number of cyclists varies between 30 and 80 in increments of 5.
#   As you *develop* your code it will be a good idea to work with a num.its much smaller than 100,
#   but the final results should use 100. It took 2 minutes to run the complete simulation
#   on my laptop.  

#a. Create an empty container to hold the results of all the simulations.   
#   This should be a matrix of dimensions num.its by 11.
#   Provide columns names for this matrix that describe the level of effort associated with each column.
#   Show the code you used to create this matrix. 
num.its <- 100
sim.results <- matrix(data = NA, nrow = num.its, ncol = 11)
colnames(sim.results) <- c("30", "35", "40", "45", "50", "55",
                           "60", "65", "70", "75", "80")

#b. Set the random number seed to your birthday seed and show the code.
set.seed(20040601)

#c. Use a "for" loop to execute the run.sim command as the number of cyclists varies.
#   Each pass through the loop should populate one column of the results container.
#   Show the code that implements the for loop. 
#   This is the step that could be "parallelized", but that is not  part of this homework.
for (i in 1:11) {
  simcol <- run.sim(num.cyclists = 30 + 5 * (i - 1), num.orders = 30, grid.size = 36, num.its = 100)
  sim.results[,i] <- simcol
}

#d. Estimate the mean wait time for each level of effort (number of cyclists)
#   and print them below. 
effort.means <- apply(X = sim.results,
      MARGIN = 2,
      FUN = mean)

effort.means
#      30       35       40       45       50       55
#9.022000 7.117000 6.024333 5.364667 5.043667 4.622667
#      60       65       70       75       80 
#4.194667 3.990667 3.868333 3.610000 3.409000 

#e. Use the "write.csv" command to save the results container to disk and paste the command 
#   you used to do it below. Using the argument "row.names = FALSE" to write.csv, can save 
#   a little bit of pain later.  
write.csv(sim.results, file = "~/Downloads/simfile.csv", row.names = FALSE)

#Q3. (20 pts) 

#a. Make a plot that shows the level of effort (on the x-axis) against estimated average wait time on the y-axis.
#   It should also have a horizontal line added at height y = 5, so that an optimal level of effort can 
#   be visualized. Axes should be labeled appropriately (use arguments xlab and ylab). The commands "plot" and "abline"
#   are enough to make the plot. Make the y-axis go between 0 and 15 by using the argument to plot, "ylim=c(0,15)".
#   Add a text comment that states what the least number of cyclists is, to meet the effort goal.
#   You can just eyeball this answer from the plot. 
plot(x = as.numeric(colnames(sim.results)), y = effort.means,
     main = "Level of effort v. average wait time",
     xlab = "Level of effort",
     ylab = "Avg. wait time",
     ylim = c(0,15),
     abline(a = 5, b = 0),
     pch = 19)

#The least number of cyclists to meet the effort goal is 55.

#b. Make a plot that shows the level of effort (on the x-axis) against the 90% percentile of the distribution of the
#   mean wait time. That is, apply the "quantile" command to the columns of your results matrix and plot the
#   result. Axes should be labeled appropriately.  It should also have a horizontal line added at height y = 5. 
#   Add a text comment to this slide that states what the least number of cyclists is to meet the effort goal, when
#   expressed in terms of the 90-th percentile of the distribution of the mean.
#   Again, you can just eyeball this answer from the plot. 
ninety.percentile <- apply(X = sim.results, MARGIN = 2, FUN = quantile, 0.9)

plot(x = as.numeric(colnames(sim.results)), y = ninety.percentile,
     main = "Level of effort v. 90% percentile of mean wait time dist.",
     xlab = "Level of effort",
     ylab = "90% percentile of mean wait time dist.",
     ylim = c(0,15),
     abline(a = 5, b = 0),
     pch = 19)

#when expressed in terms of the 90th percentile of the distribution of the mean,
#60 cyclists would be required to meet the effort goal.

  




   


 