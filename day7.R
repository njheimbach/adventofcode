setwd("~/Desktop/advent_of_code/")
fish <- read.table("day7.txt", header = FALSE, sep = ",", dec = ".")
colnames(fish)<-NULL
rownames(fish)<-NULL

# transpose so that you have one column with all the values
coordinates <- data.frame(t(fish[1,]))


# get median
best_coordinate <- median(coordinates$X1)

#make median its own column
coordinates$X2 <- best_coordinate


# calculate cost by median - actual position
coordinates$COST <- coordinates$X2 -coordinates$X1 

# get the absolute value 
coordinates$COST <- abs(coordinates$COST)

# sum up the cost to get result
result <- sum(coordinates$COST)
result 



