## Copy your R expressions to the Moodle window (do NOT upload results or graphs).

## Make an expression to find the position of the minimum 
##value in the vector a = c(6, 4, 2, 9, 3).
which.min(a)

## Try to calculate the mean of the vector a = c(3, 7, 8, NA) 
## by using only numerical values. Use ?mean to see how to handle NA values.
mean(a, na.rm=TRUE)

## Make a scatterplot of two vectors a = c(6, 4, 2, 9, 3), b = c(5, 3, 1, 7, 4). 
## Join points using lines from bottom left to top right 
## (this means you will have to reorder the values of a and b!). 
## Explore ?plot how to add lines and other functions to order data for plots.

ord <- order(a)        
a <- a[ord]
b <- b[ord]

plot(a, b, type = "b")
