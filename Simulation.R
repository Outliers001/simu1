# number of elements in the population

N <- 160000000

# Population parameter. Proportion of individuals responding “YES”.

p <- .7 # Change the value as you wish.



# Generating population data

# Number of YES, i.e., 1s = N * .7

# Number of NO i.e., 0s = N * .3



pop <- c(rep(1, N * p), rep(0, N * (1 – p) ))



# Copy and paste on your R workspace.

# Internal function to compute 95% confidence Intervals;



conf<- function(x)
  
{
  
  # counting the number of ones
  
  ones <- as.vector(as.matrix(table(x))[2,1])
  
  # Calculating sample proportion
  
  phat <- ones/length(x)
  
  # standard error of sample proportion
  
  phat.sd <- sqrt(phat*(1-phat)/length(x))
  
  # 95% confidence interval for population proportion
  
  interval <- c((mean(x) – 1.96 * phat.sd), (mean(x)+ 1.96 * phat.sd))
  
  #returns the interval
  
  interval
  
}



# Copy and paste on your R workspace.

# Function to calculate coverage probability



coverage <- function (n, P = pop, R)
  
{
  
  sample.data <- matrix(sample(P, n * R), nrow = n, ncol = R)
  
  phat <- apply(sample.data, 2, conf)
  
  phat
  
}



# How many intervals would include the true value of the

# population proportion if we sample repeatedly (say, 1000 times)?



# Fixing the random seed ensures that the results are reproducible.

# Setting the seed to 10.



set.seed(10)



# Draw 1000 random sample each of size 100

# Compute 95% confidence intervals based on each sample generated.



out <- coverage(n = 100, R = 1000)



# Lower confidence limit

lcl <- out[1,]



# Upper confidence limit

ucl <- out[2,]



# How many of these intervals include the true value, p ?

table(lcl <= p & ucl >= p)

Results
> table(lcl <= p & ucl >= p)



FALSE  TRUE

51   949

