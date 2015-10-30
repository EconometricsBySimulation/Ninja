# The travelling Vampire Problem

rm(list=ls())

library(gtools)
library(dplyr)

# Define a funtion to solve the traveling vamp problem
travelingvamp <- function(N,dim=2, visualize=FALSE) {
  # N is the number of maidens and dim is the dimensional space
  # the default is 2 but you can imagine higher or lower dimensional as well
  
  # Generate the matrix of maidens
  maids <- matrix(rnorm(N*dim,dist), ncol=dim)
  
  # Write a function to minimize the euclidean distance.
  # You could imagine alternative distance tools
  euclid <- function(y) 
    (y[-1,]-y[-nrow(y),])^2 %>% 
    apply(1,function(x) sum(x) %>% sqrt) %>%
    sum

  # Calculate all of the alternative routes
  routes <- permutations(N,N)
  
  # K is a measure of the number of routes 
  K <- prod(1:N)
  
  # Create an empty vector of distance for each route
  dist <- rep(NA, K)
  
  # Calculate the distance needed to travel from the origin 0 to 
  # through the route
  for (i in 1:K) dist[i] <- euclid(rbind(0,maids[routes[i,],]))
  
  # Find the minimum and the maximum distance routes
  rmin <- routes[(dist==min(dist))]
  rmax <- routes[(dist==max(dist))]
  
  # If visualize is on, the graph both the quickest and longest routes
  if (visualize) {
    par(mfrow=c(2,1), mar=c(.5,0,0,0))
    plot(rbind(0,maids[rmin,]), type='b', lwd=2, xaxt='n')
      points(0,0, col="darkred", lwd=4)
    par(mar=c(0,0,.5,0))
    plot(rbind(0,maids[rmax,]), type='b', lwd=2, xaxt='n')
      points(0,0, col="darkred", lwd=4)
      mtext(paste("Round:",N, "#:", K, "Min:", round(min(dist),2),"Max:",round(max(dist),2)))
  }
  
  # Return the fastest and shortest routes
  list(rmin=rbind(0,maids[rmin,]), 
       rmax=rbind(0,maids[rmax,])
)}

# Looks like it is working well
travelingvamp(3, visualize=TRUE)

# Set the number of routes to check
N <- 10
time <- rep(0, N)

# Skip the first one as it is too short
for (i in 2:N) time[i] <- system.time(travelingvamp(i, visualize=TRUE))

# Notice that the time taken to calculate each route is growing very quickly
time

time[-1]/time[-10]
# The ratio of times is not a smooth relationship as we might expect.
# This is probably due to more than simply processing power being used in the computation.
# As other resources such as avaialable memory begins to run low the computation
# becomes increasingly challenging
