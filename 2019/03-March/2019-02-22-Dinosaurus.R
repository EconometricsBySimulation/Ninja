library(ggplot2)
library(dplyr)

################################################################
datagenerator <- function(n = 50, meanx = 9, varX = 11, b0 = 3, b1 = .5, corXY = .8,
                          xFUN = function(n) rnorm(n), 
                          uFUN = function(x) rnorm(length(x)),
                          varE = 0) {
  
  # Mean y and variance y have to be set with some care as they are dependent upon x to some extent
  # since population mean(y) = b0 + b1 * mean(x) + mean(u) = b0 + b1*meanx 
  # variance(y) = b1^2 * variance(x) + variance(u) + 2*cov(x,u)
  
  # So we know that
  # meany     = b0+b1*meanx
  
  SSE <- (b1^2 * varX)*n
  # SST <- SSE + SSU
  
  # SSU = SSE/corXY - SSE
  # varianceU = SSU/n
  # varianceU = (SST - SSE)/n
  # Since corXY^2 = (SSE/SST)
  # SST =  SSE/corXY^2
  varianceu = (SSE/corXY^2 - SSE)/n + varE
  
  # variancey = b1^2 * varX + varianceu
  
  # First off let us try to generate our data
  x <- xFUN(n)
  
  # Adjust x to have meanx and varX
  a <- (varX/var(x))^.5
  x <- a*(x-mean(x)) + meanx
  mean(x) ; var(x)
  
  # if (is.na(set2)) set2 <- set
  
  u <- uFUN(x)
  
  # Adjust u to be uncorrelated with x
  cfux <- coef(lm(u~x))
  u <- u - cfux[1] - cfux[2]*x
  
  # Adjust u to have varianceu
  a <- (varianceu/var(u))^.5
  u <- a*(u-mean(u))
  
  y = b0 + b1*x + u
  
  xy <- data.frame(x,y)
}
################################################################

# List of datasets
objectlistcsv <- c("batman.csv", "bird.csv", "cat.csv", "dino.csv",           
                   "flower.csv", "georgewashington.csv", "trump2.csv", 
                   "brokenheart.csv", "trex.csv", "trump1.csv", "putin.csv",
                   "bart.csv")

objectlist <- gsub(".csv", "", objectlistcsv)

# Inverse the y axis
invy <- function(x) data.frame(x=x[,1],y=-x[,2])

# Read the datasets from github
for (i in 1:length(objectlist))
  assign(objectlist[i], 
         read.csv(paste0('https://raw.githubusercontent.com/EconometricsBySimulation/Ninja/master/2019/03-March/',
                         objectlistcsv[i])) %>% data.frame() %>% invy)

# Plot some examples of the raw datasets
ggplot(bart, aes(x=x,y=y)) + geom_point() + theme_bw() 
ggplot(cat, aes(x=x,y=y)) + geom_point() + theme_bw()
ggplot(flower, aes(x=x,y=y)) + geom_point() + theme_bw()
ggplot(georgewashington, aes(x=x,y=y)) + geom_point() + theme_bw()

# Generate modified data sets with the correlation between X and Y set to 0.
for (i in 1:length(objectlist))
  assign(paste0(objectlist[i],"corXY0"), 
         datagenerator(b1=0,
             xFUN = function(n) get(objectlist[i])[,1], uFUN = function(x) get(objectlist[i])[,2], varE=1))

# The see how forcing the correlation to be zero between X and Y affected the graphs.
ggplot(georgewashingtoncorXY0, aes(x=x,y=y)) + geom_point() + theme_bw()
ggplot(catcorXY0, aes(x=x,y=y)) + geom_point() + theme_bw()

# We can see that b0=0 and the means of x and y are exactly as expected
lm(y~x,georgewashingtoncorXY0)
summary(georgewashingtoncorXY0)

# Save the corXY0 as std (standardized data sets)
for (i in 1:length(objectlist))
  write.csv(get(paste0(objectlist[i],"corXY0")), 
            file=paste0(objectlist[i],"std.csv"))

################################################################
# Next I began to focus on how to create the animations I was going
# to display.

# The filler function sets up a mechanism for filling out a smaller dataset
# that contains some points to be as large as the largest dataset (1000 points)
# This allows unique points to transition from one graph to the next
filler <- function(xy, n=1000) {
  fill <- (n-nrow(xy))
  if (fill > 0) for (i in 1:(n-nrow(xy))) {
    j <- sample(nrow(xy), 1)
    # Calculate Euclidean distances
    euclidean <- ((xy[j,1] - xy[-j,1])^2 + (xy[j,2] - xy[-j,2])^2)^.5
    
    xy <- ((xy[j,] + xy[-j,][euclidean==min(euclidean),][1,])/2) %>% rbind(xy,.)
    
  }
  
  # Pass the xyNew data through the generator to smooth out mean and variance changes
  datagenerator(b1=0, xFUN = function(n) xy[,1], uFUN = function(x) xy[,2], varE=1)
  
}

# Rescale the points to set the mean and variances to be the same.
# Also include in angle2 filled in points to ensure that 1000 points are filled. 
for (i in 1:length(objectlist)){
  xy <- get(objectlist[i]) %>% filler
  assign(paste0(objectlist[i],"corXY0_filled"), 
         datagenerator(b1=0,
                       xFUN = function(n) xy[,1], uFUN = function(x) xy[,2], varE=1))
}

# Filling in dot images seems to make them feel like they were drawn with a heavier hand
ggplot(catcorXY0, aes(x=x,y=y)) + geom_point() + theme_bw()
ggplot(catcorXY0_filled, aes(x=x,y=y)) + geom_point() + theme_bw()

# Fof larger sets there is no difference
ggplot(trump1corXY0, aes(x=x,y=y)) + geom_point() + theme_bw()
ggplot(trump1corXY0_filled, aes(x=x,y=y)) + geom_point() + theme_bw()

# Flower feels more heavily filled in
ggplot(flowercorXY0, aes(x=x,y=y)) + geom_point() + theme_bw()
ggplot(flowercorXY0_filled, aes(x=x,y=y)) + geom_point() + theme_bw()


################################################################
# Generates a set of new points that merge between the current image and the next
transformer <- function(xy1, xy2, n=100, method = "") {
  if (method == "sortX") {
    xy1 <- xy1[order(xy1[,1]),]
    xy2 <- xy2[order(xy1[,1]),]
  }
  if (method == "sortY") {
    xy1 <- xy1[order(xy1[,2]),]
    xy2 <- xy2[order(xy1[,2]),]
  }
  if (method == "sortXY") {
    xy1X <- (scale(xy1[,1])/3) %>% round(1) 
    xy1Y <- (scale(xy1[,2])/3) %>% round(1) 
    
    xy2X <- (scale(xy2[,1])/3) %>% round(1) 
    xy2Y <- (scale(xy2[,2])/3) %>% round(1) 
    
    xy1 <- xy1[order(xy1X, xy1Y),]
    xy2 <- xy2[order(xy2X, xy2Y),]
  }
  if (method == "closestXY") {
    xy2in <- cbind(xy2, flag=FALSE)
    xy2out <- xy2 * 0
    
    for (i in 1:nrow(xy1)) {
      xyi <- xy1[i,]
      xydist <- ((xy2in[,1] - xyi[1,1])^2 + (xy2in[,2] - xyi[1,2])^2)^.5 + xy2in[,3]*100
      xy2out[i,] <- xy2[xydist==min(xydist),][1,]
      xy2in[xydist==min(xydist),3] <- TRUE
    }
    
    xy2 <- xy2out
  }
  
  
  xyout <- list(xy1, xy1, xy1, xy1, xy1)
  xystep <- (xy2 - xy1)/n
  
  for (i in 1:n) {
    # Take i steps towards the new values
    xyNew <- xy1 + xystep * i
    
    # Tranform new values so that means and variances stay the same
    xyout[[i+5]] <- datagenerator(b1=0, xFUN = function(n) xyNew[,1], uFUN = function(x) xyNew[,2], varE=1)
    
  }
  
  xyout
}
#### End of transformer

# Generate a list 
xy1toxy2 <- transformer(flowercorXY0_filled, trump2corXY0_filled)

# Looks like a mix between the two images
ggplot(xy1toxy2[[50]], aes(x=x,y=y)) + geom_point() + theme_bw()

################################################################
# Now that we have all of the pieces let's start working on the animation.
 

setwd("z:/datasaurus")
dir.create("gifset")
setwd("z:/datasaurus/gifset")

library(stringr) # Use stringr function str_pad to make file names sequential

n <- 15 # Numpter of 
for (i in 1:length(objectlist)){
  xy1 <- get(paste0(objectlist[i],"corXY0_filled"))
  xy2 <- get(paste0(objectlist[ifelse(i<length(objectlist),i+1,1)],"corXY0_filled"))
  
  xy <- transformer(xy1, xy2, n, method="closestXY")
  
  for (j in 1:length(xy)){
    png(paste0(str_pad(i, 3, pad = "0"), str_pad(j, 3, pad = "0"),".png"))
    p <- ggplot(xy[[j]], aes(x=x,y=y)) + 
      xlab(NULL) + ylab(NULL) + geom_point(size=2) + 
      theme_void() + theme(legend.position="none")
    print(p)
    dev.off()
    print (paste(i,j))
  }
}

library(magick) # Use image magick package to merge pngs

pngs <- dir(pattern="*.png")

# The c() function combined with image magic objects works a little funky
# had to make sure the contents contained an image magic object (not c()).
images <- c(image_read(pngs[1]))
for (v in 2:length(pngs)) images <- c(images, image_read(pngs[v]))

# Merge the animation
animation <- image_animate(images, fps = 10, dispose = "previous")

setwd("z:/datasaurus/")
image_write(animation, "Transition3.gif")

################################################################
# Created the flower transition animation

setwd("z:/datasaurus")
dir.create("flower5")
setwd("z:/datasaurus/flower5")

k <- 0
n <- 15
bmax <- 13
bstep <- .3
i <- 0
bseq <- seq(-bmax,bmax,bstep)
bseq <- bseq^2 * sign(bseq)
for (b in bseq){
  i <- i + 1
  
  xy <- datagenerator(b1=b, meanx = 0, varX = 1, b0 = 0, corXY = .8,
         xFUN = function(n) flower[,1], uFUN = function(x) flower[,2])

  png(paste0(str_pad(i, 3, pad = "0"),".png"))
  
  
  
  title <- paste0("b:", round(b, 2), " mean(X):", round(mean(xy[,1]),2), 
                  " mean(Y):", round(mean(xy[,2]),2),
                  " var(X):", round(var(xy[,1]),2), " var(Y):", round(var(xy[,2]),2))
  
  p <- ggplot(xy, aes(x=x,y=y)) + ggtitle(title) +
      #xlab(NULL) + ylab(NULL) + 
      geom_point(size=2) + 
      geom_abline(slope=b, size=2, alpha=.5) +
      #geom_smooth(method='lm', se=FALSE) + # +theme_void() + theme(legend.position="none")  
      theme_bw() + scale_y_continuous(limits = c(-10, 10))
    print(p)
    dev.off()
    print (paste(b))
  
}

pngs <- dir(pattern="*.png")

images <- c(image_read(pngs[1]))
for (v in 2:length(pngs)) images <- c(images, image_read(pngs[v]))

animation <- image_animate(images, fps = 10, dispose = "previous")

setwd("z:/datasaurus/")
image_write(animation, "flower.gif")
