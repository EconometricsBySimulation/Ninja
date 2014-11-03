
# setwd('C:/Dropbox/Econometrics by Simulation/2014-11-November/')

circ.graph <- function(
  z,             # Input variable to be graphed
  zrescale=TRUE, # Automatically rescale z so that largest=1
  zmin   = 0,    # Any z less than z min will have text outside circles
  xmarg  = 0,    # Add margin to either side of graph
  ymarg  = 0,    # Add margin to top and bottom of graph
  ymax   = 0,    # This parameter if set to zero will cause graph
                 # not to fan out. Otherwise this sets the rate of
                 # fanning with more rapid fanning at values closer to zero
  tmarg  = .01,  # How much of a margin between text plotted and that located
                 # outside of the plot
  xwidth = 4,    # How wide will the plot be in which the x parameter can vary
  fcol   = 'darkred', # fill color
  border = NA,   # Border as in border used in polygon
  tcol   = 'white', # Text color inside
  tcol2  = 'black', # Text color outside
  labels  = NULL, # Circle labels
  g      = .5,   # Rate at which verticle gain is translated into horizontal movement
  gapR   = 1.1,  # Scaling factor when calculating gap size =1 with gapF=0 causes
                 # sequential circles to touch
  gapF   = .1,   # A fixed gap size to ensure a basic level of separation is 
                 # maintained between circlee
  tracer = FALSE,# A trace between circles
  circle.smooth = 200) # A smoothing parameter when constructing circles from polygons

  {
  if (zrescale) z <- z/max(z)
  # Use n to reference the length of the input vector
  n <- length(z)
  
  # Margin is what percent of the average diplacement should be
  # set between each circle.
  
  # Define a circle function
  circle <- function(x=0,y=0,rad=1, n=100, ...) {
    xyr <- cbind(x,y,rad)
    for (i in 1:nrow(xyr)) {
      z <- seq(0, 2*pi, length.out=n)
      polygon(xyr[i,1]+cos(z)*xyr[i,3], xyr[i,2]+sin(z)*xyr[i,3], ...)
    }
  }
  
  # Calculate the radius of each point
  r <- (z/pi)^.5
  
  # Calculate the displacement of each point
  d <- (r[-1]+r[-n])*gapR + gapF
  
  # Define a function that will incrementally add to the position
  # of each circle until the distance between each progressive circle is
  # at least d.
  f <- function(p, x0, y0, d) {
    x <- x0
    y <- y0
    repeat {
      p  <- p+min(d)/10^5
      y  <- y+abs(sin(p))
      if (ymax!=0) x  <- sin(y*g)*xwidth*y/ymax
      if (ymax==0) x  <- sin(y*g)*xwidth
      if ((x-x0)^2 + (y-y0)^2 > d^2) return(c(p,x,y))
    }
  }
  
  x <- 0
  y <- 0
  p <- 0
  
  for (i in 2:(length(z))) {
    fset <- f(p, x[i-1],y[i-1], d[i-1])
    p    <- fset[1] 
    x[i] <- fset[2]
    y[i] <- fset[3]
  }
  
  # Remove the plot margins
  par(mar=c(0,0,0,0))
  
  # Plot empty values to prepare the plot for 
  plot(c(min(x)-xmarg, max(x)+xmarg),c(min(y)-ymarg, max(y)+ymarg), type='n')
  
  # Draw the trace if defined
  if (tracer) lines(x,y, type='l')
  
  # Draw the circles
  circle(x,y,r, col=fcol, border=border, n=circle.smooth)
  
  # If labels are defined, apply them
  if (length(labels)==length(x)) {
    text(x[z>=zmin],y[z>=zmin],labels[z>=zmin], col=tcol)
    
    if (length(labels[z<zmin])>0) 
      text(x[z<zmin]+r[z<zmin]+tmarg,y[z<zmin] ,labels[z<zmin], col=tcol2, adj=0)
  }

} # End circ.graph

# Looking at AIDS data from WHO:
aids <- read.csv("2014-11-01-HIV.csv", stringsAsFactors=FALSE)
# find on github ()
aids[,4:ncol(aids)] <- sapply(aids[,4:ncol(aids)], as.numeric)

png(filename = "2014-11-circle%03d.png", width=800, height=2400)

circ.graph((1:26)^6, labels=format((1:26)^6, scientific=FALSE), 
           ymax=15, g=.8, gapF=.1, gapR=1, zmin=.2,
           xmarg=.3, border=rgb(.5,0,0))

circ.graph((1:20)^3, ymax=15, g=.8, gapF=.1, gapR=1)

globalaids <- subset(aids, Place=="Global")
circ.graph(globalaids$HIV_adults, 
           label = paste0(globalaids$Year,"\n", globalaids$HIV_adults/10^6),
           ymax=15, g=.5, gapF=.1, gapR=1)


dev.off()
