# Monster Mash - Spatial Multi-Agent Simulation

# This simualtion demonstrates how to build a simple spatial multi-agent simulations using R.

# There is a 20 x 20 grid in which the agents occupy.  If they walk off one edge they end up on the other side.

gridx = 20
gridy = 20

# Let's first specify the initial number of agents.
n.Bob = 4
n.Frank = 1
n.Dracula = 1
n.Hunter = 2

# Specify number of moves before the monsters start fighting and expanding
n.moves = 10

# Now we will create a movement matrix for each monster.
t=0
for(v in c("Bob","Frank","Dracula","Hunter")){
t=t+1
# Zombie is type 1, Frankenstien's Monster is type 2, Vampires is type 3, and Hunter type 4.

for(i in 1:get(paste("n.","Bob",sep=""))) {
  # Specify initial positions
  x = ceiling(gridx*runif(1))
  y = ceiling(gridy*runif(1))
  type = t
  
  # Create a vector for each monster in the simulation.
  assign(paste(v,i,sep=""), t(as.matrix(c(x=x,y=y,t=type))))
}
}
Bob1
Frank1


# Due to number of files separate objects it is not very easy to plot them.
plot(0,0,xlim=c(1,gridx),ylim=c(1,gridy), xlab="X", ylab="Y", main="Zombie-Grey, Frankenstien-Green, Vamp-Red, Hunter-Purple")

# Let's speficy the color of each monster type and hunter.
# Remember Zombie is 1, Frankenstien's 2, Vampires 3, and Hunter 4.
mon.col = c("gray", "green", "red", "purple")

for(v in c("Bob","Frank","Dracula","Hunter")){
for(i in 1:get(paste("n.","Bob",sep=""))) {
  handle = get(paste(v,i,sep=""))
  handle.l=handle[nrow(handle),]
  points(handle.l[1],handle.l[2], col=mon.col[handle.l[3]], cex=3, pch=19)
}
}

# Speficy a minmax function that keeps the monsters in the grid
minmax <- function(x,xmin,xmax) max(min(x,xmax),xmin)

for(ii in 1:n.moves) {
for(v in c("Bob","Frank","Dracula","Hunter")){
for(i in 1:get(paste("n.","Bob",sep=""))) {
  handle = get(paste(v,i,sep=""))
  handle.l=handle[nrow(handle),]
  final = rbind(handle,c(minmax(handle.l[1]+(-1)^rbinom(1,1,.5), 1, gridx),
      minmax(handle.l[2]+(-1)^rbinom(1,1,.5), 1, gridy), handle.l[3]))
  
  assign(paste(v,i,sep=""),final)
}
}
}

Bob1

plot(0,0,xlim=c(1,gridx),ylim=c(1,gridy), xlab="X", ylab="Y", main=c("Monster Movement", "Zombie-Grey, Frankenstien-Green, Vamp-Red, Hunter-Purple"))

# Let's speficy the color of each monster type and hunter.
# Remember Zombie is 1, Frankenstien's 2, Vampires 3, and Hunter 4.
mon.col = c("gray", "green", "red", "purple")

for(v in c("Bob","Frank","Dracula","Hunter")){
for(i in 1:get(paste("n.","Bob",sep=""))) {
  handle = get(paste(v,i,sep=""))
  turn <- nrow(handle)
  handle.l=handle[turn,]
  points(handle[1,1],handle[1,2], col=mon.col[handle.l[3]], cex=2, pch=19)
   arrows(x1=handle[-1,1], y1=handle[-1,2],
         x0=handle[-turn,1],y0=handle[-turn,2], 
         col=mon.col[handle.l[3]],pch=19, length=.1)  
  points(handle.l[1],handle.l[2], col=mon.col[handle.l[3]], cex=3, pch=19)
}
}

# The agents do not interact with each other so this simulation really is not that interesting.
