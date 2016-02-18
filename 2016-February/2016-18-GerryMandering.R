# How to Win An Election Without Winning the Popular Vote: Gerrymandering

#Need to install development version of ggplot2
# devtools::install_github("hadley/ggplot2")

library(data.table)
library(ggplot2)
library(dplyr)

res <- data.table()

# Imagine we have a simple state with 
nx <- 20
ny <- 20

# Number of voting districts
ndist <- 10


# How voters lean
lean <- (j*.01-.5)/2

sim <- 400

N <- nx*ny


# Popularity

# Popular Vote
pop <- rep(NA,sim)
for (i in 1:sim)  {
  state <- 
    data.table(propensity=runif(N),
               x=1:nx, y=rep(1:ny, each=nx))
  pop[i] <- mean(state$propensity > runif(N)+lean)
}

pop %>% mean

state$propensity <- runif(N)
state$rand <- runif(N)

state <- state %>% arrange(propensity)

# At the top of the list are those least likely to vote for you.
# At the bottom of the list are those most likely to vote for you.

# Let's say we can group those least likely to vote for us 
# into the first two districts.
# The remaining voters we will randomize into districts.
state$propensity <- runif(N)


setwd('C:/Users/fsmar/Dropbox/Econometrics by Simulation/2016-02-February')

gold <- (1+5^.5)/2

png('2016-18-GerryManderingD0.png', width=1200, height=1050)
state %>% 
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill = propensity, width=1, height=1), size=1.25)+
  theme_bw(base_size = 22) + xlab(NULL) +
  ggtitle('No Gerrymandering') +
  geom_hline(yintercept=seq(2.5,18.5,2), color='white') +
  geom_text(data=data.table(x=1,y=seq(1,19,2),label=paste("District:",1:10)),
            aes(label=label), size=10, color='white', hjust=0)+
  theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())
dev.off()

# Now let's Gerrymander 2 districts
state <- state %>% arrange(propensity)

state$district2 <- 
  c(rep(1:2, each=N/ndist), 
    sample(rep((3):ndist, each=N/ndist), N-(2*N/ndist))) %>% as.factor()

# Now let's mix up the remaining voters within districts
state <- state %>% arrange(district2,rand)

state$x=1:nx
state$y=rep(1:ny, each=nx)

png('2016-18-GerryManderingD2.png', width=1200, height=1050)
state %>% 
  ggplot(aes(x, y)) + 
  geom_tile(aes(fill = propensity, width=1, height=1), size=1.25)+
  theme_bw(base_size = 22) + xlab(NULL) +
  ggtitle('Dristricts 1 and 2 Gerrymandered') +
  geom_hline(yintercept=seq(2.5,18.5,2), color='white') +
  geom_text(data=data.table(x=1,y=seq(1,19,2),label=paste("District:",1:10)),
            aes(label=label), size=10, color='white', hjust=0)+
  theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())
dev.off()

# Simulate a batch of results
for (g in 1:5)   {
  
  # Number of districts Gerrymandered
  gerry <- g
  

  for (j in 1:100) {
    print(paste('g:',g,"  j:", j))
    
    # Popular Vote
    pop <- rep(NA,sim)
    for (i in 1:sim)  {
      state <- 
        data.table(propensity=runif(N),
                   x=1:nx, y=rep(1:ny, each=nx))
      pop[i] <- mean(state$propensity > runif(N)+lean)
    }
        

    gerrymandering <- rep(0,sim)
    for (i in 1:sim){
      # Cast votes
      state$propensity <- runif(N)
      
      state <- state %>% arrange(propensity)
      
      state$district2 <- 
        c(rep(1:gerry, each=N/ndist), 
          sample(rep((gerry+1):ndist, each=N/ndist), N-(gerry*N/ndist))) %>% as.factor()
      
      # Calculate district results
      gerrymandering[i] <- state %>% group_by(district2) %>% 
        dplyr::summarise(mean=as.numeric(mean(propensity>runif(N)+lean)>.5)) %>% 
        `[`(,mean) %>% mean()
      
      # Calculate district results
    }
    gerrymandering %>% mean
    
    # 
    mpop            <- (pop>.5) %>% mean
    mgerrymandering <- (gerrymandering>.5) %>% mean
  
    res <- data.table(j, g, lean, mpop, mgerrymandering) %>%
      rbind(res, ., fill=TRUE)
  }
}


# Make mpop the popular vote
res2 <- rbind(res[,.(g,lean,dif,difmin,MG=mgerrymandering)],
              data.table(g=0,res[g==1,.(lean,dif,difmin)], MG=res[g==1,mpop]),
              fill=TRUE)

res2[,dif    := abs(MG-.5)]
res2[,difmin := min(dif), by=g]

# Figure 1
png('2016-18-GerryMandering.png', width=1200, height=1200/gold)

res2 %>%
  ggplot(aes(x=lean*100, y=MG*100, color=as.factor(g), group=g)) +
  geom_hline(yintercept=50, size=1.5, alpha=.5, linetype =2)+
  geom_line(size=2) + 
  xlab('How many points voters favor your opponent above you') + 
  ylab('Likelihood of Winning') +
  theme_bw(base_size = 22) + 
  theme(legend.position="top")+
  geom_text(data=res2[dif==difmin, ],
            aes(label=(lean*100) %>% round,
                   x = lean*100-.75, y = 50 + 2), 
            size=10, hjust=1, color='black')+
  guides(color=guide_legend(title="# of Districts Gerrymandered", 
                          direction = "horizontal",keywidth = 4, keyheight = 1,
                          title.position = "top",
                          title.theme = element_text(size=25, angle = 0),
                          title.hjust=.5))
  
dev.off()
