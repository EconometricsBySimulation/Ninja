# MLM

# This is a simulation of multilevel marketing business structure.
# It accompanies the blog post: http://www.econometricsbysimulation.com/2015/10/MLM.html

rm(list=ls())
# First off, let's assume you have a fixed population. I know this might be a big assumption
# since many MLM leaders seem to suggest that you will be able to recruit others indefintely
# into the future as the population continues to grow.

# But for now, fixed population.
Population <- 10^5 # Number of people in the world

# Let's say there is a fixed sales rate for any person encountered per period but that once
# approached people will not consider another offer this period.
RateSales <- .2  # Sales rate

# Identical with recruitment except that once recruited people stay in the system until
# dropping out.
RateRecruit <- .10

# Let's assume that each individual can only attempt so many sales per month.
SaleAttempt <- 30

# Let's assume that attempting to sell or recruit others costs effort.
SaleCost <- 1

# Let's assume there is some kind of ongoing costs of participating in the system.
# This might be membership fees, mandatory product purchasement etc.
Fee <- 10

# Let's assume that the each item sold yeilds a certain amount of revenue.
SaleProfit <- 10    # Revenue from sales

# Let's finally assume that anybody that if you recruit someone you get a commission on
# their sales and their fee.
Commission <- .25

# Replacement Rate, this is the rate at which the population is either
# replaced or forgets about the MLM scheme.
ReplaceRate <- .00

# Let's finally assume that there is a certain amount of losses a member will suffer in a period
# before that person drops out of the system. After someone drops out of the system
# they are immune to sales and to future recruitment efforts.
DropOut <- -40

# In order to simulate this we will simply loop over a series of time periods. 
TimeFrame <- 50

# In each period each IBO will attempt to make sales and recruit individuals.
# If that person makes a sale then that person will earn the sales value and
# anybody upstream of that person will earn that value as well.

# Generate flags for who is an IBO in the population. Initially the only person is the
# MLM business model initiator.
IBO <- c(TRUE,rep(FALSE,Population-1))

# Generate a vector which lists the person who recruited you.
recruitedBy <- rep(0, Population)

# Round enter
roundEnter <- c(1,rep(0, Population-1))


nIBO <- aveProfit <- maxProfit <- minProfit <- 
  medProfit <- pImmune <- PProfit <-  Pdropout <- 
  NULL

aveProfitLevel <- minProfitLevel <- maxProfitLevel<- 
  medProfitLevel <- nIBOLevel <- sumProfitLevel <- as.list(rep(0, TimeFrame))

immune <- rep(FALSE,Population)

for (t in 1:TimeFrame) {
  HighestLevel <- max(roundEnter)
  print (paste("Round:",t, " # of IBOs:", sum(IBO), "Levels:", HighestLevel))
  if(sum(IBO)==0) break
  t <- t+1
  # IBOs cannot be recruited or sold to:
  immune[IBO] <- TRUE
  
  # Replace members of the population (who are not IBOs)
  immune[rbinom(Population,1,ReplaceRate) & !IBO] <- FALSE
  
  # Create a vector of those you can sell to this period and those that could be recruited
  # this period.
  susceptible   <- !immune

  # Create a vector of profits this period. If you are in the system you have membership fees.
  # Everybody else has zero profit.
  profits <- -Fee*IBO
  
  # Clear the new IBO list for new recruits
  newIBO <- NULL
  
  # It time to get busy! In order to simulate this we will loop through each sales person.
  # So as not to favor anybody in the list of salespeople I will loop through a random 
  # ordering of them each period.
  for (i in sample((1:Population)[IBO])) {
    
    # Attempting to sell costs resources
    profits[i] <- profits[i]-SaleAttempt*SaleCost

    # IF this is not an IBO skip this person.
    if (!IBO[i] | all(!susceptible)) next
    
    # Each period each person will attempt the maximum number of sales.
    targets <- sample(Population, SaleAttempt)
    
    # Only susceptible targets can be targetted
    targets <- targets[susceptible[targets]]
    
    # If there are no targets remaining skip to next
    if (length(targets)==0) next
    
    
    # In order to succeed with sales we have to get our pitch right (RateSales)
    # and be talking with someone who is succeptible to sales attempts.
    sucsale <- 
      targets[rbinom(length(targets),1,RateSales)==1]
  
    # So we have succeeded at sales. Now let's distribute profits
    for (ii in sucsale) {
      # First you get your cut
      # And the person who recruited you get's his/her cut.
      top <- FALSE
      pos <- i # Who is going to get paid
      deep <- 0 # This is however many layers of recruitment exist
      
      while (top == FALSE) {
        # IBO's get their commission
        profits[pos] <- profits[pos]+SaleProfit*(Commission^deep)
        
        # If there is nobody above, stop digging
        if (recruitedBy[pos]==0) top <- TRUE
        
        # If there is somebody above, set that person as the next
        # in line to get a commission of a commission.
        if (recruitedBy[pos]!=0) pos <- recruitedBy[pos]
        
        # Mark another level of depth in the pyramid
        deep <- deep+1
        
        if (deep>1000) stop("Too DEEP")
        
      }
    }
    
  
    # Check for those successfuly recruited.
    sucRecruit <- targets[rbinom(length(targets),1,RateRecruit)==1]
    
    # Anybody who is recruited now lists the recruiter as the person above them
    recruitedBy[sucRecruit] <- i
    
    # Record period of recruitment
    roundEnter[sucRecruit] <- HighestLevel+1
    
    # Mark those who attempted to be sold to as no longer susceptible this period
    susceptible[targets] <- FALSE
    
    # Add to new IBO list
    newIBO <- c(newIBO, sucRecruit)
  
  }
  
  # Calculate summary stats for current period IBOs
  nIBO <- c(nIBO, sum(IBO)) # Count IBOs
  aveProfit <- c(aveProfit, mean(profits[IBO]))
  maxProfit <- c(maxProfit, max(profits[IBO]))
  minProfit <- c(minProfit, min(profits[IBO]))
  medProfit <- c(medProfit, median(profits[IBO]))
  pImmune <- c(pImmune, mean(immune))
  PProfit <- c(PProfit, mean(profits[IBO]>0))
  
  for (i in 1:HighestLevel) {
    aveProfitLevel[[i]] <- c(aveProfitLevel[[i]], mean(profits[roundEnter==i & IBO]))
    minProfitLevel[[i]] <- c(minProfitLevel[[i]], min(profits[roundEnter==i & IBO]))
    maxProfitLevel[[i]] <- c(maxProfitLevel[[i]], max(profits[roundEnter==i & IBO]))
    medProfitLevel[[i]] <- c(medProfitLevel[[i]], median(profits[roundEnter==i & IBO]))
    sumProfitLevel[[i]] <- c(sumProfitLevel[[i]], sum(profits[roundEnter==i & IBO]))
    nIBOLevel[[i]]      <- c(nIBOLevel[[i]],      sum(roundEnter==i & IBO))
  }
  # mrecruitedBy[,t] <- recruitedBy
    
  # End of period conver to to IBO
  IBO[newIBO] <- TRUE
  
  dropouts <- (1:Population)[profits <= DropOut & IBO]
  
  Pdropout <- c(Pdropout, length(dropouts)/sum(IBO))
  
  
  for (i in dropouts)
    recruitedBy[recruitedBy==i] <- recruitedBy[i] 
  
  # End of period, check for IBO drop outs
  IBO[dropouts]        <- FALSE
  roundEnter[dropouts] <- 0
  
  # If you drop out then you are no longer a beneficiary of commissions
  recruitedBy[dropouts] <- 0
  
}
###########################################################
# END SIMULATION 
###########################################################

levelLast <- sum(sapply(nIBOLevel, length)>1)
levelSet  <- 1:levelLast

aveProfitLevel <- aveProfitLevel[1:levelLast]
sumProfitLevel <- sumProfitLevel[1:levelLast]


data.frame(nIBO, aveProfit, medProfit, maxProfit, minProfit, pImmune, PProfit)

plot(c(0,levelLast),c(0,max(unlist(nIBOLevel))), type='n')
for (i in levelSet) lines(i+1:(length(nIBOLevel[[i]][-1])), nIBOLevel[[i]][-1])

AggIBOs <- NULL
for (i in levelSet)
  AggIBOs <- 
    rbind(AggIBOs, cbind(level=i, 
          t=i+1:(length(nIBOLevel[[i]][-1])),
          n=nIBOLevel[[i]][-1],
          aveP=aveProfitLevel[[i]][-1],
          maxP=maxProfitLevel[[i]][-1],
          minP=minProfitLevel[[i]][-1],
          medP=medProfitLevel[[i]][-1],
          sumP=sumProfitLevel[[i]][-1]
    ))
###########################################################
# END SIMULATION 
###########################################################

AggIBOs <- as.data.frame(AggIBOs)

library(ggplot2)
library(plyr)

label.pos <-ddply(AggIBOs, .(level), summarize, 
                  t=head(t,1), 
                  n=head(n,1), 
                  aveP=head(aveP,1), 
                  maxP=head(maxP,1),
                  minP=head(minP,1),
                  medP=head(medP,1),
                  sumP=head(sumP,1))

setwd("C:/Users/fsmar/Dropbox/Econometrics by Simulation/2015-10-October/MLM1")

head <- paste0('ReplacementRate',ReplaceRate*100,"Pcnt")
i <- 0

i <- i+1; png(paste0(head, i, '.png'), width=1200, height=800)
ggplot(AggIBOs, aes(x=t,y=n,group=level, color=level, label=level)) + 
  geom_line(size=1.5,alpha=.65) + 
  geom_text(data=label.pos, aes(x=t,y=n,fontface=1.5), vjust=-1) +
  geom_point(data=label.pos, aes(x=t,y=n)) +
  ggtitle(paste0("# of IBOs at Each Level in Each Period (", head, ")")) +
  xlab("Period") +
  ylab("# of IBOs") 
dev.off()

i <- i+1; png(paste0(head, i, '.png'), width=1200, height=800)
ggplot(AggIBOs, aes(x=t,y=aveP,group=level, color=level, label=level)) + 
  geom_line(size=1.5,alpha=.65) + 
  geom_text(data=label.pos, aes(x=t,y=aveP,fontface=2), vjust=1.5) +
  geom_point(data=label.pos, aes(x=t,y=aveP)) +
  ggtitle(paste0("Average Profitability For Each Level Each Period (", head, ")")) +
  xlab("Period") +
  ylab("Average Profit")
dev.off()

i <- i+1; png(paste0(head, i, '.png'), width=1200, height=800)
ggplot(AggIBOs, aes(x=t,y=maxP,group=level, color=level, label=level)) + 
  geom_line(size=1.5,alpha=.65) + 
  geom_text(data=label.pos, aes(x=t,y=maxP,fontface=2), vjust=1.5) +
  geom_point(data=label.pos, aes(x=t,y=maxP)) +
  ggtitle(paste0("Max Profitability For Each Level Each Period (", head, ")")) +
  xlab("Period") +
  ylab("Max Profit")
dev.off()

i <- i+1; png(paste0(head, i, '.png'), width=1200, height=800)
ggplot(AggIBOs, aes(x=t,y=medP,group=level, color=level, label=level)) + 
  geom_line(size=1.5,alpha=.65) + 
  geom_text(data=label.pos, aes(x=t,y=medP,fontface=2), vjust=1.5) +
  geom_point(data=label.pos, aes(x=t,y=medP)) +
  ggtitle(paste0("Median Profitability For Each Level Each Period (", head, ")")) +
  xlab("Period") +
  ylab("Median Profit")
dev.off()

i <- i+1; png(paste0(head, i, '.png'), width=1200, height=800)
ggplot(AggIBOs, aes(x=t,y=sumP,group=level, color=level, label=level)) + 
  geom_line(size=1.5,alpha=.65) + 
  geom_text(data=label.pos, aes(x=t,y=sumP,fontface=2), vjust=-.5, hjust=1) +
  geom_point(data=label.pos, aes(x=t,y=sumP)) +
  ggtitle(paste0("Total Profitability For Each Level Each Period (", head, ")")) +
  xlab("Period") +
  ylab("Total Profit")
dev.off()
