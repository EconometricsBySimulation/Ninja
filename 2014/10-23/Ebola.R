setWindowTitle(getwd())
### Francis Smart & Mario Cortina Borja, 20.10.14
# This code is used to generate the Ebola outbreak predictions
# seen in forcoming article published on Significance Magazine/Econometrics by Simulation


require('XML'); require('reshape2'); require('ggplot2'); require('magrittr')
library(forecast)
library(labeling)
library(splines)
library(randtests) ### for runs.test
library(gtable)
library(grid)

library(plyr); library(scales)

# Find the table at https://github.com/EconometricsBySimulation/Ninja/2014/20-23/Ebola.csv
ebola <- read.csv("2014-10-22-Ebola.csv", stringsAsFactors=FALSE)

ebola$Date <- as.Date(ebola$Date, format="%m/%d/%Y")

# Convert data from wide to long for analysis
ebola.long <- melt(ebola, id.vars=c('Date','Day'))

# Keep only entries that have values
ebola.long <- ebola.long[!is.na(ebola.long$value),]

# Create a variable for country
ebola.long$Country <- ''
ebola.long$Country[grepl("T",ebola.long$variable)]   <- "Total"
ebola.long$Country[grepl("G",ebola.long$variable)]   <- "Guinea"
ebola.long$Country[grepl("Lib",ebola.long$variable)] <- "Liberia"
ebola.long$Country[grepl("SL",ebola.long$variable)]  <- "Sierra Leone"
ebola.long$Country[grepl("Nig",ebola.long$variable)] <- "Nigeria"
ebola.long$Country[grepl("Sen",ebola.long$variable)] <- "Senegal"
ebola.long$Country[grepl("US",ebola.long$variable)]  <- "United States"
ebola.long$Country[grepl("Sp",ebola.long$variable)]  <- "Spain"

# Create and indicator for the three most afflicted countries
ebola.long$Triad <- FALSE
ebola.long$Triad[grepl("G",ebola.long$variable)]   <- TRUE
ebola.long$Triad[grepl("Lib",ebola.long$variable)] <- TRUE
ebola.long$Triad[grepl("SL",ebola.long$variable)]  <- TRUE

# Create an indicator of type 'Cases' or 'Deaths'
ebola.long$type <- 'Cases'
ebola.long$type[grepl("Deaths",ebola.long$variable)]   <- "Death"

#### Table 1 ################################################################

with(ebola.long[ebola.long$Day==312,], tapply( value, list(type,Country), sum))


#      Guinea Liberia Nigeria Senegal Sierra Leone Spain Total United States
#Cases   1519    4262      20       1         3410     1  9216             3
#Death    862    2484       8       0         1200     0  4555             1
 


##### Figure 1 ################################################################

# Plot and save the deaths within three countries
#png(filename='2014-10-17TriadDeaths.png', width=800, height=500)
ggplot(subset(ebola.long, type=="Death"&Day>120&Triad), 
       aes(y=value, x=Date, 
           group=Country, color=Country, shape=Country)) + 
  geom_line(size=2)+
  geom_point(size=4, alpha=1)+ 
  ## no smoothing as we're presenting data as such
  scale_x_date(name="Month, 2014") +
  scale_y_continuous(name="Number of Deaths", limits=c(-10,2500)) +
  ggtitle(' ') +
  theme_bw()+
  theme(legend.position=c(0.1,0.9))+
  scale_colour_manual(values=c("#810f7c", "#8856a7", "#8c96c6"))
#dev.off()

##### Figure 2 ################################################################


# Plot and save the cases within three countries
#png(filename='2014-10-17TriadCases.png', width=800, height=500)
ggplot(subset(ebola.long, type=="Cases"&Day>120&Triad), 
       aes(y=value, x=Date, 
           group=Country, color=Country, shape=Country)) + 
  geom_line(size=2)+
  geom_point(size=4, alpha=1)+ 
  ## no smoothing as we're presenting data as such
  scale_x_date(name="Month, 2014") +
  scale_y_continuous(name="Number of Cases", limits=c(-30,4500)) +
  ggtitle(' ') +
  theme_bw()+
  theme(legend.position=c(0.1,0.9))+
  scale_colour_manual(values=c("#810f7c", "#8856a7", "#8c96c6"))
#dev.off()


##### Figure 3 ################################################################


#png(filename='2014-10-17LogTotals.png', width=800, height=500)
p1<- ggplot(subset(ebola.long, Country=="Total"&Day>120), 
       aes(y=value, x=Date, 
           group=type, color=type, shape=type)) + 
  geom_line(size=2)+
  geom_point(size=4, alpha=.6)+
  scale_x_date(name="Month, 2014") +
  scale_y_continuous(name="Total cases and deaths in Guinea, Liberia and Sierra Leone",
  breaks=c(250, 500,seq(1000,9000,by=1000))) +
  ggtitle(' ') +
  coord_trans(ytrans="log") + 
  theme_bw() +
  theme(legend.position=c(0.1,0.9))+
  scale_colour_manual(values=c("#FF0000", "#000000"))
#dev.off()
p1

fig3<- ebola.long[ ebola.long$Country=='Total' & ebola.long$Day>120,]
fig3$Ratio<- c(fig3$value[71:140]/fig3$value[1:70], rep(NA,70))
### only first 70 values have mortality rate

##### Figure 4 ##############################################################

p2<-  ggplot(subset(fig3, type=='Cases'), aes(y=Ratio, x=Date, color=3)) + 
	   geom_line(size=2) + theme_bw() +
	    theme(legend.position = "none")+
	   theme(panel.background = element_rect(fill = NA)) +
	   scale_x_date(name="Month, 2014") +
	   scale_y_continuous(name="Mortality rate", labels=percent) + 
	   ggtitle(' ')
	   
p2




##### Forecasting ###############################################################
# Let's use only the data since June or so
deathlong <- subset(ebola.long, variable=='TDeaths'&Day>180)

# Open empty data set for deaths
ts.deaths <- rep(NA, max(deathlong$Day)+1)

# Fill in known deaths values
ts.deaths[deathlong$Day+1] <- deathlong$value
ts.deaths<- ts(ts.deaths, start=1,frequency=1,deltat=1)

# Fit the time series to an arima

fit<- auto.arima(ts.deaths[51:length(ts.deaths)])
summary(fit) ## arima (0,2,0) is enough!



### model selection

arimas.aic<- arimas.bic<- rep(NA,48)
k0<-0
for ( i in 0:3) for (j in 0:2) for (k in 0:3)
{
k0<-k0+1
if(k0 !=19 &  k0!=27 & k0!=31 & k0!=40) #can't fit these models
{
fit.aux<- Arima(ts.deaths[51:length( ts.deaths)], 
order=c(i,j,k))
print(paste(letters[9:11],c(i,j,k),sep=' = ',collapse='   '),quote=FALSE)
print(fit.aux)
print(paste(rep('-',60),collapse=''),quote=FALSE)
arimas.bic[k0]<- fit.aux$bic
arimas.aic[k0]<- fit.aux$aic
}
names(arimas.bic)[k0]<- paste(letters[9:11],c(i,j,k),sep='',collapse='')
names(arimas.aic)[k0]<- paste(letters[9:11],c(i,j,k),sep='',collapse='')
}

arimas.bic<- sort(arimas.bic)
arimas.aic<- sort(arimas.aic)

k0 ##OK
### optimal with BIC = arima(0,2,1)
### optimal with AIC = arima(3,2,1)


### try this 
#fit<- Arima(ts.deaths[51:length( ts.deaths)], order=c(1,2,1)) #a good fit

###optimal 2,2,0
fit<- Arima(ts.deaths[51:length( ts.deaths)], 
#order=c(0,2,1))  ## optimal BIC ##max 12500, min=5000
order=c(2,2,1))  #### optimal AIC, much better model
#order=c(2,2,2)) ###3rd opt aic, some std errors NA's
#order=c(3,2,2)) 

fit


# Now we select the time period to project into
projection <- 60

# Select the days to project into
last.day <- max(deathlong$Day)

# Forcast into the future
fc<-forecast(fit, h=projection, level=95)

# Create a matrix to how the projection values
qf<- matrix(0, nrow=22, ncol=projection) ###

# Change the rownames to different percentiles
rownames(qf) <-c(seq(5,95,5), 99, 99.5, 99.9)

# m is the mean
m<-fc$mean

# Calculate the standard error
s <- (fc$upper-fc$lower)/1.96/2

# For each point in the projection, calculate value
for(h in 1:projection) 
  qf[,h] <- qnorm(c(seq(5,95,5),99,99.5, 99.9)/100, m[h], s[h])

# Turn the projection into long
d.projct <- melt(qf)

# Rename columns
names(d.projct) <- c('percentile', 'day', 'value')

# 
d.projct$type <- 'projection'
d.projct$day <- d.projct$day+last.day

observed.deathlong <- with(deathlong,
                           data.frame(percentile=50, 
                                      day=Day,
                                      value=value,
                                      type='observed'))

# Bind the observed with the projected
death.proj <- rbind(observed.deathlong, d.projct)

# Constuct the alpha based on how far each value is from 50%
death.proj$alpha <- 50 - abs(50-death.proj$percentile)


test<- as.Date(min(ebola.long$Date), format='%m/%d/%Y')
### NB I've changed ebola$Date to ebola.long$Date
test<- test+ death.proj$day
death.proj$date <- test

################ Figure 5

#png(filename='2014-10-17LogTotalDeathProj.png', width=800, height=500)
ggplot(death.proj, aes(y=value, x=date, 
                   color=percentile, alpha = alpha))+
  geom_line(size=2, alpha=0.6)+
  geom_point(size=2) + theme_bw()+
   theme(legend.position = "none") +
  scale_x_date(name="Month, 2014") +
  scale_y_continuous(name="Total cumulative Deaths in Guinea, Liberia and Sierra Leone",
         breaks=seq(0,14000,by=2000)) +
  ggtitle(' ') +
  scale_alpha(guide = 'none')+
  geom_abline(intercept = 20000)+
  geom_line(aes(y=value, x=date), size=2,
            subset = .(percentile==50&day>last.day), 
			color='black', alpha=.5) 
			

			
#dev.off()


#### Table 2 ##############################
table2<- death.proj[
is.element(death.proj$percentile, c(5,25,50,75,95,99,99.5,99.9)) & death.proj$type=='projection'
    & is.element(death.proj$day,c(seq(313,369,by=7), 372)), ]

labs.table2a<- as.character(table2$date[table2$percentile==1] )
	
table2a<-round(matrix(table2$value, ncol=8, byrow=TRUE))
dimnames(table2a)<- list( labs.table2a, paste('Q',c(5,25,50,75,95,99, 99.5, 99.9),"%",sep=''))

 apply(table2a, 2, function(x){median(diff(x))})



#####################################################
