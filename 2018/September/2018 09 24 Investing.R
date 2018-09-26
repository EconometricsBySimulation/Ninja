# 2018 Federal Tax Rates

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

setwd("Z:/Dropbox/Econometrics by Simulation/2018_09_September/")

taxrateIndividual <- data.frame(bracket=1:7, 
      rate      = c(.1  ,.12   ,.22    ,.24     ,.32     ,.35     ,.37      ),
      incomemin = c(0   , 9525 ,38701  ,82501   ,157501  ,200001  ,500001   ),
      incomemax = c(9526,38700 ,82500  ,157500  ,200000  ,500000  , 10^9    ),
      base      = c(0   , 952.5, 4453.5, 14089.5, 32089.5, 45689.5, 150689.5))

taxrateJoint <- data.frame(bracket=1:7, 
      rate      = c(.1   ,.12   ,.22    ,.24     ,.32     ,.35     ,.37      ),
      incomemin = c(0    ,19051 ,77401  ,165001  ,315001  ,400001  ,600001   ),
      incomemax = c(19050,77400 ,165000 ,315000  ,400000  ,600000  , 10^9    ),
      base      = c(0    ,  1905,  8907 , 28179  , 64179  , 91379  ,161379   ))

incometax <- function(income, taxrate) {
  taxslice <- taxrate[taxrate$incomemin <= income & taxrate$incomemax >= income,]
  ((income-taxslice$incomemin)*taxslice$rate + taxslice$base)[1]
}

IncomeNTax <- data.frame(income = c(0,(1:2000)*500))
IncomeNTax <- IncomeNTax %>% mutate(IndividualTax = sapply(income, incometax, taxrateIndividual))
IncomeNTax <- IncomeNTax %>% mutate(JointTax = sapply(income, incometax, taxrateJoint))

IncomeNTax$SS <- 0
IncomeNTax$SS[IncomeNTax$income <= 128000] <- 
  IncomeNTax$income[IncomeNTax$income <= 128000]*.062

IncomeNTax$SS[IncomeNTax$income > 128000] <- 128000*.062

IncomeNTax$Med <- IncomeNTax$income*.029

IncomeNTax <- IncomeNTax %>% mutate(
  Indrate = IndividualTax / income, Jointrate = JointTax / income,
  IndrateSSM = (IndividualTax+SS+Med) / income, JointrateSSM = (JointTax+SS+Med) / income,
           r10=.1*income,  r12=.12*income, r22=.22*income, 
           r24=.24*income, r32=.32*income, r35=.35*income, r37=.37*income)

IncomeNTax[1,] <- 0
IncomeNTax %>% head

# Generate rates Long Version
ratesLong = IncomeNTax[,c(1,6:9)] %>% gather(income,rate = Indrate:JointrateSSM)

colnames(ratesLong) <- c("Income", "Type", "Rate")

vsegXY <- data.frame(
  x = c(12140, 32140, 16460, 59039, 102500, 160000, 250000),
  line = c("Poverty Line","Median Income (50%)","Household Poverty Line (12.5%)",
           "Household Median (50%)", "Household (75%)", "Household (90%)", "Household (97%)"),
  type = c("Individual", 
           "Individual",
           "Joint", "Joint", "Joint", "Joint", "Joint"), stringsAsFactors=FALSE)

ratesLong[ratesLong$Type == "Indrate"  ,2] <- "Individual"
ratesLong[ratesLong$Type == "Jointrate",2] <- "Joint"

ratesLong$line = "Effective Tax Rate"

options(scipen=10000)

ratesLong$SSM = grepl("SSM",ratesLong$Type)
ratesLong$Type[ratesLong$Type == "IndrateSSM"]   = "Individual"
ratesLong$Type[ratesLong$Type == "JointrateSSM"] = "Joint"


png(file="TaxRateByIncome.png",width=1000,height=800,res=72)
ratesLong %>% filter(Income<500000) %>% filter(!SSM) %>% ggplot(aes(x=Income, y=Rate)) + geom_line(aes(color=Type), size=1.2) + theme_bw() +
  geom_segment(aes(x=x, xend=x, y=-.002, yend=.27, color=type), data=vsegXY, size=1.2) +
  geom_text(data=vsegXY, aes(x=x-100, y=.3, label=line, angle=90)) + 
  coord_cartesian(ylim = c(0, .35), xlim = c(0, 300000)) +
  ggtitle("Tax by Income Level (Joint & Individual Filing)")
dev.off()

png(file="TaxRateByIncomeSSM.png",width=1000,height=800,res=72)
ratesLong %>% filter(Income<500000) %>% filter(SSM) %>% ggplot(aes(x=Income, y=Rate)) + geom_line(aes(color=Type), size=1.2) + theme_bw() +
  geom_segment(aes(x=x, xend=x, y=-.002, yend=.325, color=type), data=vsegXY, size=1.2) +
  geom_text(data=vsegXY, aes(x=x-100, y=.35, label=line, angle=90)) + 
  coord_cartesian(ylim = c(.175, .38), xlim = c(0, 300000)) +
  ggtitle("Tax by Income Level (Joint & Individual Filing) Including Social Security and Medicare")
dev.off()
#IncomeNTax %>% filter(income<100000) %>% ggplot(aes(x=income, y=Indrate)) + geom_line(size=1.2) + theme_bw() +
#  geom_vline(xintercept = 12000) + geom_vline(xintercept = 32140)


taxes_long = gather(IncomeNTax %>% select(-Indrate) %>% select(-Jointrate), income, taxes = IndividualTax:r37, factor_key=TRUE)

colnames(taxes_long) <- c("Income", "Variable", "Taxes")

taxes_long$Rate = taxes_long$Variable %>% sub("r", "", .)

taxes_long %>% filter(Variable=="IndividualTax") %>% 
  ggplot(aes(x=Income, y=Taxes, group=Variable)) + geom_line(size=4) + theme_bw() + 
  geom_line(aes(x=Income, y=Taxes, group=Rate, color=Rate),size=.75, 
            data =  taxes_long %>% filter(Variable != "IndividualTax")) + 
  coord_cartesian(ylim = c(0, 120*10^3), xlim = c(0, 300000)) +


png(file="TaxByIncome.png",width=1000,height=800,res=72)
taxes_long %>% filter(Variable=="IndividualTax" | Variable=="JointTax") %>% 
  ggplot(aes(x=Income, y=Taxes, group=Variable)) + geom_line(aes(color=Variable),size=3) + theme_bw() + 
  geom_line(aes(x=Income, y=Taxes, group=Variable, color=Variable),size=.75, 
            data =  taxes_long %>% filter(!(Variable %in% c("IndividualTax", "JointTax")))) + 
  geom_segment(aes(x=x, xend=x, y=0, yend=43000, color=type, group=type), data=vsegXY, size=1.2, alpha=.6) +
  geom_text(data=vsegXY, aes(x=x-100, y=48000, label=line, angle=90, group=type)) + 
  coord_cartesian(ylim = c(0, 55000), xlim = c(0, 200000)) +
  ggtitle("Tax by Income Level (Joint & Individual Filing)")
dev.off()

#####
# Simulate savings plan
#####

incomesimulator <- function(
    
  # Starting income
  income0 = 75000,
  
  # Annual "real" income growth
  raise   = .005,
  
  # Gross savings - what amount of gross income goes into 401k
  s401k = 0,
  
  # Roth IRA Contribution 
  roth = 0,
  
  # Direct Investment - such as buying stokcs
  direct = 0,

  # Matching Rate
  matching = 0,
  
  # Investment return rate
  returnRate = .05,
  
  # Capital gains tax rate - kindof
  # I am saying treating this as the tax paid on any withdraws from direct investment
  capGainsTax = .20,
  
  # Number of years working
  yrsWorking = 35,
  
  # Number of years retired
  yrsRetired = 20,
  
  # Retirement withdrawl rate
  withdrawlRate = .04,
  
  # Secondary Taxable Retirement Income - Income sources at retirement
  retirementIncome = 15000) {
  
  #### Simulate Retirment
  Sim <- data.frame(year = 1:(yrsWorking+yrsRetired), 
                    status=c(rep(1, yrsWorking), rep(0, yrsRetired)),
                    wages=c(income0*(1+raise)^(0:(yrsWorking-1)), rep(retirementIncome, yrsRetired)))
  
  Sim$i401k <- 0
  Sim$i401k[1:yrsWorking] <- s401k
  Sim$T401k <- 0
  
  Sim$iroth <- 0
  Sim$iroth[1:yrsWorking] <- roth
  Sim$TRoth <- 0
  
  Sim$idir <- 0
  Sim$idir[1:yrsWorking] <- direct
  Sim$Tdir <- 0
  
  Sim$Inc401k <- 0
  Sim$IncRoth <- 0
  Sim$IncDir  <- 0
  
  Sim$taxIncome      <- 0 # Taxable Income
  Sim$IncTaxes       <- 0 # Taxes Pain
  Sim$CapTaxes       <- 0 # Capital Gains Taxes
  Sim$taxes          <- 0 # Taxes Pain
  
  Sim$Tassests <- 0
  Sim$eIncome <- 0
  
  for (i in 1:nrow(Sim)) {
    if (i == 1)                   {
      Sim$T401k[i]    <- Sim$i401k[i] * (1+matching)
      Sim$TRoth[i]    <- Sim$iroth[i]
      Sim$Tdir[i]     <- Sim$idir[i]
    }
    if (i <= yrsWorking & i > 1)  {
      Sim$T401k[i]    <- Sim$i401k[i] * (1+matching) + Sim$T401k[i-1] * (1+returnRate)
      Sim$TRoth[i]    <- Sim$iroth[i]  + Sim$TRoth[i-1] * (1+returnRate)
      Sim$Tdir[i]     <- Sim$idir[i]   + Sim$Tdir[i-1] * (1+returnRate)
    }
    if (i > yrsWorking)           {
      # Into retirement start withdrawing form 401k
      Sim$Inc401k[i] <- Sim$T401k[i-1] * withdrawlRate
      
      # Calculate growth in investment as withdrawls less growth rate
      Sim$T401k[i] <- Sim$T401k[i-1] * (1+returnRate) - Sim$Inc401k[i]
      
      # Into retirement start withdrawing form roth
      Sim$IncRoth[i] <- Sim$TRoth[i-1] * withdrawlRate
      
      # Calculate growth in investment as withdrawls less growth rate
      Sim$TRoth[i] <- Sim$TRoth[i-1] * (1+returnRate) - Sim$IncRoth[i]
  
      # Into retirement start withdrawing form 401k
      Sim$IncDir[i] <- Sim$Tdir[i-1] * withdrawlRate
      
      # Calculate growth in investment as withdrawls less growth rate
      Sim$Tdir[i] <- Sim$Tdir[i-1] * (1+returnRate) - Sim$IncDir[i]
      
      }
  
    # Effective Income
    Sim$taxIncome[i] <- Sim$wages[i] - Sim$i401k[i] + Sim$Inc401k[i]
    
    # Calculate Taxes
    Sim$IncTaxes[i] <- incometax(Sim$taxIncome[i], taxrateJoint)
    
    Sim$CapTaxes[i] <- Sim$IncDir[i] * capGainsTax
    
    Sim$taxes[i] <- Sim$IncTaxes[i] + Sim$CapTaxes[i]
    
  }
  
  # Effective Income
  Sim$eIncome <- Sim$taxIncome + Sim$IncRoth + Sim$IncDir - Sim$iroth - Sim$idir - Sim$taxes
  
  # Capital Accumulation
  Sim$Tassests <- Sim$T401k + Sim$TRoth + Sim$Tdir
  
  Sim
}

###########################
### Income Level 55,000

income0 = 55*10^3

investments = 5500

# Someone who saves nothing for retirement 50k per year, 15k per year retirment income (Social Security)
NoSavings = incomesimulator(                           , income0 = income0, retirementIncome = income0/3) %>% round
NoSavings = incomesimulator(                           , income0 = income0, retirementIncome = income0/3) %>% round
i401km00  = incomesimulator(s401k = investments        , income0 = income0, retirementIncome = income0/3) %>% round
i401km00b = incomesimulator(s401k = investments + 1000 , income0 = income0, retirementIncome = income0/3) %>% round
i401km05  = incomesimulator(s401k = investments, matching = .5, income0 = income0, retirementIncome = income0/3) %>% round
i401km10  = incomesimulator(s401k = investments, matching =  1, income0 = income0, retirementIncome = income0/3) %>% round
iRoth     = incomesimulator(roth  = investments               , income0 = income0, retirementIncome = income0/3) %>% round
iDirect   = incomesimulator(direct = investments              , income0 = income0, retirementIncome = income0/3) %>% round

eIncome = data.frame(
  year     = 1:nrow(NoSavings),
  NoSavings= NoSavings$eIncome, 
  i401km00 = i401km00$eIncome, 
  i401km00b = i401km00b$eIncome, 
  i401km05 = i401km05$eIncome, 
  i401km10 = i401km10$eIncome, 
  iRoth    = iRoth$eIncome, 
  iDirect  = iDirect$eIncome)

totIncome <- apply(eIncome, 2, sum)[-1] %>% data.frame(mechanic = names(.), value=.)
totIncome %>% ggplot(aes(mechanic, value)) + geom_col() + ggtitle("Total Lifetime Effective Income")

Tassests = data.frame(
  year     = 1:nrow(NoSavings),
  NoSavings= NoSavings$Tassests, 
  i401km00 = i401km00$Tassests, 
  i401km00b = i401km00b$Tassests, 
  i401km05 = i401km05$Tassests, 
  i401km10 = i401km10$Tassests, 
  iRoth    = iRoth$Tassests, 
  iDirect  = iDirect$Tassests)[nrow(NoSavings),-1]


Tassests = data.frame(mechanic = names(Tassests), value = Tassests %>% unlist, type="Total Assets")

Tassests_earnings = rbind(Tassests, cbind(totIncome, type="Total Earnings"))


png(file="LifetimeEarn55.png",width=800,height=600,res=72)
Tassests_earnings %>% ggplot(aes(mechanic, value, fill = type)) + geom_col() + 
  ggtitle("Total Lifetime Effective Income + End of Life Assests (annual income 55,000 USD, Savings 5,500 USD)")
dev.off()


eIncome55 <- eIncome

eIncomeLong <- eIncome %>% gather(year) 
colnames(eIncomeLong) <- c("Year", "Instrument", "eIncome")

eIncomeLong$Instrument <- eIncomeLong$Instrument %>% sub("i","",.)

JeIncomeLong <- eIncomeLong

jitter <- 25
speed  <- 0


for (i in 1:nrow(eIncomeLong)) {
  
  speed <- speed-sign(jitter)*abs(jitter)+rnorm(1)*5
    
  jitter <- jitter + speed 
  
  JeIncomeLong[i,3] <- eIncomeLong[i,3] + jitter
}

png(file="Income55k.png",width=800,height=600,res=72)
JeIncomeLong %>% ggplot(aes(x=Year, y=eIncome, color=Instrument)) + geom_line(aes(), size=1, alpha = .95) + theme_bw() +
  geom_text_repel(data=JeIncomeLong %>% filter(Year == max(Year)), aes(x=Year+2, y=eIncome, label=Instrument), point.padding = NA) +
  ggtitle("Effective Income    ----   Starting Income 55,000 - Investments 5,500 annually")  + 
  guides(color=FALSE)
dev.off()

########################
### Income Level 110,000

income0 = 110*10^3

investments = 5500
# Someone who saves nothing for retirement 50k per year, 15k per year retirment income (Social Security)
NoSavings = incomesimulator(                           , income0 = income0, retirementIncome = income0/3) %>% round
i401km00  = incomesimulator(s401k = investments               , income0 = income0, retirementIncome = income0/3) %>% round
i401km00b = incomesimulator(s401k = investments + 2000 , income0 = income0, retirementIncome = income0/3) %>% round
i401km05  = incomesimulator(s401k = investments, matching = .5, income0 = income0, retirementIncome = income0/3) %>% round
i401km10  = incomesimulator(s401k = investments, matching =  1, income0 = income0, retirementIncome = income0/3) %>% round
iRoth     = incomesimulator(roth  = investments               , income0 = income0, retirementIncome = income0/3) %>% round
iDirect   = incomesimulator(direct = investments              , income0 = income0, retirementIncome = income0/3) %>% round


eIncome = data.frame(
  year     = 1:nrow(NoSavings),
  NoSavings= NoSavings$eIncome, 
  i401km00 = i401km00$eIncome, 
  i401km00b = i401km00b$eIncome, 
  i401km05 = i401km05$eIncome, 
  i401km10 = i401km10$eIncome, 
  iRoth    = iRoth$eIncome, 
  iDirect  = iDirect$eIncome)

totIncome <- apply(eIncome, 2, sum)[-1] %>% data.frame(mechanic = names(.), value=.)
totIncome %>% ggplot(aes(mechanic, value)) + geom_col() + ggtitle("Total Lifetime Effective Income")

Tassests = data.frame(
  year     = 1:nrow(NoSavings),
  NoSavings= NoSavings$Tassests, 
  i401km00 = i401km00$Tassests, 
  i401km00b = i401km00b$Tassests, 
  i401km05 = i401km05$Tassests, 
  i401km10 = i401km10$Tassests, 
  iRoth    = iRoth$Tassests, 
  iDirect  = iDirect$Tassests)[nrow(NoSavings),-1]


Tassests = data.frame(mechanic = names(Tassests), value = Tassests %>% unlist, type="Total Assets")

Tassests_earnings = rbind(Tassests, cbind(totIncome, type="Total Earnings"))

png(file="LifetimeEarn110_55.png",width=800,height=600,res=72)
Tassests_earnings %>% ggplot(aes(mechanic, value, fill = type)) + geom_col() + 
  ggtitle("Total Lifetime Effective Income + End of Life Assests (annual income 110,000 USD, Savings 5,500 USD)")
dev.off()

eIncomeLong <- eIncome %>% gather(year) 
colnames(eIncomeLong) <- c("Year", "Instrument", "eIncome")

eIncomeLong$Instrument <- eIncomeLong$Instrument %>% sub("i","",.)

JeIncomeLong <- eIncomeLong; jitter <- 50; speed  <- 0

for (i in 1:nrow(eIncomeLong)) {
  
  speed <- speed-sign(jitter)*abs(jitter)+rnorm(1)*25
  
  jitter <- jitter + speed 
  
  JeIncomeLong[i,3] <- eIncomeLong[i,3] + jitter
}


png(file="Income110k.png",width=800,height=600,res=72)
JeIncomeLong %>% ggplot(aes(x=Year, y=eIncome, color=Instrument)) + geom_line(aes(), size=1, alpha = .95) + theme_bw() +
  geom_text_repel(data=JeIncomeLong %>% filter(Year == max(Year)), aes(x=Year+2, y=eIncome, label=Instrument), point.padding = NA) +
  ggtitle("Effective Income    ----   Starting Income 110,000 - Investments 5,500 annually")  + 
  guides(color=FALSE)
dev.off()

########################
### Income Level 110,000 - Investments 11000

income0 = 110*10^3
investments = 11*10^3

# Someone who saves nothing for retirement 50k per year, 15k per year retirment income (Social Security)
NoSavings = incomesimulator(                                  , income0 = income0, retirementIncome = income0/3) %>% round
i401km00  = incomesimulator(s401k = investments               , income0 = income0, retirementIncome = income0/3) %>% round
i401km00b = incomesimulator(s401k = investments + 3000        , income0 = income0, retirementIncome = income0/3) %>% round
i401km05  = incomesimulator(s401k = investments, matching = .5, income0 = income0, retirementIncome = income0/3) %>% round
i401km10  = incomesimulator(s401k = investments, matching =  1, income0 = income0, retirementIncome = income0/3) %>% round
iRoth     = incomesimulator(roth  = investments               , income0 = income0, retirementIncome = income0/3) %>% round
iDirect   = incomesimulator(direct = investments              , income0 = income0, retirementIncome = income0/3) %>% round


eIncome = data.frame(
  year     = 1:nrow(NoSavings),
  NoSavings= NoSavings$eIncome, 
  i401km00 = i401km00$eIncome, 
  i401km00b = i401km00b$eIncome, 
  i401km05 = i401km05$eIncome, 
  i401km10 = i401km10$eIncome, 
  iRoth    = iRoth$eIncome, 
  iDirect  = iDirect$eIncome)

totIncome <- apply(eIncome, 2, sum)[-1] %>% data.frame(mechanic = names(.), value=.)
totIncome %>% ggplot(aes(mechanic, value)) + geom_col() + ggtitle("Total Lifetime Effective Income")

Tassests = data.frame(
  year     = 1:nrow(NoSavings),
  NoSavings= NoSavings$Tassests, 
  i401km00 = i401km00$Tassests, 
  i401km00b = i401km00b$Tassests, 
  i401km05 = i401km05$Tassests, 
  i401km10 = i401km10$Tassests, 
  iRoth    = iRoth$Tassests, 
  iDirect  = iDirect$Tassests)[nrow(NoSavings),-1]

Tassests = data.frame(mechanic = names(Tassests), value = Tassests %>% unlist, type="Total Assets")

Tassests_earnings = rbind(Tassests, cbind(totIncome, type="Total Earnings"))

png(file="LifetimeEarn110_11k.png",width=800,height=600,res=72)
Tassests_earnings %>% ggplot(aes(mechanic, value, fill = type)) + geom_col() + 
  ggtitle("Total Lifetime Effective Income + End of Life Assests (annual income 110,000 USD, Savings 5,500 USD)")
dev.off()

eIncome110 <- eIncome

eIncomeLong <- eIncome %>% gather(year) 
colnames(eIncomeLong) <- c("Year", "Instrument", "eIncome")

eIncomeLong$Instrument <- eIncomeLong$Instrument %>% sub("i","",.)

JeIncomeLong <- eIncomeLong; jitter <- 50; speed  <- 0

for (i in 1:nrow(eIncomeLong)) {
  
  speed <- speed-sign(jitter)*abs(jitter)+rnorm(1)*25
  
  jitter <- jitter + speed 
  
  JeIncomeLong[i,3] <- eIncomeLong[i,3] + jitter
}

png(file="Income110_11k.png",width=800,height=600,res=72)
JeIncomeLong %>% ggplot(aes(x=Year, y=eIncome, color=Instrument)) + geom_line(aes(), size=1, alpha = .95) + theme_bw() +
  geom_text_repel(data=JeIncomeLong %>% filter(Year == max(Year)), aes(x=Year+2, y=eIncome, label=Instrument), point.padding = NA) +
  ggtitle("Effective Income    ----   Starting Income 110,000 - Investments 11,000 annually")  + 
  guides(color=FALSE)
dev.off()

########################
### Income Level Comparisons

relative_income <- ((eIncome110/eIncome55) %>% apply(2,mean))[-1] %>% data.frame()

write.csv(relative_income, file = "relative_income.csv")

relative_income <- 2- ((eIncome110/eIncome55) %>% apply(2,mean))[-1] %>% data.frame() 
