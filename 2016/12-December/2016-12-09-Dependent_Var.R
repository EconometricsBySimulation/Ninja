library(dplyr)
library(knitr)

simest <- function(
  
  # Number of simulations
  Nsim = 10^4,
  
  # Sample size minimum and maximum
  Size = 100,
  
  # Alternative dependent variable
  sigma_y = .5
  
) {
  
  ####################
  #### Simulation ####
  outdat <- NULL
  
  for (i in 1:Nsim) {
    
    y <- MASS::mvrnorm(Size, c(0,0), 
                           Sigma=matrix(c(1,sigma_y, sigma_y,1), 2))
    
    x <- rnorm(Size)

    p1 <- (lm(y[,1]~x) %>% summary %>% coefficients)[2,4]
    p2 <- (lm(y[,2]~x) %>% summary %>% coefficients)[2,4]
    
    if (p1<=p2) pout <- p1
    if (p1>p2)  pout <- p2
    
    outdat <- c(pout=pout,p1=p1,p2=p2, sigma_y=sigma_y) %>% rbind(outdat)
    
    if (i/10 == round(i/10)) print(paste(i, "of", Nsim))
  }
  outdat
}

outdat <- rbind(simest(sigma_y=-1),
                simest(sigma_y=-.75),
                simest(sigma_y=-.5),
                simest(sigma_y=-.25),
                simest(sigma_y=0), 
                simest(sigma_y=.25),
                simest(sigma_y=.5),
                simest(sigma_y=.75),
                simest(sigma_y=1))


outdat %>% tbl_df %>% group_by(sigma_y) %>% 
  summarize(a10=mean(pout<.1),
            a05=mean(pout<.05),
            a01=mean(pout<.01)) %>% 
  kable(format = "markdown")
