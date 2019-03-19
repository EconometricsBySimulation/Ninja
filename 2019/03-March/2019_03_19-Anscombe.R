# # I recently stumbled across Francis Anscombe's seminal paper on "Graphs in Statistical" analysis (American Statistician, 1973) and found myself easily convinced by the strength of his arguments yet also curious as to how he produced the mock up data that fit his statistical argument so perfectly.
# 
# # Not only did I want a data set that could make his aguments but I wanted to be a able to generate an arbitrary number of datasets and data. I tried a few different methods of producing the data that I wanted. 
# 
# # For instance say I wanted  to draw 11 x points with with mean 9 and variance 11 as found in the data. I attempted to draw 10 points then adjust the mean and variance by selectively drawing the 11th point. This approach however quickly fails as it relies too much on the 11th point. Say the mean from the first draws was unusually low with a mean of 8. In order to weight the sample mean back to 9 the 11th point would therefore need to be 19 in order to balance the x values at 9. Then you have to somehow figure out how to manage the variance which you know is already going to be blown up by the presence of your 11 value.
# 
# # Next I tried some search algorithms trying to use computation to search for possible values that fit my needed data. This was a highly problematic attempt that failed to produce any useful results.
# 
# # Fortunately, after a little reflection I realized the smarter approach was to make use of what I know about means and variances as well as correlations to modifying the sample to fit my desired outcome. For instance, no matter what x I started with (so long as x had any variation) I could adjust it to fit my needs. If the mean of x needs to be xmean. Then we can force it to be that:
# # (1) x = x-mean(x) + xmean
# 
# # Slightly more challenging, we could modify the variance of x by scaling the demeaned values of the sample. Since we know that 
# # (2) Var(aX)=a^2 * Var(x).
# 
# # Define a to be a multiplicative scalar for x
# # (3) a = (varX/var(x))^.5
# 
# # Thus we can have X be a sample with exact meanX and varianceX if we adjust it in the following manner:
# # (4)  x = a*(x-mean(x)) + mean(x)
# 
# a <- (varX/var(x))^.5
# x <- a*(x-mean(x)) + meanx
# mean(x) ; var(x)
# 
# # We now have x fitting our desired sample parameters.
# 
# # That was the easy part. What Professor Anscombe also did was he had regression values all match (ei b0=3 and b1=5) as well as correlations between x and y as well as mean and variances of y.
# 
# # From that list many people will realize that if b0 and b1 are specified and correlation x and y are specified and mean and variance x and y is specified then the mean and variance of y will be determined deterministically within the system.
# 
# # I used some known identities to figure out how much variance I would allow in the unexplained error u. To do that I started with knowing the 
# # SST = SSE + SSU (Sum of Squares Total = Sum of Squares Explained + Sum of Squares Unexplained)
# 
# # From (2) we know that the explained variance being contributed by x into y is:
# # (5) SSE = ((b1^2) * varX)*n
# 
# # Knowing that Variance(u) is SSU/n we can calculated what varianceu  needs to be.
# # (6) varianceu = (SSE/corXY^2 - SSE)/n
# 
# SSE <- (b1^2 * varX)*n
# varianceu = (SSE/corXY^2 - SSE)/n
# 
# # We also need to impose orthogonality between our sample x and our sample u in order to ensure that our regression coefficients will be exact. In order to do this I regress x on u then remove that best fit line from u resulting in correlation(x,u) = 0
# cfux <- coef(lm(u~x))
# u <- u - cfux[1] - cfux[2]*x
# 
# 
# # I, also adjust the variance of u using the same method as (3) and (4) to specify exactly the exact variance and mean (0).
# a <- (varianceu/var(u))^.5
# u <- a*(u-mean(u))
# 
# # Finally we are ready to combine.
# y = b0 + b1*x + u


################################################################

datagenerator <- function(n = 50, meanx = 9, varX = 11, b0 = 3, b1 = .5, corXY = .8,
                          xFUN = function(n) rnorm(n), 
                          uFUN = function(x) rnorm(length(x))) {
  
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
  varianceu = (SSE/corXY^2 - SSE)/n
  
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

library(ggplot2)

# Anscombe Quartet
xy1 <- datagenerator()
xy2 <- datagenerator(xFUN = function(n) 1:n, 
                     uFUN = function(x) {n <- length(x); sinpi(pi/4*(0:(n-1))/(n-1))})
xy3 <- datagenerator(xFUN = function(n) c(rep(0,n-1),1))
xy4 <- datagenerator(xFUN = function(n) sort(rnorm(n), decreasing=TRUE), 
                     uFUN = function(x) c(x[1]*(-5), x[2] , x[-(1:2)]*(-5)))


# Generate a situation were one outlier can completely misrepresent the relationships in the data
xy5 <- datagenerator(xFUN = function(n) c(2*n,(n-1):1), 
                     uFUN = function(x) c(x[1] * 50, x[-1] * 0))

# An alternative function that looks like a quardratic but is not
xy6 <- datagenerator(uFUN = function(x) -log(x))

# Heteroskedastic data
xy7 <- datagenerator(xFUN = function(n) 1:n, uFUN = function(x) (rnorm(length(x)))*rank(x))

# Heteroskedastic data
xy8 <- datagenerator(xFUN = function(n) rbinom(n,1,.5))


xy <- cbind(xy1,xy2,xy3,xy4,xy5,xy6, xy7, xy8)

apply(xy, 2,  mean); apply(xy, 2,  var)

summary(lm(y~x, data=xy1))
summary(lm(y~x, data=xy2))
summary(lm(y~x, data=xy3))
summary(lm(y~x, data=xy4))
summary(lm(y~x, data=xy5))
summary(lm(y~x, data=xy6))
summary(lm(y~x, data=xy7))
summary(lm(y~x, data=xy8))

for (i in 1:8)
assign(paste0("s",i), 
 ggplot(get(paste0("xy",i)), aes(x=x,y=y)) + 
   xlab(NULL) + ylab(NULL) + geom_point(size=2) + ggtitle(i)+
   geom_smooth(method='lm', se=FALSE)  + theme_void() + theme(legend.position="none") +
   theme(plot.title = element_text(hjust = 0.5))
)


setwd("Z:/Dropbox/Econometrics by Simulation/2019_03_March")
xylong <- cbind(set = rep(1:7,each=30), rbind(xy1,xy2,xy3,xy4,xy5,xy6,xy7))

write.csv(xylong, "Anscome.csv")

library(gridExtra)

grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, ncol=2)

png("Anscome.png", width=500, height=600)
grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, ncol=2)
dev.off()
################################################################


# Anscombe Quartet
xy1 <- datagenerator(b1 = -.5)
xy2 <- datagenerator(b1 = -.5, xFUN = function(n) 1:n, 
                     uFUN = function(x) {n <- length(x); sinpi(pi/4*(0:(n-1))/(n-1))})
xy3 <- datagenerator(b1 = -.5, xFUN = function(n) c(rep(0,n-1),1))
xy4 <- datagenerator(b1 = -.5, xFUN = function(n) sort(rnorm(n), decreasing=TRUE), 
                     uFUN = function(x) c(x[1]*(-5), x[2] , x[-(1:2)]*(-5)))


# Generate a situation were one outlier can completely misrepresent the relationships in the data
xy5 <- datagenerator(b1 = -.5, xFUN = function(n) c(2*n,(n-1):1), 
                     uFUN = function(x) c(x[1] * 50, x[-1] * 0))

# An alternative function that looks like a quardratic but is not
xy6 <- datagenerator(b1 = -.5, uFUN = function(x) -log(x))

# Heteroskedastic data
xy7 <- datagenerator(b1 = -.5, xFUN = function(n) 1:n, uFUN = function(x) (rnorm(length(x)))*rank(x))

# Heteroskedastic data
xy8 <- datagenerator(b1 = -.5, xFUN = function(n) rbinom(n,1,.5))

xy <- cbind(xy1,xy2,xy3,xy4,xy5,xy6, xy7, xy8)

apply(xy, 2,  mean); apply(xy, 2,  var)

summary(lm(y~x, data=xy1))
summary(lm(y~x, data=xy2))
summary(lm(y~x, data=xy3))
summary(lm(y~x, data=xy4))
summary(lm(y~x, data=xy5))
summary(lm(y~x, data=xy6))
summary(lm(y~x, data=xy7))
summary(lm(y~x, data=xy8))

for (i in 1:8)
  assign(paste0("s",i), 
         ggplot(get(paste0("xy",i)), aes(x=x,y=y)) + 
           xlab(NULL) + ylab(NULL) + geom_point(size=2) + ggtitle(i)+
           geom_smooth(method='lm', se=FALSE)  + theme_void() + theme(legend.position="none") +
           theme(plot.title = element_text(hjust = 0.5))
  )


setwd("Z:/Dropbox/Econometrics by Simulation/2019_03_March")
xylong <- cbind(set = rep(1:7,each=30), rbind(xy1,xy2,xy3,xy4,xy5,xy6,xy7))

write.csv(xylong, "Anscome2.csv")

library(gridExtra)

grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, ncol=2)

png("Anscome2.png", width=500, height=600)
grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, ncol=2)
dev.off()
