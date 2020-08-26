library(parallel)
require(ggplot2)
require(gridExtra)
library(crayon)


numDice = sample(3:10,1)
dice = c(4,6,8,10,12,20,100)
#numPositions = sample(c(3:10,20,50),1)
numPositions = 100

diceSet = sample(dice,numDice,replace=TRUE)

rolledSet <- sort(unlist(mclapply(1:numPositions, function (x)
{
  roll <- mclapply(diceSet, function(x)
  {
    rolled <- sample(1:x,1)
    return(rolled)
  })
  return(sum(unlist(roll)))
})))

hist(rolledSet)

m <- median(rolledSet)
s <- sqrt(sum(rolledSet-m)^2/(length(rolledSet)-1))

z <- (rolledSet-m)/s

pnorm(z)
hist(pnorm(z))
plot(z)
mean(z)

lower <- abs(mean(c(z[which(z<0)])))
upper <- abs(mean(c(z[which(z>0)])))
averaged <- mean(c(upper,lower))

expectedQuartile <- abs(qnorm(.25, mean = 0, sd =1))

newS <- (upper+lower)*s/(expectedQuartile*2)
#mean(pnorm(z))
newSetZ <- (rolledSet-m)/newS

twentyFivePercent <- m - (lower * s)
seventyFivePercent <- m + (upper * s)
newMean <- mean(c(twentyFivePercent,seventyFivePercent))
newSMean <- (seventyFivePercent-twentyFivePercent)/(expectedQuartile*2)
newZMean <- (rolledSet-newMean)/newSMean
summary(pnorm(newZMean))


#the problem is not linear.  The distances have to be symmetrical from a center to make it linear

finalZ= (newZMean+newSetZ)/2

summary(pnorm(newSetZ))
boxplot(pnorm(newSetZ))


summary(pnorm((newZMean)))