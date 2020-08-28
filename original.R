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
#s <- sqrt(sum(rolledSet-m)^2/(length(rolledSet)-1))

probs <- c(0, .1, .25, .5, .75, .9, 1)
bowleysSummary <- quantile(rolledSet, probs)

m <-median(rolledSet)

bowleyDeltaMedianSqr <- (bowleysSummary-m)^2

#multiplying by % delta's = dividing by that amount if every value were deviated by median multiplied by that % equivalence (i.e. 1/count)
stratifiedMAD <- sqrt(sum(diff(probs, lag = 1, differences = 1) * c(bowleyDeltaMedianSqr[1:3],bowleyDeltaMedianSqr[5:7])))

s <- stratifiedMAD

z <- (rolledSet-m)/s

pnorm(z)
hist(pnorm(z))
plot(z)
mean(z)

equal <- which(z==0)
lower <- which(z<0)
upper <- which(z>0)

#lowerAvg <- sum(abs(z[lower]))/(length(z[lower])+length(z[equal])/2)
#upperAvg <- sum(abs(z[upper]))/(length(z[upper])+length(z[equal])/2)

lowerSum <- sum(abs(z[lower]))
upperSum <- sum(abs(z[upper]))

#averaged <- (lowerAvg+upperAvg)/2
sumAveraged <- (lowerSum+upperSum)/2

#expectedQuartile <- abs(qnorm(.25, mean = 0, sd =1))

#factor <- expectedQuartile/averaged

factor <- sumAveraged

newZ <- z
#newZ[lower] <- z[lower]/lowerAvg*factor
#newZ[upper] <- z[upper]/upperAvg*factor

newZ[lower] <- z[lower]*factor/lowerSum
newZ[upper] <- z[upper]*factor/upperSum


sum(abs(newZ[lower]))
sum(abs(newZ[upper]))

summary(pnorm(newZ))