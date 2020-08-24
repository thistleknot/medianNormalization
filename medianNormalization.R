library(parallel)
require(ggplot2)
require(gridExtra)

numDice = sample(3:10,1)
dice = c(4,6,8,10,12,20,100)
numPositions = sample(c(3:10,20,50),1)

maxReScaledZ <- 2.25

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

plot(rolledSet)

probs <- c(0, .1, .25, .5, .75, .9, 1)
bowleysSummary <- quantile(rolledSet, probs)

m <-median(rolledSet)

bowleyDeltaMedianSqr <- (bowleysSummary-m)^2

#multiplying by % delta's = dividing by that amount if every value were deviated by median multiplied by that % equivalence (i.e. 1/count)
stratifiedMAD <- sqrt(sum(diff(probs, lag = 1, differences = 1) * c(bowleyDeltaMedianSqr[1:3],bowleyDeltaMedianSqr[5:7])))

zScores <- (rolledSet-m)/stratifiedMAD

lowerSum <- abs(sum(zScores[zScores<0]))
upperSum <- sum(zScores[zScores>0])
average <- mean(c(lowerSum,upperSum))

lowerFactor <- abs(lowerSum)/average
upperFactor <- abs(upperSum)/average

lower <- which(zScores<0)
upper <- which(zScores>0)

colors <- rainbow(2)

reScaled <- zScores
reScaled[lower] <- zScores[lower]*upperFactor
reScaled[upper] <- zScores[upper]*lowerFactor

limit <- mean(c(abs(min(reScaled)),max(reScaled)))
reFactor2 <- maxReScaledZ/limit

reScaled[lower] <- reScaled[lower]*reFactor2
reScaled[upper] <- reScaled[upper]*reFactor2

mean(zScores)
mean(reScaled)

df <- data.frame(rolledSet,pnorm(zScores),pnorm(reScaled))

plot1 <- ggplot(df, aes(rolledSet)) +                  # basic graphical object
  geom_point(aes(y=pnorm.zScores.), colour="red") +  # first layer
  geom_line(aes(y=pnorm.reScaled.), colour="green")  # second layer

df <- data.frame(rolledSet,zScores,reScaled)

plot2 <- ggplot(df, aes(rolledSet)) +                  # basic graphical object
  geom_point(aes(y=zScores), colour="red") +  # first layer
  geom_line(aes(y=reScaled), colour="green")  # second layer

df <- data.frame(1:numPositions,zScores,reScaled)

plot3 <- ggplot(df, aes(X1.numPositions)) +                    # basic graphical object
  geom_point(aes(y=zScores), colour="red") +  # first layer
  geom_line(aes(y=reScaled), colour="green")  # second layer

grid.arrange(plot1, plot2, plot3, ncol=2, nrow=2)

summary(pnorm(zScores))
summary(pnorm(reScaled))

summary(zScores)
summary(reScaled)
