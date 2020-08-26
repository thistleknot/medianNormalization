library(parallel)
require(ggplot2)
require(gridExtra)
library(crayon)


numDice = sample(3:10,1)
dice = c(4,6,8,10,12,20,100)
#numPositions = sample(c(3:10,20,50),1)
numPositions = 50

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

{
probs <- c(0, .1, .25, .5, .75, .9, 1)
bowleysSummary <- quantile(rolledSet, probs)

m <-median(rolledSet)

bowleyDeltaMedianSqr <- (bowleysSummary-m)^2

#multiplying by % delta's = dividing by that amount if every value were deviated by median multiplied by that % equivalence (i.e. 1/count)
stratifiedMAD <- sqrt(sum(diff(probs, lag = 1, differences = 1) * c(bowleyDeltaMedianSqr[1:3],bowleyDeltaMedianSqr[5:7])))

zScores <- (rolledSet-m)/stratifiedMAD

lowerAvg <- mean(abs(c(zScores[zScores<0])))
upperAvg <- mean(c(zScores[zScores>0]))
average <- mean(c(lowerAvg,upperAvg))

expectedQuartile <- abs(qnorm(.25, mean = 0, sd =1))
#factorVal <- expectedQuartile/average
factorVal <- expectedQuartile/average

#Confirmed

#quantile(rnorm(10000000),probs=c(.25))

lowerFactor <- lowerAvg/average*factorVal
upperFactor <- upperAvg/average*factorVal

lower <- which(zScores<0)
upper <- which(zScores>0)

colors <- rainbow(2)

reScaled <- zScores
reScaled[lower] <- zScores[lower]*upperFactor
reScaled[upper] <- zScores[upper]*lowerFactor

mean(zScores)
mean(reScaled)

twentyFivePercent <- m-lowerAvg*stratifiedMAD
seventyFivePercent <- m+upperAvg*stratifiedMAD

cat(blue(paste ("\n","blue 25/75%")))
cat(blue(paste ("\n",twentyFivePercent,seventyFivePercent)))

twentyFivePercentRescaled <- m-expectedQuartile*stratifiedMAD*factorVal
seventyFivePercentRescaled <- m+expectedQuartile*stratifiedMAD*factorVal

m
#linear, but 50% is no longe median, it's close and within 25 to 75%
newMean <- mean(c(twentyFivePercent,seventyFivePercent))
#newSdev <- (seventyFivePercent-twentyFivePercent)/((lowerAvg+upperAvg)/2)
newSdev <- (seventyFivePercent-twentyFivePercent)/(expectedQuartile*2)

newSetZ <- (rolledSet-newMean)/newSdev
newSetP <- pnorm(newSetZ)

#linear, 50% is median/median, 25/75% move a bit
#newSdev2 <- ((seventyFivePercent-m)+(m-twentyFivePercent))/((lowerAvg+upperAvg)/2)
newSdev2 <- ((seventyFivePercent-m)+(m-twentyFivePercent))/(expectedQuartile*2)
newSetZ2 <- (rolledSet-m)/newSdev2
newSetP2 <- pnorm(newSetZ2)

mean(c(twentyFivePercentRescaled,seventyFivePercentRescaled))

#linear, preserves median at 50%
newSdevRescaled <-  (seventyFivePercentRescaled-twentyFivePercentRescaled)/(expectedQuartile*2)
newSetZRescaled <- (rolledSet-m)/newSdevRescaled
newSetPRescaled <- pnorm(newSetZRescaled)

cat(yellow(paste ("\n","orange 25/75%")))
cat(yellow(paste ("\n",twentyFivePercentRescaled,seventyFivePercentRescaled)))

cat(paste("\n","actual","\n"))
print(summary(rolledSet))

df <- data.frame(rolledSet,pnorm(zScores),pnorm(reScaled),newSetP,newSetPRescaled, newSetP2)

plot1 <- ggplot(df, aes(rolledSet)) +                  # basic graphical object
  geom_point(aes(y=pnorm.zScores.), colour="red") +  # first layer
  geom_line(aes(y=pnorm.reScaled.), colour="green") +  # second layer
  geom_line(aes(y=newSetP), colour="blue") + # third layer
  geom_line(aes(y=newSetPRescaled), colour="orange") +  # fourth layer
  geom_line(aes(y=newSetP2), colour="magenta")  # fifth layer

df <- data.frame(rolledSet,zScores,reScaled,newSetZRescaled,newSetZ,newSetZ2)

plot2 <- ggplot(df, aes(rolledSet)) +                  # basic graphical object
  geom_point(aes(y=zScores), colour="red") +  # first layer
  geom_line(aes(y=reScaled), colour="green") + # second layer
  geom_line(aes(y=newSetZRescaled), colour="blue") + # third layer
  geom_line(aes(y=newSetZ), colour="orange") +  # fourth layer
  geom_line(aes(y=newSetZ2), colour="magenta")  # fourth layer

df <- data.frame(1:numPositions,zScores,reScaled,newSetZRescaled,newSetZ)

plot3 <- ggplot(df, aes(X1.numPositions)) +                    # basic graphical object
  geom_point(aes(y=zScores), colour="red") +  # first layer
  geom_line(aes(y=reScaled), colour="green") +  # second layer
  geom_line(aes(y=newSetZRescaled), colour="blue") + # third layer
  geom_line(aes(y=newSetZ), colour="orange") +  # fourth layer
  geom_line(aes(y=newSetZ), colour="magenta")  # fourth layer

grid.arrange(plot1, plot2, plot3, ncol=2, nrow=2)

cat (red(paste ("\n","Red:", "pnorm(ZScores), median 50%, avg !50% linear:","\n", collapse="")))
print(summary(pnorm(zScores)))
print(summary(zScores))

cat (green(paste ("\n","Green:", "pnorm(reScaled), median 50%, avg 50% nonlinear:","\n", collapse="")))
print(summary(pnorm(reScaled)))
print(summary(reScaled))

cat (blue(paste ("\n","Blue:", "newSetP, median ~50%, avg 50%, linear:","\n", collapse="")))
print(summary(newSetP))
print(summary(newSetZ))

cat (yellow(paste ( "\n","Orange:","newSetPRescaled, median 50%, avg !50%, linear:", "\n", collapse="")))
print(summary(newSetPRescaled))
print(summary(newSetZRescaled))

cat (magenta(paste ( "\n","magenta:","newSetP2, median 50%, avg ~50%, linear:", "\n", collapse="")))
print(summary(newSetP2))
print(summary(newSetZ2))

}