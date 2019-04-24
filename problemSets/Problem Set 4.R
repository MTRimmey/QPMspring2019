# Problem Set 4

# Question 1

getwd()

UCNotStopped <- 14
UCBribeRequested <- 6
UCStoppedWarned <- 7

LCNotStopped <- 7
LCBribeRequested <- 7
LCStoppedWarned <- 1

a <- .1
sd <- 4.147
n <- 42
xbar <- 7

z <- (xbar-a)/(sd/sqrt(n))
sum(z)

UCBribeRequested[1:10] - LCNotStopped[1:10]

help(rstandard)
pchisq(12, 5, lower.tail = FALSE)

# Question 2

PrecinctAssignedLawns <- 0.016
PrecinctAdjacentToLawnSigns <- 0.013
Constant <- 0.011

treat.PrecinctAssignedLawns <- [social$treatment=="Treatment"]

control.PrecinctAssignedLawns <- social$primary2006[social$treatment=="Control"]

t.test(treat.turnouot, control.turnout) # two-tailed


# Question 3

setwd("GitHub/QPMspring2019")
load("problemSets/Women_Dataset")

# Question 4

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

?ifelse
?lm

ifelse(Prestige$type)

Prestige$type[Prestige$type=="prof", YES]

Prestige$type <- row.names(Prestige) <- 1:nrow(Prestige)

# Question 5

library("faraway")
data("newhamp")
colnames(newhamp)

plot(newhamp$pObama, newhamp$votesys,
     main="NH",
     col = "Green", pch = 15,
     xlab= "O",
     ylab= "H")

?cor

cor(x = newhamp$pObama)


abline(newhamp$votesys, col = "red", lwd = 2)
abline(a = .3310, b = .4554, col = "blue", lwd = 2)

mod_self <- lm(newhamp$votesys ~ newhamp$pObama, data = newhamp)

lm(votesys ~ pObama)

# Question 6

setwd("GitHub/QPMspring2019")
read.csv("problemSets/incumbents_subset.csv")
Incumbents <- read.csv("problemSets/incumbents_subset.csv")
colnames(Incumbents)


scatterplot(Incumbents$voteshare ~ Incumbents$difflog,
            main="Model One", xlab="Diff Log", ylab="Vote Share")
ModelOne <- (lm(voteshare ~ difflog, data=Incumbents))
abline(lm(ModelOne, col="Red", lwd=2))
summary(ModelOne)

ResidualOne = (residuals(ModelOne))

plot(residuals(ModelOne) ~ fitted(ModelOne), data=Incumbents)
abline(h=0)
summary(ResidualOne)

coef(ModelOne)
abline(a=coef(ModelOne)[1], b=coef(ModelOne)[2], 
       lwd=2, col="Blue")

#6 B

scatterplot(Incumbents$presvote ~ Incumbents$difflog,
            main="Model Two", xlab="Diff Log", ylab="Presidential Vote")
ModelTwo <- (lm(presvote ~ difflog, data=Incumbents))
abline(ModelTwo, col="Red", lwd=2)
summary(ModelTwo)

ResidualTwo = (residuals(ModelTwo))

residuals <- plot(residuals(ModelTwo) ~ fitted(ModelTwo), data=Incumbents)
abline(h=0)
summary(ResidualTwo)

coef(ModelTwo)
abline(a=coef(ModelTwo)[1], b=coef(ModelTwo)[2], 
       lwd=2, col="Blue")

#6 C

scatterplot(Incumbents$presvote ~ Incumbents$voteshare,
            main="Model Three", xlab="Presidential Vote", ylab="Vote Share")
ModelThree <- (lm(presvote ~ voteshare, data=Incumbents))
abline(ModelThree, col="Red", lwd=2)
summary(ModelThree)

ResidualThree = (residuals(ModelThree))

residuals <- plot(residuals(ModelThree) ~ fitted(ModelThree), data=Incumbents)
abline(h=0)
summary(ResidualThree)

coef(ModelThree)
abline(a=coef(ModelThree)[1], b=coef(ModelThree)[2], 
       lwd=2, col="Blue")

#6 D

scatterplot(ResidualOne ~ ResidualTwo,
            main="Residual Comparison", xlab="Residual Two", ylab="Residual One")
ResidualComparison <- (lm(ResidualOne ~ ResidualTwo, data=Incumbents))
abline(ModelThree, col="Red", lwd=2)
summary(ModelThree)

#6 E

Incumbents$difflogpresvote = Incumbents$difflog + Incumbents$presvote

scatterplot(Incumbents$voteshare ~ Incumbents$difflogpresvote,
            main="Model Five", xlab="", ylab="Vote Share")
ModelFive <- (lm(presvote ~ voteshare, data=Incumbents))
abline(ModelFive, col="Red", lwd=2)
summary(ModelFive)






