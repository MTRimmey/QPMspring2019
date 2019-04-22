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

help(rstandard)
pchisq(12, 5, lower.tail = FALSE)

# Question 2



# Question 3

# Question 4

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

professional = 1

?ifelse

Prestige$type 

# Question 5

library("faraway")
data("newhamp")
colnames(newhamp)

# Question 6

load(problemSets/incumbents_subset.csv)
