#####################################################
## File: Lab13.R                                   ##
## Multiple Regression in R                        ##
#####################################################

setwd("~/Documents/QPMspring2019/lab13")


## Goals:
## 1. Multiple regression 
## 2. Interaction




##
## 1. Multiple Regression
##

# Let's use the dataset from last week
library(faraway)
data(sat)
?sat
load(sat)

# We found that expend has a positive effect on takers with
# 95% level of significance.
sat1 <- lm(takers ~ expend, data=sat)
summary(sat1)
sat2 <- lm(expend ~ takers, data=sat)
summary(sat2)

# What if we control for other factors?
sat2 <- lm(takers ~ expend + ratio + salary, data=sat)
summary(sat2)

# How do you interpret the coefficient for salary?


# What if we include all the variables in the data?
sat3 <- lm(takers ~ ., data=sat)
summary(sat3)
# total is NA because it is verbal + math (Saturated Model)




##
## 2. Interaction
##

# Load a fake dataset
setwd("GitHub/QPMspring2019")
load("labs/lab13/Fake.RData")

table(fake.dat$partyID, fake.dat$treatment)

# Let's visualize the data
plot(fake.dat$treatment,
     fake.dat$y,
     type="n",
     ylab="y (Outcome)",
     xlab="Treatment",
     xaxt="n",
     xlim=c(-0.3,1.3))

axis(1, at=c(0,1), label=c("0=Control", "1=Treatment"))

# Republicans not treated
points(fake.dat$treatment[fake.dat$partyID=="R" & fake.dat$treatment==0], 
       fake.dat$y[fake.dat$partyID=="R" & fake.dat$treatment==0],
       col="red")

# Republicans treated
points(fake.dat$treatment[fake.dat$partyID=="R" & fake.dat$treatment==1], 
       fake.dat$y[fake.dat$partyID=="R" & fake.dat$treatment==1],
       col="red",
       pch=3)

# Democrats not treated
points(fake.dat$treatment[fake.dat$partyID=="D" & fake.dat$treatment==0], 
       fake.dat$y[fake.dat$partyID=="D" & fake.dat$treatment==0],
       col="blue",
       pch=2)

# Democrats treated
points(fake.dat$treatment[fake.dat$partyID=="D" & fake.dat$treatment==1], 
       fake.dat$y[fake.dat$partyID=="D" & fake.dat$treatment==1],
       col="blue",
       pch=5)


# interaction.plot is a good way to show differential effects.
interaction.plot(fake.dat$treatment,
                 fake.dat$partyID,
                 fake.dat$y,
                 type="b",
                 pch=c(21,19), 
                 ylim=c(0,15),
                 ylab="y",
                 xlab="Treatment")
# According to the plot, our "treatment" has different effects depending
# on the subject's party ID. For Democrats, the treatment effect seems to
# be positive, whereas for Republicans, it is negative...


no.int <- lm(y ~ treatment, data=fake.dat)
summary(no.int)
# Treatment is not statistically different from 0
no.int <- lm(y ~ treatment + partyID, data=fake.dat)
summary(no.int)

# Let's try interaction
int1 <- lm(y ~ treatment + partyID + treatment:partyID, data=fake.dat)
summary(int1)


# Simpler
int2 <- lm(y ~ treatment*partyID, data=fake.dat)
summary(int2)


# How do we interpret the coefficients?

# 1. The intercept is 5.7423.

# This is the predicted outcome of the subject who (does/does not)
# receive treatment and whose party ID is (R/D).


# 2. The coefficient for treatment is 4.7351.

# This is the marginal effect of treatment when the subject's 
# party ID is (R/D).


# 2. The coefficient for partyID (R) is 4.0408.

# This is the marginal effect of being (R/D), if the 
# subject (does/does not) receive treatment.


# 3. The coefficient for the interaction term is -10.4521.

# 3a. If the subject is a Republican, the marginal effect of treatment 
# is _________.

# 3b. If the subject receives treatment, the marginal effect of being
# a Republican is _________.








#### Group Work ####


# 1. Write the names of all group members.




# Some studies show that politically irrelevant events, such as 
# sports events and shark attacks, affect voters' retrospective
# evaluation of government performance. For example, Busby et al.
# (2017) find that the outcome of a college football game affects
# presidental job approval among students.

load("Busby_Football.RData")
colnames(x)

# Experimental setting: Busby et al. (2017) randomly assigned students 
# from Ohio State University (OSU) and University of Oregon (UO) to 
# answer a survey before and after the 2015 College Football Playoff 
# National Championship game. OSU won the game 42-20, and thus, OSU is 
# the "winning school" and UO is the "losing school." They find that among 
# OSU students, presidential approval was higher for those who answered 
# the survey after the game than those who answered the survey before the 
# game. By contast, among UO students, presidential approval was lower for
# those who answered the survey after the game than those who answered the 
# survey before the game. t-tests below confirm their findings.

# papprove: presidential approval
# Post: 0 = pre-game survey vs 1 = post-game survey
# osu: 0 = UO (losing) vs 1 = OSU (winning)

# Comparison of OSU students before and after the game
t.test(papprove ~ Post, data=x[x$osu==1,])

# Comparison of UO students before and after the game
t.test(papprove ~ Post, data=x[x$osu==0,])

# Source: Besby, Ethan C, James N. Druckman, and Alexandria Fredendall,
# 2017, "The Political Relevance of Irrelevant Events," Journal of 
# Politics 79(1).




# 2. Run a linear model with papprove as a dependent variable and 
#    Post, osu, and the interaction of the two as independent variables.




# 3. Answer the following questions based on the results.

# 3a. What is the predicted presidential approval of OSU students who received
#     the survey BEFORE the game?


# 3b. What is the predicted presidential approval of OSU students who received
#     the survey AFTER the game?


# 3c. What is the predicted presidential approval of UO students who received
#     the survey BEFORE the game?


# 3d. What is the predicted presidential approval of UO students who received
#     the survey AFTER the game?


# 3e. What is the marginal effect of Post on presidential approval
#     when osu=1?




################ Additional Question ################ 

# 4. The dataset includes a lot of variables about student characteristics,
#    like age and gender. Add some of these variables to the model above and
#    re-run a regression. Does the inclusion of these controls attenuate their
#    findings? Why or why not?