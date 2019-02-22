# set working directory
setwd("~/GitHub/QPMspring2019/problemSets/PS2/MT_answers")

# Question 1

# Question 2

set.seed(123)
rnorm(36, mean = 4.6, sd = 3.2)

# Question 3

rnorm(mean = 4.23, sd = 1.39)

z95 <- qnorm((1 - .95)/2, lower.tail = FALSE)
n <- length(na.omit(Polviews))
sample_mean <- mean(Polviews, na.rm = TRUE)
sample_sd <- sd(Polviews, na.rm = TRUE)
lower_95 <- sample_mean - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95)

(1 = "Liberal", 4 = "Moderate", 7 "Conservative")

# Question 4

help(Normal)

rnorm(50, mean = 0, sd = 1)

# Question 5

set.seed(12345)

Salaries <- rnorm(n=10000, mean = 40000, sd = 15000)

table <- table(Salaries$num)

# Question 6



# Question 7

drugCoverage <- read.csv("drugCoverage.csv")
View(drugCoverage)

hist(drugCoverage)

help(boxplot)

help(scatter.smooth)

# Question 8

wnominatehouse <- read.csv("wnominatehouse.csv")
View(wnominatehouse)  

help(subset)

HouseParty <- subset(wnominatehouse, party == 100 & congress == 88, select = c(party, congress))
View(HouseParty)
