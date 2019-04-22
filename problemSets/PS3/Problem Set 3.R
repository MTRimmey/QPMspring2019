## Question 1

install.packages("faraway")
library("faraway")
data("newhamp")
help("newhamp")

boxplot(newhamp)

boxplot(newhamp$Obama, newhamp$Dean, xlab="Dean/Obama")

Dean mean: 02846768)
Obama mean: 374.1993
  
# Question 2

x <- seq(-4, 4, length=1000)
y <- dnorm(x, mean=0, sd=1)
plot(x,y, type="l", lwd=1)

DF1 <- dt(x, 20)
lines(x, DF1, type="l", lwd=1, col="Red")

DF2 <- dt(x, 3)
lines(x, DF2, type="l", lwd=1, col="Green")

DF3 <- dt(x, 1)
lines(x, DF3, type="l", lwd=1, col="Blue")

?dt
?seq
?plot
?lines
?dt

# Question 3

install.packages("Zelig")
library("Zelig")
data("voteincome")
?voteincome

table(voteincome$year)
table(voteincome$income)
table(voteincome$age)/1283

sd(voteincome$age)
mean(voteincome$age)
(voteincome$age)
sum(voteincome$vote)

s = 17.47113
m = 49.26133
n = 1500
xbar = 100
z = (100 - 49.26133)/ (17.47113/sqrt(1500))
z = 112.476997148275

pnorm(-abs(z))
2 * (1-pnorm(xbar, m - m, sd = s/sqrt(n)))

(Null Hypothesis: average voting age is different than 50)
(Alternative Hypothesis: average voting age is 50)

std <- function(x) sd(x)/sqrt(length(x))
std(c(1,2,3,4))

se <- function(x) sqrt(var(x)/length(x))

z95 <- qnorm((1 - .95)/2, lower.tail = FALSE)
n <- length(na.omit(voteincome$age))
sample_mean <- mean(voteincome$age, na.rm = TRUE)
sample_sd <- sd(voteincome$age, na.rm = TRUE)
lower_95 <- sample_mean - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95)

# Question 4

Null Hypothesis: 10 books purchased per year
ALternative Hopythesis: More than 10 books purchased per year

n = 16
m = 9.5
s = 1.2
mosd = 4.799674
t = ( - 49.26133)/ (17.47113/sqrt(1500))

sample_means = rep(NA, 16)
for(i in 1:16){
  sample_means[i] = mean(rexp(40,0.2))
}
mean(sample_means)


# Question 5

Population <- (698)
mean(Population)
  
  
  
  
# Question 6

(https://onlinelibrary.wiley.com/action/ssostart?redirectUri=/doi/full/10.1111/1468-2508.t01-1-00126)

# Question 7

mean <- 870
n <- 31
sample(mean, n, replace = FALSE, prob = NULL)
sample(mean)

sd(741, 27)

x = 124
y = 861
mean(y)

mean = 2.99
s = 2.34
n = 1117
xbar = 5.10

z = (xbar - mean)/ (s/sqrt(n))
2 * pnorm(-abs(z))
pnorm(-abs(z))


mean = 2.86
s = 2.22
n = 870
xbar = 4.04

z = (xbar - mean)/ (s/sqrt(n))
2 * pnorm(-abs(z))
pnorm(-abs(z))


# Question 8

mean = 2.99
s = 2.34
n = 1117
xbar = 5.10

mean = 2.86
s = 2.22
n = 870
xbar = 4.04

z = (xbar - mean)/ (s/sqrt(n))
2 * pnorm(-abs(z))
pnorm(-abs(z))

