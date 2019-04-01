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
plot(x, y, type="l", lwd=1)

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
table(voteincome$age)/1500

sd(voteincome$age)
mean(voteincome$age)

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



# Question 4

Null Hypothesis: 10 books purchased per year
ALternative Hopythesis: More than 10 books purchased per year

n = 16
m = 9.5
s = 1.2



# Question 5

No R?

# Question 6

(https://onlinelibrary.wiley.com/action/ssostart?redirectUri=/doi/full/10.1111/1468-2508.t01-1-00126)

# Question 7

t.test()

# Question 8

t.test()

