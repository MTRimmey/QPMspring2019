# set working directory
setwd("~/GitHub/QPMspring2019/problemSets/PS2/MT_answers")

# Question 1

# It would not make sense to do so, since the data would not
# be predicting anything since the information is already available.

# Question 2

set.seed(123)
rnorm(x, mean = 5.2, sd = 3)

rnorm(36, mean = 4.6, sd = 3.2)

vec1 <- rnorm(100000, mean=0, sd=1)  # 100000 random numbers with mean=0 and sd=1
vec1

plot(density(vec1),
     main="Distribution of vec1",
     xlab="")

vec2 <- rnorm(x, mean=5.2, sd=3)
vec3 <- rnorm(36, mean=4.6, sd=3.2)  
plot(density(vec2), 
     main="Tribal Society",
     xlab="Distribution",
     col="yellow",
     xlim=c(0,100))
lines(density(vec3), lty=2, col="red")




rnorm()

z95 <- qnorm((1 - .95)/2, lower.tail = FALSE)## (1-confidence coefficient)/2
n <- length(na.omit(anes$bushiraq))
sample_mean <- mean(anes$bushiraq, na.rm = TRUE)
sample_sd <- sd(anes$bushiraq, na.rm = TRUE)
lower_95 <- sample_mean - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95) 

set.seed(123)
rnorm(36, mean = 4.6, sd = 3.2)
pnorm(mean(36), mean = 4.6, sd = 3.2)
dev.next()

x <- runif(100000 , min = -1, max= 1)
x <- sqrt (1 + x) + 2 * x ^3 - 23 * x + abs (log (abs (x))) + 2 * (x >
        .5) + -2* (x < -.5)

x [x > -5 & x < 5] <- 0
plot (density (x, bw = .75) , main = "Tribal Society Families" , xlab = "Data",
       col="Blue")

# Question 3

rnorm(1294, mean = 4.23, sd = 1.39)

z95 <- qnorm((1 - .95)/2, lower.tail = FALSE)
n <- length(na.omit(Polviews))
sample_mean <- mean(Polviews, na.rm = TRUE)
sample_sd <- sd(Polviews, na.rm = TRUE)
lower_95 <- sample_mean - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95)

# Question 4

rnorm(57.25, mean = 50, sd = 36)

rnorm(50.45, mean = 50, sd = 36)

rnorm(52.4 & 59.4, mean = 50, sd = 36)

pnorm(57.25, mean=50, sd=36)

pnorm(50.45, mean = 50, sd = 36)

pnorm(52.4 & 59.4, mean = 50, sd = 36)



help(Normal)

# Question 5

set.seed(12345)

salaries <- rnorm(n=10000, mean = 40000, sd = 15000)

pdf("salaries.pdf", width=7, height=7)
plot(salaries,
     ylab="Salaries", xlab="Plot")
dev.off()


pdf ("~/GoogleDrive/Documents/QPMspring2019/Salaries/.pdf")
hist(Salaries)
dev.off()
# Question 6


xx <- seq ( min ( x ) , max( x ) , length =100)

plot(xx, dnorm(xx, mean = 0, sd = 0.4) , type = "l" , xlab = " " , ylab = 
"Probability density" , cex.lab = 1.5)

xx <- seq ( min ( x ) , max( x ) , length =100)

plot(xx, dnorm(xx, mean = 0, sd = 3) , type = "l" , xlab = " " , ylab = 
       "Probability density" , cex.lab = 1.5)

xx <- seq ( min ( x ) , max( x ) , length =100)

plot(xx, dnorm(xx, mean = 3, sd = 3) , type = "l" , xlab = " " , ylab = 
       "Probability density" , cex.lab = 1.5)

xx <- seq ( min ( x ) , max( x ) , length =100)

plot(xx, dnorm(xx, mean = 3, sd = 0.4) , type = "l" , xlab = " " , ylab = 
       "Probability density" , cex.lab = 1.5)

xx <- seq ( min ( x ) , max( x ) , length =100)

plot(xx, dnorm(xx, mean = -2, sd = 5) , type = "l" , xlab = " " , ylab = 
       "Probability density" , cex.lab = 1.5)

xx <- seq ( min ( x ) , max( x ) , length =100)

plot(xx, dnorm(xx, mean = -2, sd = 0.25) , type = "l" , xlab = " " , ylab = 
       "Probability density" , cex.lab = 1.5)


# Question 7

drugCoverage <- read.csv("drugCoverage.csv")
View(drugCoverage)

DrugsInMedia <- drugCoverage[drugCoverage$drugsmedia==100 & drugCoverage$may-77==88,]

hist(drugCoverage$approval,
     breaks = 20,
     main = "Drug-Related Stories by Month",
     xlab = "Stories by Month",
     col = "Green",
     ylim = c(0,20))

boxplot(drugCoverage)

quantile(drugCoverage$approval, probs=c(0, 0.25, 0.5, 0.75, 1))

boxplot(drugCoverage$approval, main="Boxplot", ylab="Presidential Approval", ylim=c(0,100))

boxplot(drugCoverage$drugsmedia, main="Boxplot", ylab="Media Coverage", ylim=c(0,100))

scatter.smooth(drugCoverage$drugsmedia, drugCoverage$unemploy, main="Scatter Plot", xlab="Unemployment", ylab="Drug-Related Stories")

scatter.smooth(drugCoverage$drugsmedia, drugCoverage$approval, main="Scatter Plot", xlab="Presidential Approval", ylab="Drug-Related Stories")

plot(drugCoverage$drugsmedia, main="Scatter Plot", xlab="Media Coverage", ylab="Drug-Related Stories")

plot(drugCoverage$approval, main="Scatter Plot", xlab="Presidential Approval", ylab="Drug-Related Stories")

dev.next

# Question 8

wnominatehouse <- read.csv("wnominatehouse.csv")
View(wnominatehouse)  

help(rnorm)

House88thDems <- wnominatehouse[wnominatehouse$party==100 & wnominatehouse$congress==88,]
View(House88thDems)

House107thDems <- wnominatehouse[wnominatehouse$party==100 & wnominatehouse$congress==107,]
View(House107thDems)

House88thReps <- wnominatehouse[wnominatehouse$party==200 & wnominatehouse$congress==88,]
View(House88thReps)

House107thReps <- wnominatehouse[wnominatehouse$party==200 & wnominatehouse$congress==107,]
View(House107thReps)

mean(House88thDems==100)
median(House88thDems==100)
sd(House88thDems==100)

mean(House107thDems==200)
median(House107thDems==200)
sd(House107thDems==200)

mean(House88thReps==100)
median(House88thReps==100)
sd(House88thReps==100)

mean(House107thReps==200)
median(House107thReps==200)
sd(House107thReps==200)

hist(wnominatehouse$congress,
     breaks = 20,
     main = "88th Congress",
     xlab = "Democrats",
     ylab = "Republicans",
     col = "Blue",
     xlim = c(0,2),
     ylim = c(0,2),
     prob = TRUE)
points(seq(min(wnominatehouse), max(wnominatehouse), length.out=500), 
       dnorm(seq(minwnominatehouse), max(wnominatehouse), length.out=500),
       mean(wnominatehouse), sd(wnominatehouse), type="l", col="blue") 
     
hist(wnominatehouse$congress,
     breaks = 20,
     main = "107th Congress",
     xlab = "Democrats",
     ylab = "Republicans",
     col = "Blue",
     xlim = c(0,2),
     ylim = c(0, 2),
     prob = TRUE)
points(seq(min(wnominatehouse), max(wnominatehouse), length.out=500), 
       dnorm(seq(minwnominatehouse), max(wnominatehouse), length.out=500),
       mean(wnominatehouse), sd(wnominatehouse), type="l", col="blue")     
     
