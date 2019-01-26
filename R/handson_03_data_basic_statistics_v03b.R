#-------------------------------------------------.
# @author: Flavio Lichtenstein
# @data update:   2017/04/26
# @data created:  2017/04/24
# @local: IBiochemistry and Biophysics Laboratory / Butantan Institute
#-------------------------------------------------.


#--- distribuição de frequência ---
peso.rato01 = c(100, 90, 80, 70, 65)
peso.rato02 = c(103, 87, 85, 73, 69)
peso.rato03 = c(105, 93, 81, 75, 69)

df = rbind(peso.rato01, peso.rato02, peso.rato03 )
colnames(df) = c("d1", "d2", "d3", "d4", "d5")
df

media.dias = c(mean(df[,1]), mean(df[,2]), mean(df[,3]), mean(df[,4]), mean(df[,5]))
media.dias
names(media.dias)  = c("d1", "d2", "d3", "d4", "d5")
media.dias

barplot(media.dias)

dist = media.dias / sum(media.dias)
dist

barplot(dist)


par(mfrow=c(1,2))
barplot(media.dias, main = "Mean Weight Frequency Plot")
barplot(dist, main = "Mean Weight Distribution")


par(mfrow=c(2,1))
barplot(media.dias, main = "Mean Weigth Frequency Plot",ylab="<weight>")
barplot(dist, main = "Mean Weigth Distribution",ylab="<weight>")


par(mfrow=c(2,1))
barplot(media.dias, main = "Mean Weigth Frequency Plot",
        ylab="<weight>", col=c("red", "green", "blue", "yellow", "purple"))
barplot(dist, main = "Mean Weigth Distribution", 
        ylab="<weight>", col=c("red", "green", "blue", "yellow", "purple"))



par(mfrow=c(1,1))


#--- media, moda, mediana ------

?rbinom
data <- rbinom(n=1000, size=5, prob=.4)
hist(data)

mu = mean(data)
med = median(data)

abline(v=mu, col="red")
abline(v=med, col="blue")


# --- are both disribution similar ??? ----
par(mfrow=c(2,1))

dataC <- rbinom(n=10000, size=100, prob=.5)
hist(dataC)

data <- rbinom(n=10000, size=100, prob=.2)
hist(data)

# --- and now ??? xlim is the solution ----

par(mfrow=c(2,1))

dataC <- rbinom(n=10000, size=100, prob=.5)
hist(dataC, xlim=c(0,100))

mu = mean(dataC)
med = median(dataC)

abline(v=mu, col="red")
abline(v=med, col="blue")


data <- rbinom(n=10000, size=100, prob=.2)
hist(data, xlim=c(0,100))

mu = mean(data)
med = median(data)

abline(v=mu, col="red")
abline(v=med, col="blue")

#--- Ronald Fisher problem ------

# https://en.wikipedia.org/wiki/Ronald_Fisher
# 
# Se size = 2 temos 2 alelos A e a
# 1 combinação AA
# 2 possíveis combinações Aa ou aA
# 1 combinação aa

par(mfrow=c(1,1))

size = 2
data <- rbinom(n=10000, size=size, prob=.5)
title = paste('Binomial with size =', size)
hist(data, main=title)

# mas quando os números de genes envolvido num suposto fenótipo
# aumenta a distribuição tende a uma curva normal (ainda que discreta)

par(mfrow=c(2,2))

for (size in c(5, 10, 20, 100)) {
  data <- rbinom(n=10000, size=size, prob=.5)
  title = paste('Binomial with size =', size)
  hist(data, main=title)
}

par(mfrow=c(1,1))


#--- Poisson distribution ------

par(mfrow=c(1,1))

?rpois
data <- rpois(1000, .7)
hist(data,breaks=50)

mu = mean(data)
med = median(data)

abline(v=mu, col="red")
abline(v=med, col="blue")


data <- rpois(1000, 5)
h = hist(data,breaks=50)

mu = mean(data)
med = median(data)

abline(v=mu, col="red")
abline(v=med, col="blue")


# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
# The mode is the value that has highest number of occurrences in a set of data. Unike mean and median, mode can have both numeric and character data.
# R does not have a standard in-built function to calculate mode. So we create a user function to calculate mode of a data set in R. This function takes the vector as input and gives the mode value as output.

# Create the function.
getmode <- function(v) {
  h$breaks[which(h$counts == max(h$counts))[1]]
}

mod = getmode(data)
abline(v=mod, col="green")


#--- uniform distribution -------------

cat("This is Brian Caffo material, Boot Camp - Jonh Hopking")
xvals <- seq(-.5, 1.5, by = .001)
yvals <- rep(0, length(xvals))
yvals[xvals > 0 & xvals < 1] <- 1

# png(filename = "uniform.png", width = 960, height = 480)
plot(xvals, yvals, type = "n", ylab = "density", xlab = "support", frame = FALSE)
lines(xvals, yvals, lwd = 3)
# dev.off()


#--- normal distribution --------------
?rnorm

data = rnorm(100)
mean(data)
sd(data)

set.seed(35)
data = rnorm(100)
mean(data)
sd(data)

set.seed(35)
data = rnorm(1000)
mean(data)
sd(data)

set.seed(35)
data = rnorm(10000)
mean(data)
sd(data)


hist(data)


#--- sampling with R ------------------
?sample

#--- repeat 1000 times bad N samples, what occurs? ------
?sample
par(mfrow=c(1,1))

dataPop = rnorm(10000, mean=30, sd=5)
hist(dataPop)

par(mfrow=c(3,2))

for (n in c(4, 6, 30)) {
  mus = c(); sdvs = c()
  
  for (i in 1:n) {
    data = sample(dataPop, 10)
    
    mus = c(mus, mean(data))
    sdvs = c(sdvs, sd(data))
  }
  

  title = paste("n=", n, "- observe the mean \nand sdv from 'mu' !!!")
  muMUs = round(mean(mus), 3)
  sdMUs = round(sd(mus), 3)
  title = paste(title, "\nmean[mu] =", muMUs, "SD[mu] =", sdMUs)
  hist(mus, main=title, freq=F) # , xlim=c(-0.5, .5)
  
  title = paste("n=", n, "- observe the mean\nand sdv from 'sdv' !!!")
  muSDs = round(mean(sdvs), 3)
  sdSDs = round(sd(sdvs), 3)
  title = paste(title, "\nmean[sd] =", muSDs, "SD[sd] =", sdSDs)
  hist(sdvs, main=title, freq=F) # , xlim=c(0.5,1.5)
}




#--- bootstrap ------

par(mfrow=c(3,2))

pop = 12527
dataPop = rnorm(pop, mean=30, sd=11.3)
muG = round(mean(dataPop),1)
sdG = round(sd(dataPop),2)
muG
sdG

title = paste("Population n=", n, "\nmuG = ", muG, "sdG", sdG)
hist(dataPop, main=title, freq=F, breaks=100)

poor.exp = 8
my.data = sample(dataPop, poor.exp)
mean(data)
sd(data)
hist(my.data, main="my experimente", freq=F)


colors=c("red", "green", "blue", "yellow")

# resampling
count = 0
for (n in c(2, 3, 4, 5)) {
  mus = c(); sdvs = c()
  count = count + 1
  
  ressampling = 50; all.samples=c()
  for (i in 1:ressampling) {
    data = sample(my.data, n)
    all.samples = c(all.samples, data)

    mus = c(mus, mean(data))
    sdvs = c(sdvs, sd(data))
  }
  
  x=seq(0,60,.1)
  y<-dnorm(x, mean=mean(mus), sd=sd(sdvs))
  title = paste("boot n=", n, "ressamp ", ressampling)
  hist(all.samples, main=title, freq=F)
  lines(x,y, col=colors[count], lty=2)
}

par(mfrow=c(1,1))

#--- standard error ------
par(mfrow=c(3,2))

pop = 12527
dataPop = rnorm(pop, mean=30, sd=11.3)
muG = round(mean(dataPop),1)
sdG = round(sd(dataPop),2)

title = paste("Population n=", n, "\nmuG = ", muG, "sdG", sdG)
hist(dataPop, main=title, freq=F, breaks=100)

colors=c("red", "green", "blue", "yellow", "purple")

# my bad experiment ... to a good n = 30
x=seq(0,60,.1)

count = 0
for (poor.exp in c(3, 5, 10, 30, 50)) {
  mus = c(); sdvs = c()
  count = count + 1
  
  data = sample(dataPop, poor.exp)

  
  mu = round(mean(data),1)
  sdv = round(sd(data),1)
  
  y<-dnorm(x, mean=mu, sd=sdv)
  
  if (poor.exp >= 20) {
    title = "N samples= "
  } else {
    title = "Poor samples= "
  }
  title = paste(title, poor.exp, " mean(sdv)\n", mu, " (", sdv, ")", sep = "")
  hist(data, main=title, freq=F)
  lines(x,y, col=colors[count], lwd=2)
}


#--- normal distribution: quantiles --------------
par(mfrow=c(1,1))


data = rnorm(10000, mean=0, sd=1)
h = hist(data, breaks = 100, xlim = c(-4, 4), freq=F)

x=seq(-4,4,.1)
y<-dnorm(x, mean=0, sd=1)
lines(x,y, col="blue", lwd=2)


sdv=1
z=0
pnorm(q=z)
pnorm(q=1)
pnorm(q=0, mean=0, sd=sdv )

cuts <- cut(h$breaks, c(-Inf,z,Inf))
plot(h, col=cuts, freq=F)
lines(x,y, col="blue", lwd=2)

z = 0-2*sdv
z
perc = pnorm(q=z )
perc = round(perc,3)
perc
cuts <- cut(h$breaks, c(-Inf,z,Inf))
title = paste("for z =", z, "area \nfrom -inf to z is", perc)
plot(h, col=cuts, freq=F, main=title)
lines(x,y, col="blue", lwd=2)


#--- confidence interval ------------

qnorm(p=.5)
qnorm(p=0.95 )
qnorm(p=0.975 )
qnorm(p=0.99 )

pnorm(q=1.959964)
1-pnorm(q=1.959964)
2*(1-pnorm(q=1.959964))

1-(2*(1-pnorm(q=1.959964)))
#-- 95% confidence level?
#-- https://en.wikipedia.org/wiki/Confidence_interval

#--- http://www.cyclismo.org/tutorial/R/confidence.html .
mu <- 5
sdv <- 2

data = rnorm(10000, mean=mu, sd=sdv)
h = hist(data, breaks = 100, freq=F)

n <- 20
error <- qnorm(0.975)*sdv/sqrt(n)
left <- round(mu-error, 4)
right <- round(mu+error, 4)


cuts <- cut(h$breaks, c(-Inf,left, right,Inf))
title = paste("CI left =", left, " right", right)
plot(h, col=cuts, freq=F, main=title)

x=seq(-1,12,.1)
y<-dnorm(x, mean=mu, sd=sdv)
lines(x,y, col="blue", lwd=2)

CI = seq(left, right, 0.01)
yCI = rep(0.05, length(CI))
lines(CI, yCI, col="white", lwd=2)

#----------- Cumulative Distribution ---------
par(mfrow=c(2,1))

z = seq(-5, 5, 0.1)

data = rnorm(10000)
hist(data, breaks = 100, freq=F, main="Density distribution = f(z)")
lines(z, dnorm(z), col="blue", lwd=2)


y = pnorm(z)
plot(z, y, main="Cumulative Distribution = F")
lines(z, y, col="blue", lwd=2)


#-- qual a probabilidade de eu encontrar uma peça com 83 kg ----

# dado que uma peça pesa em média 80kg
# dado que o desvio padrão 1 kg

y = rnorm(1000, mean=80, sd=1)
hist(y, breaks=40)
abline(v=83, col="red")

sdv=3
pnorm(q=83, mean=80, sd=sdv)

p.gt.83 = 1 - pnorm(q=83, mean=80, sd=sdv)
p.gt.83
round(100*p.gt.83,2)

perc = round(100*p.gt.83,2)
print(paste("The prob that a device weights greater than 83 Kg is", perc, "%"))


#--- Is this normal or different ???  ------------
library(manipulate)

mu=76.3; sdv=11.8
maleWeight = rnorm(n=10000, mean=mu, sd=sdv)

h =hist(maleWeight,col="blue",breaks=100)

z = mu+(0*sdv)
pi = 1 - pnorm(q=z, mean=mu, sd=sdv)
pi

cuts <- cut(h$breaks, c(-Inf,z,Inf))
plot(h, col=cuts, freq=F)

alpha = 0.05
myPlot <- function(z){
  pi = 1 - pnorm(q=z, mean=mu, sd=sdv)
  pi = 100*(1-round(pi,2))
  
  cuts <- cut(h$breaks, c(-Inf,z,Inf))
  
  title = paste("The probability that someone has less than ",z, " Kg is ", pi,"%",sep="")
  title = paste(title, "\n and more than ", z, " Kg is ", 100-pi,"%",sep="")
  title = paste(title, "\n  significance level, also denoted as alpha or α,")
  title = paste(title, "\n  if ", pi, "less than alpha (", alpha, "), H0 must be rejected")
  title = paste(title, "\n  if ", 100-pi, "greater than 1-alpha (", 1-alpha, "), H0 must be rejected")
  if (pi < 100*alpha) {
    title = paste(title, "\n  pi < ", alpha, ". We cannot assume that is a 'normal' weight.")
  } else if (pi > 100*(1 - alpha)) {
    title = paste(title, "\n  pi > ", 1-alpha, ". We cannot assume that is a 'normal' weight.")
  }
  levels(cuts) = c("blue", "yellow")
  colors = as.character(cuts)
  plot(h, col=colors, freq=F, main=title, cex.main=.7)
}
manipulate(myPlot(z), z = slider(mu-5*sdv, mu+5*sdv, step = sdv/10))


#--- quantile -------------
?quantile

z = rnorm(n=1000)
q = quantile(z)
q

?boxplot
boxplot(z, horizontal=T)
abline(v=q[2], col="red")
abline(v=q[3], col="navy")
abline(v=q[4], col="brown")


icd = q[4] - q[3]
icd

abline(v=q[4]+3*icd, col="darkred", lty=2)


boxplot(z, horizontal=T)

qtl = quantile(z, probs=c(0, .25, .50, .75, .9, .95, .975, .99))
qtl
abline(v=qtl[1], col="red")
abline(v=qtl[2], col="orange")
abline(v=qtl[3], col="blue")
abline(v=qtl[4], col="green")
abline(v=qtl[5], col="brown")
abline(v=qtl[6], col="purple")

hist(z,breaks=20)
abline(v=qtl[1], col="red")
abline(v=qtl[2], col="orange")
abline(v=qtl[3], col="blue")
abline(v=qtl[4], col="green")
abline(v=qtl[5], col="brown")
abline(v=qtl[6], col="purple")



#--- z distribution ~ N(0, 1)----------

z = rnorm(1000, mean=0, sd=1 )
hist(z, freq=F)

mu = mean(z)
med = median(z)

abline(v=mu, col="red")
abline(v=med, col="blue")

q = quantile(z)
q
abline(v=q[2], col="darkred")
abline(v=q[4], col="darkred")

qnorm(p=.5)
qnorm(p=.9)
qnorm(p=.95, mean=0, sd=1 )
qnorm(p=.975, mean=0, sd=1 )
qnorm(p=.99, mean=0, sd=1 )

#-- z distribution with plot ----------------

zvals <- seq(-3, 3, by = .001)
dvals <- dnorm(zvals)
#postscript("stdNormal.eps", width = 5, height = 5, horizontal = FALSE)
plot(zvals, dvals, type = "l", xlab = "z", ylab = "density", frame = F, lwd = 3)
abline(v = 0)
abline(v = 1)
abline(v = -1)
abline(v = 2)
abline(v = -2)
abline(v = 3)
abline(v = -3)
#dev.off()



#--- z distribution II the revange ----------
par(mfrow=c(2,1))

data = rnorm(1000, mean=3.5, sd=2 )
hist(data, breaks=20)


mu = mean(data)
sdv = sd(data)
z = (data - mu) / sdv

hist(z, breaks=20)

par(mfrow=c(1,1))


#---- integral - areas in standard distribtution ---
z = rnorm(100, mean=0, sd=1 )
h = hist(z, breaks=50, freq=F)
z.9 = qnorm(p=.9)
z.neg.9 = -z.9
abline(v=z.neg.9, col="darkred")
abline(v=z.9, col="darkred")

cuts <- cut(h$breaks, c(-Inf,z.neg.9,z.9,Inf))
plot(h, col=cuts, freq=F)


x=seq(-4,4,.1)
y<-dnorm(x)
lines(x,y, col="blue", lwd=2)#and there's no response!no line showed up!


#--- summary -------------
summary(z)


#---- a particular distribution ----------
data = rnorm(1000, mean=3.5, sd=2 )

par(mfrow=c(1,1))
hist(data, breaks=20,freq=F)
x=seq(-3,3,.1)
y<-dnorm(x)
lines(x,y)#and there's no response!no line showed up!
text(x=6, y=.15, labels="that is wrong! why?",col="red")
#-- wrong !!! why?

hist(data, breaks=20,freq=F,col="lightblue")
x=seq(-3,10,.1)
y<-dnorm(x, mean=3.5, sd=2 )
lines(x,y, col="navy")#and there's no response!no line showed up!
#-- yes!


#--- standard deviation ----
hist(data, breaks=20,freq=F,col="lightblue")
x=seq(-3,10,.1)
y<-dnorm(x, mean=3.5, sd=1 )
lines(x,y, col="red")#and there's no response!no line showed up!
y<-dnorm(x, mean=3.5, sd=2 )
lines(x,y, col="navy")#and there's no response!no line showed up!
y<-dnorm(x, mean=3.5, sd=3 )
lines(x,y, col="green")#and there's no response!no line showed up!
text(x=7, y=.18, labels="something wrong.",col="red")


hist(data, breaks=20,freq=F,col="lightblue",ylim=c(0,.4))
x=seq(-3,10,.1)
y<-dnorm(x, mean=3.5, sd=1 )
lines(x,y, col="red")#and there's no response!no line showed up!
y<-dnorm(x, mean=3.5, sd=2 )
lines(x,y, col="navy")#and there's no response!no line showed up!
y<-dnorm(x, mean=3.5, sd=3 )
lines(x,y, col="green")#and there's no response!no line showed up!



#---------------- histograms objetcts -------------------------

par(mfrow=c(1,1))

data = rnorm(10000, mean=3.5, sd=2 )

hist(data, breaks=10)
hist(data, breaks=20)
hist(data, breaks=50)

ret = hist(data, breaks=20)
names(ret)

ret$breaks
ret = hist(data, breaks=seq(-5, 12, .5))

ret$counts
ret$breaks
ret$density
ret = hist(data, breaks=seq(-5, 12, .5), freq=F)

ret$mids
ret$xname
ret$equidist

par(mfrow=c(1,2))
hist(data, breaks=20, freq=F)
hist(data, breaks=20, freq=T)

par(mfrow=c(1,2))
hist(data, breaks=20, freq=F, main="Data distribution")
hist(data, breaks=20, freq=T, main="Data frequence")


hist(data, breaks=20, freq=F, main="Data distribution", xlab="width", ylab="density(%)")
hist(data, breaks=20, freq=T, main="Data frequence", xlab="width", ylab="frequence(#)")

?hist
hist(data, breaks=20, freq=F, angle = 45, col = "lightblue", 
     main="Data distribution", xlab="width", ylab="density(%)")
hist(data, breaks=20, freq=T, angle = 45, col = "pink", 
     main="Data frequence", xlab="width", ylab="frequence(#)")

par(mfrow=c(1,1))

#---------------- qqnrom -------------------------

qqnorm(z)

dado1 = c(1,2,3,3,5,5,7,9,3,5,9,12,1,3,8)
mu = mean(dado1)
va = var(dado1)
sdv = sd(dado1)
qqnorm(dado1)

dado1.z = (dado1-mu)/sdv
qqnorm(dado1.z)


dado2 = c(3,4.1,3.5,3,5,5,7,9,3,5,9,11,4.5,3,8)
mu2 = mean(dado2)
va2 = var(dado2)
sdv2 = sd(dado2)
qqnorm(dado2)

dado2.z = (dado2-mu2)/sdv2
qqnorm(dado2.z)


shapiro.test(dado1)
shapiro.test(dado2)


#---------------- qqplot - como funciona -------------------------

set.seed(1)          # for reproducibility 
Z <- rexp(1000)      # random sample from exponential distribution
p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(Z,p=p) # percentiles of the sample distribution
plot(qexp(p) ,q, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qexp,col="blue", lty=2)


#---------------- plots -- mtcars -------------------------
# http://www.r-tutor.com/r-introduction/data-frame 
# http://r.789695.n4.nabble.com/N-td3475132.html

attach(mtcars)

str(mtcars)

dim(mtcars)
class(mtcars)
colnames(mtcars)
rownames(mtcars)


# mpg	Miles/(US) gallon
# cyl	Number of cylinders
# disp	Displacement (cu.in. - cubic inch)
# hp	Gross horsepower
# drat	Rear axle ratio
# wt	Weight (1000 lbs)
# qsec	1/4 mile time
# vs	V engine/Straight engine
# am	Transmission (0 = automatic, 1 = manual)
# gear	Number of forward gears
# carb	Number of carburetors

1/0.26417
# L * 0.26417 = 1 G
# 1 G = 3.785 L

# 1 mile = 1.60934 Km

km.per.liter = 1.60934 / 3.785
km.per.liter 
# 0.4251889
# km = 1.60934 mils
# liter = 3.785 g
# km.per.liter = 0.4251889 mpg


kpl = mtcars$mpg * 0.4251889
mtcars = cbind(mtcars, kpl=kpl)
head(mtcars)
tail(mtcars)

summary(mtcars)

#--- filtering 4 cyl cars -------------------
four.cyl = mtcars[mtcars$cyl == 4, ]
four.cyl

par(mfrow=c(1,1))

boxplot(four.cyl$kpl, main="Boxplot - 4 cylinders, km/L")



par(mfrow=c(2,1))

boxplot(four.cyl$kpl, main="Boxplot - 4 cylinders, km/L", 
        horizontal=T, col = "lightgreen")

hist(four.cyl$kpl, breaks=5, freq=F, angle = 45, col = "lightblue", 
     main="Km/L distribution", xlab="Km/L", ylab="density(%)")
abline(v=mu, col = "red")


mu = mean(four.cyl$kpl)
mu
qt = quantile(four.cyl$kpl)
qt


par(mfrow=c(2,1))

boxplot(four.cyl$kpl, main="Boxplot - 4 cylinders, km/L", 
        horizontal=T, col = "lightgreen")

hist(four.cyl$kpl, breaks=5, freq=F, angle = 45, col = "lightblue", 
     main="Km/L distribution", xlab="Km/L", ylab="density(%)")
abline(v=mu, col = "red")
abline(v=qt[3], col = "green")
abline(v=qt[2], col = "black")
abline(v=qt[4], col = "black")

?abline

dim(four.cyl)
nrow(four.cyl)


#----- boxplot + histogram --------------

par(mfrow=c(2,1))

boxplot(four.cyl$kpl, main="Boxplot - 4 cylinders, km/L", 
        horizontal=T, col = "lightgreen", xlimit=c(8,16))

hist(four.cyl$kpl, breaks=5, freq=F, angle = 45, col = "lightblue", 
     main="Km/L distribution", xlab="Km/L", ylab="density(%)", xlimit=c(8,16))
# ???? wrong

?boxplot

boxplot(four.cyl$kpl, main="Boxplot - 4 cylinders, km/L", 
        horizontal=T, col = "lightgreen", ylim=c(8,16))

my.limits = 8:16
hist(four.cyl$kpl, breaks=my.limits, freq=F, angle = 45, col = "lightblue", 
     main="Km/L distribution", xlab="Km/L", ylab="density(%)")


abline(v=mu, col = "red")
abline(v=qt[3], col = "green")
abline(v=qt[2], col = "black")
abline(v=qt[4], col = "black")


lines(density(four.cyl$kpl), col="blue", lwd=2) # add a density estimate with defaults
lines(density(four.cyl$kpl, adjust=2), lty="dotted", col="darkgreen", lwd=2)

#----- scatter plot --------------

splot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")

