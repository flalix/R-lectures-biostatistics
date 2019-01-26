#-------------------------------------------------.
# @author: Flavio Lichtenstein
# @data update:   2017/05/01
# @data created:  2017/05/01
# @local: IBiochemistry and Biophysics Laboratory / Butantan Institute
#-------------------------------------------------.



#--- always begin setting you work folder ------
setwd("~/cursor/R")
# confirming
getwd()

#------------------------------------------------------------.
#------------- Dados de galton: pais x filhos ---------------
#------ A data.frame of the galton (1888) height and cubit data set ----
#------------------------------------------------------------.
# install.packages("psych")
library(psych)

data(galton)

# library(UsingR)
# data(galton)

dim(galton)
colnames(galton)
class(galton)


#-----------------------------------------------------.
#------------- Analise Descritiva ---------------------
#-----------------------------------------------------.

head(galton)
tail(galton)

mean(galton$child)
sd(galton$child)

mean(galton$parent)
sd(galton$parent)

dadies = length(galton$parent)
muD = mean(galton$parent)
sdD = sd(galton$parent)

suns = length(galton$child)
muS = mean(galton$child)
sdS = sd(galton$child)


print(paste("Temos", dadies, "pais e", suns, "filhos."))
print(paste("Pais tem media", muD, "e SD amostral", sdD))
print(paste("Filhos tem media", muS, "e SD amostral", sdS))

#--- Vamos melhorar ??? ----------

dadies = length(galton$parent)
muD = round(mean(galton$parent), 1)
sdD = round(sd(galton$parent),1)

suns = length(galton$child)
muS = round(mean(galton$child), 1)
sdS = round(sd(galton$child), 1)


print(paste("Temos", dadies, "pais e", suns, "filhos."))
print(paste("Pais tem altura media", muD, "m e SD amostral", sdD, "m"))
print(paste("Filhos tem altura media", muS, "m e SD amostral", sdS, "m"))

#---- Desafio: qual o teste estatístico para comparar os 2 grupos? ----

summary(galton)

#-----------------------------------------------------.
#------------- Distribuição de alturas ---------------
#-----------------------------------------------------.

par(mfrow=c(1,2))
hist(galton$parent,col="blue",breaks=20)
hist(galton$child,col="lightblue",breaks=20)

hist(galton$parent,col="blue",breaks=10)
hist(galton$child,col="lightblue",breaks=10)


#--- vamos fazer o fitting ? -------------

mini = min( c( as.numeric(galton$parent), as.numeric(galton$child)))
maxi = max( c( as.numeric(galton$parent), as.numeric(galton$child)))
xvals <- seq(mini, maxi, by = .001)

#--- Parents
muD = round(mean(galton$parent), 1)
sdD = round(sd(galton$parent),1)
yvalsD <- dnorm(xvals, muD, sdD)

hist(galton$child,col="blue",breaks=10,freq=F,main="Parents")
lines(xvals, yvalsD, lwd = 3, col="yellow")

#--- Children
muS = round(mean(galton$child), 1)
sdS = round(sd(galton$child), 1)
yvalsC <- dnorm(xvals, muS, sdS)

hist(galton$parent,col="lightblue",breaks=10,freq=F,main="Children")
lines(xvals, yvalsC, lwd = 3, col="yellow")


#---- analysing the distribution with boxplot ---

par(mfrow=c(1,1))

boxplot(galton$parent, galton$child,
        names=c( "parents", "children"),
        main="galton table", 
        xlab="children x parents", ylab="heigth",
        col = c("blue", "lightblue"))


#--- Desafio: interprete este gráfico
boxplot(child ~ parent, data=galton, main="galton table", 
        xlab="children x parents", ylab="heigth")


#------------------------------------------------------------.
#------------- MSE: mean square error -----------------------
#              MSE: sum( (val - mean)^2 ) / n
#------------------------------------------------------------.
# install.packages("manipulate")

library(manipulate)
par(mfrow=c(1,1))

myHist <- function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu, mu), c(0, 150),col="red",lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(63, 150, paste("mu = ", mu))
  text(63, 120, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))




#------------------------------------------------------------.
#------------- Regressão ------------------------------------
#------------------------------------------------------------.
?par
?mfrow
par(mfrow=c(2,1), mar=c(bottom=2,left=1,top=2,right=1))

plot(galton$parent,galton$child,pch=19,col="blue",
     xlab="parent", ylab="child", main="galton's regression")

fit = lm(child ~ parent, data=galton)
ab = fit$coefficients
abline(ab, col="navy")

resi = fit$residuals
resi

resi.mu = mean(resi)
resi.sd = sd(resi)

colors = rep("blue", length(galton$parent))
for (i in 1:length(galton$parent)) {
  if (abs(resi[i]) >=  (resi.mu + 2* resi.sd) ) {
    colors[i] = "red"
  }
}
plot(galton$parent, resi, col=colors, 
     xlab="parent", ylab="residuals")
abline(h=0, col="navy")
abline(h=resi.mu, col="red")
abline(h=resi.mu+c(-2,2)*resi.sd , col="green")

par(mfrow=c(1,1))

# Desafio: O que são os pontos em vermelho ???
# Exercício: no primeiro plot (regressão) desenhe 
# duas linhas paralelas à regressão que significam
# regressão mais (ou menos) 2 standard deviations
# faça uma linha pontilhada e com cor vermelha.


#------------------------------------------------------------.
#----------- Regressão - minimizando MSE --------------------
#------------------------------------------------------------.


y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")

myPlot <- function(beta){
  
  plot(
    as.numeric(as.vector(freqData$parent)),
    as.numeric(as.vector(freqData$child)),
    pch = 21, col = "black", bg = "lightblue",
    cex = .1 * freqData$freq,
    xlab = "parent",
    ylab = "child"
  )
  abline(0, beta, lwd = 3)
  points(0, 0, cex = 1, pch = 19)
  mse <- mean( (y - beta * x)^2 )
  title(paste("beta = ", round(beta,3), "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.2, 1.2, step = 0.02))


