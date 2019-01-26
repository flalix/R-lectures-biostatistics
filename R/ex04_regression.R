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
#------------- RNA-seq expression data in CPM ---------------
#------------------------------------------------------------.

#-- What is the difference about: FPKM, RPKM, TPM, CPM ???
#-- see:
#--    http://www.rna-seqblog.com/rpkm-fpkm-and-tpm-clearly-explained/
#--    https://haroldpimentel.wordpress.com/2014/05/08/what-the-fpkm-a-review-rna-seq-expression-units/
#--    https://hemberg-lab.github.io/scRNA.seq.course/normalization-for-library-size.html


#-----------------------------------------------------.
#------------- Analise Descritiva ---------------------
#-----------------------------------------------------.

# library(UsingR)
# data(galton)

library(psych)
data(galton)


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

summary(Galton)

#-----------------------------------------------------.
#------------- Distribuição de alturas ---------------
#-----------------------------------------------------.

par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=10)
hist(galton$parent,col="lightblue",breaks=10)

#--- vamos fazer o fitting ? -------------

mini = min( c( as.numeric(galton$parent), as.numeric(galton$child)))
maxi = max( c( as.numeric(galton$parent), as.numeric(galton$child)))
xvals <- seq(mini, maxi, by = .001)

#--- Parents
muD = round(mean(galton$parent), 1)
sdD = round(sd(galton$parent),1)
yvalsD <- dnorm(xvals, muD, sdD)

par(mfrow=c(1,2))

hist(galton$child,col="blue",breaks=10,freq=F,main="Parents")
lines(xvals, yvalsD, lwd = 3, col="yellow")

#--- Children
muS = round(mean(galton$child), 1)
sdS = round(sd(galton$child), 1)
yvalsC <- dnorm(xvals, muS, sdS)

hist(galton$parent,col="lightblue",breaks=10,freq=F,main="Children")
lines(xvals, yvalsC, lwd = 3, col="yellow")


par(mfrow=c(1,1))

boxplot(galton$parent, galton$child,
        names=c( "parents", "children"),
        main="Galton table", 
        xlab="children x parents", ylab="heigth",
        col = c("blue", "lightblue"))


#------------------------------------------------------------.
#------------- MSE: mean square error -----------------------
#------------------------------------------------------------.

library(manipulate)
par(mfrow=c(1,1))

myHist <- function(mu){
  hist(galton$child, col="blue", breaks=20)
  lines(c(mu, mu), c(0, 150),col="red",lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(63, 150, paste("mu = ", mu))
  text(63, 120, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))



#------------------------------------------------------------.
#------------- Regressão e MSE ------------------------------
#------------------------------------------------------------.

#--- o que faz uma regressão? minimiza o MSE

plot(galton$parent,galton$child,pch=19,col="blue")


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


#------------------------------------------------------------.
#------------- Mais regressão ------------------------------------
#------------------------------------------------------------.

par(mfrow=c(2,1), mar=c(bottom=2.3,left=2,top=2,right=1))

plot(galton$parent,galton$child,pch=19,col="blue",
     xlab="parent", ylab="child", main="Galton's regression")

fit = lm(child ~ parent, data=galton)
ab = fit$coefficients
abline(ab, col="yellow",lwd=2)

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


