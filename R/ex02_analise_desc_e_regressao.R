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
#------------- Tres formas de abrir tabelas   ---------------
#------------------------------------------------------------.

# data(<<tabela interna>>)
# attach(<<tabela interna>>)
# 
# read.csv("filename")  # csv2, delimiter
# ?read.csv
# 
# load(<<tabela tipo RData>>)
# ?load

#------------------------------------------------------------.
#------------- Dados de galton: pais x filhos ---------------
#------ A data.frame of the galton (1888) height and cubit data set ----
#------------------------------------------------------------.
# install.packages("psych")
library(psych)

data(galton)

# install.packages("UsingR")
# library(UsingR)
# data(galton)

dim(galton)
colnames(galton)
class(galton) # data.frame

v = 1:5
class(v)
names(v)
# NULL

letters
names(v) = letters[1:5]
v
names(v)
length(v)

#-----------------------------------------------------.
#------------- Analise Descritiva ---------------------
#-----------------------------------------------------.

length(galton)

head(galton)
tail(galton)

dim(galton)
nrow(galton)
ncol(galton)

#-------- incluindo coluna ID -----------
nrow(galton)
id = 1:nrow(galton)
id
length(id)

#-- incluir colunas: cbind  (column bind)
galton2 = cbind(id, galton)
dim(galton2)
head(galton2)
colnames(galton2)
galton3 = galton2[ , -c(1)]
head(galton3)

all.equal(galton2, galton3)
all.equal(galton, galton3)

#-- reorder columns R
colnames(galton)

galton.inv = galton[ , c("parent","child")]
colnames(galton)
colnames(galton.inv)

galton[1, ]
galton.inv[1, ]

#------------------------------------.
#---- gal<tab> auto-completion ------
#------------------------------------.
mean(galton$parent)
sd(galton$parent)

mean(galton$child)
sd(galton$child)

dadies = length(galton$parent)
muD = mean(galton$parent)
sdD = sd(galton$parent)

suns = length(galton$child)
muS = mean(galton$child)
sdS = sd(galton$child)

#------ CI - confidence interval -----
#--- aproximado

seD = sdD /sqrt(dadies)
ciD = muD + c(-1,1)*2*seD
ciD
muD
muS

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


#--- no Linux um dos comandos de leitura/echo cat

for (i in 1:1) {
  cat(paste("Temos", dadies, "pais e", suns, "filhos."))
  cat(paste("Pais tem altura media", muD, "m e SD amostral", sdD, "m"))
  cat(paste("Filhos tem altura media", muS, "m e SD amostral", sdS, "m"))
  
}

for (i in 1:1) {
  cat(paste("Temos", dadies, "pais e", suns, "filhos.\n"))
  cat(paste("Pais tem altura media", muD, "m e SD amostral", sdD, "m\n"))
  cat(paste("Filhos tem altura media", muS, "m e SD amostral", sdS, "m\n"))
  
}

#---- O que quer dizer este objeto? str(<objeto) ----
str(galton)

#---- Desafio: qual o teste estatístico para comparar os 2 grupos? ----
#--- Resposta
#       Comparação de 2 grupos
#       Variáveis contínuas
#       Distribuição normal ... ou próxima a normal
#       teste: t-student


#--- t.test ----
t.test(galton$child, galton$parent)

# -- copie o resultado, cubra a região, dê um <CTRL><Shift>+<C>
# Welch Two Sample t-test
# 
# data:  galton$child and galton$parent
# t = -2.1677, df = 1672, p-value = 0.03032
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4185305 -0.0209092
# sample estimates:
#   mean of x mean of y
# 68.08847  68.30819

#---- Descrição estatística das colunas ----
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

hist(galton$parent ,col="blue",breaks=10,freq=F,main="Parents")
lines(xvals, yvalsD, lwd = 3, col="orange")
abline(v=muD,col="orange")

#--- Children
muS = round(mean(galton$child), 1)
sdS = round(sd(galton$child), 1)
yvalsC <- dnorm(xvals, muS, sdS)

hist(galton$child,col="lightblue",breaks=10,freq=F,main="Children")
lines(xvals, yvalsC, lwd = 3, col="orange")
abline(v=muS,col="orange")

#--- vamos deixar todas abcisass e ordenadas iguais


hist(galton$parent,col="blue",breaks=12,freq=F,main="Parents",xlim=c(60,76), ylim=c(0,.25))
lines(xvals, yvalsC, lwd = 3, col="orange")
abline(v=muS,col="orange", lwd=3)

hist(galton$child,col="lightblue",breaks=12,freq=F,main="Children",xlim=c(60,76), ylim=c(0,.25))
lines(xvals, yvalsD, lwd = 3, col="orange")
abline(v=muD,col="orange", lwd=3)


#---- analysing the distribution with boxplot ---
par(mfrow=c(1,1))

boxplot(galton$parent, galton$child,
        names=c( "parents", "children"))

boxplot(galton$parent, galton$child,
        names=c( "parents", "children"),
        main="galton table", 
        xlab="children x parents", ylab="heigth",
        col = c("blue", "lightblue"))


#--- Desafio: interprete este gráfico
par(mfrow=c(2,1))


plot(galton$parent,galton$child,pch=19,col="blue",
     xlab="parent", ylab="child", main="children distribution per parent interval")

boxplot(child ~ parent, data=galton, main="galton table", 
        xlab="parents", ylab="children")

par(mfrow=c(1,1))

#------------------------------------------------------------.
#------------- MSE: mean square error -----------------------
#              MSE: sum( (val - mean)^2 ) / n
#              a minimização do MSE aponta para a média
#------------------------------------------------------------.
# install.packages("manipulate")

library(manipulate)
par(mfrow=c(1,1))

calc.mu = round(mean(as.numeric(galton$child)),2)

myHist <- function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu, mu), c(0, 150),col="red",lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(64, 150, paste("mu = ", mu, " calc = ", calc.mu, sep=""))
  text(63, 120, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))


#------------------------------------------------------------.
#------------- Regressão ------------------------------------
#   a minimização do MSE aponta para a melhor regressão
#------------------------------------------------------------.
?par
?mfrow
par(mfrow=c(2,1), mar=c(bottom=2,left=2,top=2,right=1))

plot(galton$parent,galton$child,pch=19,col="blue",
     xlab="parent", ylab="child", main="galton's regression")

fit = lm(child ~ parent, data=galton)
r = cor(galton$child,  galton$parent)
r

ab = fit$coefficients
abline(ab, col="orange", lwd=3)
text(x=66, y=72, paste("Correlation =", round(r,3)), cex=1.5, col="red")

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
     xlab="parent", ylab="residuals", cex=2, lwd=2)
abline(h=0, col="navy")
abline(h=resi.mu, col="red")
abline(h=resi.mu+c(-2,2)*resi.sd , col="green")

# Desafio: O que são os pontos em vermelho ???
# Exercício: no primeiro plot (regressão) desenhe 
# duas linhas paralelas à regressão que significam
# regressão mais (ou menos) 2 standard deviations
# faça uma linha pontilhada e com cor vermelha.


#------------------------------------------------------------.
#----------- Regressão - minimizando MSE --------------------
#------------------------------------------------------------.

par(mfrow=c(1,1))

y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")

myPlot <- function(beta){
  
  plot(
    as.numeric(as.vector(freqData$parent)),
    as.numeric(as.vector(freqData$child)),
    pch = 21, col = "black", bg = "lightblue",
    cex = .05 * freqData$freq,
    xlab = "parent",
    ylab = "child"
  )
  abline(0, beta, lwd = 3)
  points(0, 0, cex = 1, pch = 19)
  mse <- mean( (y - beta * x)^2 )
  title(paste("beta = ", round(beta,3), "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.2, 1.2, step = 0.02))


