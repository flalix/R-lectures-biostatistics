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

library(ggplot2)


#------------------------------------------------------------.
#------------- Simulação e bottleneck -----------------------
#------------------------------------------------------------.

fenotipos = c("feno1", "feno2", "feno3", "feno4")

#-- propomos uma simulação onde os alelos FF, Ff, ff 
#   e simulamos os fenotipos abaixo:

feno1 = c( rep("Ff1", 30), rep("FF1", 150), rep("ff1", 50))
feno2 = c( rep("Ff2", 30), rep("FF2", 150), rep("ff2", 50))
feno3 = c( rep("Ff3", 30), rep("FF3", 150), rep("ff3", 50))
feno4 = c( rep("Ff4", 30), rep("FF4", 150), rep("ff4", 50))

# ex01.1) Qual o tamanhao da população? Quantos fenótipos foram
#  simulados? Qual a porcentagem de Heterozigotos e homozigotos?
#  Desafio01: Esta população está em equilibrio Hardy-Weinberg?

percFf1 = length(grep("Ff1", feno1)) / length(feno1)
percFf1
# este valor era esperado??
percFF1 = length(grep("FF1", feno1)) / length(feno1)
percFF1
percff1 = length(grep("ff1", feno1)) / length(feno1)
percff1

# ?sample
#-- Vamos amostrar nossa população com tamanho amostral de 900
#-- Isto seria como se 900 organismos poudessem se reproduzir
#-- gerando novos 900 organismos ... e assim por diante
#-- se você simular várias vezes, pense que os 100 que não se reproduziram
#-- permaneceram vivos e os 900 morreram gerando 900 novos organismos
#-- simule várias vezes, o que acontece? consegue observar flutuação?

#-- corra da linha 91 a linha 125
size = 30
s1 = sample(feno1, size)
s2 = sample(feno2, size)
s3 = sample(feno3, size)
s4 = sample(feno4, size)


vAA = length(grep("FF1", s1))
vAa = length(grep("Ff1", s1))
vaa = length(grep("ff1", s1))
v1 = c(vAA, vAa, vaa)

vAA = length(grep("FF2", s2))
vAa = length(grep("Ff2", s2))
vaa = length(grep("ff2", s2))
v2 = c(vAA, vAa, vaa)


vAA = length(grep("FF3", s3))
vAa = length(grep("Ff3", s3))
vaa = length(grep("ff3", s3))
v3 = c(vAA, vAa, vaa)

vAA = length(grep("FF4", s4))
vAa = length(grep("Ff4", s4))
vaa = length(grep("ff4", s4))
v4 = c(vAA, vAa, vaa)

df = data.frame(pheno1 = v1, pheno2 = v2, pheno3 = v3, pheno4 = v4)
rownames(df) = c("AA", "Aa", "aa")
df

barplot( as.matrix(df), main="Phenotypic distribution", col=c("navy","red", "yellow"))
legend("bottomright",legend=rownames(df), fill=c("navy","red", "yellow"))

#--------------------------------------------.
#------ Transforming data for ggplot --------
#--------------------------------------------.

colnames(df)

dfy =            data.frame(pheno = df$pheno1, class="pheno1")
dfy = rbind(dfy, data.frame(pheno = df$pheno2, class="pheno2"))
dfy = rbind(dfy, data.frame(pheno = df$pheno3, class="pheno3"))
dfy = rbind(dfy, data.frame(pheno = df$pheno4, class="pheno4"))
head(dfy)
tail(dfy)

ggplot(dfy, aes(class, pheno)) + 
  geom_bar(stat = "identity", aes(fill=class))


#--------------------------------------------.
#------ Our first function! Congrats ... ----
#--------------------------------------------.

#-- having feno1, .... to feno4
feno1 = c( rep("Ff1", 30), rep("FF1", 20), rep("ff1", 10))
feno2 = c( rep("Ff2", 30), rep("FF2", 20), rep("ff2", 10))
feno3 = c( rep("Ff3", 30), rep("FF3", 20), rep("ff3", 10))
feno4 = c( rep("Ff4", 30), rep("FF4", 20), rep("ff4", 10))


simulation <- function(size) {
  s1 = sample(feno1, size)
  s2 = sample(feno2, size)
  s3 = sample(feno3, size)
  s4 = sample(feno4, size)
  
  
  vAA = length(grep("FF1", s1))
  vAa = length(grep("Ff1", s1))
  vaa = length(grep("ff1", s1))
  v1 = c(vAA, vAa, vaa)
  
  vAA = length(grep("FF2", s2))
  vAa = length(grep("Ff2", s2))
  vaa = length(grep("ff2", s2))
  v2 = c(vAA, vAa, vaa)
  
  
  vAA = length(grep("FF3", s3))
  vAa = length(grep("Ff3", s3))
  vaa = length(grep("ff3", s3))
  v3 = c(vAA, vAa, vaa)
  
  vAA = length(grep("FF4", s4))
  vAa = length(grep("Ff4", s4))
  vaa = length(grep("ff4", s4))
  v4 = c(vAA, vAa, vaa)
  
  df = data.frame(pheno1 = v1, pheno2 = v2, pheno3 = v3, pheno4 = v4)
  rownames(df) = c("AA", "Aa", "aa")

  dfy =            data.frame(qtt = df$pheno1, class="pheno1", allele = c("AA","Aa","aa"))
  dfy = rbind(dfy, data.frame(qtt = df$pheno2, class="pheno2", allele = c("AA","Aa","aa")))
  dfy = rbind(dfy, data.frame(qtt = df$pheno3, class="pheno3", allele = c("AA","Aa","aa")))
  dfy = rbind(dfy, data.frame(qtt = df$pheno4, class="pheno4", allele = c("AA","Aa","aa")))

  return(dfy)
}

dfOther <- simulation(size = 5)
dfOther

sum(dfOther[dfOther$allele == "AA", "qtt"])
sum(dfOther[dfOther$allele == "Aa", "qtt"])
sum(dfOther[dfOther$allele == "aa", "qtt"])

dfOther

ggplot(data=dfOther, aes(allele, qtt)) + 
  geom_bar(stat = "identity", aes(fill=allele))

n.Ff = 5
n.FF = 15
n.ff = 10

feno1 = c( rep("Ff1", n.Ff), rep("FF1", n.FF), rep("ff1", n.ff))
feno2 = c( rep("Ff2", n.Ff), rep("FF2", n.FF), rep("ff2", n.ff))
feno3 = c( rep("Ff3", n.Ff), rep("FF3", n.FF), rep("ff3", n.ff))
feno4 = c( rep("Ff4", n.Ff), rep("FF4", n.FF), rep("ff4", n.ff))

dfOther <- simulation(size = 3)
ggplot(data=dfOther, aes(allele, qtt)) + 
  geom_bar(stat = "identity", aes(fill=allele))


ggplot(data=dfOther, aes(allele, qtt)) + 
  geom_bar(stat = "identity", aes(fill=allele)) +
  ggtitle("Distribuição de alelos")


ggplot(data=dfOther, aes(allele, qtt)) + 
  geom_bar(stat = "identity", aes(fill=allele)) +
  ggtitle("Distribuição de alelos") +
  scale_x_discrete(limits = c("AA","Aa","aa"))


ggplot(data=dfOther, aes(allele, qtt)) + 
  geom_bar(stat = "identity", aes(fill=allele)) +
  ggtitle("Distribuição de alelos") +
  scale_fill_manual(values = c("blue", "green", "salmon"))


ggplot(data=dfOther, aes(allele, qtt)) + 
  geom_bar(stat = "identity", aes(fill=allele), alpha=.7) +
  ggtitle("Distribuição de alelos") +
  scale_fill_manual(values = c("cyan", "darkolivegreen4", "salmon"))


#------------------------------------------------------------.
#------------- Dados de Galton: pais x filhos ---------------
#------------------------------------------------------------.

# library(UsingR)
# data(galton)

library(psych)
data(galton)

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
hist(galton$parent,col="blue",breaks=10)
hist(galton$child,col="lightblue",breaks=10)

df = data.frame(values = galton$parent, class = "parent")

ggplot(data=df, aes(values)) + 
  geom_histogram(binwidth=.7, colour="black", fill="white")

ggplot(data=df, aes(values)) + 
  geom_histogram(binwidth=.7, colour="blue", fill="cyan")

ggplot(data=df, aes(values)) + 
  geom_histogram(binwidth=.7, colour="blue", fill="cyan", alpha=.4)

ggplot(data=df, aes(values)) + 
  geom_density(colour="blue", fill="blue", alpha=.2)


#----------------
source("myFunctions.R")

p1 <- ggplot(data=df, aes(values)) +
  geom_histogram(binwidth=.5, colour="blue", fill="cyan", alpha=.3)

p2 <- ggplot(data=df, aes(values)) + 
  geom_density(colour="blue", fill="navy", alpha=.3)

multiplot(p1,p2,layout=matrix(c(1,2),ncol=2, byrow=T))

#--- vamos fazer o fitting ? -------------

dim(df)
colnames(df)
head(df)

yNorm <- dnorm(df$values, muD, sdD)

df = cbind(df, yNorm)
head(df)

ggplot(data=df, aes(values)) +
  geom_histogram(binwidth=.5, colour="blue", fill="cyan", alpha=.3)

ggplot(data=df, aes(values)) +
  geom_histogram(aes(y=..density..), binwidth=.5, colour="blue", fill="cyan", alpha=.3)

ggplot(data=df, aes(values)) +
  geom_histogram(aes(y=..density..), binwidth=.5, colour="blue", fill="cyan", alpha=.3) +
  stat_function(fun=dnorm, args=list(mean=muD, sd=sdD))+
  labs(title="Parents distribution", y="percentual (%)")


#-----------------------------------------------------.
#------------- Boxplot -------------------------------
#-----------------------------------------------------.

par(mfrow=c(1,1), mar=c(bottom=2.3,left=2,top=2,right=1))

#--- simple plot
boxplot(galton$parent, galton$child,
        names=c( "parents", "children"),
        main="Galton table", 
        xlab="children x parents", ylab="heigth",
        col = c("blue", "lightblue"))


#--- preparing data for ggplot boxplot ---
df =           data.frame(heights = galton$parent, class="parent")
df = rbind(df, data.frame(heights = galton$child,  class="child"))
head(df)
tail(df)

ggplot(data=df, aes(class, heights)) + geom_boxplot()

ggplot(data=df, aes(class, heights, fill=class)) + geom_boxplot()


ggplot(data=df, aes(class, heights, fill=class)) + 
       geom_boxplot() + geom_jitter(width = 0.1)


ggplot(data=df, aes(class, heights, fill=class)) + 
  geom_boxplot() + coord_flip()

ggplot(data=df, aes(class, heights, fill=class)) + 
  geom_boxplot(notch = TRUE) + coord_flip()

ggplot(data=df, aes(class, heights, fill=class)) + 
  geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 1) +
  coord_flip()


#----------- transcriptome --------------

fileExp = "../tables/expression.csv"
data = read.csv(fileExp, sep="\t")
dim(data)
colnames(data)

#--- preparing data for ggplot boxplot ---
df =           data.frame(cpm = data$CPM_CTRL, class="control", color="cyan")
df = rbind(df, data.frame(cpm = data$CPM_STRESS1,  class="high glucose", color="brown1"))
df = rbind(df, data.frame(cpm = data$CPM_STRESS2,  class="low glucose", color="darkolivegreen3"))

head(df)
tail(df)

ggplot(data=df, aes(class, cpm, fill=class)) + 
  geom_boxplot() + geom_jitter(width = 0.1)


#--- what happens ??? ----------------
#--- gene expression are lognormal !!! ---

dfControl = df[df$class == "control", ]
dim(dfControl)

ggplot(data=dfControl, aes(cpm)) + 
  geom_histogram(colour="black", fill="red", alpha=.4)


sum(dfControl$cpm == 0)
sum(dfControl$cpm < 0)

dfControl$cpm[dfControl$cpm == 0 ] = 0.1
dfControl$cpm = log10(dfControl$cpm)

ggplot(data=dfControl, aes(cpm)) + 
  ggtitle("Lognormal control histogram") +
  geom_histogram(colour="black", fill="red", alpha=.4)

ggplot(data=dfControl, aes(cpm)) + 
  ggtitle("Lognormal control histogram") +
  geom_histogram(colour="black", fill="red", alpha=.4,  bins=100)

bads = which(dfControl$cpm <= 0.1)
dfControl = dfControl[-c(bads), ]

ggplot(data=dfControl, aes(cpm)) + 
  ggtitle("Lognormal control histogram") +
  geom_histogram(colour="black", fill="red", alpha=.4, bins=50)

mu = mean(as.numeric(dfControl$cpm))
sdv = sd(as.numeric(dfControl$cpm))

ggplot(data=dfControl, aes(cpm)) + 
  ggtitle("Lognormal control histogram") +
  geom_histogram(aes(y=..density..), colour="black", fill="red", alpha=.4) +
  stat_function(fun=dnorm, args=list(mean=mu, sd=sdv))

#-------- get rid of low values and zeros to calc log ---------------

dflog = df
bads = which(dflog$cpm <= 0.1)
dflog = dflog[-c(bads), ]

sum(dflog$cpm == 0)
sum(dflog$cpm < 0)

dflog$cpm = log10(dflog$cpm)

ggplot(data=dflog, aes(class, cpm, fill=class)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) 

ggplot(data=dflog, aes(class, cpm, fill=class)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, alpha=.3) 

table(dflog$class)
table(dflog$color)
names(table(dflog$color))
colors = names(table(dflog$color))

ggplot(data=dflog, aes(class, cpm, fill=class)) + 
  geom_boxplot(outlier.colour = "salmon", outlier.shape = 1, alpha=.4) +
  scale_fill_manual(values = colors )


ggplot(data=dflog, aes(class, cpm, fill=class)) + 
  geom_boxplot(outlier.colour = "salmon", outlier.shape = 1, alpha=.4) +
  scale_fill_manual(values = colors ) +
  geom_jitter(width = 0.1, alpha=.05) # + coord_flip()


#------------ many density plots together ------------

head(dflog)
table(dflog$class)
length(names(table(dflog$class)))

source("myFunctions.R")

bins = 100
filename = ""
xlab = "CPM"
density = T

limits = c()

title = "Control CPM distribution"
col = "yellow"

head(dflog)

dfControl = dflog[dflog$class == "control", ]
p1 <- print_histogram(dfControl, title, xlab, col, filename, bins=bins, xlimits=c(), density=density, printPlot=NA, ret=T, is.log=F)
p1

colnames(dflog) = c("value", "class", "color")
head(dflog)
dfControl = dflog[dflog$class == "control", ]

p1 <- print_histogram(dfControl, title, xlab, col, filename, bins=bins, xlimits=c(), density=density, printPlot=NA, ret=T, is.log=F)
p1


#------------ male --------------------
table(dflog$class)
title = "High Glucose CPM distribution"
col = "red"
dfLow = dflog[dflog$class == "high glucose", ]
p2 <- print_histogram(dfLow, title, xlab, col, filename, bins=bins, xlimits=c(), density=density, printPlot=NA, ret=T, is.log=T)
p2


#------------ mean --------------------
title = "Low Glucose CPM distribution"
col = "blue"
dfLow = dflog[dflog$class == "low glucose", ]
p3 <- print_histogram(dfLow, title, xlab, col, filename, bins=bins, xlimits=c(), density=density, printPlot=NA, ret=T, is.log=T)
p3

#------------ all distributions together --------------------

head(dflog)
title = "CPM distribution - 3 conditions"
col = c("yellow", "red", "blue")
p4 <- print_histogram(dflog, title, xlab, col, filename, bins=bins, xlimits=c(), density=density, printPlot=NA, ret=T, is.log=T)
p4


multiplot(p1,p2,p3,p4,layout=matrix(c(1,2,3,4),ncol=2, byrow=T))


