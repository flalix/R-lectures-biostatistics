#-------------------------------------------------.
# @author: Flavio Lichtenstein
# @data update:   2017/05/02
# @data created:  2017/05/02
# @local: IBiochemistry and Biophysics Laboratory / Butantan Institute
#-------------------------------------------------.

#--- always begin setting you work folder ------
setwd("~/cursor/R")
# confirming
getwd()

library(ggplot2)
source("myFunctions.R")

#-----------------------------------------------------.
#---------- Comparing Heights ------------------------
#-----------------------------------------------------.
# library(UsingR)
library(psych)
data(galton)

#---- t.test - variaveis numérica, distrib normal, 2 grupos ----
summary(galton)


df = data.frame(values = galton$parent, class = "parent")

ggplot(data=df, aes(values)) + 
  geom_bar(colour="blue", fill="blue", alpha=.2)


df = rbind(data.frame(values = galton$parent, class = "parent"),
           data.frame(values = galton$child, class = "child"))

ggplot(data=df, aes(values, fill=class)) + 
  geom_histogram(colour="black", alpha=.2, bins=10)

ggplot(data=df, aes(values, fill=class)) + 
  geom_histogram(colour="black", alpha=.2, position="dodge", bins=10)


colors = c("blue", "green")
ggplot(data=df, aes(values, fill=class)) + 
  geom_histogram(colour="black", alpha=.2, position="dodge", bins=10) +
  scale_fill_manual(values = colors )


dadies = length(galton$parent)
muD = round(mean(galton$parent), 1)
sdD = round(sd(galton$parent),1)

suns = length(galton$child)
muS = round(mean(galton$child), 1)
sdS = round(sd(galton$child), 1)

ggplot(data=df, aes(values)) +
  geom_histogram(aes(y=..density..), binwidth=.5, colour="blue", fill="cyan", alpha=.3) +
  stat_function(fun=dnorm, args=list(mean=muD, sd=sdD), col="navy", size=1.2)+
  stat_function(fun=dnorm, args=list(mean=muS, sd=sdS), col="green", size=1.2)+
  labs(title="Parents distribution", y="percentual (%)")


#---------------- qqnrom -------------------------
qtPar = quantile(galton$parent)
qtPar

qtChi = quantile(galton$child)
qtChi

dadies
suns

#---------------- qqnrom -------------------------

x = rnorm(1000)
qqnorm(x)

qqnorm(galton$parent)

valPar = as.numeric(galton$parent)
zPar = (valPar - mean(valPar)) / sd(valPar)
hist(zPar)
qqnorm(zPar)

qqplot(galton$parent, galton$child)
abline(c(0,1), col="red", lwd=2)


#-- what is t-test? besides p-value what is the more interesting parameter to analyze?
t.test(galton$parent, galton$child)

?t.test

alpha = .01
ci = 1 - alpha
t.test(galton$parent, galton$child, conf.level = ci)



#------------------------------------------------------------.
#------------- HCS experiment -------------------------------
#------------------------------------------------------------.

filename = "../tables/xxxxx.csv"
df = read.csv(filename, sep="\t")
df[1:3, 1:5]

rownames(df) = df$X
df = df[,-c(1)]
df[1:3, 1:5]

#------------------------------------------------------------.
#------------- ANOVA ----------------------------------------
#------------------------------------------------------------.

#-- quero comparar grupos ??? ANOVA
# transformar numa matriz em que cada linha é uma CLASSE e os X são suas medidas para diversas condições (p.ex.)

data = data.frame()
for (i in 1:nrow(df)) {
  v = as.numeric(df[i,])
  dfClasse = data.frame(val = v, class = rownames(df)[i])
  data = rbind(data, dfClasse)
}

head(data)
tail(data)

ggplot(data=data, aes(class, val, fill=class)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, alpha=.4) +
  theme(legend.position="none")


fit = lm(val ~ class, data)
anova(fit)
# manheee, como eu interpreto isto ????

which(data$class == "H")
which(data$class %in% c("G", "H"))

data = data[-c(which(data$class %in% c("G", "H"))),]
head(data)
tail(data)

ggplot(data=data, aes(class, val, fill=class)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, alpha=.4) +
  theme(legend.position="none")


fit = lm(val ~ class, data)
anova(fit)

#-- t.test

lineA = as.numeric(df["A",])
lineB = as.numeric(df["B",])
lineG = as.numeric(df["G",])
lineH = as.numeric(df["H",])

t.test(lineA, lineB)
t.test(lineG, lineH)
t.test(lineA, lineH)

#-- be careful: multiple comparisons
#-- or use Bonferoni or FDR (false discoverd rate)


#------------------------------------------------------------.
#------------- chi.square test ------------------------------
#------------------------------------------------------------.
#---- vars: integer -- comparing independence of distributions
#------------------------------------------------------------.
#-- Pearson's chi-squared test.
#-- assumption of independent normally distributed data
#-- all values >= 5, otherwise Fisher Exact Test
#-- https://en.wikipedia.org/wiki/Chi-squared_test

#              | POS  |  NEG | Marginal
# -------------------------------------.
# gold standar | 1320 |  650 | 1970
# -------------------------------------.
# new test kit | 1105 |  850 | 1955
# -------------------------------------.
# marginal     | 2425 | 1500 | 3925

gold.standard = c(1320,  650)
fluorescence = c(1110-5, 860-10)

df = data.frame(gold.standard, fluorescence)
df

rownames(df) = c("positive", "negative")
df

#--- transpose, i --> j and j -->i
df = t(df)
df

chisq.test(df)


