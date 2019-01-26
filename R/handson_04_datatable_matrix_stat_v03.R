#-------------------------------------------------.
# @author: Flavio Lichtenstein
# @data update:   2017/04/26
# @data created:  2017/04/24
# @local: IBiochemistry and Biophysics Laboratory / Butantan Institute
#-------------------------------------------------.


#--- always begin setting you work folder ------
setwd("~/cursor/R")
# confirming
getwd()

# https://rstudio-pubs-static.s3.amazonaws.com/150253_2ebf6860e6324d8f9aec3f49a2bf405e.html
#--- reaing a table remotely -----------

url = "http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter04/chap04e1HumanGeneLengths.csv"

humanGeneLengths <- read.csv(url)
head(humanGeneLengths)
dim(humanGeneLengths)
colnames(humanGeneLengths)
class(humanGeneLengths)

hist(humanGeneLengths$geneLength, 
     # breaks = seq(0,15000,500), 
     xlab = "Gene length (Number of nucleotides)", 
     ylab = "Frequency", 
     col = "lightblue", 
     las = 1, 
     main = "", 
     right = FALSE)

genes100 = as.numeric(humanGeneLengths$geneLength)
genes100 = genes100[genes100 >= 100]
genes100 = genes100[genes100 <  10^4]


hist(genes100, 
     breaks = seq(0,10^5,500), 
     xlab = "Gene length (Number of nucleotides)", 
     ylab = "Frequency", 
     col = "lightblue", 
     las = 1, 
     main = "", 
     right = FALSE)


genesLLog = log2(as.numeric(humanGeneLengths$geneLength))
hist(genesLLog, freq=F,
     #breaks = seq(0,10^5,500), 
     xlab = "Gene length (Number of nucleotides)", 
     ylab = "Frequency", 
     col = "lightblue", 
     las = 1, 
     main = "I'm a log normal distribution", 
     right = FALSE)

boxplot(genesLLog)

#------------------------------------------------------------

nucs = c("A", "C", "G", "T")

oligo = rep(nucs, 1000)
oligo

oligo[1:40]

L = length(oligo)
L

?sample
lGene = L/4
samps = sample(1:L, lGene ,replace=T) 

set.seed(36)
samps = sample(1:L, lGene ,replace=T) 
samps

gene = oligo[samps]

length(gene)
gene

gene
grep("A", gene)

percA = length(grep("A", gene)) / lGene
percC = length(grep("C", gene)) / lGene
percG = length(grep("G", gene)) / lGene
percT = length(grep("T", gene)) / lGene

v = c(percA, percC, percG, percT)
v
names(v) = c("A", "C", "G", "T")
v
barplot(v)


dev.off()


1:12
seq(1,12)
seq(1,12, 3)
letters
letters[1:5]

for (i in 1:12) {
  print(i)
}

barplot(v)

#------------ antropometic data -------------------

fileAnt = "../tables/antropometric.csv"
df = read.csv(fileAnt, sep=",")
head(df)
dim(df)
nrow(df)
ncol(df)
colnames(df)
rownames(df)

colnames(df) = c("age", "gender",  "weight",  "eye.color", "hair.color")
colnames(df)

df[ , 2]
table(df[ , 2])
df[ , "gender"]

colnames(df)
df[ , c("gender", "age", "hair.color")]

df[2, ]
df[2:5, ]

colnames(df)
colnames(df) == "weight"
which(colnames(df) == "weight")

df[ , colnames(df) == "weight"]

df[c(2,5),]

df[ ,c(1, length(colnames(df)))]

df[c(2:5),c(2,4)]


v = 1:10
v
v2 = c(1,2,3,4,5,6,7,8,9,10)
all.equal(v,v2)

length(v)
dim(df)

class(df)
df$gender
table(df$gender)

class(df$age)
class(df$eye.color)
class( as.character(df$hair.color))

#-- que que é isto ??? "factor"


#--- Eu não uso isto ! brrrr

df2 <- subset(df, (age > 16 & age <= 23) & (eye.color == "brown"),  select=c("age", "gender", "weight", "eye.color"))
df2

#--- fantastic ----

aggregate(cbind(age, weight) ~ gender, data=df, FUN=mean)


#--- selecting fields ---
df

df2 = df[, c("age", "gender", "weight")]
dim(df2)
head(df2)

df2 = df[3:7, c("age", "gender", "weight")]
dim(df2)
df2

rownames(df2)
rownames(df2) = 1:5
rownames(df2)
df2

cols = colnames(df) %in% c("age", "weight")
cols
cols = which(colnames(df) %in% c("age", "weight"))
cols

df2 = df[, cols]
dim(df2)
head(df2)


df2 = df[df$age >= 23, cols]
dim(df2)
df2
hist(df2$age, breaks=seq(23,25,.5))


df2 = df[ (df$age > 16 & df$age <= 23), cols]
dim(df2)
head(df2)

df2 = df[ (df$age < 20 & df$age >= 23) & (df$eye.color == "brown"), cols]
dim(df2)
head(df2)


df
df2 = df[order(df$age), ]
df2

df2 = df2[order(-df2$age), ]
df2


#----------- transcriptome --------------

fileExp = "../tables/expression.csv"
df = read.csv(fileExp, sep="\t")
dim(df)
colnames(df)
head(df)
tail(df)

df2 = df[order(df$CPM_STRESS1), ]

df2[1:5, c(1, 4,5,6)]

rownames(df2) = NULL
df2[1:15, c(1, 4,5,6)]



rownames(df2) = df2$symbol
df2[1:4, 1:3]
df2 = df2[ , -c(1)]
df2[1:5, 1:3]
df2 = df2[ , -c(1,2)]
df2[1:5, 1:3]


rownames(df2)[30]
vGene = c("PER3", "PITPNM2", "DLX5")
vColor = c("AMARELO", "VERDE", "AZUL")

dfNew = data.frame(vGene, vColor)
dfNew
rownames(dfNew) = dfNew$vGene
dfNew

dfMagic = cbind(df2, dfNew[rownames(df2),])
dfMagic[1:35, ]

dim(df)
nrow(df)
ncol(df)


#--------------------------------

1320 +  650 - 1110

gold.standard = c(1320,  650)
fluorescence = c(1110-5, 860-10)

df = data.frame(gold.standard, fluorescence)
df

rownames(df) = c("positive", "negative")
df

chisq.test(df)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  df
# X-squared = 45.229, df = 1, p-value = 1.753e-11

# H0 - ambas as distribuições são similares, logo meu experimento mede bem o fenomeno proposto
# H0 - ambas as distribuições são diferentes, logo meu experimento não mede bem o fenomeno proposto

# Qual foi o resultado? Sua técnica de fluorescência é boa?
# Posso usar o teste chi-quadrado? Porque?


#----------- segundo experimento -----------------

filename = "../tables/xxxxx.csv"
df = read.csv(filename, sep="\t")
df

dim(df)
ncol(df)
nrow(df)

mean(df$X1)
sd(df$X1)
hist(df$X1)

mean(df[1,])
df[1:3, 1:5]


rownames(df) = df$X
df
df = df[ , -c(1)]
df
dim(df)

par(mfrow=c(3,4))

for (i in 1:ncol(df)) {
  mu = mean(df[,i])
  sdv = sd(df[,i])
  print(paste(i,") mu ", mu, " sdv", sdv, sep=""))
  hist(df[,i], main=as.character(i), cex=2)
}

mean(df[1,])
#--- deu erro ?


mean(df[1,])
# ???? porque ???
mean(as.numeric(df[1,]))

mat = as.matrix(df)

par(mfrow=c(1,1))

mean(mat[1,])
sd(mat[1,])
hist(mat[1,])

#-- quero comparar grupos ??? ANOVA
# transformar numa matriz em que cada linha é uma CLASSE e os X são suas medidas para diversas condições (p.ex.)

data = data.frame()
for (i in 1:nrow(df)) {
  v = as.numeric(df[i,])
  dfClasse = data.frame(val = v, class = rownames(df)[i])
  data = rbind(data, dfClasse)
}

data

df[1:3, 1:5]
fit = lm(val ~ class, data)

anova(fit)
# manheee, como eu interpreto isto ????

boxplot(val ~ class, data)
  