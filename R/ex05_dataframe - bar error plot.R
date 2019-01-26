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
source("myFunctions.R")

#------------------------------------------------------------.
#------------- HCS experiment -------------------------------
#------------------------------------------------------------.

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

ggplot(data=data, aes(class, val, fill=class)) + 
  geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 1, alpha=.4)
  # + coord_flip()

#--------------------------------------------.
#------ Transforming data for ggplot --------
#--------------------------------------------.


rows = rownames(df)
line = rows[1]

alfa = .025
z = qnorm(1-alfa)
z

colors = rainbow(length(rows))
dfParams = data.frame()

for (i in 1:length(rows)) {
  line = rows[i]
  x = as.numeric(df[i,])
  mu  = mean(x)
  sdv = sd(x)
  n = length(x)
  se = z * sdv/sqrt(n)
  
  ci = mu + c(-1,1) * se
  dfParams = rbind(dfParams, data.frame(line, mu, sdv, n, se,
                                        ciInf = ci[1], ciSup =ci[2], 
                                        color = colors[i]) )
}

dfParams
colnames(dfParams)

plotTop = max(dfParams$ciSup)

par(mfrow=c(1,1), mar=c(bottom=2.3,left=2,top=2,right=1))

p1 <- barplot(height = dfParams$mu,
                      names.arg = dfParams$line,
                      beside = true, las = 2,
                      ylim = c(0, plotTop),
                      cex.names = 0.75, xaxt = "n",
                      main = "High content screening experiment",
                      ylab = "Fluorescence",
                      border = "black", axes = TRUE, col = colors)


# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = p1, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = dfParams$line, xpd = TRUE)

segments(p1, dfParams$ciInf, p1, dfParams$ciSup, lwd = 1.5)

arrows(p1, dfParams$ciInf, p1, dfParams$ciSup, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)


#-------------------------------------------------------

ggplot(dfParams, aes(x=line, y=mu, fill=color)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=ciInf, ymax=ciSup),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(legend.position="none")


#------------------------------------------------------

p1 <- ggplot(dfParams, aes(x=line, y=mu, fill=color)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=ciInf, ymax=ciSup),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
      scale_y_continuous(limits = c(0, 1200)) +
      theme(legend.position="none")


p2 <- ggplot(data=data, aes(class, val, fill=class)) + 
      geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 1, alpha=.4) +
      scale_y_continuous(limits = c(0, 1200)) +
      scale_fill_manual(values = colors) + 
      theme(legend.position="none")
  

multiplot(p1,p2,layout=matrix(c(1,2),ncol=2, byrow=T))



#----- ANOVA again ---------------------


data = data.frame()
for (i in 1:(nrow(df)-2)) {
  v = as.numeric(df[i,])
  dfClasse = data.frame(val = v, class = rownames(df)[i])
  data = rbind(data, dfClasse)
}

fit = lm(val ~ class, data)

anova(fit)
