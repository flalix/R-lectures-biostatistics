#-------------------------------------------------.
# @author: Flavio Lichtenstein
# @data update:   2017/04/24
# @data created:  2017/04/24
# @local: IBiochemistry and Biophysics Laboratory / Butantan Institute
#-------------------------------------------------.


#------ Install packages from CRAN ----------------
?install.packages

.libPaths()

install.packages("igraph")
library(igraph)

install.packages("DBI")
install.packages("devtools", dependencies = TRUE)
install.packages("plotrix")

install.packages(c("rafalib", "VennDiagram"))
install.packages(c("Rmisc","plot3D"))

#------ Install packages  more ----------------
remove.packages("this.package")
update.packages()


#------ Install packages from Github ----------------
library(devtools)
install_github("dosorio/Peptides", force = TRUE)
install_github("genomicsclass/dagdata")

library(Peptides)
?Peptides
Peptides::mw
Peptides::charge()

?mw
?charge

#--- for windows ONLY -----------

# To build binary packages on windows, 
# Rtools (found at http://cran.r-project.org/bin/windows/Rtools/) needs to be on the path. 
# The default installation process does not add it, so this script finds 
# it (looking first on the path, then in the registry). 
# It also checks that the version of rtools matches the version of R.

# https://www.rdocumentation.org/packages/devtools/versions/1.9.1/topics/find_rtools

# You need devtools for this to work.

#-- only windows ----
library(devtools)
find_rtools(debug = FALSE)

#--- you need first to install gcc ---------
# see install gcc in external documentation ----

install.packages("gridExtra")
install.packages('ggplot2',dependencies = TRUE)

#--- try and put in memory some packages ------
library("gplots")




#------ Install packages in bioconductor ----------------

# This package is used to install and update Bioconductor, CRAN, and (some) github packages

# You can downgrade Bioconductor to version 3.0 with this command:
# install.packages("BiocInstaller", repos="http://bioconductor.org/packages/3.0/bioc")
# install.packages("BiocInstaller", repos="http://bioconductor.org/packages/3.1/bioc")
# install.packages("BiocInstaller", repos="http://bioconductor.org/packages/3.2/bioc")
install.packages("BiocInstaller", repos="http://bioconductor.org/packages/3.3/bioc")

source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
library(Biobase)




#--- Comandos de Manutenção
?BiocUpgrade
biocLite("BiocUpgrade")

library(BiocInstaller)
biocLite()
biocVersion()
biocValid()
update.packages()

biocLite("STRINGdb")
biocLite("mygene")
biocLite(c("affy", "limma"))
biocLite("IRanges")
biocLite("GenomicRanges")


#--- Homo sapiens
biocLite("org.Hs.eg.db")
#--- Mus musculus
biocLite("org.Mm.eg.db")


#---- very very far in a  URL ------

install.packages(("downloader"))
library(downloader)
url = "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"

filename = "../tables/femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)

getwd()
df = read.csv(filename)
head(df)

dim(df)
rownames(df)
colnames(df)

df$Bodyweight

colnames(df)
mua = mean(df$Bodyweight)
sda = sd(df$Bodyweight)

n = length(df$Bodyweight)
n
se = sda/sqrt(n)

mua
sda
n
se

hist(df$Bodyweight, freq=F, ylim=c(0,.5))
abline(v=mua, col="blue")
abline(v=mua+c(-1,1)*sda, col="red")
abline(v=mua+2*c(-1,1)*se, col="green", lwd=2)


x=seq(15,35,.1)
y<-dnorm(x, mean=mua, sd=sda)
lines(x,y, col="blue", lwd=2)

yPrecisao <- dnorm(x, mean=mua, sd=se)
lines(x,yPrecisao, col="purple", lwd=2)

yPrecisao <- dnorm(x, mean=mua, sd=1)
lines(x,yPrecisao, col="orange", lwd=2)


# 23.9 (3.4), n = 225

#-- IC - 95% confiabilidade

ic = mua + c(-1, 1) * 2 * se
ic = round(ic, 1)
ic

filename = "ex01_basic_functions_URL.R"
url = "https://drive.google.com/drive/folders/0B_NwG-08qD7NT3QwSmZGblE2Ulk/ex01_basic_functions.R"
if (!file.exists(filename)) download(url,destfile=filename)


#--------------- testing packages ----------------------
install.packages("RUnit")
biocLite("bsseq")
library(bsseq)

require("bsseq") || stop("unable to load bsseq")
BiocGenerics:::testPackage("bsseq")






