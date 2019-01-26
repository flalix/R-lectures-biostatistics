#-------------------------------------------------.
# @author: Flavio Lichtenstein
# @data update:   2017/03/06
# @data created:  2016/07/20
# @local: IBiochemistry and Biophysics Laboratory / Butantan Institute
#-------------------------------------------------.

#----- where I am - directory - folder -----------------------
getwd()
setwd("~/cursor/R")
getwd()


#----- files: does it exists? -----------------------

dir()

file.exists("ex01_basic_functions.R")
file.exists("xxxxx.R")

#-------- directories -----------

#-- get work directory
getwd()

dir()

setwd("~")
getwd()

dir.exists("cursor")
dir.create("cursor")

my.dir = "~/cursor"
setwd(my.dir)
getwd()

dir.create("R")
dir.create("tables")
dir.create("results")

my.dir = "~/cursor/R"
setwd(my.dir)

getwd()
dir()

#----- internal table: mtcars -----------------------

?mtcars
library(datasets)
attach(mtcars)

?write.csv
write.csv(mtcars, "mtcars.csv")

getwd()
dir()

write.csv(mtcars, "../tables/mtcars.csv")
dir()
unlink("mtcars.csv")
dir("../tables")

#----- internal table: iris -----------------------

attach(iris)
head(iris)
tail(iris)

getwd()
write.csv(iris, "../tables/iris.csv")
dir("../tables")


iris$Species
table(iris$Species)

pairs(iris)
pairs(iris, col=iris$Species)


#----- global environments -----------------------

a = 1
b = 3
c = "my_chair"

ls()

rm(list=ls())

.libPaths()
sessionInfo()


Sys.getenv("PATH")
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "/home/flalix/R/x86_64-pc-linux-gnu-library/3.3/msa/tex/texshade.sty", sep=":"))
Sys.getenv("PATH")




