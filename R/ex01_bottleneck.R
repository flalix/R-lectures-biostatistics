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
#---------- Amostragem de nucleotídeos ----------------------
#------------------------------------------------------------.

nucs = c("A", "C", "G", "T")

oligo = rep(nucs, 1000)
oligo

oligo[1:40]

L = length(oligo)
L

lGene = L/4
samps = sample(1:L, lGene ,replace=T) 

gene = oligo[samps]

length(gene)
gene


percA = length(grep("A", gene)) / lGene
percC = length(grep("C", gene)) / lGene
percG = length(grep("G", gene)) / lGene
percT = length(grep("T", gene)) / lGene

v = c(percA, percC, percG, percT)
v
names(v) = c("A", "C", "G", "T")
v
barplot(v)

#--- simule várias vezes da linha 29 a 46, depois de correr tudo uma vez.

#------------------------------------------------------------.
#------------- Simulação e bottleneck -----------------------
#------------------------------------------------------------.

# ex01) como vimos podemos simular os nucleotídeos "randomicos", mas
# ao invés de nucleotídeos se estes fossem fenótipos (quaisquer)
# poderíamos simular um bottleneck. Veja o link e faça um simulação
# quando a população diminui bastante.


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

barplot( as.matrix(df), main="Phenotypic distribution", col=c("navy","red", "yellow"))
legend("bottomright",legend=rownames(df), fill=c("navy","red", "yellow"))


#-- Que dados existem em df? (data frame)


# ex01.b) Na linha 91 nós setamos uma amostragem de 900 indivíduos
# dos 1000 propostos nas linhas 65 a 68
#   Agora vá diminuindo gradativamente a população e veja o que acontece
#   com os fenótipos. Você observou que alguma população homozigota some?

# desafio02) Diminua bem a quantidade de heterozigotos, e simule
#            Agora, também diminua a população. 
# Será que você pode prever a extinsão desta população?
# Como e porque?



#-- dê uma olhada: https://en.wikipedia.org/wiki/Population_bottleneck
#-- https://pt.khanacademy.org/science/biology/her/heredity-and-genetics/v/hardy-weinberg


