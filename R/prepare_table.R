#--- always begin setting you work folder ------
setwd("~/cursor/R")
# confirming
getwd()

file = "c:/equus/result/degs/LFC_5000_Equus_Melanoma_time_series_g2_6h_LRT.tsv"
df = read.csv(file, sep="\t", header = T)

head(df)

paste(colnames(df), collapse = ", ")
df2 = df[, c("symbol", "entrezid", "synonymous", "mean1", "mean2", "mean3")]
colnames(df2) = c("symbol", "entrezid", "synonymous", "CPM_CTRL", "CPM_STRESS1", "CPM_STRESS2")

head(df2)


fileExp = "../tables/expression.csv"
?write.table
write.table(df2, fileExp, sep="\t", quote = F, row.names = FALSE, col.names = TRUE)


df3 = read.csv(fileExp, sep="\t")
head(df3)