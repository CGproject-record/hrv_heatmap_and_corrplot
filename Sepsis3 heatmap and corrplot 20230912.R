

Y<-"sofa3m"
data <- read.csv('Sepsis_top20_data.csv',header = T)
data2 <- data[,-1]

library(stringr)
##====== corplot=======================================
data3<-data2
check<-nchar(names(data2))
names(data3)[check>11]<-c(
  "LF/HF.L.S.",
  "LF.Power.L.S",
  "M.F._c1",
  "VLF.P.L.S.",
  "PL.Slope.L.S"
  
)


# [2]<-"LF/HF.L.S."
library(corrplot)
M <- cor(data3)


# sort corrplot
orderind<-corrMatOrder(M, order = c("hclust"),
                       hclust.method = c("mcquitty"))

order_M<-M[orderind,orderind]
corrplot(order_M, method = "circle")

##====== corplot=======================================
data3<-data2
names(data3)[check>11]<-c(
  "LF/HF.L.S.",
  "LF.Power.L.S",
  "M.F._c1",
  "VLF.P.L.S.",
  "PL.Slope.L.S"
  
)

df <- as.matrix((scale(data3))) 
head(df)




library(dendextend)# order for rows
Rowv <- data3 %>% scale %>% dist('euclidean') %>% 
  hclust('mcquitty') %>% as.dendrogram %>%
  set("branches_k_color", k = 2) %>% 
  set("branches_lwd", 1.2) %>% ladderize# Order for columns# 



library(gplots)

heatmap.2(df, scale = "none", col = bluered(100), Rowv = Rowv, trace = "none", density.info = "none",cexCol =0.8)




