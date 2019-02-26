# Pacotes ------------------------------------------------------------

library(readxl)
library(fastDummies)
library(xlsx)
library(stats)
library(graphics)
library(esquisse)
library(cluster)
library(stats)
library(heatmaply)
library(cluster)
library(factoextra)
library(DT)
library(corrplot)

# Entrada ROCHA NATUAL -------------------------------------------------------------
data <- as.data.frame(read_excel("Z:/Atividades/201902 - Clusterização de produtos/Bases_Estruturadas_Plano_Comercial_-_(QTZ,RN_-_CH,BL)__2019.xlsx", 
                                    range = "A1:M831", sheet = "Base RN"))
rownames(data) <- data$ID
aux <- data
data$ID <- NULL

data$`Máx. de PM ($/M2)` <- normalize(data$`Máx. de PM ($/M2)` ) #Normalizando o preço médio

## Se quiser entrar dados do clipboard
#data <- read.table(file = "clipboard", header=TRUE)

#ver data
#knitr::kable(data)

# Transformar em Dummies -------------------------------------------------
datac <- fastDummies::dummy_cols(data)
rownames(datac) <- aux$ID

# Salvando em Excel -----------------------------------------------------
#write.table(Dummie_results, sep = "\t")


# Cluster ---------------------------------------------------------------


#Retirar a colunas originais

datac$Material <- NULL
datac$Type <- NULL
datac$`Background Color` <- NULL
datac$Tonality <- NULL
datac$Moviment <- NULL
datac$Hardness <- NULL
datac$TERRITÓRIO <- NULL
datac$Região <- NULL
datac$Produto <- NULL
datac$Espessura <- NULL
datac$qualidade <- NULL
datac$`Máx. de PM ($/M2)` <- NULL



# Quantidade de Clusters ------------------------------------------------------

#' Compute Gower distance
gower_dist <- daisy(datac, metric = "euclidean")
gower_mat <- as.matrix(gower_dist)

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)


# Cluster K-means ------------------------------------------------------------

cl <- kmeans(datac, centers = 6, nstart = 10)

data <- cbind(data, cl$cluster)
write.table(data, "B_cluster.xls", sep="\t")

#esquisse::esquisser()

#matriz de distancias
distance <- get_dist(datac)
fviz_dist(distance, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),
          show_labels = TRUE,
          lab_size = 1)


datatable(data, options = list(pageLength = 500))



#corrplot(datac, method="color")

## Hierarchical Clustering #######

# Dissimilarity matrix
#d <- dist(datac, method = "euclidean")
# Hierarchical clustering using Complete Linkage
#hc1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
#plot(hc1, cex = 0.6, hang = -1)




