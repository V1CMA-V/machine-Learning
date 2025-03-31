# ---------------------------------------------
# CÓDIGO COMPLETO - EVALUACIÓN BOOTSTRAP CLUSTERS
# ---------------------------------------------

# Instalar paquetes si no los tienes
# install.packages("fpc")
# install.packages("cluster")

# 1. Cargar librerías
library(fpc)
library(cluster)

# 2. Leer el dataset
protein <- read.table("schoo/Bootstrap/protein.txt", header=TRUE, sep="\t")

# 3. Inspección inicial
summary(protein)

# 4. Seleccionar las variables numéricas (excluimos 'Country')
vars.to.use <- colnames(protein)[-1]

# 5. Escalar los datos (media 0 y varianza 1)
pmatrix <- scale(protein[, vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

# 6. Clustering jerárquico (Ward)
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D2")

# 7. Dendrograma
plot(pfit, labels=protein$Country, main="Dendrograma de paises por consumo de proteinas")
rect.hclust(pfit, k=5)  # Puedes cambiar k para probar

# 8. Etiquetas de clusters
groups <- cutree(pfit, k=5)  # Define número de clusters deseado

# 9. Función para imprimir países por cluster
print_clusters <- function(labels, k) {
  for (i in 1:k) {
    cat("\nCluster", i, ":\n")
    print(protein[labels==i, c("Country", "RedMeat", "Fish", "Fr.Veg")])
  }
}
print_clusters(groups, 5)

# ---------------------------------------------
# ----------- Bootstrap ----------------------
# ---------------------------------------------

# 10. Evaluación bootstrap de los clusters
set.seed(123)  # Para reproducibilidad
kbest.p <- 5   # Número de clusters propuesto

cboot.hclust <- clusterboot(pmatrix,
                            clustermethod = hclustCBI,
                            method = "ward.D2",
                            k = kbest.p)

# 11. Resultados bootstrap
cat("\nEstabilidad de los clusters (bootstrap):\n")
print(cboot.hclust$bootmean)

cat("\nNúmero de veces que se disolvió cada cluster:\n")
print(cboot.hclust$bootbrd)

# 12. Visualizar clusters finales
groups <- cboot.hclust$result$partition
print_clusters(groups, kbest.p)

# ---------------------------------------------
# Extra (opcional): Visualizar estabilidad
# ---------------------------------------------

# Gráfica rápida de estabilidad de los clusters
barplot(cboot.hclust$bootmean,
        names.arg=paste("Cluster", 1:kbest.p),
        ylim=c(0,1),
        main="Estabilidad de Clusters (Bootstrap)",
        ylab="Coeficiente de estabilidad")
abline(h=0.6, col="red", lty=2)
abline(h=0.75, col="orange", lty=2)
abline(h=0.85, col="darkgreen", lty=2)
legend("topright", legend=c("Bajo (<0.6)","Moderado (0.6-0.75)","Alto (>0.85)"),
       col=c("red","orange","darkgreen"), lty=2, cex=0.8)
