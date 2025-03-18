library(mlr)
library(tidyverse)

# Cargando  y explorando el conjunto de datos GvHD
data(GvHD, package = "mclust")
gvhdTib <- as_tibble(GvHD.control)
gvhdTib %>% head()

gvhdScale <- gvhdTib %>% scale()

library(GGally)

ggpairs(GvHD.control,
        upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5)),
        diag = list(continuous = "densityDiag")) + theme_bw()

# Defininedo nuestra tarea y aprendizaje
gvhdTask <- makeClusterTask(data = as.data.frame(gvhdScale))

listLearners("cluster")$class

library(clue)
kMeans <- makeLearner("cluster.kmeans",
                      par.vals = list(iter.max = 100, nstart =10)
)

# Ejercicio 1: Ajuste de iter.max
kmeans200 <- makeLearner("cluster.kmeans",
                         par.vals = list(iter.max = 200, nstart = 10)
)
# -------------------------------------------------------------------

# Ajuste de K eleccion del algoritmo para nuestro modelo de K-medias

getParamSet(kMeans)

kMeansParamSpace <- makeParamSet(makeDiscreteParam("centers", values = 3:8),
                                 makeDiscreteParam("algorithm", values = c("Hartigan-Wong", "Lloyd", "MacQueen"))
)

gridSearch <- makeTuneControlGrid()
kFold <- makeResampleDesc("CV", iters = 10)

library(clusterSim)

tunedK <- tuneParams(kMeans, task = gvhdTask,
                     resampling = kFold,
                     par.set = kMeansParamSpace,
                     control = gridSearch,
                     measures = list(db, G1) # Nota importnate, en esta version no permite usar dunn
)

# Ejercicio 1: Ajuste de iter.max
tunedK200 <- tuneParams(kmeans200, task = gvhdTask,
                        resampling = kFold,
                        par.set = kMeansParamSpace,
                        control = gridSearch,
                        measures = list(db, G1)
)
# -------------------------------------------------------------------

kMeansTuningData <- generateHyperParsEffectData(tunedK)
kMeansTuningData$data

# Ejercicio 1: Ajuste de iter.max
kMeansTuningData200 <- generateHyperParsEffectData(tunedK200)
kMeansTuningData200$data
# -------------------------------------------------------------------

gatheredTuningData <- gather(kMeansTuningData$data,
                             key = "Metric",
                             value = "Value",
                             c(-centers, -iteration, -algorithm)
)

# Ejercicio 1: Ajuste de iter.max
gatheredTuningData200 <- gather(kMeansTuningData200$data,
                                key = "Metric",
                                value = "Value",
                                c(-centers, -iteration, -algorithm)
)
# -------------------------------------------------------------------

ggplot(gatheredTuningData, aes(centers, Value, col = algorithm)) +
  facet_wrap(~Metric, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Iter.max = 100")

# Ejercicio 1: Ajuste de iter.max
ggplot(gatheredTuningData200, aes(centers, Value, col = algorithm)) +
  facet_wrap(~Metric, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Iter.max = 200")
# -------------------------------------------------------------------

# Entrenamiento del modelo k-means final y optimizado

tunedKMeans <- setHyperPars(kMeans, par.vals = tunedK$x)
tunedKMeansModel <- train(tunedKMeans, gvhdTask)
kMeansModelData <- getLearnerModel(tunedKMeansModel)
kMeansModelData$iter

# Ejercicio 1: Ajuste de iter.max
tunedKMeans200 <- setHyperPars(kmeans200, par.vals = tunedK200$x)
tunedKMeansModel200 <- train(tunedKMeans200, gvhdTask)
kMeansModelData200 <- getLearnerModel(tunedKMeansModel200)
kMeansModelData200$iter
# -------------------------------------------------------------------

gvhdTib <- mutate(gvhdTib,
                  kMeansCluster = as.factor(kMeansModelData$cluster)
)

# Ejercicio 1: Ajuste de iter.max
gvhdTib <- mutate(gvhdTib,
                  kMeansCluster200 = as.factor(kMeansModelData200$cluster)
)
# -------------------------------------------------------------------
ggpairs(gvhdTib, aes(col = kMeansCluster),
        upper = list(continuous = "density")) +
  theme_bw()

# Ejercicio 1: Ajuste de iter.max
ggpairs(gvhdTib, aes(col = kMeansCluster200),
        upper = list(continuous = "density")) +
  theme_bw()
# -------------------------------------------------------------------

# Usando nuestro modelo para predecir clusteres de nuevos datos
newCell <- tibble(CD4 = 510,
                  CD8b = 26,
                  CD3 = 500,
                  CD8 = 122) %>%
  scale(center = attr(gvhdScale, "scaled:center"),
        scale = attr(gvhdScale, "scaled:scale")) %>%
  as_tibble()

predict(tunedKMeansModel, newdata = newCell)

# Ejercicio 2: Agrupa el conjunto de datos GvHD.pos de la misma manera que lo hicimos con el conjunto de datos GvHD.control

# Cargar datos
gvhdPosTib <- as_tibble(GvHD.pos)

# Escalar datos
gvhdPosScaled <- gvhdPosTib %>% scale()

ggpairs(gvhdPosScaled, upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5)),
        diag = list(continuous = "densityDiag")) + theme_bw()

# Crear tarea de clustering
gvhdPosTask <- makeClusterTask(data = as.data.frame(gvhdPosScaled))

# Definir el modelo K-Means
kMeans <- makeLearner("cluster.kmeans",
                      par.vals = list(iter.max = 200, nstart = 10))

# Ajustar hiperparámetros
kMeansParamSpace <- makeParamSet(
  makeDiscreteParam("centers", values = 3:8),
  makeDiscreteParam("algorithm", values = c("Hartigan-Wong", "Lloyd", "MacQueen"))
)

gridSearch <- makeTuneControlGrid()
kFold <- makeResampleDesc("CV", iters = 10)

tunedKPos <- tuneParams(kMeans, task = gvhdPosTask,
                        resampling = kFold,
                        par.set = kMeansParamSpace,
                        control = gridSearch,
                        measures = list(db, G1))

# Ver resultado del ajuste
tunedKPos

# Entrenar modelo con el número óptimo de clusters o definir manualmente
optimal_k <- tunedKPos$x$centers
tunedKMeansPos <- setHyperPars(kMeans, par.vals = list(centers = optimal_k))

tunedKMeansModelPos <- train(tunedKMeansPos, gvhdPosTask)
kMeansModelDataPos <- getLearnerModel(tunedKMeansModelPos)
kMeansModelDataPos$iter

# Asignar clusters al dataset original
gvhdPosTib <- mutate(gvhdPosTib,
                      kMeansCluster = as.factor(kMeansModelDataPos$cluster))

# Visualización de los clusters
ggpairs(gvhdPosTib, aes(col = kMeansCluster),
        upper = list(continuous = "density")) +
  theme_bw()

library(factoextra)
fviz_cluster(kmeans(gvhdScale, centers = tunedK$x$centers, nstart = 10), data = gvhdScale)

fviz_cluster(kmeans(gvhdPosScaled, centers = tunedKPos$x$centers, nstart = 10), data = gvhdPosScaled)



# Otro -------------------
# Probar con un número diferente de centros
manualCenters <- 7  # Cambia este valor
manualKMeans <- makeLearner("cluster.kmeans",
                            par.vals = list(centers = manualCenters,
                                            algorithm = "Hartigan-Wong",
                                            iter.max = 100,
                                            nstart = 10))

# Entrenar el modelo con el nuevo número de centros
manualKMeansModel <- train(manualKMeans, gvhdPosTask)
manualKMeansModelData <- getLearnerModel(manualKMeansModel)

# Evaluar el modelo
dbIndex <- index.DB(gvhdPosScaled, manualKMeansModelData$cluster)$DB
G1Index <- index.G1(gvhdPosScaled, manualKMeansModelData$cluster)

cat("Índice de Davies-Bouldin:", dbIndex, "\n")
cat("Pseudo estadística F (G1):", G1Index, "\n")

# Visualizar los resultados
gvhdPosTib <- mutate(gvhdPosTib, kMeansCluster = as.factor(manualKMeansModelData$cluster))
ggpairs(gvhdPosTib, aes(col = kMeansCluster),
        upper = list(continuous = "density")) +
  theme_bw()