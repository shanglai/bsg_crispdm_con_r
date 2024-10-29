# Instalar las librerías necesarias
#install.packages("MASS") # Contiene el dataset
#install.packages("caret") # Para manejo de modelos
#install.packages("dplyr") # Para manejo de datos

#install.packages("skimr")
#install.packages("corrplot")
#install.packages("summarytools")
#install.packages("factoextra")

# Cargar librerías
library(MASS)
library(caret)
library(dplyr)
library(ggplot2)
library(GGally)

library(skimr)
#library(corrplot)
library(summarytools)
library(factoextra)


######## Caso 1


# 1. Entendimiento Negocio
# https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database

# Cargar el dataset
data("Pima.tr") # Parte del paquete MASS

# 2. Exploración de los datos
str(Pima.tr)
summary(Pima.tr)

hist(runif(300,0,1))
hist(rnorm(300,0,1))

x <- sort(sample(runif(300,0,1),20,replace=T))
sort(
  sample(
    runif(300,0,1),20,replace=T
    )
  )

runif(300,0,1) %>% sample(size=20,replace=T) %>% sort()

summary(x)
mean(x)
sum(x) / length(x)

head(Pima.tr)
skim(Pima.tr)
dim(Pima.tr)
pima2 <- Pima.tr %>% mutate(type=ifelse(type=='Yes',1,0)) %>% mutate(type=factor(type)) %>% data.frame()
summary(pima2)

Pima.tr %>% ggplot() + geom_boxplot(aes(x=type,y=npreg))
Pima.tr %>% ggplot() + geom_histogram(aes(x = npreg,fill= type,group=type)) + facet_grid(type ~ .)

GGally::ggpairs(Pima.tr)
GGally::ggpairs(Pima.tr,mapping=aes(color=type))
dfSummary(Pima.tr) %>% stview()


# 3. Preparación

# Dividir los datos en conjunto de entrenamiento (70%) y prueba (30%)
# ???Validación?

set.seed(123)
runif(3,0,1)


trainIndex <- createDataPartition(Pima.tr$type, p = 0.7, list = FALSE)
trainData <- Pima.tr[trainIndex,]
testData <- Pima.tr[-trainIndex,]
dim(Pima.tr)
dim(trainData)
dim(testData)

# Verificar la distribución
table(trainData$type)
table(testData$type)

# 4. Modelado
# Entrenar el modelo de regresión logística
model <- train(type ~ ., data = trainData, method = "glm", family = binomial())


# 5. Evaluación: Evaluar el modelo en los datos de prueba
predictions <- predict(model, newdata = testData)
confusionMatrix(predictions, testData$type)
# Matriz de confusión
confusionMatrix(predictions, testData$type)
#https://en.wikipedia.org/wiki/Sensitivity_and_specificity


# 6. Implementación



######## Caso 2
##### Coloca en los números las fases del método CRISP-DM al que pertenece.
# Iris Dataset

#1
# http://www.lac.inpe.br/~rafael.santos/Docs/CAP394/WholeStory-Iris.html#:~:text=The%20Iris%20Dataset%20contains%20four,model%20to%20classify%20the%20species.

#2
# Cargar dataset iris
data("iris")

# Exploración de los datos
str(iris)
summary(iris)
GGally::ggpairs(iris)
# Escalar los datos
#3
iris_scaled <- scale(iris[, -5])
# Aplicar K-means con 3 clusters (sabemos que hay 3 especies)
set.seed(123)
kmeans_model <- kmeans(iris_scaled, centers = 7)
#4
# Revisar los resultados
kmeans_model$cluster
table(kmeans_model$cluster, iris$Species)

library(factoextra)

#5
# Visualizar los clusters
fviz_cluster(kmeans_model, data = iris_scaled, geom = "point")

fviz_pca_ind(pca.1, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5) 

#6
