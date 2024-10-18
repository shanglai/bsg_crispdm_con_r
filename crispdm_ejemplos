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



# 1. Entendimiento Negocio
# https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database

# Cargar el dataset
data("Pima.tr") # Parte del paquete MASS

# 2. Exploración de los datos
str(Pima.tr)
summary(Pima.tr)
skim(Pima.tr)
pima2 <- Pima.tr %>% mutate(type=ifelse(type=='Yes',1,0)) %>% data.frame()
summary(pima2)
GGally::ggpairs(Pima.tr)
GGally::ggpairs(Pima.tr,mapping=aes(color=type))
dfSummary(Pima.tr) %>% stview()


# 3. Preparación

# Dividir los datos en conjunto de entrenamiento (70%) y prueba (30%)
# ???Validación?

set.seed(123)
trainIndex <- createDataPartition(Pima.tr$type, p = 0.7, list = FALSE)
trainData <- Pima.tr[trainIndex,]
testData <- Pima.tr[-trainIndex,]

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






# Cargar dataset iris
data("iris")

# Exploración de los datos
str(iris)
summary(iris)
# Escalar los datos
iris_scaled <- scale(iris[, -5])
# Aplicar K-means con 3 clusters (sabemos que hay 3 especies)
set.seed(123)
kmeans_model <- kmeans(iris_scaled, centers = 3)

# Revisar los resultados
kmeans_model$cluster
table(kmeans_model$cluster, iris$Species)

# Visualizar los clusters
fviz_cluster(kmeans_model, data = iris_scaled, geom = "point")

