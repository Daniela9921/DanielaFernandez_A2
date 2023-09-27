library(tidyverse)
library(ggplot2)

# to get current script folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)
# Accessing by file address
Social_Network_Ads <-
  read_csv("~/GUERRA/Social_Network_Ads.csv")
test_motion_data <- read_csv("~/GUERRA/Social_Network_Ads.csv")
View(Social_Network_Ads)


############### creacion de codigo xd

# Instala y carga las bibliotecas necesarias
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}

library(caret)
library(e1071)

# Función para realizar kNN con diferentes normalizaciones
perform_knn <- function(k_values, dataset) {
  results <- list()
  
  # Convertir la columna Purchased en un factor
  dataset$Purchased <- factor(dataset$Purchased, levels = c(0, 1), labels = c("No", "Sí"))
  
  for (k in k_values) {
    # Sin normalización
    knn_model <- train(Purchased ~ Age + EstimatedSalary, data = dataset, method = "knn", trControl = trainControl(method = "cv"), tuneGrid = data.frame(k = k))
    knn_predictions <- predict(knn_model, newdata = dataset)
    results[[paste("Original", k)]] <- knn_predictions
    
    # Normalización Min-Max
    min_max_dataset <- as.data.frame(scale(dataset[, c("Age", "EstimatedSalary")]))
    min_max_dataset$Purchased <- dataset$Purchased
    knn_model_min_max <- train(Purchased ~ Age + EstimatedSalary, data = min_max_dataset, method = "knn", trControl = trainControl(method = "cv"), tuneGrid = data.frame(k = k))
    knn_predictions_min_max <- predict(knn_model_min_max, newdata = min_max_dataset)
    results[[paste("MinMax", k)]] <- knn_predictions_min_max
  }
  
  return(results)
}

# Cargar el conjunto de datos (reemplaza 'tu_archivo.csv' con el nombre de archivo real)
data <- read.csv("~/GUERRA/Social_Network_Ads.csv")  # Ajusta la ruta según tu ubicación

# Define los valores de k a probar
k_values_to_test <- c(3, 5, 7)

# Obtener predicciones para los tres casos
predictions <- perform_knn(k_values_to_test, data)

# Obtener predicciones para los tres casos
predictions <- perform_knn(k_values_to_test, data)

# Gráfico de dispersión para las predicciones en el conjunto de entrenamiento (k=3)
# Cambia "Original 3" por el caso y el valor de k que desees visualizar
predicciones_k3 <- as.factor(predictions[["Original 3"]])

# Agregar las predicciones al conjunto de datos original
data$Predicciones <- predicciones_k3

# Crear un gráfico de dispersión para visualizar las predicciones en el conjunto de entrenamiento
ggplot(data, aes(x = Age, y = EstimatedSalary, color = Predicciones)) +
  geom_point() +
  scale_color_manual(values = c("No" = "red", "Sí" = "green")) +
  labs(title = "Gráfico de Dispersión de Predicciones (k=3)",
       x = "Edad", y = "Salario Estimado") +
  theme_minimal()

