rm(list=ls())
source("~/Documents/UFMG/9/Redes Neurais/TP1/prediction-competition-accent-recognition/RBF_kmeans/trainRBF.R")
source("~/Documents/UFMG/9/Redes Neurais/TP1/prediction-competition-accent-recognition/RBF_kmeans/YRBF.R")
source("~/Documents/UFMG/9/Redes Neurais/exemplos/escalonamento_matrix.R")
library(caret)

correct <- c(1, 1, -1, -1, 1, 1, -1, 1, -1, 1, -1, -1, 1, -1, -1, 1, 1, 1, 1, 1, -1, -1)
# Carregando base de dados:
path <- file.path("~/Documents/UFMG/9/Redes Neurais/TP1/prediction-competition-accent-recognition/databases", "treino.csv")
data_train <- read.csv(path)
path <- file.path("~/Documents/UFMG/9/Redes Neurais/TP1/prediction-competition-accent-recognition/databases", "teste.csv")
data_test <- read.csv(path)

# Separando dados de entrada e saída e treino e teste:
x_train <- as.matrix(data_train[1:53, 2:13])
y_train <- as.matrix(data_train[1:53, 14])
x_test <- as.matrix(data_test[1:22, 2:13])

# Escalonando os valores dos atributos para que fiquem restritos entre 0 e 1
x_all <- rbind(x_train, x_test)
x_all <- staggeringMatrix(x_all, nrow(x_all), ncol(x_all))
x_train <- x_all[1:53, ]
x_test <- x_all[54:75, ]

p <- 5 # número de neurônios
executions <- 31
results <- matrix(nrow = nrow(x_test), ncol = executions)
for (index in 1:executions){
  # Treinando modelo:
  modRBF<-trainRBF(x_train, y_train, p)
  
  # Calculando acurácia de treinamento
  length_train <- length(y_train)
  y_hat_train <- as.matrix(YRBF(x_train, modRBF), nrow = length_train, ncol = 1)
  yt <- (1*(y_hat_train >= 0)-0.5)*2
  accuracy_train<-((sum(abs(yt + y_train)))/2)/length_train
  print(accuracy_train)
  
  # Rodando dados de teste:
  y_hat_test <- as.matrix(YRBF(x_test, modRBF), nrow = length_test, ncol = 1)
  yt <- (1*(y_hat_test >= 0)-0.5)*2
  results[,index] <- yt
  accuracy_test<-((sum(abs(yt + correct)))/2)/22
  print(accuracy_test)
}


y <- rep(0, 22)
for (index in 1:22) {
  if(sum(results[index,] == 1) > (executions/2)){
    y[index] <- 1
  }
  else{
    y[index] <- -1
  }
}
print(y-correct)
print(y)
accuracy_test<-((sum(abs(y + correct)))/2)/22
print(accuracy_test)

Id <- 54:75
table <- data.frame(Id, y)
write.csv(table, "prediction_rbf5.csv", row.names = FALSE)

