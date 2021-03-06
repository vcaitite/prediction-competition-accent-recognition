rm(list=ls())
source("~/Documents/UFMG/9/Redes Neurais/TP1/prediction-competition-accent-recognition/ELM/trainELM.R")
source("~/Documents/UFMG/9/Redes Neurais/TP1/prediction-competition-accent-recognition/ELM/YELM.R")
source("~/Documents/UFMG/9/Redes Neurais/exemplos/escalonamento_matrix.R")
library(caret)

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

p <- 20 # número de neurônios
executions <- 31
results <- matrix(nrow = nrow(x_test), ncol = executions)
for (index in 1:executions){
  # Treinando modelo:
  retlist<-trainELM(x_train, y_train, p, 1)
  W<-retlist[[1]]
  H<-retlist[[2]]
  Z<-retlist[[3]]
  
  # Calculando acurácia de treinamento
  length_train <- length(y_train)
  y_hat_train <- as.matrix(YELM(x_train, Z, W, 1), nrow = length_train, ncol = 1)
  accuracy_train<-((sum(abs(y_hat_train + y_train)))/2)/length_train
  #print(accuracy_train)
  
  # Rodando dados de teste:
  y_hat_test <- as.matrix(YELM(x_test, Z, W, 1), nrow = length_test, ncol = 1)
  results[,index] <- y_hat_test
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

Id <- 54:75
table <- data.frame(Id, y)
write.csv(table, "prediction_eml20.csv", row.names = FALSE)

