rm(list=ls())

# Carregando base de dados:
path <- file.path("~/Documents/UFMG/9/Redes Neurais/TP1/prediction-competition-accent-recognition/databases", "treino_plot.csv")
data_train <- read.csv(path)

for (i in 1:53) {
  if (data_train$y[i] == -1){
    data_train$y[i] = 1
  }
  else{
    data_train$y[i] = 2
  }
}

plot(data_train, col=c("red", "blue")[unclass(data_train$y)])