

library(caret)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(caretEnsemble)
# library(doParallel) # windows
library(doMC)
library(plyr)
library(broom)
library(purrr)


set.seed(123)

data(iris)

glimpse(iris)

corrplot(cor(iris[,1:4]), method = "square")

anyNA(iris)

boxplot(iris[-5])

# poner data random

iris_random <- iris[sample(1:nrow(iris)), ]

# particion de data

X = iris_random[, -5]
y = iris_random[, 5]

part.index <- createDataPartition(iris_random$Species, p = 0.75, list = FALSE)

X_train <- X[part.index, ]
X_test <- X[-part.index, ]
y_train <- y[part.index]
y_test <- y[-part.index]

# procesamiento paralelo
# registerDoParallel(2) # 2 cores # windows
# getDoParWorkers() # windows
# registerDoMC(cores = 2) # linux
registerDoMC(cores = parallel::detectCores()) # linux # para trabajar con todos los cores

# control de modelos

set.seed(123)
my_control = trainControl(method = "cv",
                          number = 5,
                          savePredictions = "final",
                          classProbs = TRUE,
                          allowParallel = TRUE
                          )

model_list = caretList(X_train,
                       y_train,
                       trControl = my_control,
                       methodList = c("rf", "rda"),
                       tuneList = NULL,
                       continue_on_fail = FALSE, # para si sale algo mal
                       preProcess = c("center", "scale")
)

# model_list$rf
# model_list$xgbTree

# resultados de los modelos
model_results <- data.frame(RF = max(model_list$rf$results$Accuracy),
                            RNG = max(model_list$ranger$results$Accuracy),
                            RDA = max(model_list$rda$results$Accuracy))

print(model_results)


# ver los resultados de los modelos e intervalos de confianza

res = caret::resamples(model_list)
res$methods
lattice::bwplot(resamples(model_list))
lattice::dotplot(res)


# ensemble (error para multiclass)
# ensemble1 = caretEnsemble(model_list, metric = "kappa", trControl = my_control)

# intento de ensemble 2
# ensemble2 = caretStack(model_list, method = "glmnet", metric = "Kappa", trControl = my_control)


############
# predicciones
###########

pred_rf = predict.train(model_list$rf, newdata = X_test)
pred_ranger = predict.train(model_list$ranger, newdata = X_test)
pred_rda = predict.train(model_list$rda, newdata = X_test)

# resultado de predicciones
confusionMatrix(data = pred_rf, reference = y_test)

table(pred_rf, y_test)

# importancia de variables

plot(varImp(model_list$rf))

# multiClassSummary(data = X_test, lev = y_test)



#############################
# probamos aplicar caret con modelr
#############################

library(tidyr)

iris1 = iris
iris2 = iris

df = data.frame(rbind(cbind(iris, df_num = 1),
                      cbind(iris, df_num = 2)))

df = df %>%
  select(df_num, everything())

by_df_num =
  df %>%
  group_by(df_num) %>%
  tidyr::nest()


modelo = function(df){
  
  iris = df
  
  iris_random <- iris[sample(1:nrow(iris)), ]
  
  # particion de data
  
  X = iris_random[, -5]
  y = iris_random[, 5]
  
  part.index <- createDataPartition(iris_random$Species, p = 0.75, list = FALSE)
  
  X_train <- X[part.index, ]
  X_test <- X[-part.index, ]
  y_train <- y[part.index]
  y_test <- y[-part.index]
  
  # procesamiento paralelo
  # registerDoParallel(2) # 2 cores # windows
  # getDoParWorkers() # windows
  # registerDoMC(cores = 2) # linux
  registerDoMC(cores = parallel::detectCores()) # linux # para trabajar con todos los cores
  
  # control de modelos
  
  set.seed(123)
  my_control = trainControl(method = "cv",
                            number = 5,
                            savePredictions = "final",
                            classProbs = TRUE,
                            allowParallel = TRUE
  )
  
  model_rf = train( x = X_train,
                    y = y_train,
                    trControl = my_control,
                    method = "rf",
                    tuneLength = 5,
                    preProcess = c("center", "scale")) 
  
  model_knn = train(x = X_train,
                    y = y_train,
                    trControl = my_control,
                    method = "knn",
                    tuneLength = 5,
                    preProcess = c("center", "scale"))
  
  model_list <- list("RF" = model_rf,
                    "KNN" = model_knn)
  
  
  return(model_list)
}



# aplico el modelo

by_df_num =
  by_df_num %>%
  mutate(
    modelo1 = map(by_df_num$data, modelo)
  )

modelo1 = map(  by_df_num$data, modelo)

m = modelo(iris)

by_df_num %>%
  mutate(
    a = m
  )
  
############################

rfMod <- train( x = X_train,
                y = y_train,
                method = "ranger",
                tuneLength = 2,
                num.trees = 5000,
                trControl = my_control)
