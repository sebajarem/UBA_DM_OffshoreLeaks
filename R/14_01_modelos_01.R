# levanta los grafos seleccionados y hace mineria
# de a uno porque explota sino



#-----------
# limpieza de entorno
#-----------
# limpia todas las variables del entorno y libera la memoria
rm(list=ls())
gc()

# incluyo archivo con defines y funciones
source("./R/00_Defines_VariablesGlobales_Universos.R")

library(glue)
library(doMC)
library(caret)

#######################
# funciones
##########################

metrica_sobre_lista <- function(l){
  
  df_aux = data.frame( name = rownames(as.data.frame(l$btw_estimado_cutoff)),
                       btw_estimado_cutoff = l$btw_estimado_cutoff,
                       closeness_centrality_estimado_cutoff = l$closeness_centrality_estimado_cutoff,
                       page_rank = l$page_rank$vector,
                       hub_score = l$hub_score$vector,
                       authority_score = l$authority_score$vector,
                       eigen_centrality = l$eigen_centrality$vector,
                       grado = l$grado,
                       transitivity = l$transitivity,
                       assortivity_comunidad = l$assort,
                       densidad_comunidad = l$densidad_comunidad
  )
  
  return(df_aux)
  
}


modelos_estandar = function(df, n, g_com){
  
  # del nodo principal me quedo con el id y la clase
  n =
    n %>%
    select(name, rol_nodo)
  
  # del grafo saco la membresia
  mem =
    data_frame(
      membership = g_com$membership,
      name = g_com$names
    )
  
  # uno todo en un dataframe
  df = 
    df %>%
    inner_join(n, by = "name") %>%
    inner_join(mem, by = "name")
  
  # hay nulos en transitivity
  # sapply(df %>% select(-transitivity), anyNA) 
  # 
  # table(df$transitivity)
  
  df_random <- df[sample(1:nrow(df)), ]
  df_random = as.data.frame(df_random)
  # particion de data
  
  X = df_random %>%
    select(-rol_nodo)
  
  
  
  X = as.data.frame(X)
  Y = df_random %>% select(rol_nodo)
  Y = as.data.frame(Y)
  Y$rol_nodo = as.factor(Y$rol_nodo)
  
  df_random$rol_nodo = as.factor(df_random$rol_nodo)
  
  indices_train <- createDataPartition(df_random$rol_nodo, p = 0.75, list = FALSE)
  
  X_train <- X[indices_train, ]
  X_train =
    X_train %>%
    select(-name)
  X_test <- X[-indices_train, ]
  X_test =
    X_test %>%
    select(-name)
  Y_train <- Y[indices_train,]
  Y_test <- Y[-indices_train,]
  
  # Y_train = as.factor(Y_train$rol_nodo)
  # Y_test = as.factor(Y_test$rol_nodo)
  
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
  
  sapply(X_train, anyNA)
  # X_train = as.data.frame(X_train)
  X_train = X_train %>% select(-transitivity) # la eliin
  
  model_rf = train( x = X_train,
                    y = Y_train,
                    trControl = my_control,
                    method = "rf",
                    tuneLength = 5,
                    preProcess = c("knnImpute", "center", "scale" )) 
  # Y_train = as.data.frame(Y_train)
  # Y_train$Y_train = as.factor(Y_train$Y_train)
  model_knn = train(x = X_train,
                    y = Y_train,
                    trControl = my_control,
                    method = "knn",
                    tuneLength = 5,
                    preProcess = c("knnImpute","center", "scale"))
  
  model_list <- list("RF" = model_rf,
                     "KNN" = model_knn)
  
  
  ############
  # predicciones
  ###########
  X_test = X_test %>% select(-transitivity)
  
  pred_rf = predict.train(model_list$RF, newdata = X_test)
  pred_knn = predict.train(model_list$KNN, newdata = X_test)

  # resultado de predicciones
  confmatrix_rf = confusionMatrix(data = pred_rf, reference = Y_test)
  confmatrix_knn = confusionMatrix(data = pred_knn, reference = Y_test)
  
  table(pred_rf, Y_test)
  
  # importancia de variables
  
  # plot(varImp(model_list$RF))
  # plot(varImp(model_list$KNN))
  # 
  lista = list()
  
  lista_rf = list()
  lista_knn = list()
  
  lista_rf$modelo = model_rf
  lista_rf$x_train = X_train
  lista_rf$x_test = X_test
  lista_rf$y_train = Y_train
  lista_rf$y_test = Y_test
  lista_rf$prediccion = pred_rf
  lista_rf$confmatrix = confmatrix_rf
  
  lista_knn$modelo = model_knn
  lista_knn$x_train = X_train
  lista_knn$x_test = X_test
  lista_knn$y_train = Y_train
  lista_knn$y_test = Y_test
  lista_knn$prediccion = pred_knn
  lista_knn$confmatrix = confmatrix_knn
  
  lista$RF = lista_rf
  lista$KNN = lista_knn
  
  return(lista)
}





# ----------
# semilla
# ---------

set.seed(123456)

# dejo comentado el grafo inicial porque esta en el workspace
# ar02 = read_rds(path = "./salidas_datos/grafo_02/ar02.rds")
n02 = read_rds(path = "./salidas_datos/grafo_02/n02.rds")
# 
# 
# ar02 =
#   ar02 %>%
#   select(from, to, everything())
# 
n02 =
  n02 %>%
  select(name, everything())
# 
# g02 = igraph::graph_from_data_frame(d = ar02, vertices = n02, directed = F)

indices_seleccion = readr::read_rds("./salidas_datos/indices_seleccion.rds")
tamanios_seleccion = readr::read_rds("./salidas_datos/tamanios_seleccion.rds")






#########################
# mineria sobre dataframe
########################

###################
# pruebo con el indice solo de 216 que es chico
###################



l = list()
for(i in indices_seleccion){
  
  print(paste("empieza grafo numero ", i))
  
  # leo el grafo
  lista_g = readr::read_rds(glue("./salidas_datos/lista_de_grafos/grafo_{i}.rds"))
  
  
  print(paste("empieza louvain grafo numero ", i))
  # leo datasets
  df_louv = readr::read_rds(glue("./salidas_datos/tablon_de_grafos/grafo_{i}_louv.rds"))
  l_louv = modelos_estandar(df_louv, n02, lista_g$glouv)
  
  print(paste("empieza infomap grafo numero ", i))
  df_infomap = readr::read_rds(glue("./salidas_datos/tablon_de_grafos/grafo_{i}_infomap.rds"))
  l_infomap = modelos_estandar(df_infomap, n02, lista_g$ginfomap)
  
  print(paste("empieza fastgreedy grafo numero ", i))
  df_fastgreedy = readr::read_rds(glue("./salidas_datos/tablon_de_grafos/grafo_{i}_fastgreedy.rds"))
  l_fastgreedy = modelos_estandar(df_fastgreedy, n02, lista_g$gfastgreedy)
  
  # df_clustercountry = readr::read_rds(glue("./salidas_datos/tablon_de_grafos/grafo_{i}_clustercountry.rds"))
  # l_country = modelos_estandar(df_clustercountry, n02, lista_g$g.cluster.country)
  
  # agrego los resultados a la lista de modelos seleccionados
  l[[i]] = list(l_louv, l_infomap, l_fastgreedy)
  
  print("guardo")
  
  saveRDS(l[[i]], glue("./salidas_datos/modelos/modelos_{i}.rds"))
    
}

print("guardo todos los modelos")

saveRDS(l, "./salidas_datos/modelos/todos_los_modelos_seleccionados.rds")

# res = caret::resamples(model_list)
# res$methods
# lattice::bwplot(resamples(model_list))
# lattice::dotplot(res)

