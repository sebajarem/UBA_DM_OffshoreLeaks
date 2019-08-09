# levanta los resultados de los modelos
# hace graficos para informa

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
library(cowplot)

indices_seleccion = readr::read_rds("./salidas_datos/indices_seleccion.rds")
tamanios_seleccion = readr::read_rds("./salidas_datos/tamanios_seleccion.rds")


for(i in indices_seleccion){
  
  l = readr::read_rds(glue("./salidas_datos/modelos/modelos_{i}.rds"))
  
  # armo el model_list en vez de ser modelos son las comunidades
  model_list = list(
    RF_louv = l[[1]]$RF$modelo,
    RF_infomap = l[[2]]$RF$modelo,
    RF_fastgreedy = l[[3]]$RF$modelo)
  
  res = caret::resamples(model_list)
  res$methods
  lattice::bwplot(resamples(model_list))
  lattice::dotplot(resamples(model_list))
  
  a = data.frame( model = "RF_louv", results = l[[1]]$RF$modelo$results)
  b = data.frame( model = "RF_infomap", results = l[[2]]$RF$modelo$results)
  e = data.frame( model = "RF_fastgreedy", results = l[[3]]$RF$modelo$results)
  c = rbind(a, b, e)
  
  lattice::bwplot( results.Accuracy~model, c)
  
  featurePlot(x = c[,3:4], 
              y = c$model, 
              plot = "box",
              horizontal = F,
              ## Pass in options to bwplot() 
              scales = list(y = list( rot = 90),
                            x = list(rot = 90)),  
              # layout = c(4,1 ), 
              auto.key = list(columns = 2))
  
  
  # lattice::bwplot( as.data.frame(RF_louv = l[[1]]$RF$modelo$results,
  #                                RF_infomap = l[[2]]$RF$modelo$results),
  #                  c(0,0.3,0.6,1), 
  #                  auto.key = TRUE)
  
  lattice::bwplot(l[[1]]$RF$modelo$results)
  lattice::bwplot(l[[2]]$RF$modelo$results)
  lattice::bwplot(l[[3]]$RF$modelo$results)
  
  plot_grid(
    plot(varImp(model_list$RF_louv)),
    plot(varImp(model_list$RF_infomap)),
    plot(varImp(model_list$RF_fastgreedy)),
    nrow = 3,
    labels = c("Louvain", "Infomap", "fastgreedy")
  )
  
  l[[1]]$RF$confmatrix # louv
  l[[2]]$RF$confmatrix # infomap
  l[[3]]$RF$confmatrix # fastgreedy


}

saveRDS(l[[i]], glue("./salidas_datos/modelos/modelos_{i}.rds"))

# plot(varImp(model_list$RF))
# plot(varImp(model_list$KNN))

# res = caret::resamples(model_list)
# res$methods
# lattice::bwplot(resamples(model_list))
# lattice::dotplot(res)



model_list = list(
  RF_louv = l[[1]]$RF$modelo,
  RF_infomap = l[[2]]$RF$modelo,
  RF_fastgreedy = l[[3]]$RF$modelo)

res = caret::resamples(model_list)
res$methods
lattice::bwplot(resamples(model_list))
lattice::dotplot(resamples(model_list))

plot_grid(
  plot(varImp(model_list$RF_louv), ),
  plot(varImp(model_list$RF_infomap)),
  # plot(varImp(model_list$RF_fastgreedy)),
  nrow = 3,
  labels = c("Louvain", "Infomap", "fastgreedy")
)
