
#####################
# lee la lista de grafos (sin metricas en comunidad)
# se fija los tamaños de los grafos
# hace una seleccion al azar estratiicado por tamaños
# guarda los indices de los gafos
# lee cada uno de los grafos individuales
# los guarda en las listas por tamaño de cada uno
#######################


#-----------
# limpieza de entorno
#-----------
# limpia todas las variables del entorno y libera la memoria
rm(list=ls())
gc()

# incluyo archivo con defines y funciones
source("./R/00_Defines_VariablesGlobales_Universos.R")

library(glue)

# ----------
# semilla
# ---------

set.seed(123456)



# dejo comentado el grafo inicial porque esta en el workspace
# ar02 = read_rds(path = "./salidas_datos/grafo_02/ar02.rds")
# n02 = read_rds(path = "./salidas_datos/grafo_02/n02.rds")
# 
# 
# ar02 =
#   ar02 %>%
#   select(from, to, everything())
# 
# n02 =
#   n02 %>%
#   select(name, everything())
# 
# g02 = igraph::graph_from_data_frame(d = ar02, vertices = n02, directed = F)



##########################################
# me fijo cuantos grafos hizo hasta el momento
#######################################

lista_grafos = readr::read_rds("./salidas_datos/lista_grafos_sin_metricas_por_comunidades.rds")

indices_lista_no_nulos = which(!sapply(lista_grafos, is.null))

cant_grafos = length(indices_lista_no_nulos)

tamanio_grafos_con_metricas = vector(mode = "integer")
for (i in indices_lista_no_nulos) {
  tamanio_grafos_con_metricas = c(tamanio_grafos_con_metricas, lista_grafos[[i]]$vN)
}

print(paste("cantidad de subgrafos con metricas: ",length(tamanio_grafos_con_metricas)))

table(tamanio_grafos_con_metricas)

#######################
# elijo indices con los que se van a trabajar
# las bajadas de dataframes no terminaron todas
# llego ashat el numero 1354, asique los indices que se elijan tienen que ser menores a ese numero para saber que tenemso toda la info
#######################

indices_tamanio_100_500 = indices_lista_no_nulos[which(tamanio_grafos_con_metricas > 100 & tamanio_grafos_con_metricas < 500)]
indices_tamanio_100_500 = indices_tamanio_100_500[indices_tamanio_100_500 < 1355]
length(indices_tamanio_100_500)

indices_tamanio_500_10000 = indices_lista_no_nulos[which(tamanio_grafos_con_metricas > 500 & tamanio_grafos_con_metricas < 10000)]
indices_tamanio_500_10000 = indices_tamanio_500_10000[indices_tamanio_500_10000 < 1355]
length(indices_tamanio_500_10000)

indices_tamanio_10000_mas = indices_lista_no_nulos[which(tamanio_grafos_con_metricas > 10000)]
indices_tamanio_10000_mas = indices_tamanio_10000_mas[indices_tamanio_10000_mas < 1355]
length(indices_tamanio_10000_mas)

tamanio_grafos <- function(lista, indices){
  
  tamanio = vector(mode = "integer")
  for (i in indices) {
    tamanio = c(tamanio, lista[[i]]$vN)
  }
  return(tamanio)
}


# eleccion al azar
tam_pequenios = tamanio_grafos( lista = lista_grafos, indices = indices_tamanio_100_500)
tam_mediano = tamanio_grafos( lista = lista_grafos, indices = indices_tamanio_500_10000)
tam_grande = tamanio_grafos( lista = lista_grafos, indices = indices_tamanio_10000_mas)

# > indices_tamanio_100_500
# [1]  216 1354
# > indices_tamanio_500_10000
# [1] 1251 1337
# > indices_tamanio_10000_mas
# [1]    1 1157

# > tam_pequenios
# [1] 102 487
# > tam_mediano
# [1] 528 599
# > tam_grande
# [1] 216139 310675

indices_seleccion = c(indices_tamanio_100_500, indices_tamanio_500_10000, indices_tamanio_10000_mas)
tamanios_seleccion = c(tam_pequenios, tam_mediano, tam_grande)

# comento lo siguiente porque explota por falta de ram
# lista_grafos_seleccion = list()
# for(i in indices_seleccion){
#   
#   lista_grafos_seleccion[[i]] = readr::read_rds(glue("./salidas_datos/lista_de_grafos/grafo_{i}.rds"))
# }
# 
# indices_seleccion = which(!sapply(lista_grafos_seleccion, is.null))

# grabo la lista reducida
# saveRDS(lista_grafos_seleccion, "./salidas_datos/lista_grafos_seleccion.rds")

# grabo los indices
saveRDS(indices_seleccion, "./salidas_datos/indices_seleccion.rds")
saveRDS(tamanios_seleccion, "./salidas_datos/tamanios_seleccion.rds")
