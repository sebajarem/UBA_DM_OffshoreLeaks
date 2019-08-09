
#####################
# lee grafos de a 1 para armar tablon
# 1 tablon por cada tipo de comunidad
#######################


#-----------
# limpieza de entorno
#-----------
# limpia todas las variables del entorno y libera la memoria
rm(list=ls())
gc()

# incluyo archivo con defines y funciones
source("./R/00_Defines_VariablesGlobales_Universos.R")
# source("./R/funciones/metricas_sobre_grafo.R")

library(glue)

# ----------
# semilla
# ---------

set.seed(123456)


#############################
# funciones
##########################
metricas_sobre_grafo <- function(g, cutoff){
  
  # lsita donde se van a guardar todos los resultados
  lista_datos = list()
  
  lista_datos$grafo = g
  
  lista_datos$cutoff = cutoff
  
  # betweenness estimado
  tbtw_cutoff_inicio = now()
  btw_estimado_cutoff = estimate_betweenness( graph = g, vids = V(g), directed = FALSE, cutoff = cutoff)
  lista_datos$btw_estimado_cutoff = btw_estimado_cutoff
  tbtw_cutoff_fin = now()
  lista_datos$tiempo_btw_estimado_cutoff = difftime(tbtw_cutoff_fin, tbtw_cutoff_inicio, units = "mins")
  print(paste("btw cutoff", cutoff, "tarda (minutos): ", lista_datos$tiempo_btw_estimado_cutoff))
  # calculo closeness centrality
  tcloseness_cutoff_inicio = now()
  closeness_centrality_estimado_cutoff = estimate_closeness(graph = g, vids = V(g), cutoff = cutoff)
  lista_datos$closeness_centrality_estimado_cutoff = closeness_centrality_estimado_cutoff
  tcloseness_cutoff_fin = now()
  lista_datos$tiempo_closeness_estimado_cutoff = difftime(tcloseness_cutoff_fin, tcloseness_cutoff_inicio, units = "mins")
  print(paste("btw cutoff", cutoff, "tarda (minutos): ", lista_datos$tiempo_closeness_estimado_cutoff))
  # pagerank
  page_rank = page_rank(graph = g, algo = "prpack", vids = V(g), directed = FALSE)
  lista_datos$page_rank = page_rank
  # hub_score
  hub_score = hub_score(graph = g)
  lista_datos$hub_score = hub_score
  # authority_score
  authority_score = authority_score(graph = g)
  lista_datos$authority_score = authority_score
  # eigen_centrality
  eigen_centrality = eigen_centrality( graph = g, directed = FALSE)
  lista_datos$eigen_centrality = eigen_centrality
  #power_centrality
  #power_centrality = power_centrality(graph = g, nodes = V(g), loops = FALSE)
  # transitividad
  transitivity = transitivity(graph = g, type = "local", vids = V(g) )
  lista_datos$transitivity = transitivity
  # grado
  grado = degree(graph = g, v = V(g), mode = "all", loops = FALSE )
  lista_datos$grado = grado
  # assortivity
  assort = assortativity_degree(graph = g, directed = FALSE)
  lista_datos$assort = assort
  # densidad de la comunidad
  densidad_comunidad = igraph::graph.density(graph = g, loops = FALSE)
  lista_datos$densidad_comunidad = densidad_comunidad
  # caminos todos contra todos
  # dist = igraph::distances(graph = g, v = V(g)$name, to = V(g)$name, mode = "all")
  # lista_datos$dist = dist
  # los apoc quedan en las columnas y los no apoc en las filas
  
  return(lista_datos)
  
  
}


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


#####################
# fin de funciones
########################3


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
# me fijo cuantos grafos hizo hasta el momento en el script que calcula metricas y comunidades
# sin pagerank ni bwtweenness
#######################################


# cant_grafos_con_mericas = length(lista_grafos)
# 
# tamanio_grafos_con_metricas = vector(mode = "integer")
# for (i in 1:cant_grafos_con_mericas) {
#   
#   tamanio_grafos_con_metricas = c(tamanio_grafos_con_metricas, lista_grafos[[i]]$vN)
#   
# }
# 
# print(paste("cantidad de subgrafos con metricas: ",length(tamanio_grafos_con_metricas)))
# 
# hist(tamanio_grafos_con_metricas)

# max(tamanio_grafos_con_metricas)

# table(tamanio_grafos_con_metricas)

#######################
# me fijo del vector original cuales grafos tienen mas de 200 nodos
#####################

# table(tamanio > 200)

# indices_vector_tamanio_mayor_200 = which(tamanio > 200)

##########
# iteracion por cada subgrafo
#########


# creo un tiempo de referencia para hacer una cuenta y que grabe si se supero un determinado tiempo
t_referencia_para_grabar = now()

##########
# iteracion por cada subgrafo
# se genera un grafo inducido por cada tipo de comunidad y en cada uno se calculan todas las metricas de nuevo
#########

# tipos de comunidades:
# btw se intento pero no terminaba mas y no se podia poner cutoff. el cutoff es solo para el valor de centralidad de btw.
# lista_grafos[[1]]$glouv
# lista_grafos[[1]]$ginfomap
# lista_grafos[[1]]$gfastgreedy
# lista_grafos[[1]]$g.cluster.country
# lista_grafos[[1]]$g.cluster.rol_nodo_1
# lista_grafos[[1]]$g.cluster.rol_nodo2

# armo una lista para datos dentro de cada tipo de comunidad
# estas listas se deben guardar en la lista de grafos dentro del subgrafo conexo correspondiente
# lista_datos_comunidades_louv = list()
# lista_datos_comunidades_infomap = list()
# lista_datos_comunidades_fastgreedy = list()
# lista_datos_comunidades_clustercountry = list()
# lista_datos_comunidades_clusterrolonodo1 = list()

# pongo 1 en el grafo pero es la misa estructura para cualquiera
# indices_comunidades = c(which(names(lista_grafos[[1]]) == "glouv"),
#                         which(names(lista_grafos[[1]]) == "ginfomap"),
#                         which(names(lista_grafos[[1]]) == "gfastgreedy"),
#                         which(names(lista_grafos[[1]]) == "g.cluster.country"),
#                         which(names(lista_grafos[[1]]) == "g.cluster.rol_nodo_1"))

# indices_comunidades = c(19,20,21,22,23)

# hay 5 tipos de comunidades (louvain, infomap, etc, etc)
# cant_tipo_comunidades = 5

# listas generales
lista_comunidades = list()
lista_datos_comunidades = list()

indices_lista_no_nulos = readr::read_rds("./salidas_datos/indices_lista_no_nulos.rds")

# recorro la lista de grafos que contienen subgrafos y me quedo con los que no son nulos
for (indice in indices_lista_no_nulos) {
  
  # dentro de cada subgrafo conexo verifico que la cantidad de nodos sea mayor a 1
  # obtengo la lista de un grafo
  lista_datos_grafo = readr::read_rds(glue("./salidas_datos/lista_de_grafos/grafo_{indice}.rds"))
  
  print(paste("grafo numero", indice, "empieza"))
  
  # lista_comunidades tiene la lista con todos los valores calculados de cada comunidad
  # lista_comunidades = list(lista_datos_grafo$lista_datos_comunidades_louv,
  #                          lista_datos_grafo$lista_datos_comunidades_infomap, 
  #                          lista_datos_grafo$lista_datos_comunidades_fastgreedy, 
  #                          lista_datos_grafo$lista_datos_comunidades_clustercountry, 
  #                          lista_datos_grafo$lista_datos_comunidades_clusterrolonodo1)
  
  ####################
  # louvain
  ####################
  
  print(paste("exporto dataframe louvain del grafo numero ", indice))
  
  # lap tiene lista de datasets con las metricas
  lista_datasets_metricas = lapply(lista_datos_grafo$lista_datos_comunidades_louv, metrica_sobre_lista)
  # bindeo los dataframes
  df <- do.call(rbind, lista_datasets_metricas)
  df = dplyr::as_data_frame(df)
  # guardo el df
  saveRDS(object = df, file = glue("./salidas_datos/tablon_de_grafos/grafo_{indice}_louv.rds"))
  
  ####################
  # infomap
  ####################
  
  print(paste("exporto dataframe infomap del grafo numero ", indice))
  
  # lap tiene lista de datasets con las metricas
  lista_datasets_metricas = lapply(lista_datos_grafo$lista_datos_comunidades_infomap, metrica_sobre_lista)
  # bindeo los dataframes
  df <- do.call(rbind, lista_datasets_metricas)
  df = dplyr::as_data_frame(df)
  # guardo el df
  saveRDS(object = df, file = glue("./salidas_datos/tablon_de_grafos/grafo_{indice}_infomap.rds"))
  
  ####################
  # lista_datos_comunidades_fastgreedy
  ####################
  
  print(paste("exporto dataframe fastgreedy del grafo numero ", indice))
  
  # lap tiene lista de datasets con las metricas
  lista_datasets_metricas = lapply(lista_datos_grafo$lista_datos_comunidades_fastgreedy, metrica_sobre_lista)
  # bindeo los dataframes
  df <- do.call(rbind, lista_datasets_metricas)
  df = dplyr::as_data_frame(df)
  # guardo el df
  saveRDS(object = df, file = glue("./salidas_datos/tablon_de_grafos/grafo_{indice}_fastgreedy.rds"))
  
  ####################
  # lista_datos_comunidades_clustercountry
  ####################
  
  print(paste("exporto dataframe clustercountry del grafo numero ", indice))
  
  # lap tiene lista de datasets con las metricas
  lista_datasets_metricas = lapply(lista_datos_grafo$lista_datos_comunidades_clustercountry, metrica_sobre_lista)
  # bindeo los dataframes
  df <- do.call(rbind, lista_datasets_metricas)
  df = dplyr::as_data_frame(df)
  # guardo el df
  saveRDS(object = df, file = glue("./salidas_datos/tablon_de_grafos/grafo_{indice}_clustercountry.rds"))
  
  ####################
  # lista_datos_comunidades_clusterrolonodo1
  ####################
  
  print(paste("exporto dataframe clusterrolnodo1 del grafo numero ", indice))
  
  # lap tiene lista de datasets con las metricas
  lista_datasets_metricas = lapply(lista_datos_grafo$lista_datos_comunidades_clusterrolonodo1, metrica_sobre_lista)
  # bindeo los dataframes
  df <- do.call(rbind, lista_datasets_metricas)
  df = dplyr::as_data_frame(df)
  # guardo el df
  saveRDS(object = df, file = glue("./salidas_datos/tablon_de_grafos/grafo_{indice}_rolnodo1.rds"))
  
  
}

print("fin de exportacion de tablones")

