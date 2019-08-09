# levanta los grafos seleccionados 
# soluciona el problema de que no coincidian la cantidad de nodos en comunidades con el grafo original
# problema: nunca se puso en cero la lista_comunidades y se sobre escribia
# si la siguiente comundiad tenia  menos cantida de comunidades, nunca se borraban esas y parecia que tenia mas nodos


#-----------
# limpieza de entorno
#-----------
# limpia todas las variables del entorno y libera la memoria
rm(list=ls())
gc()

# incluyo archivo con defines y funciones
source("./R/00_Defines_VariablesGlobales_Universos.R")

library(glue)

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

indices_seleccion = readr::read_rds("./salidas_datos/indices_seleccion.rds")
tamanios_seleccion = readr::read_rds("./salidas_datos/tamanios_seleccion.rds")

##########################
# arreglo el problema de las listas de comunidades que tengo por no haber puesta la lista_comunidades vacia entre cada iteracion en los scripts 09_01 y 09_02
###########################

# leo el grafo y me fijo cuantas comunidades deberia tener cada uno y me quedo con la lista con esa cantidad unicamente

for(i in indices_seleccion){
  
  # leo el grafo
  lista_g = readr::read_rds(glue("./salidas_datos/lista_de_grafos/grafo_{i}.rds"))
  
  ###############
  # louvain
  ##############
  
  length(communities(lista_g$glouv))
  length(lista_g$lista_datos_comunidades_louv)
  # son distintas asique hay error
  l = list()
  for (k in 1:length(communities(lista_g$glouv))) {
    l[[k]] = lista_g$lista_datos_comunidades_louv[[k]]
  }
  lista_g$lista_datos_comunidades_louv = l
  # comprueba que sean iguales
  length(communities(lista_g$glouv))
  length(lista_g$lista_datos_comunidades_louv)
  
  #################
  # infomap
  ##################
  
  length(communities(lista_g$ginfomap))
  length(lista_g$lista_datos_comunidades_infomap)
  # son distintas asique hay error
  l = list()
  for (k in 1:length(communities(lista_g$ginfomap))) {
    l[[k]] = lista_g$lista_datos_comunidades_infomap[[k]]
  }
  lista_g$lista_datos_comunidades_infomap = l
  # comprueba que sean iguales
  length(communities(lista_g$ginfomap))
  length(lista_g$lista_datos_comunidades_infomap)

  
  #################
  # fastgreedy
  ##################
  
  length(communities(lista_g$gfastgreedy))
  length(lista_g$lista_datos_comunidades_fastgreedy)
  # son distintas asique hay error
  l = list()
  for (k in 1:length(communities(lista_g$gfastgreedy))) {
    l[[k]] = lista_g$lista_datos_comunidades_fastgreedy[[k]]
  }
  lista_g$lista_datos_comunidades_fastgreedy = l
  # comprueba que sean iguales
  length(communities(lista_g$gfastgreedy))
  length(lista_g$lista_datos_comunidades_fastgreedy)
  
  
  #################
  # g.cluster.country
  ##################
  
  length(communities(lista_g$g.cluster.country))
  length(lista_g$lista_datos_comunidades_clustercountry)
  # son distintas asique hay error
  l = list()
  for (k in 1:length(communities(lista_g$g.cluster.country))) {
    l[[k]] = lista_g$lista_datos_comunidades_clustercountry[[k]]
  }
  lista_g$lista_datos_comunidades_clustercountry = l
  # comprueba que sean iguales
  length(communities(lista_g$g.cluster.country))
  length(lista_g$lista_datos_comunidades_clustercountry)
  
  
  #################
  # rol nodo 1
  ##################
  
  length(communities(lista_g$g.cluster.rol_nodo_1))
  length(lista_g$lista_datos_comunidades_clusterrolonodo1)
  # son distintas asique hay error
  l = list()
  for (k in 1:length(communities(lista_g$g.cluster.rol_nodo_1))) {
    l[[k]] = lista_g$lista_datos_comunidades_clusterrolonodo1[[k]]
  }
  lista_g$lista_datos_comunidades_clusterrolonodo1 = l
  # comprueba que sean iguales
  length(communities(lista_g$g.cluster.rol_nodo_1))
  length(lista_g$lista_datos_comunidades_clusterrolonodo1)
  
  # grabo el grafo con las comunidades bien
  saveRDS(lista_g, glue("./salidas_datos/lista_de_grafos/grafo_{i}.rds"))
  
}

###########################3
# exporto tablones de grafos seleccionados
##########################33


# recorro la lista de grafos que contienen subgrafos y me quedo con los que no son nulos
for (indice in indices_seleccion) {
  
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


