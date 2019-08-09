
#####################
# lee el workspace anterior (nombre_workspace_analisis_03_03)
# pregunta si el grafo tiene mas de 200 nodos
# ahi hace los calculos
# asi no se hacen mas grafos con nodos chicos
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

load(nombre_workspace_analisis_03_03)

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


cant_grafos_con_mericas = length(lista_grafos)

tamanio_grafos_con_metricas = vector(mode = "integer")
for (i in 1:cant_grafos_con_mericas) {
  
  tamanio_grafos_con_metricas = c(tamanio_grafos_con_metricas, lista_grafos[[i]]$vN)
  
}

print(paste("cantidad de subgrafos con metricas: ",length(tamanio_grafos_con_metricas)))

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
lista_datos_comunidades_louv = list()
lista_datos_comunidades_infomap = list()
lista_datos_comunidades_fastgreedy = list()
lista_datos_comunidades_clustercountry = list()
lista_datos_comunidades_clusterrolonodo1 = list()

# pongo 1 en el grafo pero es la misa estructura para cualquiera
indices_comunidades = c(which(names(lista_grafos[[1]]) == "glouv"),
                        which(names(lista_grafos[[1]]) == "ginfomap"),
                        which(names(lista_grafos[[1]]) == "gfastgreedy"),
                        which(names(lista_grafos[[1]]) == "g.cluster.country"),
                        which(names(lista_grafos[[1]]) == "g.cluster.rol_nodo_1"))

# hay 5 tipos de comunidades (louvain, infomap, etc, etc)
cant_tipo_comunidades = 5

# listas generales
lista_comunidades = list()
lista_datos_comunidades = list()

# recorro la lista de grafos que contienen subgrafos y me quedo con los que no son nulos
for (indice in indices_lista_no_nulos) {
  
  # dentro de cada subgrafo conexo verifico que la cantidad de nodos sea mayor a 1
  
  if (length(V(dg[[indice]])) > 1 ) {
    
    # si es mayor a 1, quiere decir que se calcularon comunidades
    # itero por cada tipo de comunidad
    
    # obtengo todos los datos se este subgrafo
    lista_datos_grafo = lista_grafos[[indice]]
    
    # obtengo el grafo
    g = lista_datos_grafo$g
    
    for(indice_tipo_comunidad in indices_comunidades){
      t09_01 = now()
      
      # obtengo la cantidad de comunidades que tiene
      cant_comunidades = max(lista_datos_grafo[[indice_tipo_comunidad]]$membership)
      
      # itero por cada comunidad (i:cant_comunidades)
      for (i in 1:cant_comunidades) {
        
        # limpio la lista de datos de comunidades por cada nueva comunidad para que no tenga valores de otra anterior
        lista_datos_comunidades = list()

        # por cada comunidad hago un grafo inducido
        g_inducido = induced_subgraph(graph = g, vids = communities(lista_datos_grafo[[indice_tipo_comunidad]])[[i]])
        
        print(paste("comunidad numero: ", i, " empieza calculo de metricas"))
        
        ############## me da error la funcion asique copio y pego lo que habia puesto ahi
        lista_datos_comunidades = metricas_sobre_grafo(g = g_inducido, cutoff= 10)
        ###########################
        
        # # parametro que pasaba en la funcion
        # g = g_inducido
        # cutoff = define_cutoff
        # 
        # #########
        # # inicio contenido de la funcion
        # ###############
        # 
        # 
        # # lsita donde se van a guardar todos los resultados
        # lista_datos = list()
        # 
        # lista_datos$grafo = g
        # 
        # lista_datos$cutoff = cutoff
        # 
        # # betweenness estimado
        # tbtw_cutoff_inicio = now()
        # btw_estimado_cutoff = estimate_betweenness( graph = g, vids = V(g), directed = FALSE, cutoff = cutoff)
        # lista_datos$btw_estimado_cutoff = btw_estimado_cutoff
        # tbtw_cutoff_fin = now()
        # lista_datos$tiempo_btw_estimado_cutoff = difftime(tbtw_cutoff_fin, tbtw_cutoff_inicio, units = "mins")
        # print(paste("btw cutoff", cutoff, "tarda (minutos): ", lista_datos$tiempo_btw_estimado_cutoff))
        # # calculo closeness centrality
        # tcloseness_cutoff_inicio = now()
        # closeness_centrality_estimado_cutoff = estimate_closeness(graph = g, vids = V(g), cutoff = cutoff)
        # lista_datos$closeness_centrality_estimado_cutoff = closeness_centrality_estimado_cutoff
        # tcloseness_cutoff_fin = now()
        # lista_datos$tiempo_closeness_estimado_cutoff = difftime(tcloseness_cutoff_fin, tcloseness_cutoff_inicio, units = "mins")
        # print(paste("btw cutoff", cutoff, "tarda (minutos): ", lista_datos$tiempo_closeness_estimado_cutoff))
        # # pagerank
        # page_rank = page_rank(graph = g, algo = "prpack", vids = V(g), directed = FALSE)
        # lista_datos$page_rank = page_rank
        # # hub_score
        # hub_score = hub_score(graph = g)
        # lista_datos$hub_score = hub_score
        # # authority_score
        # authority_score = authority_score(graph = g)
        # lista_datos$authority_score = authority_score
        # # eigen_centrality
        # eigen_centrality = eigen_centrality( graph = g, directed = FALSE)
        # lista_datos$eigen_centrality = eigen_centrality
        # #power_centrality
        # #power_centrality = power_centrality(graph = g, nodes = V(g), loops = FALSE)
        # # transitividad
        # transitivity = transitivity(graph = g, type = "local", vids = V(g) )
        # lista_datos$transitivity = transitivity
        # # grado
        # grado = degree(graph = g, v = V(g), mode = "all", loops = FALSE )
        # lista_datos$grado = grado
        # # assortivity
        # assort = assortativity_degree(graph = g, directed = FALSE)
        # lista_datos$assort = assort
        # # densidad de la comunidad
        # densidad_comunidad = igraph::graph.density(graph = g, loops = FALSE)
        # lista_datos$densidad_comunidad = densidad_comunidad
        # # caminos todos contra todos
        # dist = igraph::distances(graph = g, v = V(g)$name, to = V(g)$name, mode = "all")
        # lista_datos$dist = dist
        # # los apoc quedan en las columnas y los no apoc en las filas
        # 
        # lista_datos_comunidades = lista_datos
        # 
        # ##################
        # # fin de contenido de la funcion
        # #################
        
        print("")
        print(paste("comunidad: ",i, " calculada"))
        print(paste("total de comunidades: ", cant_comunidades))
        print("")

        # guardo los datos calculados en esa comunidad en la lista general
        lista_comunidades[[i]] = lista_datos_comunidades
        
      }
      
      # pregunto en que tipo de comunidad estamos
      if(which(names(lista_grafos[[1]]) == "glouv") == indice_tipo_comunidad){
        # si es el loop que corresponde a louvain
        lista_datos_comunidades_louv = lista_comunidades
        lista_datos_grafo$lista_datos_comunidades_louv = lista_datos_comunidades_louv
      }else{
        if(which(names(lista_grafos[[1]]) == "ginfomap") == indice_tipo_comunidad){
          # si es el loop que corresponde a infomap
          lista_datos_comunidades_infomap = lista_comunidades
          lista_datos_grafo$lista_datos_comunidades_infomap = lista_datos_comunidades_infomap
        }else{
          if(which(names(lista_grafos[[1]]) == "gfastgreedy") == indice_tipo_comunidad){
            lista_datos_comunidades_fastgreedy = lista_comunidades
            lista_datos_grafo$lista_datos_comunidades_fastgreedy = lista_datos_comunidades_fastgreedy
          }else{
            if(which(names(lista_grafos[[1]]) == "g.cluster.country") == indice_tipo_comunidad){
              lista_datos_comunidades_clustercountry = lista_comunidades
              lista_datos_grafo$lista_datos_comunidades_clustercountry = lista_datos_comunidades_clustercountry
            }else{
              # si no es ninguna de las anteriores quiere decir que es de rol_nodo1
              lista_datos_comunidades_clusterrolonodo1 = lista_comunidades
              lista_datos_grafo$lista_datos_comunidades_clusterrolonodo1 = lista_datos_comunidades_clusterrolonodo1
            }
          }
        }
      }
      
      # vuelvo a guardar los datos en la superlista
      lista_grafos[[indice]] = lista_datos_grafo
      
    }
    
    tcontrol = now()

    # vuelvo a guardar los datos en la superlista
    lista_grafos[[indice]] = lista_datos_grafo
    
    if(as.numeric(difftime(tcontrol, t_referencia_para_grabar, units = "hours")) > 3 ){
      # si paso mas de 3 horas desde la ultima vez que se grabo, se vuelve a grabar
      save.image(nombre_workspace_analisis_04_01)
      # se setea la nueva referencia
      t_referencia_para_grabar = now()
      print("")
      print(paste("ultima vez que se grabo el workspace", t_referencia_para_grabar))
      print("")
    }
    
    
    print(paste("metricas en comunidades del grafo numero",indice,"hecho"))
    print(paste("cantidad de grafos totales ",length(lista_grafos)))
    
  }else{
    print(paste("grafo número",indice,"es de 1 solo nodo"))
    print(paste("cantidad de grafos",length(lista_grafos)))
    
  }
  
}


save.image(nombre_workspace_analisis_04_01)


print("final del script")
print(now())


