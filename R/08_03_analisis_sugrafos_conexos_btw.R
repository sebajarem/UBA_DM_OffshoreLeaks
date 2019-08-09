
#####################
# lee el workspace anterior (02_02)
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


load(nombre_workspace_analisis_02_02)

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
# para page rank
#########

# hago un nuevo indice que tenga todos los que habia calculado correlativamente y los nuevos que deberian estar calculados

# puedo hacerlo preguntando si la lista no es nula y quedandome con los indices

indices_lista_no_nulos = which(!sapply(lista_grafos, is.null))


##########
# iteracion por cada subgrafo
# para btw
#########

# indice = 1

for (indice in indices_lista_no_nulos) {
  
  if (length(V(dg[[indice]])) > 1 ) {
    
    t20 = now()
    
    # levanto los datos
    lista_datos_grafo = lista_grafos[[indice]]
    
    # tomo el grafo
    g = lista_datos_grafo$g
    
    print("calculo betweenness estimado cutoff 10")
    # betweenness
    # btw_nodo = betweenness(g, directed = F, normalized = F)
    t21 = now()
    print(paste("hora de comienzo calculo beetweennes estimado: ", t21))
    # gbtw = cluster_edge_betweenness(g)
    t22 = now()
    gbtw_estimado = estimate_betweenness(g, vids = V(g), directed = F, cutoff = 10, weights = NULL, nobigint = TRUE)
    t23 = now()
    print(paste("termino calculo de betweennnes estimado", t23))
    print(paste("tiempo que tardé betweenness estimado (minutos): ", difftime(t23, t22, units = "mins")))
    
    # lista_datos_grafo$btw_nodo = btw_nodo
    # lista_datos_grafo$gbtw = gbtw
    lista_datos_grafo$btw_estimado = gbtw_estimado
    
    lista_datos_grafo$t_btw_nodo = t21 - t20
    lista_datos_grafo$t_gbtw = t22 - t21
    lista_datos_grafo$t_btw_estimado = t23 - t22
    
    # vuelvo a guardar los datos en la superlista
    lista_grafos[[indice]] = lista_datos_grafo
    
    if(as.numeric(difftime(t23, t_referencia_para_grabar, units = "hours")) > 3 ){
      # si paso mas de 3 horas desde la ultima vez que se grabo, se vuelve a grabar
      save.image(nombre_workspace_analisis_03_03)
      # se setea la nueva referencia
      t_referencia_para_grabar = now()
      print("")
      print(paste("ultima vez que se grabo el workspace", t_referencia_para_grabar))
      print("")
    }
    
    
    print(paste("betweenness en grafo numero",indice,"hecho"))
    print(paste("cantidad de grafos totales ",length(lista_grafos)))
    
  }else{
    print(paste("grafo número",indice,"es de 1 solo nodo"))
    print(paste("cantidad de grafos",length(lista_grafos)))
    
  }
  
}


save.image(nombre_workspace_analisis_03_03)


print("final del script")
print(now())


