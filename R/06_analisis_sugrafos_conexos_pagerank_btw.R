
#####################
# lee el workspace anterior
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


load(nombre_workspace_analisis_01)

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

print(paste("cantidad de subgrafos ",length(tamanio_grafos_con_metricas)))

hist(tamanio_grafos_con_metricas)

max(tamanio_grafos_con_metricas)

table(tamanio_grafos_con_metricas)

#######################
# me fijo del vector original cuales grafos tienen mas de 200 nodos
#####################

table(tamanio > 200)

indices_vector_tamanio_mayor_200 = which(tamanio > 200)

##########
# iteracion por cada subgrafo
#########

# el indice tiene qu ser mayor al maximo de los grafos que ya se hizo

indice_minimo = length(lista_grafos)
# lista_grafos = list() #ya existe en el workspace
lista_datos_grafo = list() # info por cada grafo

# inicio un contador para que no grabe en cada iteracion sino 
contador_para_gramar_workspace = 0

# creo un tiempo de referencia para hacer una cuenta y que grabe si se supero un determinado tiempo
t_referencia_para_grabar = now()

# ciclo por cada indice de grafo que sea mayor a 200
for (indice in indices_vector_tamanio_mayor_200) {
  if (indice > length(lista_grafos)){
    # entra si el indice es mayor al maximo indice que ya se haya calculado
    
    if (length(V(dg[[indice]])) > 200 ) {
      # verifico que sea mayor a 200 por las dudas
      
      t0 = now()
      
      # tomo el grafo
      g = dg[[indice]]
      
      # extraigo los nodos y aristas
      n = igraph::as_data_frame(x = g, what = "vertices")
      ar = igraph::as_data_frame(x = g, what = "edges")
      
      # completo con "sin_datos" si hay valores NAs en ambos datsets (para evitar errores en los calculos)
      # n$countries = dplyr::na_if(n$countries, "Russia")
      n[is.na(n)] = "sin_datos"
      
      
      ## calculos por cada grafo
      # numero de nodos
      vN<-vcount(g)
      # numero de aristas
      eN = ecount(g)
      # simple ? no hay bucles o aristas repetidas ?
      simple = is.simple(g)
      if(simple == FALSE){
        g = simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb=toString)
      }
      # esta todo conectado ?
      conectado = is.connected(g)
      # grafo pesado
      pesado = is.weighted(g)
      
      t1 = now()
      
      # diametros
      gdiametro<-diameter(g)
      # recordar que cada arista vale 1 ya que no hay pesos a esta altura
      
      t2 = now()
      
      # densidad
      deng<-graph.density(g)
      
      t3 = now()
      
      # ratio: aristas / (posibles aristas)
      # transitividad
      # coefficiente de clustering
      # global
      gtransitivity<-transitivity(g, type = "global")
      t4 = now()
      # por nodo
      gtransitivity_nodo<-transitivity(g, type = "local")
      t5 = now()
      # Grados
      gdegree = degree(g)
      gmaxdg<-max(degree(g))
      gmindg<-min(degree(g))
      gavgdg<-mean(degree(g))
      
      t6 = now()
      # assortividad
      assortiv = assortativity.degree(g, directed = F)
      t7 = now()
      # auto vectores
      eigen_centrality = eigen_centrality(g)
      t8 = now()
      
      # densidad
      edge_density = edge_density(g)
      
      t9 = now()
      
      ## comunidades
      print("empieza cluster louvain")
      glouv <- cluster_louvain(g, weights = NULL)
      t10 = now()
      
      # infomap
      print("empieza infomap")
      ginfomap <- cluster_infomap(g)
      t11 = now()
      # fast_greedy
      print("empieza fast_greedy")
      gfastgreedy <- cluster_fast_greedy(g)
      t12 = now()
      # camino al azar
      #print("empieza walktrap")
      #gwalktrap = cluster_walktrap(g, steps = 4, merges = T, modularity = T, membership = T)
      t13 = now()
      # maximiza modularidad
      # gcluster_optimal = cluster_optimal(g, weights = NULL)
      t14 = now()
      
      # otros tipos de clusters 
      # pongo louvain y betweeness pweo hacen lo mismo que los que se hicieron antes
      # lo pongo para tener ejemplo de como se haria
      # g.cluster.glouv = make_clusters(g, membership = glouv$membership, algorithm = "louvain", modularity = T)
      # g.cluster.btw = make_clusters(g, membership = gbtw$membership, algorithm = "betweenness", modularity = T)
      
      # cluster por pais
      print("cluster countries")
      g.cluster.country = make_clusters(g, membership = as.numeric(as.factor(n$countries)), algorithm = "countries", modularity = T)
      t15 = now()
      # cluster por rol_nodo_1
      print("cluster rol_nodo_1")
      g.cluster.rol_nodo_1 = make_clusters(g, membership = as.numeric(as.factor(n$rol_nodo)), algorithm = "rol_nodo_1", modularity = T)
      t16 = now()
      # cluster por rol_nodo_2
      print("cluster rol_nodo_2")
      g.cluster.rol_nodo2 = make_clusters(g, membership = as.numeric(as.factor(if_else(n$rol_nodo_2 == "sin_datos", n$rol_nodo, n$rol_nodo_2))), algorithm = "rol_nodo_2", modularity = T)
      t17 = now()
      
      print("empieza a grabar")
      # inicio la lista vacia
      lista_datos_grafo = list()
      
      # pongo toda la info en la sublista
      lista_datos_grafo$g = g
      lista_datos_grafo$n = n
      lista_datos_grafo$ar = ar
      lista_datos_grafo$vN = vN
      lista_datos_grafo$eN = eN
      lista_datos_grafo$simple = simple
      lista_datos_grafo$conectado = conectado
      lista_datos_grafo$pesado = pesado
      lista_datos_grafo$gdiamtero = gdiametro
      lista_datos_grafo$deng = deng
      lista_datos_grafo$gtransitivity = gtransitivity
      lista_datos_grafo$gdegree = gdegree
      lista_datos_grafo$gmaxdg = gmaxdg
      lista_datos_grafo$gmindg = gmindg
      lista_datos_grafo$gavgdg = gavgdg
      lista_datos_grafo$assortiv = assortiv
      lista_datos_grafo$eigen_centrality = eigen_centrality
      lista_datos_grafo$edge_density = edge_density
      lista_datos_grafo$glouv = glouv
      lista_datos_grafo$ginfomap = ginfomap
      lista_datos_grafo$gfastgreedy = gfastgreedy
      #lista_datos_grafo$gwalktrap = gwalktrap
      # lista_datos_grafo$gcluster_optimal = gcluster_optimal
      lista_datos_grafo$g.cluster.country = g.cluster.country
      lista_datos_grafo$g.cluster.rol_nodo_1 = g.cluster.rol_nodo_1
      lista_datos_grafo$g.cluster.rol_nodo2 = g.cluster.rol_nodo2
      # tiempos
      lista_datos_grafo$tdiametro = t2 - t1
      lista_datos_grafo$tdensidad = t3 - t2
      lista_datos_grafo$ttransitivitytotal = t4 - t3
      lista_datos_grafo$ttransitivitynodo = t5 - t4
      lista_datos_grafo$tgrados = t6 - t5
      lista_datos_grafo$tassortiv = t7 - t6
      lista_datos_grafo$teigencentrality = t8 - t7
      lista_datos_grafo$tedgedensity = t9 - t8
      lista_datos_grafo$tlouv = t10 - t9
      lista_datos_grafo$tinfomap = t11 - t10
      lista_datos_grafo$tfastgreedy = t12 - t11
      lista_datos_grafo$twalktrap = t13 - t12
      lista_datos_grafo$tclusteroptimal = t14 - t13
      lista_datos_grafo$tclustercountry = t15 - t14
      lista_datos_grafo$tclusternodo1 = t16 - t15
      lista_datos_grafo$tclusternodo2 = t17 - t16
      lista_datos_grafo$ttotalgrafo = t17 - t0
      
      
      # guardo en la lista general de grafos
      lista_grafos[[indice]] = lista_datos_grafo
      
      
      
      
      if(as.numeric(difftime(t17, t_referencia_para_grabar, units = "hours")) > 3 ){
        # si paso mas de 3 horas desde la ultima vez que se grabo, se vuelve a grabar
        save.image(nombre_workspace_analisis_01_01)
        # se setea la nueva referencia
        t_referencia_para_grabar = now()
        print("")
        print(paste("ultima vez que se grabo el workspace", t_referencia_para_grabar))
        print("")
      }
      
      print("")
      print(paste("grafo numero",indice,"hecho"))
      print(paste("tiene ", vN, "nodos y ", eN, "aristas"))
      print(paste("cantidad de grafos totales ",length(tamanio)))
      print("")
      
    }else{
      print(paste("grafo número",indice,"es de 1 solo nodo"))
      print(paste("cantidad de grafos",length(tamanio)))
      
    }
    
  }

}

print("final de la primera parte del script donde calcula varias metricas")
print(now())


##########
# iteracion por cada subgrafo
# para page rank
#########


# indice = 1

# hago un nuevo indice que tenga todos los que habia calculado correlativamente y los nuevos que deberian estar calculados

# puedo hacerlo preguntando si la lista no es nula y quedandome con los indices

indices_lista_no_nulos = which(!sapply(lista_grafos, is.null))


for (indice in indices_lista_no_nulos) {
  
  if (length(V(dg[[indice]])) > 1 ) {
    
    t18 = now()
    
    # levanto los datos
    lista_datos_grafo = lista_grafos[[indice]]
    
    # tomo el grafo
    g = lista_datos_grafo$g
    
    
    lista_datos_grafo$page_rank = page_rank(g, algo = "prpack", vids = V(g), directed = F, damping = 0.85, personalized = NULL, weights = NULL, options = NULL)
    
    
    t19 = now()
    
    lista_datos_grafo$tpagerank = t19 - t18
    
    
    # vuelvo a guardar los datos en la superlista
    lista_grafos[[indice]] = lista_datos_grafo
    
    if(as.numeric(difftime(t19, t_referencia_para_grabar, units = "hours")) > 3 ){
      # si paso mas de 3 horas desde la ultima vez que se grabo, se vuelve a grabar
      save.image(nombre_workspace_analisis_02_01)
      # se setea la nueva referencia
      t_referencia_para_grabar = now()
      print("")
      print(paste("ultima vez que se grabo el workspace", t_referencia_para_grabar))
      print("")
    }
    
    
    print(paste("page rank en grafo numero",indice,"hecho"))
    print(paste("cantidad de grafos totales ",length(tamanio)))
    
  }else{
    print(paste("grafo número",indice,"es de 1 solo nodo"))
    print(paste("cantidad de grafos",length(tamanio)))
    
  }
  
  indice = indice + 1
  
}


save.image(nombre_workspace_analisis_02_01)


print("final del script calculo solo de page rank")
print(now())



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
    
    
    # betweenness
    # btw_nodo = betweenness(g, directed = F, normalized = F)
    t21 = now()
    # gbtw = cluster_edge_betweenness(g)
    t22 = now()
    gbtw_estimado = estimate_betweenness(g, vids = V(g), directed = F, cutoff = 10, weights = NULL, nobigint = TRUE)
    t23 = now()
    
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
      save.image(nombre_workspace_analisis_03_01)
      # se setea la nueva referencia
      t_referencia_para_grabar = now()
      print("")
      print(paste("ultima vez que se grabo el workspace", t_referencia_para_grabar))
      print("")
    }
    
    
    print(paste("betweenness en grafo numero",indice,"hecho"))
    print(paste("cantidad de grafos totales ",length(tamanio)))
    
  }else{
    print(paste("grafo número",indice,"es de 1 solo nodo"))
    print(paste("cantidad de grafos",length(tamanio)))
    
  }
  
  indice = indice + 1
  
}


save.image(nombre_workspace_analisis_03_01)


print("final del script")
print(now())


