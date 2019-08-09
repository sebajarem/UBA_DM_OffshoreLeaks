

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
  dist = igraph::distances(graph = g, v = V(g)$name, to = V(g)$name, mode = "all")
  lista_datos$dist = dist
  # los apoc quedan en las columnas y los no apoc en las filas
  
  return(lista_datos)
  

}