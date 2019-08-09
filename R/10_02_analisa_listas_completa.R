
#####################
# lee lista completa con metricas calculadas hasta el momento
# revisa hasta que indice de grafo llego para seguir con el analisis
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

# load(nombre_workspace_analisis_04_01)

# saveRDS(lista_grafos, "./salidas_datos/lista_grafos_completa_con_metricas_por_comunidades.rds")

lista_grafos = readr::read_rds("./salidas_datos/lista_grafos_completa_con_metricas_por_comunidades.rds")

# ----------
# semilla
# ---------

set.seed(123456)

library(glue)
# exportar subgrafos

indices_lista_no_nulos = which(!sapply(lista_grafos, is.null))
# saveRDS( indices_lista_no_nulos, "./salidas_datos/indices_lista_no_nulos.rds")

largo_listas = sapply(lista_grafos, length)

table(largo_listas)

lista_grafos[[1]]$

for(indice in indices_lista_no_nulos){
  
  # lista_grafos[[indice]]
  
  print(glue("./salidas_datos/lista_de_grafos/grafo_{indice}.rds"))
  
  saveRDS(lista_grafos[[indice]], glue("./salidas_datos/lista_de_grafos/grafo_{indice}.rds"))
  
}

print("fin de grabacion de listas")

