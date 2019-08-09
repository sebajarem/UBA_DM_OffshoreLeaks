
#####################
# lee el workspace anterior (nombre_workspace_analisis_04_01)
# exporta los subgrafos por separado porque es muy pesado
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

load(nombre_workspace_analisis_04_01)

saveRDS(lista_grafos, "./salidas_datos/lista_grafos_completa_con_metricas_por_comunidades.rds")

# ----------
# semilla
# ---------

set.seed(123456)

library(glue)
# exportar subgrafos

indices_lista_no_nulos = which(!sapply(lista_grafos, is.null))

for(indice in indices_lista_no_nulos){
  
  # lista_grafos[[indice]]
  
  print(glue("./salidas_datos/lista_de_grafos/grafo_{indice}.rds"))
  
  saveRDS(lista_grafos[[indice]], glue("./salidas_datos/lista_de_grafos/grafo_{indice}.rds"))
  
}

print("fin de grabacion de listas")

