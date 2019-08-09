
library(tidyverse)
library(readr)
library(stringr)
library(dlookr)
library(lubridate)
library(igraph)


define_cutoff = 10


nombre_workspace_analisis_01 = "./salidas_datos/workspaces/metricas_y_comunidades_sin_btw.RDATA"
nombre_workspace_analisis_01_01 = "./salidas_datos/workspaces/metricas_y_comunidades_grafos_grandes_sin_btw_sin_pagerank.RDATA"
nombre_workspace_analisis_02 = "./salidas_datos/workspaces/metricas_y_comunidades_con_btw.RDATA"
nombre_workspace_analisis_02_01 = "./salidas_datos/workspaces/metricas_y_comunidades_grafos_grandes_con_pagerank.RDATA"
nombre_workspace_analisis_03 = "./salidas_datos/workspaces/metricas_y_comunidades_con_pagerank_btw.RDATA"
nombre_workspace_analisis_03_01 = "./salidas_datos/workspaces/metricas_y_comunidades_grafos_grandes_con_pagerank_con_btw.RDATA"

nombre_workspace_analisis_02_02 = "./salidas_datos/workspaces/metricas_y_comunidades_grafos_grandes_con_pagerank_script07.RDATA"
# 03_02 tiene btw estimado con cutoff 5, tarda menos de 4 horas
nombre_workspace_analisis_03_02 = "./salidas_datos/workspaces/metricas_y_comunidades_grafos_grandes_con_btw_script08.RDATA"

# 03_03 tiene btw estimado con cutoff 10
nombre_workspace_analisis_03_03 = "./salidas_datos/workspaces/metricas_y_comunidades_grafos_grandes_con_btw_script08_03_cutoff10.RDATA"

# 03_04 tiene btw estimado con cutoff 20 y sin cutoff
nombre_workspace_analisis_03_04 = "./salidas_datos/workspaces/metricas_y_comunidades_grafos_grandes_con_btw_script08_04_cutoff10.RDATA"

# 04_01 hace metricas en las comunidades de cada subgrafo
nombre_workspace_analisis_04_01 = "./salidas_datos/workspaces/metricas_sobre_comunidades_script09_01_cutoff10.RDATA"

