#################
# plan de ejecucion
# analisis de datos para grafo
# metricas en los grafos
#################

# seteo wd
setwd("..")

print("Empieza programa")
# resumen de exploracion y armado inicial de datos para grafo
source('./R/06_analisis_sugrafos_conexos_sin_pagerank_sin_btw.R')
source('./R/07_analisis_sugrafos_conexos_pagerank.R')
# source('./R/08_analisis_sugrafos_conexos_btw.R') # esta corriendo en la nube para algunos grafos, no lo corro para que no pise resultados. dsps correrlo individualcon al condicion de poner indice mayor al valor que ya llegó. de paso pruebo cuanto tarda


print("fin de programa")




