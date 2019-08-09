#################
# plan de ejecucion
# analisis de datos para grafo
# metricas en los grafos
#################

# seteo wd
setwd("..")

print("Empieza programa")
# resumen de exploracion y armado inicial de datos para grafo
rmarkdown::render('./R/05_analisis_sugrafos_conexos.Rmd', encoding = 'UTF-8', output_file = '../salidas_informe_tableros/05_analisis_sugrafos_conexos.Rmd')

print("fin de programa")




