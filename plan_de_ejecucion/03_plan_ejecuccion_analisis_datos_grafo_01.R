#################
# plan de ejecucion
# analisis de datos para grafo
#################

# seteo wd
setwd("..")

print("Empieza programa")
# resumen de exploracion y armado inicial de datos para grafo
rmarkdown::render('./R/04_analisis_datos_para_grafos.Rmd', encoding = 'UTF-8', output_file = '../salidas_informe_tableros/04_analisis_datos_para_grafos.Rmd')

print("fin de programa")




