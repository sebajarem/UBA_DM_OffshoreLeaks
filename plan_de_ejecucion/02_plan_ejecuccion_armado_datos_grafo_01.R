#################
# plan de ejecucion
# armado datos para grafo
# primer grafo
#################

# seteo wd
setwd("..")

print("Empieza programa")
# resumen de exploracion y armado inicial de datos para grafo
rmarkdown::render('./R/03_data_armado_para_grafos.Rmd', encoding = 'UTF-8', output_file = '../salidas_informe_tableros/03_data_armado_para_grafos.html')

print("fin de programa")




