#################
# plan de ejecucion
# unir datos
#################

# seteo wd
setwd("..")

print("Empieza programa")
# leo datos originales desde rds
# junto aristas
rmarkdown::render('./R/00_exploracion.Rmd', encoding = 'UTF-8', output_file = '../salidas_informe_tableros/00_exploracion.html')

print("fin de programa")




