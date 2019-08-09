#################
# plan de ejecucion
# unir datos
#################

# seteo wd
setwd("..")

print("Empieza programa")
# leo datos originales desde rds
# junto aristas
rmarkdown::render('./R/02_data_join.Rmd', encoding = 'UTF-8', output_file = './salidas_informe_tableros/02_data_join.html')

print("fin de programa")




