########################
# levanta la informacion de las 4 investigaciones del icij y las junta
######################


source("./R/00_Defines_VariablesGlobales_Universos.R")

########
# investigacion: bahamas
#########

bahamas_leaks_edges <- read_csv("./datos/datos_originales_icij/csv_bahamas_leaks.2017-12-19/bahamas_leaks.edges.csv")
bahamas_leaks_nodes_address <- read_csv("./datos/datos_originales_icij/csv_bahamas_leaks.2017-12-19/bahamas_leaks.nodes.address.csv")
bahamas_leaks_nodes_entity <- read_csv("./datos/datos_originales_icij/csv_bahamas_leaks.2017-12-19/bahamas_leaks.nodes.entity.csv")
bahamas_leaks_nodes_intermediary <- read_csv("./datos/datos_originales_icij/csv_bahamas_leaks.2017-12-19/bahamas_leaks.nodes.intermediary.csv")
bahamas_leaks_nodes_officer <- read_csv("./datos/datos_originales_icij/csv_bahamas_leaks.2017-12-19/bahamas_leaks.nodes.officer.csv")

########
# investigacion: offshore_leaks
#########

offshore_leaks_edges <- read_csv("./datos/datos_originales_icij/csv_offshore_leaks.2018-02-14/offshore_leaks.edges.csv")
offshore_leaks_nodes_address <- read_csv("./datos/datos_originales_icij/csv_offshore_leaks.2018-02-14/offshore_leaks.nodes.address.csv")
offshore_leaks_nodes_entity <- read_csv("./datos/datos_originales_icij/csv_offshore_leaks.2018-02-14/offshore_leaks.nodes.entity.csv")
offshore_leaks_nodes_intermediary <- read_csv("./datos/datos_originales_icij/csv_offshore_leaks.2018-02-14/offshore_leaks.nodes.intermediary.csv")
offshore_leaks_nodes_officer <- read_csv("./datos/datos_originales_icij/csv_offshore_leaks.2018-02-14/offshore_leaks.nodes.officer.csv")

###########
# investigacion: panama_papers
############

panama_papers_edges <- read_csv("./datos/datos_originales_icij/csv_panama_papers.2018-02-14/panama_papers.edges.csv")
panama_papers_nodes_address <- read_csv("./datos/datos_originales_icij/csv_panama_papers.2018-02-14/panama_papers.nodes.address.csv")
panama_papers_nodes_entity <- read_csv("./datos/datos_originales_icij/csv_panama_papers.2018-02-14/panama_papers.nodes.entity.csv")
panama_papers_nodes_intermediary <- read_csv("./datos/datos_originales_icij/csv_panama_papers.2018-02-14/panama_papers.nodes.intermediary.csv")
panama_papers_nodes_officer <- read_csv("./datos/datos_originales_icij/csv_panama_papers.2018-02-14/panama_papers.nodes.officer.csv")

###########
# investigacion: paradise_papers
############

paradise_papers_edges <- read_csv("./datos/datos_originales_icij/csv_paradise_papers.2018-02-14/paradise_papers.edges.csv")
paradise_papers_nodes_address <- read_csv("./datos/datos_originales_icij/csv_paradise_papers.2018-02-14/paradise_papers.nodes.address.csv")
paradise_papers_nodes_entity <- read_csv("./datos/datos_originales_icij/csv_paradise_papers.2018-02-14/paradise_papers.nodes.entity.csv")
paradise_papers_nodes_intermediary <- read_csv("./datos/datos_originales_icij/csv_paradise_papers.2018-02-14/paradise_papers.nodes.intermediary.csv")
paradise_papers_nodes_officer <- read_csv("./datos/datos_originales_icij/csv_paradise_papers.2018-02-14/paradise_papers.nodes.officer.csv")

################
# guardo datos originales como rds
#################

saveRDS(bahamas_leaks_edges, file = "./salidas_datos/originales/bahamas_leaks_edges.rds")
saveRDS(bahamas_leaks_nodes_address, file = "./salidas_datos/originales/bahamas_leaks_nodes_address.rds")
saveRDS(bahamas_leaks_nodes_entity, file = "./salidas_datos/originales/bahamas_leaks_nodes_entity.rds")
saveRDS(bahamas_leaks_nodes_intermediary, file = "./salidas_datos/originales/bahamas_leaks_nodes_intermediary.rds")
saveRDS(bahamas_leaks_nodes_officer, file = "./salidas_datos/originales/bahamas_leaks_nodes_officer.rds")

saveRDS(offshore_leaks_edges, file = "./salidas_datos/originales/offshore_leaks_edges.rds")
saveRDS(offshore_leaks_nodes_address, file = "./salidas_datos/originales/offshore_leaks_nodes_address.rds")
saveRDS(offshore_leaks_nodes_entity, file = "./salidas_datos/originales/offshore_leaks_nodes_entity.rds")
saveRDS(offshore_leaks_nodes_intermediary, file = "./salidas_datos/originales/offshore_leaks_nodes_intermediary.rds")
saveRDS(offshore_leaks_nodes_officer, file = "./salidas_datos/originales/offshore_leaks_nodes_officer.rds")

saveRDS(panama_papers_edges, file = "./salidas_datos/originales/panama_papers_edges.rds")
saveRDS(panama_papers_nodes_address, file = "./salidas_datos/originales/panama_papers_nodes_address.rds")
saveRDS(panama_papers_nodes_entity, file = "./salidas_datos/originales/panama_papers_nodes_entity.rds")
saveRDS(panama_papers_nodes_intermediary, file = "./salidas_datos/originales/panama_papers_nodes_intermediary.rds")
saveRDS(panama_papers_nodes_officer, file = "./salidas_datos/originales/panama_papers_nodes_officer.rds")

saveRDS(paradise_papers_edges, file = "./salidas_datos/originales/paradise_papers_edges.rds")
saveRDS(paradise_papers_nodes_address, file = "./salidas_datos/originales/paradise_papers_nodes_address.rds")
saveRDS(paradise_papers_nodes_entity, file = "./salidas_datos/originales/paradise_papers_nodes_entity.rds")
saveRDS(paradise_papers_nodes_intermediary, file = "./salidas_datos/originales/paradise_papers_nodes_intermediary.rds")
saveRDS(paradise_papers_nodes_officer, file = "./salidas_datos/originales/paradise_papers_nodes_officer.rds")


