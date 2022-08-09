# OBJECTIF = COMPARER LA LISTE PORT VS HORS PORT VOIR CE QUI CHANGE 

library(dplyr)

#importation des données ###############
naturel <- read.csv("biodiv_milieu_naturel.csv", header=T, row.names=1) %>%
  t(.) %>%
  as.data.frame(.) 
naturel <- tibble::rownames_to_column(naturel, var = "X")

port <- read.csv("teleo_presence_oct21_repassee.csv")
port$X <- sub(pattern = " ",  replacement = "_", port$X)


##maintenant on va chercher pour chaque  si certaines espèces de notre liste des sp. des ports sont pas détectées ailleurs

sp_port_not_hors_port <- anti_join(port,naturel, by = "X")

write.csv(sp_port_not_hors_port,"sp_port_not_hors_port.csv")

sp_not_in_port <- anti_join(naturel,port, by = "X")

write.csv(sp_not_in_port,"sp_not_in_port.csv")
