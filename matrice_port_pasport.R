### Load the libraries
library(dplyr)
library(rsq)
library(purrr)
library(ggplot2)
library(vegan)
library(betapart)

###METADATA
meta_port <- read.csv("metadata_eDNA_port_october_2021.csv", header=T)
#homogénéisation meta_port 
meta_port <- meta_port[,1:9]
meta_port <- meta_port[,-2]
meta_port <- meta_port[,-2]
meta_port <- meta_port[,-5]

port <- matrix(data="true", nrow= nrow(meta_port), ncol = 1)
meta_port <- cbind(meta_port,port)

meta2 <- read.csv("metadata_milieu_naturel.csv", header=T)
#homogénéisation meta
meta2 <- meta2[,c('spygen_code','Date', 'Site', 'Region', 'projet', 'protection')]
colnames(meta2) <- c('code_spygen','date','station','site','project','habitat')
port <- matrix(data="false", nrow= nrow(meta2), ncol = 1)
meta2 <- cbind(meta2,port)

meta_tot <- rbind(meta2,meta_port)

write.csv(meta_tot, "metadata_tot.csv") 

###ADNe
adne_port <- read.csv("teleo_presence_oct21_repassee.csv", header = T, row.names=1) %>%
  as.data.frame(.) %>%
  tibble::rownames_to_column(var="Species") #--> prends les noms de ligne pour en faire une colonne 
adne_port$Species <- sub(pattern = " ",  replacement = "_",  adne_port$Species)
  
adne <- read.csv("biodiv_milieu_naturel.csv", header=T, row.names=1) %>%
  t(.) %>%
  as.data.frame(.) %>%
  tibble::rownames_to_column(var="Species") #--> prends les noms de ligne pour en faire une colonne 

#jointure by Species 
adne_tot <- adne %>%
  full_join(adne_port, by = "Species") %>% #NA quand sp. pas dans les matrices 
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "Species")

write.csv(adne_tot, "matrice_adne_totale_repassee.csv") 

###METADATA
meta_port <- read.csv("metadata_eDNA_port_october_2021.csv", header=T)
#homogénéisation meta_port 
meta_port <- meta_port[,1:9]
meta_port <- meta_port[,-2]
meta_port <- meta_port[,-2]
meta_port <- meta_port[,-5]

port_propre <- matrix(data=c("Port propre", 
                             "Port propre",
                             "Port propre",
                             "Port propre",
                             "Port propre",
                             "Non certifié",
                             "Non certifié",
                             "Non certifié",
                             "Port propre",
                             "Port propre",
                             "Port propre",
                             "Non certifié",
                             "Non certifié",
                             "Non certifié",
                             "Non certifié",
                             "Port propre",
                             "Port propre"), nrow= nrow(meta_port), ncol = 1)
port <- matrix(data="true", nrow= nrow(meta_port), ncol = 1)
meta_port <- cbind(meta_port,port,port_propre)

write.csv(meta_port, "metadata_port.csv") 

###ADNe
adne_port <- read.csv("teleo_presence_oct21.csv", header = T, row.names=1) %>%
  as.data.frame(.) %>%
  tibble::rownames_to_column(var="Species") #--> prends les noms de ligne pour en faire une colonne 
adne_port$Species <- sub(pattern = " ",  replacement = "_",  adne_port$Species)

write.csv(adne_port, "matrice_adne_port.csv") 



