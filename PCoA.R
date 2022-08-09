library(vegan)
library(ggplot2)
library(ade4)
library(magrittr) 
library(dplyr)  
library(dplyr)
library(rsq)
library(purrr)
library(ggplot2)
library(vegan)
library(betapart)

#TELE0 ESPECE

# load and prepare data : table with samples in rows and species in columns
teleo <- read.csv("teleo_reads_oct21_repassee.csv", sep=",", header = T, row.names = 1) %>% replace(is.na(.), 0)
teleo <- teleo[rowSums(teleo)!=0,]
teleo <- as.data.frame(t(teleo))

# compute 'Bray' distance between samples
teleo.bray <- vegdist(teleo, method="bray")

# compute PCoA = ordination des points dans un espace
pcoa_teleo <- dudi.pco(teleo.bray, scannf = FALSE, nf = 3)

# select data to plot
teleo2plot <- pcoa_teleo$li

# ajout d'une colonne Ports et d'une colonne Habitat
habitat <- c("Port","Port", "Biohut", "Port","Port","Port","Port","Port", "Port", "Biohut","Port","Port","Port", "Port","Biohut","Port","Port")
port <- c("Agde","Agde", "Cannes", "Cannes","Cannes","La_Ciotat","La_Ciotat","Marseillan", "Marseillan", "Marseillan","Porquerolles","Porquerolles","Port_Vendres", "Port_Vendres","Saintes_Maries_de_la_Mer","Saintes_Maries_de_la_Mer","Saintes_Maries_de_la_Mer")
teleo2plot <- cbind(teleo2plot,port,habitat)


##############################################################################################################################
## Load the eDNA data (matrix species per sample)
adne <- read.csv("teleo_presence_oct21_repassee.csv", header=T, row.names=1)

# Joindre matrice metadata et ADNe
metadata <- read.csv("metadata_eDNA_port_october_2021.csv",header=T, row.names=1) %>%
  as.data.frame(.) %>%
  tibble::rownames_to_column(var="Samples") 

sp_mat <- adne %>%
  t(.) %>%
  as.data.frame(.) %>%
  tibble::rownames_to_column(var="Samples") %>%  
  #write.csv2(sp_mat, "sp_mat.csv") 
  inner_join(metadata, by=c("Samples" = "Samples")) 

## create dissimilarity matrix (Jaccard distance for presence/absence)
sp.dist<-vegdist(t(adne), method='jaccard')

## Partition beta-diversity in turnover and nestedness
beta.dist <- beta.pair(t(adne), index.family = "jac")
turnover <- beta.dist[[1]]
nestedness <- beta.dist[[2]]

## perform PERMANOVA : tester la significativité de la variable "site" 
# sur la diversité béta, le turnover et la nestedness
habitat.div<-adonis2(sp.dist~site, data=sp_mat, permutations = 999, method="jaccard")
habitat.div 
habitat.turn<-adonis2(turnover~site, data=sp_mat, permutations = 999, method="jaccard")
habitat.turn 
habitat.nest<-adonis2(nestedness~site, data=sp_mat, permutations = 999, method="jaccard")
habitat.nest 

## Multivariate dispersion : tester l'homogénéité des variances entre les site
dispersion_m<-betadisper(sp.dist, group=sp_mat$site)
permutest(dispersion_m)
dispersion_turn<-betadisper(turnover, group=sp_mat$site)
permutest(dispersion_turn)
dispersion_nest<-betadisper(nestedness, group=sp_mat$site)
permutest(dispersion_nest)


##############################################################################################################################


# plot

teleo_plot <- ggplot(teleo2plot, aes(x=A1, y=A2))+
  geom_point(aes(x=A1, y=A2, color=port, shape=habitat), size=2.5)+
  scale_shape_manual(values = c(16, 17), 
                     name = "Habitat",  labels = c("Biohut","Port"))+
  scale_color_manual(values = c("#FF575C","#F18F01","#FCF300", "#94F000", "#0087DB", "#A05CFF", "#F5A6E6"))+#, 
  #name = "Port", labels = c("Port_Vendres", "Agde", "Marseillan", "Saintes_Maries_de_la_Mer","La_Ciotat","Porquerolles","Cannes")) +
  scale_size_manual(values=c(2.5,2.5))+
  ylab("PCoA2")+
  xlab("PCoA1")+
  xlim(-0.6,0.8)+
  ylim(-0.6,0.4)+
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "#FCFCFC", colour = NA),
        axis.title = element_text(size=15),
        panel.border = element_rect(fill = NA),
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        plot.title = element_text(size=18))+
  ggtitle("PCoA teleost species composition amoung ports")

teleo_plot

ggsave("PCoA_teleo_species_bis.png", width = 11, height = 8)

