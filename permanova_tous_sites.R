### Load the libraries
library(dplyr)
library(rsq)
library(purrr)
library(ggplot2)
library(vegan)
library(betapart)

#COMPARAISON PORT ET HORS PORT TOUT CONFONDU SANS LES BH

# Metadata
meta_tot <- read.csv("metadata_tot.csv", header=T)
# Enlever les pts biohuts
meta_tot <- meta_tot[,-1]
meta_tot <- meta_tot[-94,]
meta_tot <- meta_tot[-91,]
meta_tot <- meta_tot[-84,]

## Load the eDNA data (matrix species per sample)
adne <- read.csv("matrice_adne_totale.csv", header=T, row.names=1)
# Enlever les pts biohuts
adne <- adne[,-97]
adne <- adne[,-93]
adne <- adne[,-86]

# Joindre matrice metadata et ADNe
sp_mat <- adne %>%
  t(.) %>%
  as.data.frame(.) %>%
  tibble::rownames_to_column(var="Samples") %>%
  inner_join(meta_tot, by=c("Samples" = "code_spygen"))

## create dissimilarity matrix (Jaccard distance for presence/absence)
sp.dist<-vegdist(t(adne), method='jaccard')

## Partition beta-diversity in turnover and nestedness
beta.dist <- beta.pair(t(adne), index.family = "jac")
turnover <- beta.dist[[1]]
nestedness <- beta.dist[[2]]

## perform PERMANOVA : tester la significativité de la variable "méthode d'échantillonnage"
# sur la diversité béta, le turnover et la nestedness
habitat.div<-adonis2(sp.dist~site, data=sp_mat, permutations = 999, method="jaccard")
habitat.div # signif
habitat.turn<-adonis2(turnover~site, data=sp_mat, permutations = 999, method="jaccard")
habitat.turn # signif
habitat.nest<-adonis2(nestedness~site, data=sp_mat, permutations = 999, method="jaccard")
habitat.nest # not signif

## Multivariate dispersion : tester l'homogénéité des variances entre les méthodes
dispersion_m<-betadisper(sp.dist, group=sp_mat$site)
permutest(dispersion_m)
dispersion_turn<-betadisper(turnover, group=sp_mat$site)
permutest(dispersion_turn)
dispersion_nest<-betadisper(nestedness, group=sp_mat$site)
permutest(dispersion_nest)


###########################################################
## Plots with ggplot
###########################################################
library(gridExtra)
# extract the centroids and the site points in multivariate space.  
centroids<-data.frame(grps=rownames(dispersion_m$centroids),data.frame(dispersion_m$centroids))
vectors<-data.frame(group=dispersion_m$group,data.frame(dispersion_m$vectors))

# to create the lines from the centroids to each point we will put it in a format that ggplot can handle
seg.data<-cbind(vectors[,1:3],centroids[rep(1:nrow(centroids),as.data.frame(table(vectors$group))$Freq),2:3])
names(seg.data)<-c("group","v.PCoA1","v.PCoA2","PCoA1","PCoA2")

#ajout colonne couleur 
color <- c("#F18F01","black","#F5A6E6","#0087DB","#FCF300","black","black","#A05CFF","#FF575C","#94F000")
centroids <- cbind(centroids,color)
centroids <- centroids %>% dplyr::select("color", 
                                     everything())
## Draw plot
# Create a theme
theme_mine <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 15),
  axis.text = element_text(size = 16, colour = "gray25"),
  axis.title = element_text(size = 18, colour = "gray25"),
  legend.position = "right",
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 18),
  legend.key = element_blank())

# plot
panel.a<-ggplot() + 
  #geom_polygon(data=all.hull,aes(x=v.PCoA1,y=v.PCoA2, colour=group, fill=after_scale(alpha(colour, 0.3)))) +
  geom_point(data=centroids[,1:4], aes(x=PCoA1,y=PCoA2,colour=grps, shape=grps),size=4) + 
  geom_point(data=seg.data, aes(x=v.PCoA1,y=v.PCoA2, colour=group, shape=group),size=2) +
  theme_mine + 
  scale_colour_manual(values=centroids[,1]) +#, labels = c("Hors port","Port")) + #ou choisir juste les oculeurs 
  scale_shape_manual(values=c(1,2,1,1,1,6,5,1,1,1), labels = c("Agde","Banyuls","Cannes","La Ciotat","Marseillan","Occitanie","PACA","Porquerolles","Port Vendres","Saintes Maries de la Mer")) +
  labs(title="",x="PCoA 1",y="PCoA 2")#,
       #colour = "Zone d'échantillonnage", shape = "Zone d'échantillonnage")
panel.a


ggsave("PCoA_horsport_port_bis.png", width = 11, height = 8)




