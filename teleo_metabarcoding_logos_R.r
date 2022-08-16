## ---------------------------
##
## Metabarcoding logos
##
## Author: Morgane Bruno
##
## Date Created: 2022-01-10
## ----------------------------

#_libs
library(ggplot2)
library(ggseqlogo)
library(Biostrings)

#_main

## import des séquences (mafft: fasta, marker teleo)
sequences <- Biostrings::readDNAStringSet("teleo_mafft.fasta")

## visualisation alignement
sequences

## Représentation logo
### pfm = position frequency matrix 
pfm <- Biostrings::consensusMatrix(sequences) 

ggplot() + 
  geom_logo(pfm, method = 'p', seq_type = 'dna') + 
  annotate('rect', xmin = 0.5, xmax = 17.5, ymin = -0.05, ymax = 1.05, alpha = .1, col='black', fill='yellow') +
  annotate('rect', xmin = 83.5, xmax = 104.7, ymin = -0.05, ymax = 1.05, alpha = .1, col='black', fill='yellow') +
  annotate('text', x=8, y=1.3, label='forward primer') + 
  annotate('text', x=93, y=1.3, label='reverse primer') + 
  annotate('segment', x = 21.5, xend= 66.5, y=-0.5, yend=-0.5, size=2) + 
  annotate('text', x=44, y=-0.25, label='Variable region = barcode') +
  theme_logo()
