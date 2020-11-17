# Chargement des libraries
library(tidyverse)
library(openxlsx)
library(igraph)
library(maptools)
library(ggraph)

# Chargement des données
# On va chercher le fichier Excel dans le directory StatSpatiale de Tinto
# erasmus = read.xlsx("Student_Mobility_2013-14.xlsx")
erasmus = read.xlsx("C:/Users/jacin/Desktop/WorkInProgress/StatSpatiale/Student_Mobility_2013-14.xlsx")

# Elimination des colonnes inutiles pour ce travail
erasmus_v1 = erasmus %>%
  select(c(-1,-3, -4, -7,-8,-9,-11, -15, -16, -18, -22, -24))

# Création de paires sending-receiving  
erasmus_v1 = erasmus_v1 %>%
  unite(pair, SendingCountry, ReceivingCountry, sep = ",", remove = FALSE)
  

# On garde l'info sending-receiving dans une variable
effectif = erasmus_v1 %>%
  count(pair)

# On change le type de df a tibble
effectif  = as_tibble(effectif)
class(effectif)

# On met chaque pays dans une colonne 
effectif_bycol = effectif %>%
  separate(pair, c("sending", "receiving"), sep=',')

# Matrice de flux
p = effectif_bycol %>%
  pivot_wider(names_from = receiving, values_from=n, values_fill=0) %>%
  column_to_rownames(var = "sending")

# On regle le problème de la diagonale 
p2 = p[colnames(p), ]

# On transforme son format à matrix
p2 = as.matrix(p2)


# Chaîne de Markov associé a p2
m = colnames(p)
margin = rowSums(p2)
W = matrix(nrow = 34, ncol = 34)
rownames(W) = m
colnames(W)= m
for(i in 1:nrow(p2)){
  for(j in 1:ncol(p2)){
    W[i,j] = p2[i,j]/margin[i]
  }
}

