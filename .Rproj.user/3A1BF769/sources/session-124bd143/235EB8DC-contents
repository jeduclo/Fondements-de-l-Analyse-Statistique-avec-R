# Chargement de dplyr pour la manipulation des données
library(dplyr)

# Création d'un tibble avec des indicateurs économiques
pays = tibble(age_population = c(26, 30, 28, 31, 25, 29, 30, 29),
              code_pays = c('SG', 'CN', 'UK', 'UK','CN', 'SG', 'IN', 'SG'),
              genre_rep = c('F', 'F', 'M', 'M', 'M', 'F', 'F', 'M'),
              pib_par_habitant = c(168, 169, 175, 178, 170, 170, 172, 180))

# Ajout du nom complet des pays en utilisant recode
pays_nouveau = pays %>% 
  mutate(nom_complet_pays = recode(code_pays,
                                   "SG"="Singapour",
                                   "CN"="Chine",
                                   "UK"="Royaume-Uni",
                                   "IN"="Inde"))
pays_nouveau

# Utilisation de recode_factor pour convertir en facteur
pays_nouveau = pays_nouveau %>% 
  mutate(nom_complet_pays2 = recode_factor(code_pays,
                                           "SG"="Singapour",
                                           "CN"="Chine",
                                           "UK"="Royaume-Uni",
                                           "IN"="Inde"))
pays_nouveau

####################################
#Création de catégories basées sur l'âge et le pays
# Classification des pays selon l'âge de la population et la région géographique
pays_nouveau = pays %>% 
  mutate(type = case_when(age_population >= 30 & code_pays %in% c("SG","IN","CN") ~ "asie_30+",
                          age_population < 30 & age_population >= 20 & code_pays %in% c("SG","IN","CN") ~ "asie_20+",
                          TRUE ~ "autres"))
pays_nouveau

####################################
#Groupement par tranches d'âge
# Création de groupes d'âge pour la population
pays_nouveau = pays %>% 
  mutate(groupe_age = cut(x = age_population,
                          breaks = c(-Inf, 25, 30, Inf),
                          labels = c("<=25", "26-30", ">30")))
pays_nouveau

# Utilisation de tidyverse pour plus d'options de découpage
library(tidyverse)
pays_nouveau = pays %>% 
  mutate(groupe_age = cut_interval(age_population, n=3))
pays_nouveau
summary(pays_nouveau$groupe_age)

pays_nouveau = pays %>% 
  mutate(groupe_age = cut_number(age_population, n=3))
pays_nouveau
summary(pays_nouveau$groupe_age)

####################################
#Transformation de données longues en larges
# Transformation du tibble en format large avec spread
pays_large = pays %>% 
  spread(key = code_pays, value = pib_par_habitant)
pays_large

# Remplacement des NA par la moyenne du PIB par habitant
moyenne_pib = round(mean(pays$pib_par_habitant))
pays_large2 = pays %>% 
  spread(key = code_pays, value = pib_par_habitant, fill = moyenne_pib)
pays_large2

####################################
#ransformation de données larges en longues
# Retour au format long avec gather
pays_long = pays_large %>% 
  gather(key = "code_pays", value = "pib_par_habitant", CN:UK)
pays_long

# Suppression des lignes avec NA
pays_long = pays_long %>% 
  drop_na(pib_par_habitant)
pays_long

# Vérification de l'égalité des tibbles
all_equal(pays, pays_long, ignore_row_order = T, ignore_col_order = T)

####################################
#Manipulation de chaînes de caractères
# Exemples de différentes façons d'encadrer des chaînes de caractères avec des guillemets
"atelier de statistiques"
""atelier" de statistiques"
'"atelier" de statistiques'
"\"atelier\" de statistiques"
writeLines("\"atelier\" de statistiques")

####################################
#Formatage de nombres
# Formatage de grands nombres avec des séparateurs de milliers
format(123000, big.mark = ",")
# Formatage de nombres en notation scientifique
format(123000, scientific = TRUE)
# Formatage de nombres avec un nombre spécifique de chiffres significatifs
format(1.256, digits = 3)
# Arrondi d'un nombre à un nombre spécifié de décimales
round(1.256, digits = 2)

####################################
#Concaténation de chaînes de caractères
# Concaténation de chaînes avec un espace
paste("statistiques", "atelier")
# Concaténation de chaînes sans espace
paste("statistiques", "atelier", sep = "")
# Autre méthode pour concaténer des chaînes sans espace
paste0("statistiques", "atelier")
# Concaténation de vecteurs de chaînes avec un autre terme
paste(c("statistiques", "atelier"), "cours")
# Concaténation et fusion de chaînes avec un séparateur spécifique
paste(c("statistiques", "atelier"), "cours", collapse = " + ")

####################################
#Utilisation de stringr pour la manipulation de chaînes
# Chargement de la bibliothèque stringr pour des fonctions avancées de manipulation de chaînes
library(stringr)
# Concaténation avec un espace en utilisant str_c
str_c("statistiques", "atelier", sep = " ")
# Concaténation de vecteurs avec un séparateur et un collapse
str_c(c("statistiques", "atelier"), "cours", sep = " ", collapse = " + ")

# Calcul de la longueur des chaînes
str_length(c("statistiques", "atelier"))
# Autre méthode pour calculer la longueur des chaînes
nchar(c("statistiques", "atelier"))
# Sous-chaînage pour extraire une partie d'une chaîne
str_sub(c("statistiques", "atelier"), start = 1, end = 3)

####################################
#Recherche et comptage dans des chaînes
# Détection de la présence d'un motif dans une chaîne
str_detect(c("statistiques", "atelier"), "stat")
# Extraction de chaînes contenant un motif spécifique
str_subset(c("statistiques", "atelier"), "stat")
# Comptage du nombre d'occurrences d'un caractère dans des chaînes
str_count(c("statistiques", "atelier"), "t")


####################################
#Division de chaînes de caractères
# Division d'une chaîne en utilisant un caractère spécifique comme séparateur
str_split(c("statistiques & apprentissage automatique atelier"), "&")
# Division avec un séparateur comprenant plusieurs caractères
str_split(c("statistiques & apprentissage automatique atelier"), " & ")
# Accès à un élément spécifique du résultat de la division
str_split(c("statistiques & apprentissage automatique atelier"), " & ")[[1]][2]
# Division de plusieurs chaînes avec un séparateur
str_split(c("statistiques & apprentissage automatique atelier", "stats & ml & atelier"), " & ")
# Division avec simplification pour obtenir une matrice
str_split(c("statistiques & apprentissage automatique atelier", "stats & ml & atelier"), " & ", simplify = TRUE)

# Remplacement d'un motif dans des chaînes
str_replace(c("statistiques & apprentissage automatique atelier", "stats & ml & atelier"), pattern = "&", replacement = "et")
# Remplacement de tous les motifs correspondants dans des chaînes
str_replace_all(c("statistiques & apprentissage automatique atelier", "stats & ml & atelier"), pattern = "&", replacement = "et")

####################################
#Manipulation avancée de chaînes de caractères
# Remplacement d'un motif dans une chaîne et manipulation des résultats
titre = "statistiques et apprentissage automatique atelier"
titre = str_replace(titre, pattern = "et", replacement = "&")
titre
a = str_split(titre, " & ")
a
b = str_c(str_sub(a[[1]][1], 1, 4), str_sub(a[[1]][1], -1, -1))
b
c = unlist(str_split(a[[1]][2], " "))
c
d = str_c(str_sub(c[1], 1, 1), str_sub(c[2], 1, 1))
d
e = str_c(b, "&", d, c[3], sep = " ")
e

# Utilisation d'expressions régulières pour détecter des motifs
library(rebus)
str_detect(c("statistiques", "apprentissage automatique"), pattern = START %R% "s")
START
str_view(c("statistiques", "apprentissage automatique"), pattern = START %R% "s")

####################################
#Utilisation des expressions régulières avancées
# Sélection de chaînes de caractères en utilisant des motifs complexes
texts = c("stats 101", "apprentissage automatique", "R 101 ABC atelier", "101 R atelier")
str_subset(texts, pattern = "apprentissage" %R% END)
str_subset(texts, pattern = ANY_CHAR %R% "101")
ANY_CHAR
str_subset(texts, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% "a")
str_subset(texts, pattern = START %R% or("stats", "R"))
str_subset(texts, pattern = one_or_more(char_class("aA")))

####################################
#Analyse de texte avec tidytext
# Utilisation de tidytext pour la tokenisation et l'analyse de fréquence des mots
install.packages("tidytext")
install.packages("stopwords")
library(tidytext)
library(stopwords)
texts = c("stats 101", "Apprentissage Automatique", "R et ML atelier", "R atelier & Statistiques avec R")
texts_df = tibble(id = 1:length(texts),
                  texte = texts)
texts_df

tidy_df <- texts_df %>% 
  unnest_tokens(token_unite, texte)
tidy_df

tidy_df2 <- texts_df %>% 
  unnest_tokens(token_unite, texte, token = "ngrams", n = 2)
tidy_df2

tidy_df %>%
  count(token_unite, sort = TRUE)

# Filtrage des mots vides
get_stopwords()
tidy_df2 = tidy_df %>%
  filter(!(token_unite %in% get_stopwords()$word)) %>% 
  count(token_unite, sort = TRUE)
tidy_df2

####################################
#Création d'une matrice Document-Terme
# Comptage des occurrences des mots par document
count_df = tidy_df %>% 
  group_by(id, token_unite) %>% 
  summarise(count=n())
count_df

# Transformation en Document-Terme Matrix (DTM)
install.packages("tm")
library(tm)
dtm = count_df %>%
  cast_dtm(id, token_unite, count)
dtm
as.data.frame(as.matrix(dtm), stringsAsFactors=FALSE)

# Manipulation de DTM avec tidytext pour une meilleure intégration avec dplyr et ggplot2
tidy_dtm = tidy(dtm)
tidy_dtm
