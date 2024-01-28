####################################
# Introduction à tidyverse et transformation de données
# Installation et chargement de tidyverse pour le traitement des données
install.packages("tidyverse")
library(dplyr)

# Chargement du jeu de données iris et vérification de son type
data("iris")
class(iris)

# Transformation d'iris en tibble pour une meilleure manipulation avec dplyr
iris_tbl = as_tibble(iris)
class(iris_tbl)

# Affichage du tibble iris
iris_tbl

####################################
#Filtrage et manipulation des données
# Identification des espèces uniques dans le dataset
unique(iris_tbl$Species)

# Filtrage des données pour l'espèce setosa
iris_tbl_subset = iris_tbl %>% 
  filter(Species == "setosa")
iris_tbl_subset
# Vérification des espèces uniques dans le sous-ensemble
unique(iris_tbl_subset$Species)
unique(as.character(iris_tbl_subset$Species))

# Filtrage supplémentaire pour setosa avec une longueur de sépale <= 5
iris_tbl_subset = iris_tbl %>% 
  filter(Species == "setosa",
         Sepal.Length <= 5)
# Trouver la longueur maximale de sépale dans ce sous-ensemble
max(iris_tbl_subset$Sepal.Length)
# Dimension du sous-ensemble filtré
dim(iris_tbl_subset)

####################################
#Tri et arrangement des données
# Trier le tibble iris par longueur de sépale croissante
iris_tbl_sorted = iris_tbl %>% 
  arrange(Sepal.Length)
iris_tbl_sorted

# Trier le tibble iris par longueur de sépale décroissante
iris_tbl_sorted = iris_tbl %>% 
  arrange(desc(Sepal.Length))
iris_tbl_sorted

# Filtrage puis tri par longueur et largeur de sépale décroissantes pour setosa
iris_tbl_subset_sorted = iris_tbl %>% 
  filter(Species == "setosa",
         Sepal.Length <= 5) %>% 
  arrange(desc(Sepal.Length), desc(Sepal.Width))
iris_tbl_subset_sorted

####################################
#Mutation et création de nouvelles variables
# Vérification du type de la variable Species avant mutation
paste("Before:", class(iris_tbl$Species))
# Mutation de Species en type caractère
iris_tbl = iris_tbl %>%
  mutate(Species = as.character(Species))
# Vérification après mutation
paste("After:", class(iris_tbl$Species))

# Création d'une nouvelle variable indiquant si la largeur du sépale est supérieure à la longueur du pétale
iris_tbl = iris_tbl %>% 
  mutate(ind = Sepal.Width > Petal.Length)
iris_tbl
# Tableau de contingence pour la nouvelle variable
table(iris_tbl$ind)

# Filtrage pour les observations où ind est TRUE
iris_tbl_subset = iris_tbl %>%
  filter(ind == TRUE)
table(iris_tbl_subset$ind)

# Filtrage direct sans créer une variable intermédiaire
iris_tbl_subset2 = iris_tbl %>%
  filter(Sepal.Width > Petal.Length)
nrow(iris_tbl_subset2)

####################################
#Sélection et transformation de colonnes
# Sélection de colonnes spécifiques
rst = iris_tbl %>%
  select(Sepal.Length, Sepal.Width, Petal.Length)
rst

# Sélection de colonnes par intervalle
rst = iris_tbl %>%
  select(Sepal.Length:Petal.Length)
rst

# Sélection de colonnes contenant un terme spécifique
rst = iris_tbl %>%
  select(contains("length"))
rst

# Sélection de colonnes commençant par un préfixe spécifique
rst = iris_tbl %>%
  select(starts_with("petal"))
rst

# Renommage d'une colonne lors de la sélection
rst = iris_tbl %>%
  select(Sepal.Length, Sepal_Width=Sepal.Width)
rst

# Création d'une nouvelle colonne en transformant les données
rst = iris_tbl %>%
  transmute(Species, Diff = abs(Sepal.Length - Petal.Length))
rst

####################################
#Travail avec les fonctions d'agrégation et de tri
# Sélection du top 1 par longueur de sépale
rst = iris_tbl %>%
  top_n(1, Sepal.Length)
rst

# Une autre méthode pour obtenir le top 1 par longueur de sépale
rst = iris_tbl %>%
  arrange(desc(Sepal.Length)) %>% 
  head(1)
rst

# Sélection du top 1 par longueur de sépale pour chaque espèce
rst = iris_tbl %>%
  group_by(Species) %>% 
  top_n(1, Sepal.Length) %>% 
  select(Species, Sepal.Length)
rst

# Calcul de la longueur maximale de sépale pour chaque espèce
rst = iris_tbl %>%
  group_by(Species) %>% 
  summarize(max_sepal_length = max(Sepal.Length))
rst

# Sélection de l'espèce avec la plus grande longueur maximale de sépale
rst = iris_tbl %>%
  group_by(Species) %>% 
  summarize(max_sepal_length = max(Sepal.Length)) %>% 
  top_n(1, max_sepal_length)
rst

####################################
#Opérations avancées sur les données
# Calcul de la moyenne des différences absolu...
rst = iris_tbl %>%
  top_n(80, Sepal.Length) %>%
  filter(Sepal.Width > Petal.Length) %>% 
  mutate(Diff = abs(Sepal.Length - Petal.Length)) %>% 
  select(Diff) %>% 
  colMeans()
rst

####################################
#Comptage et tri des observations
# Comptage du nombre d'observations par espèce
rst = iris_tbl %>%
  count(Species)
rst

# Comptage et tri des espèces selon une condition spécifique
rst = iris_tbl %>%
  filter(abs(Sepal.Length-Sepal.Width) > abs(Petal.Length-Petal.Width)) %>% 
  count(Species, sort=TRUE)
rst

####################################
#Résumé et agrégation des données
# Résumé du nombre d'observations par espèce
rst = iris_tbl %>%
  group_by(Species) %>% 
  summarise(n=n())
rst

# Résumé et tri selon une condition spécifique
rst = iris_tbl %>%
  filter(abs(Sepal.Length-Sepal.Width) > abs(Petal.Length-Petal.Width)) %>% 
  group_by(Species) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
rst

# Calcul de la longueur moyenne de sépale selon une condition spécifique
rst = iris_tbl %>%
  mutate(ind = abs(Sepal.Length-Sepal.Width) > abs(Petal.Length-Petal.Width)) %>% 
  group_by(Species, ind) %>% 
  summarise(mean_sepal_length=mean(Sepal.Length))
rst

# Dégrouper après l'agrégation pour éviter les erreurs dans les manipulations futures
rst = iris_tbl %>%
  mutate(ind = abs(Sepal.Length-Sepal.Width) > abs(Petal.Length-Petal.Width)) %>% 
  group_by(Species, ind) %>% 
  summarise(mean_sepal_length=mean(Sepal.Length)) %>% 
  ungroup()
rst

####################################
#Jointures entre tables
# Création de deux tables de données pour démontrer les jointures
a = 1:3
tbl_A = tibble(key_A=a, col_A=2*a)
tbl_B = tibble(key_B=a+1, col_B=3*a)
tbl_A
tbl_B

# Jointure interne
rst = tbl_A %>% 
  inner_join(tbl_B, by=c("key_A"="key_B"))
rst

# Jointure gauche
rst = tbl_A %>% 
  left_join(tbl_B, by=c("key_A"="key_B"))
rst

# Jointure gauche avec des doublons dans la table B
tbl_C = tbl_B %>% 
  bind_rows(tbl_B[1,])
tbl_C[nrow(tbl_C),"col_B"] = 10
tbl_C

rst = tbl_A %>% 
  left_join(tbl_C, by=c("key_A"="key_B"))
rst

# Jointure droite
rst = tbl_A %>% 
  right_join(tbl_B, by=c("key_A"="key_B"))
rst

# Jointure complète
rst = tbl_A %>% 
  full_join(tbl_B, by=c("key_A"="key_B"))
rst

# Nettoyage des NA après une jointure complète
library(tidyr)
rst = tbl_A %>% 
  full_join(tbl_B, by=c("key_A"="key_B")) %>% 
  drop_na()
rst

rst = tbl_A %>% 
  full_join(tbl_B, by=c("key_A"="key_B")) %>% 
  replace_na(list(col_A=0, col_B=0))
rst

####################################
# Importation et analyse des données externes
# Importation des données depuis des fichiers CSV en ligne
library(readr)
df_questions = read_csv("https://raw.githubusercontent.com/PacktPublishing/The-Statistics-and-Machine-Learning-with-R-Workshop/main/Chapter_2/data/questions.csv")
df_questions
summary(df_questions$score)

df_question_tags = read_csv("https://raw.githubusercontent.com/PacktPublishing/The-Statistics-and-Machine-Learning-with-R-Workshop/main/Chapter_2/data/question_tags.csv")
df_question_tags

df_tags = read_csv("https://raw.githubusercontent.com/PacktPublishing/The-Statistics-and-Machine-Learning-with-R-Workshop/main/Chapter_2/data/tags.csv")
df_tags

# Jointure des tables pour enrichir les données
df_all = df_questions %>% 
  left_join(df_question_tags, by=c("id"="question_id"))
df_all

df_all = df_all %>% 
  left_join(df_tags, by=c("tag_id"="id"))
df_all

# Filtrage pour les tags non nuls et comptage
df_all = df_all %>% 
  filter(!is.na(tag_name))
rst = df_all %>% 
  count(tag_name, sort = TRUE)
rst

# Analyse temporelle avec lubridate
library(lubridate)
rst = df_all %>% 
  mutate(year = year(creation_date)) %>% 
  count(year)
rst

# Extraction du maximum de la date de création
max(df_all$creation_date)

# Création de nouvelles variables pour l'analyse temporelle
df_all = df_all %>% 
  mutate(month = month(creation_date),
         year_month = format(creation_date, "%Y%m")) 
df_all

# Comptage et analyse moyenne par mois
rst1 = df_all %>% 
  count(year_month, month)
rst1

rst2 = rst1 %>% 
  group_by(month) %>% 
  summarise(avg_num_tag = mean(n))
rst2

# Résumé statistique par tag
rst = df_all %>% 
  group_by(tag_name) %>% 
  summarise(count = n(),
            min_score = min(score),
            mean_score = mean(score),
            max_score = max(score)) %>% 
  arrange(desc(count))
rst
