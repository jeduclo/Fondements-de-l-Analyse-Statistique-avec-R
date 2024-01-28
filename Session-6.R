#Récupération des données boursières de Google depuis 2007
####################################
## Chargement de la bibliothèque pour la manipulation des données boursières
install.packages("quantmod")
library(quantmod)

# Récupération des données boursières de Google
df = getSymbols("GOOG", auto.assign=FALSE)

# Affichage du nombre total d'observations dans df
nrow(df)

# Affichage des deux premières lignes de df
head(df, 2)

# Affichage des deux dernières lignes de df
tail(df, 2)

# Tracé des données du prix de l'action
chartSeries(df$GOOG.Close, name="Prix de l'action de Google")


####################################
#Réalisation d'une analyse de données simple
# Chargement des bibliothèques pour la manipulation et la visualisation des données
library(dplyr)
library(tibble)

# Conversion des données en tibble et ajout d'une colonne date
df_tbl = df %>% 
  as_tibble() %>% 
  add_column(date = index(df), .before = 1)

# Calcul du prix maximum depuis le début de l'année
max_ytd = df_tbl %>% 
  filter(date >= as.Date("2024-01-01")) %>% 
  summarise(price = max(GOOG.Close)) %>% 
  .$price

# Calcul du prix moyen depuis le début de l'année
avg_ytd = df_tbl %>% 
  filter(date >= as.Date("2024-01-01")) %>% 
  summarise(price = mean(GOOG.Close)) %>% 
  .$price

# Calcul du prix minimum depuis le début de l'année
min_ytd = df_tbl %>% 
  filter(date >= as.Date("2024-01-01")) %>% 
  summarise(price = min(GOOG.Close)) %>% 
  .$price


####################################
#Ajout de graphiques
# Chargement de la bibliothèque pour la création de graphiques
library(ggplot2)
# Chargement de la bibliothèque pour la manipulation de dates
library(lubridate)

# Préparation des données pour le graphique, avec ajout des mois et années
df_tbl = df_tbl %>% 
  mutate(Month = factor(month(date), levels = as.character(1:12)),
         Year = as.character(year(date)))

# Filtrage et agrégation des données pour les années 2021 à 2023
tmp_df = df_tbl %>% 
  filter(Year %in% c(2021, 2022, 2023)) %>% 
  group_by(Year, Month) %>% 
  summarise(avg_close_price = mean(GOOG.Close)) %>% 
  ungroup()

# Création d'un graphique de la moyenne mensuelle du prix de clôture entre 2019 et 2021
p = ggplot(tmp_df, 
           aes(x = Month, y = avg_close_price,
               group = Year,
               color = Year)) + 
  geom_line() + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16, face="bold"),
        legend.text=element_text(size=20)) +
  labs(title = "Prix de clôture moyen mensuel entre 2022 et 2023",
       x = "Mois de l'année",
       y = "Prix de clôture moyen")
p

####################################
#Ajout de tableaux
# Chargement de la bibliothèque pour créer des tableaux
library(knitr)

# Affichage des premières lignes de tmp_df sous forme de tableau
kable(tmp_df[1:5,])

# Changement des noms de colonnes
kable(tmp_df[1:5,], col.names=c("Année", "Mois", "Prix de clôture moyen"))

# Alignement du tableau
kable(tmp_df[1:5,], col.names=c("Année", "Mois", "Prix de clôture moyen"), align="ccc")

# Ajout d'une légende au tableau
kable(tmp_df[1:5,], col.names=c("Année", "Mois", "Prix de clôture moyen"), align="ccc", caption="Tableau 1.1 Prix de clôture moyen mensuel")

####################################
#Options de bloc de code
# Affichage du code et du résultat par défaut
tmp_df = df_tbl %>% 
  mutate(Année = as.integer(Year)) %>% 
  filter(Année >= max(Année)-5,
         Année < max(Année)) %>% 
  group_by(Année) %>% 
  summarise(max_closing = max(GOOG.Close))
kable(tmp_df)

# Exécution du bloc de code mais cachant le code et le résultat dans la sortie
total_max_price = max(df_tbl$GOOG.Close)

# Affichage du code et du résultat
total_max_price

# Exécution du bloc de code et affichage uniquement du résultat
kable(tmp_df)

# Ne pas exécuter le bloc de code mais afficher le code dans la sortie
kable(tmp_df)


####################################
####################################
####################################
#Génération de rapport en utilisant des paramètres
# Statistiques résumées de l'année spécifiée par le paramètre
df_tbl %>% 
  mutate(Année = as.integer(Year)) %>%
  filter(Année == params$Année) %>% 
  select(GOOG.Close) %>% 
  summary()

# Statistiques résumées de l'année et du trimestre spécifiés par les paramètres
df_tbl %>% 
  mutate(Trimestre = quarters(date)) %>% 
  filter(Année == params$année,
         Trimestre == params$trimestre) %>% 
  select(GOOG.Close) %>% 
  summary()

