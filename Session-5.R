
####################################
#Analyse initiale des données
####################################
library(readr)
library(dplyr)
library(ggplot2)

# Remplacer l'URL par un lien vers des données économiques réelles si disponible
df = read_csv("https://lien-vers-vos-donnees.csv")
df

# Exploration unique des alignements et sexes (dans un contexte économique, cela pourrait être des catégories d'entreprises et des régions, par exemple)
unique(df$categorie)
unique(df$region)

# Filtrage pour éliminer les NA
df = df %>% 
  filter(!is.na(categorie),
         !is.na(region))
dim(df)
sum(is.na(df$categorie))
sum(is.na(df$region))

# Tableau croisé pour analyser la distribution
table(df$categorie, df$region)

# Graphique en barres pour visualiser la distribution des catégories par région
ggplot(df, aes(x=region, fill=categorie)) +
  geom_bar() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.position = c(0.2, 0.8),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20)) 


####################################
#Propriétés et visualisation des proportions
options(scipen=999, digits=3)

# Calcul des proportions pour chaque combinaison
count_df = table(df$categorie, df$region)

# Table des proportions totales
prop.table(count_df)

# Propriétés conditionnelles sur les lignes (par catégorie)
prop.table(count_df, margin=1)
rowSums(prop.table(count_df, margin=1))

# Propriétés conditionnelles sur les colonnes (par région)
prop.table(count_df, margin=2)
colSums(prop.table(count_df, margin=2))

# Graphique en barres des proportions
df %>% 
  filter(!(region %in% c("Région1", "Région2"))) %>% 
  ggplot(aes(x=region, fill=categorie)) +
  geom_bar(position="fill") +
  ylab("proportion") + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20)) 

####################################
#Visualisation de la distribution des années
# Résumé des années (pour des données économiques, cela pourrait être des années de fondation d'entreprises, par exemple)
summary(df$Annee)

# Différentes méthodes pour visualiser la distribution des années
ggplot(df, aes(x=Annee)) +
  geom_dotplot(dotsize=0.2) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold")) 

ggplot(df, aes(x=Annee)) +
  geom_histogram() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold")) 

ggplot(df, aes(x=Annee)) +
  geom_density() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold")) 

ggplot(df, aes(x=Annee)) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold")) 

ggplot(df, aes(x=Annee)) +
  geom_boxplot() +
  facet_wrap(~region) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        strip.text.x = element_text(size = 30)) 

####################################
#Densité et histogramme par catégorie et région
# Visualisation de la densité et de l'histogramme des années par catégorie et région
df %>% 
  filter(!(region %in% c("Région1", "Région2"))) %>% 
  ggplot(aes(x=Annee)) +
  geom_density() +
  facet_grid(categorie ~ region, labeller = label_both) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 12)) 

df %>% 
  filter(!(region %in% c("Région1", "Région2"))) %>% 
  ggplot(aes(x=Annee)) +
  geom_histogram() +
  facet_grid(categorie ~ region, labeller = label_both) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 12)) 

####################################
#Résumés statistiques
# Calcul de la moyenne des apparitions (dans un contexte économique, cela pourrait être le calcul de la moyenne des investissements, par exemple)
mean(df$Investissements, na.rm = TRUE)
# Résumé des investissements pour obtenir un aperçu de la distribution
summary(df$Investissements)
# Médiane des investissements, pour identifier la valeur centrale
median(df$Investissements, na.rm = TRUE)

# Calcul du mode (la valeur la plus fréquente)
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode(df$Investissements)

# Résumé par catégorie économique
df %>% 
  group_by(Categorie) %>% 
  summarise(mean_invest = mean(Investissements, na.rm=TRUE),
            median_invest = median(Investissements, na.rm=TRUE))


####################################
#Variance et écart-type
tmp = df$Investissements[!is.na(df$Investissements)]
# Calcul de la variance de la population
pop_var = sum((tmp - mean(tmp))^2)/length(tmp)
formatC(pop_var, digits = 2, format = "f")

# Calcul de la variance de l'échantillon
sample_var = sum((tmp - mean(tmp))^2)/(length(tmp)-1)
formatC(sample_var, digits = 2, format = "f")

# Vérification avec la fonction var de R
formatC(var(tmp), digits = 2, format = "f")

# Calcul de l'écart-type
sd(tmp)

# Calcul de l'intervalle interquartile
IQR(tmp)
# Résumé pour obtenir un aperçu complet
summary(tmp)

# Exclusion de la valeur maximale pour voir son impact sur l'écart-type
tmp2 = tmp[tmp != max(tmp)]
sd(tmp2)
IQR(tmp2)

# Résumé par catégorie économique
df %>% 
  group_by(Categorie) %>% 
  summarise(sd_invest = sd(Investissements, na.rm=TRUE),
            IQR_invest = IQR(Investissements, na.rm=TRUE),
            count = n())

####################################
#Densité et distributions après 2000
# Filtrage pour les données après l'année 2000
tmp = df %>% 
  filter(Annee >= 2000)

# Visualisation de la densité des investissements avec une distinction par catégorie
ggplot(tmp, aes(x=Investissements, fill=Categorie)) + 
  geom_density(alpha=0.2) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.position = c(0.8, 0.8),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20))

# Exclusion des valeurs extrêmes pour affiner la visualisation
tmp = tmp %>% 
  filter(Investissements <= quantile(Investissements, 0.9, na.rm=TRUE))

# Répétition de la visualisation de densité sans les valeurs extrêmes
ggplot(tmp, aes(x=Investissements, fill=Categorie)) + 
  geom_density(alpha=0.2) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.position = c(0.8, 0.8),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20))

# Utilisation du logarithme des investissements pour normaliser la distribution et faciliter la visualisation
ggplot(tmp, aes(x=log(Investissements), fill=Categorie)) + 
  geom_density(alpha=0.2) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.position = c(0.8, 0.8),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size=20))

####################################
#Importation et visualisation de données financières
# Installation et chargement de yfR pour récupérer des données financières
install.packages("yfR")
library(yfR)

# Définition des dates de début et de fin pour la récupération des données
first_date = as.Date("2021-01-01")
last_date = as.Date("2024-01-01")
# Définition des tickers des entreprises à analyser
my_ticker <- c('META', 'NFLX', 'GOOG', 'AMZN', 'MSFT')

# Récupération des données
df <- yf_get(tickers = my_ticker, 
             first_date = first_date,
             last_date = last_date)
# Aperçu de la structure des données récupérées
str(df)

####################################
#Visualisation des tendances financières
# Visualisation des tendances des prix ajustés pour chaque entreprise
ggplot(df, 
       aes(x = ref_date, y = price_adjusted,
           color = ticker)) + 
  geom_line() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20))

# Histogramme des prix ajustés pour visualiser leur distribution
ggplot(df, aes(x=price_adjusted, fill=ticker)) +
  geom_histogram(bins=100) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20))

# Densité des prix ajustés pour une analyse plus fine de la distribution
ggplot(df, aes(x=price_adjusted, fill=ticker)) +
  geom_density(alpha=0.2) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20))

# Boîte à moustaches pour comparer la distribution des prix ajustés entre les entreprises
ggplot(df, aes(ticker, price_adjusted, fill=ticker)) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20))

# Résumé statistique des prix ajustés par entreprise
df %>% 
  group_by(ticker) %>% 
  summarise(mean = mean(price_adjusted, na.rm=TRUE),
            sd = sd(price_adjusted, na.rm=TRUE),
            IQR = IQR(price_adjusted, na.rm=TRUE),
            count = n())


