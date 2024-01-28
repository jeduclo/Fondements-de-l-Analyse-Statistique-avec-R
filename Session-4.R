#Introduction à ggplot2 
####################################
#
# Chargement de la bibliothèque ggplot2 pour la visualisation de données
library(ggplot2)

# Aperçu de la structure des données (ici, on imagine des données économiques similaires)
str(mtcars)

# Création d'un graphique de points avec ggplot2 (ici, remplaçons cyl par un indicateur économique et mpg par un autre)
ggplot(mtcars, aes(x=cyl, y=mpg)) +
  geom_point()

# Identification des valeurs uniques pour l'indicateur économique (cyl)
unique(mtcars$cyl)

# Modification de l'axe x pour afficher les valeurs comme des facteurs
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point()

# Création d'un diagramme en boîte pour analyser la distribution de mpg par cylindre (indicatif de distribution économique par catégorie)
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot()

####################################
#Amélioration visuelle avec couleur et taille
# Ajout de couleur au graphique en fonction de disp (ici, considérons disp comme un indicateur de la taille de l'économie)
ggplot(mtcars, aes(factor(cyl), mpg, color=disp)) +
  geom_point()

# Ajout de la dimension de la taille en fonction de hp (représentant ici la puissance économique)
ggplot(mtcars, aes(factor(cyl), mpg, color=disp, size=hp)) +
  geom_point()


####################################
#Ajout de lissage et transparence
# Utilisation de transparence et ajout d'une courbe de tendance (lissage) pour mieux visualiser la relation
ggplot(mtcars, aes(hp, mpg, color=disp)) +
  geom_point(alpha=0.6) +
  geom_smooth()

# Construction progressive d'un graphique
plt = ggplot(mtcars, aes(hp, mpg)) +
  geom_point(alpha=0.6)
plt

# Ajout de couleur et de lissage au graphique existant
plt = plt +
  geom_point(aes(color=disp)) +
  geom_smooth()
plt

####################################
#Personnalisation des points
# Personnalisation de la forme et taille des points
ggplot(mtcars, aes(hp, mpg, color=disp)) +
  geom_point(shape=1, size=4)

ggplot(mtcars, aes(hp, mpg, color=disp)) +
  geom_point(shape=2, size=2)

# Utilisation de fill pour colorier l'intérieur des points avec cyl comme catégorie économique
ggplot(mtcars, aes(hp, mpg, fill = factor(cyl))) +
  geom_point(shape = 21, size = 5, alpha = 0.6)


####################################
#Ajout de texte et jitter pour une meilleure visualisation
# Ajout de texte au graphique pour identifier les points (par exemple, noms des pays ou régions économiques)
ggplot(mtcars, aes(hp, mpg)) +
  geom_text(label=row.names(mtcars))

# Utilisation de jitter pour éviter le chevauchement des textes et ajout de style au texte
ggplot(mtcars, aes(hp, mpg)) +
  geom_text(label=row.names(mtcars),
            fontface = "bold",
            position=position_jitter(width=20,height=20))

# Utilisation de jitter avec geom_point pour mieux distinguer les points proches
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point()

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_jitter()

####################################
#Visualisation de groupes avec couleur
# Coloration des points en fonction de cyl pour distinguer les groupes économiques
ggplot(mtcars, aes(hp, mpg, color=factor(cyl))) +
  geom_point()

# Autre manière de définir la couleur en fonction de cyl
ggplot(mtcars, aes(hp, mpg)) +
  geom_point(aes(col=factor(cyl)))

# Calcul de la moyenne de toutes les variables par cylindre (groupe économique) avec dplyr
library(dplyr)
tmp = mtcars %>% 
  group_by(factor(cyl)) %>% 
  summarise_all(mean)
tmp

# Superposition des points moyens sur le graphique initial pour mettre en évidence les moyennes par groupe
ggplot(mtcars, aes(x=hp, y=mpg, color=factor(cyl))) +
  geom_point() +
  geom_point(data=tmp, shape=15, size=6)


####################################
#Histogrammes pour analyser la distribution
# Création d'un histogramme pour visualiser la distribution de hp (ici, considéré comme un indicateur économique tel que le PIB par habitant)
ggplot(mtcars, aes(x=hp)) +
  geom_histogram()

# Ajustement de la largeur des bacs pour affiner l'analyse
ggplot(mtcars, aes(x=hp)) +
  geom_histogram(binwidth=40)

# Coloration par cyl pour montrer la distribution par catégorie économique
ggplot(mtcars, aes(x=hp, fill=factor(cyl))) +
  geom_histogram(binwidth=40, position="stack")

# Utilisation de la position dodge pour comparer les distributions côte à côte
ggplot(mtcars, aes(x=hp, fill=factor(cyl))) +
  geom_histogram(binwidth=40, position="dodge")

# Affichage de la proportion au lieu du compte total pour chaque cylindre
ggplot(mtcars, aes(x=hp, fill=factor(cyl))) +
  geom_histogram(binwidth=40, position="fill") + 
  ylab("Proportion")

####################################
#Diagrammes à barres pour les comparaisons catégorielles
# Visualisation du nombre de voitures par cylindre avec un remplissage par gear (ici, imaginez cyl et gear comme des indicateurs économiques, tels que le nombre d'entreprises par secteur et leur taille)
ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +
  geom_bar(position="stack")

# Utilisation de la position fill pour voir la proportion de chaque gear dans les cylindres
ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +
  geom_bar(position="fill")

# Comparaison des catégories side by side avec dodge
ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +
  geom_bar(position="dodge")

# Ajustement précis de la position dodge pour les espacements
ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +
  geom_bar(position = position_dodge(width=0.2))


####################################
#Personnalisation de thèmes dans ggplot2
# Chargement du package zoo pour la manipulation de séries temporelles
library(zoo)
# Préparation des données JohnsonJohnson pour une visualisation, en ajoutant des dates pour une série temporelle
JohnsonJohnson2 = data.frame(qtr_earning=as.matrix(JohnsonJohnson), 
                             date=as.Date(as.yearmon(time(JohnsonJohnson))))
head(JohnsonJohnson2, n=3)

# Utilisation de dplyr pour ajouter des indicateurs et quartiles
library(dplyr)
JohnsonJohnson2 = JohnsonJohnson2 %>% 
  mutate(ind = if_else(date >= as.Date("1975-01-01"), TRUE, FALSE),
         qtr = quarters(date))
head(JohnsonJohnson2, n=3)

# Création d'un graphique linéaire pour visualiser les gains par trimestre
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning)) +
  geom_line()

# Distinguer avant et après 1975 par couleur
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=ind)) +
  geom_line()

# Coloration par quartile pour voir les variations saisonnières
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line()


####################################
#Améliorations esthétiques et utilisation de thèmes
# Amélioration esthétique avec le positionnement de la légende et ajustement de la taille de texte
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme(legend.position="bottom",
        axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20)) 

# Personnalisation avancée du thème
tmp = ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                                  color=qtr)) +
  geom_line() +
  theme(legend.position=c(0.1,0.8),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20))
tmp

# Ajustement des couleurs et lignes du thème
tmp = tmp + 
  theme(
    axis.title=element_text(color="blue"),
    axis.line = element_line(color = "black", linetype = "solid")
  )
tmp

# Suppression des grilles pour un fond épuré
tmp = tmp + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
tmp

# Utilisation de thème classique pour un look épuré
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme_classic() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20))

# Exploration de thèmes supplémentaires avec ggthemes
install.packages("ggthemes")
library(ggthemes)

# Thème inspiré par FiveThirtyEight
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20))

# Thème inspiré par Edward Tufte
ggplot(JohnsonJohnson2, aes(x=date, y=qtr_earning, 
                            color=qtr)) +
  geom_line() +
  theme_tufte() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18, face="bold"),
        legend.text = element_text(size=20))

