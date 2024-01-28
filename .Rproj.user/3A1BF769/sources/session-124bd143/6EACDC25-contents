# Affectation de chaîne de caractères
phrase = "Je suis une chaîne de caractères"
print(phrase)
# Calcul simple
somme = 1 + 2
print(somme)

####################################
# Opérations de base avec des variables économiques
prix = 5
quantite = 2
prix + quantite
prix - quantite
prix * quantite
prix / quantite
prix ^ quantite
prix %% quantite

####################################
# Types de données et opérations en économie
inflation = 1.0; taux_interet = 1; monnaie = "euro"; marche_ouvert = TRUE; secteur = factor("énergie")
class(inflation); class(taux_interet); class(monnaie); class(marche_ouvert); class(secteur)

inflation + taux_interet
class(inflation + taux_interet)

inflation + monnaie

inflation + marche_ouvert
class(inflation + marche_ouvert)
inflation + !marche_ouvert

inflation == marche_ouvert

class(as.numeric(taux_interet))

as.integer(1.8)
round(1.8)

as.factor(inflation)
as.factor(monnaie)

####################################
# Vecteurs et opérations vectorielles en économie
investissement = c(1,2,3)
epargne = c(1,1,1)
sum(investissement)
mean(investissement)
investissement + epargne
investissement + 1
investissement + c(1,2)
investissement > epargne
investissement == epargne

####################################
#  Accès aux éléments d'un vecteur
investissement[1]
investissement[c(1,3)]
investissement[c(1,2,3)]
investissement[1:3]
investissement[investissement > epargne]

####################################
# Matrices
matrice_investissement = matrix(c(investissement,epargne), nrow=2, byrow=TRUE)
matrice_investissement

rownames(matrice_investissement) = c("r1", "r2")
colnames(matrice_investissement) = c("c1", "c2", "c3")
matrice_investissement

####################################
#Accès aux éléments d'une matrice
matrice_investissement[1,2]
matrice_investissement[1:2,c(2,3)]
matrice_investissement[2,]
matrice_investissement[rownames(matrice_investissement)=="r2",]
matrice_investissement[,3]
matrice_investissement[,colnames(matrice_investissement)=="c3"]

####################################
#Opérations sur les matrices 
matrice_epargne = matrice_investissement * 2
matrice_epargne
matrice_investissement / matrice_epargne
rowSums(matrice_investissement)
colSums(matrice_investissement)
rowMeans(matrice_investissement)
colMeans(matrice_investissement)

####################################
#Combinaison de matrices
cbind(matrice_investissement, matrice_epargne)
rbind(matrice_investissement, matrice_epargne)

####################################
#Travail avec un jeu de données
data("iris") 
dim(iris)
head(iris)
tail(iris)
str(iris)

df = data.frame("investissement"=investissement, "epargne"=epargne)

####################################
#Accès et sous-ensemble d'un dataframe
df[,2]
df[1:3,2]
df[,"epargne"]
df$epargne

subset(df, investissement>2)
subset(df, investissement>2, select="epargne")

####################################
#Trier et ordonner des données
croissance = c(5,1,10)
order(croissance)
croissance[order(croissance)]

df[order(-df$investissement),]

####################################
#Listes et manipulations
liste_economique = list(taux_interet, investissement, df)
liste_economique
liste_economique[[2]]

names(liste_economique) <- c("taux_interet", "investissement", "df")
liste_economique
liste_economique[['investissement']]
liste_economique$investissement

liste_economique[['nouvelle_entree']] = "test"
liste_economique

liste_economique[['df']] = NULL
liste_economique

liste_economique[['investissement']] = c(1,2)
liste_economique

####################################
#Comparaisons et logique
1 == 2
"économie" == "finance"
TRUE == TRUE
TRUE == FALSE

1 != 2
"économie" != "finance"
TRUE != TRUE
TRUE != FALSE

1 < 2
"économie" > "finance"
TRUE > FALSE

1 >= 2
2 <= 2

(1 > 2) | (1 == 2)
(2 < 2) | (2 == 2)

investissement > 1

####################################
#Opérations logiques
TRUE & FALSE
TRUE & TRUE
FALSE & FALSE
1 > 0 & 1 < 2

TRUE | FALSE
TRUE | TRUE
FALSE | FALSE
1 < 0 | 1 < 2

!TRUE
!FALSE
!(1<0)

c(TRUE, FALSE) & c(TRUE, TRUE)
c(TRUE, FALSE) | c(TRUE, TRUE)
!c(TRUE, FALSE)

c(TRUE, FALSE) && c(FALSE, TRUE)
c(TRUE, FALSE) || c(FALSE, TRUE)

####################################
#Structures conditionnelles
x = 1
if(x > 0){
  print("positif")
} else {
  print("non positif")
}

x = 0
if(x > 0){
  print("positif")
} else if(x == 0){
  print("zéro")
} else {
  print("négatif")
}

####################################
#Boucles while
x = 2
while(x < 10){
  x = x^2
  print(x)
}
x

x = 2
while(x < 10){
  x = x^2
  if(x > 10){
    break
  }
  print(x)
}
x

####################################
#Boucles for
termes_economiques = c("inflation","et","taux_de_change")
for(i in termes_economiques){
  print(i)
}

for(i in 1:length(termes_economiques)){
  print(termes_economiques[i])
}

for(i in termes_economiques){
  if(i == "et"){
    break
  }
  print(i)
}

for(i in termes_economiques){
  if(i == "et"){
    next
  }
  print(i)
}


####################################
#Fonctions personnalisées 
fonction_test = function(x, maj=FALSE){
  msg = paste(x,"c'est amusant !")
  if(maj){
    msg = toupper(msg)
  }
  return(msg)
}
fonction_test("R")
fonction_test("R",maj=TRUE)
fonction_test()


