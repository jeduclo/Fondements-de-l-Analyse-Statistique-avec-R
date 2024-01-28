#Introduction à la régression linéaire
####################################
# Fixer la graine pour la reproductibilité
set.seed(123)
# Générer la variable indépendante X
X = runif(100, min = 1, max = 100) # 100 nombres uniformes aléatoires entre 1 et 100
# Générer du bruit
noise = rnorm(100, mean = 0, sd = 10) # 100 nombres normaux aléatoires avec moyenne 0 et écart-type 10
# Générer la variable dépendante Y
Y = 5 + 0.5 * X + noise
# Combiner X et Y dans un dataframe
data = data.frame(X, Y)

# Ajuster un modèle de régression linéaire simple
model = lm(Y ~ X, data = data)
# Afficher le résumé du modèle
summary(model)

# Tracer les données
plot(data$X, data$Y, main = "Régression Linéaire Simple", xlab = "X", ylab = "Y")
# Ajouter la ligne de régression ajustée
abline(model, col = "red")


####################################
#Introduction à la régression linéaire multiple
# Charger les données
data(mtcars)
# Construire le modèle
model = lm(mpg ~ cyl + hp + wt, data = mtcars)

# Afficher le résumé du modèle
summary(model)

####################################
#Introduction au paradoxe de Simpson
set.seed(123)
x1 = rnorm(100)
x2 = -3 * x1 + rnorm(100)
y = 2 + x1 + x2 + rnorm(100)
df = data.frame(y = y, x1 = x1, x2 = x2)

# Régression linéaire simple
single_reg = lm(y ~ x1, data = df)
summary(single_reg)

# Régression linéaire multiple
multi_reg = lm(y ~ x1 + x2, data = df)
summary(multi_reg)

####################################
#Travailler avec des variables catégorielles
# Ajuster le modèle
model <- lm(mpg ~ qsec + am, data = mtcars)
# Afficher le résumé du modèle
summary(model)

# Convertir am en variable catégorielle
mtcars$am_cat = as.factor(mtcars$am)
# Ajuster le modèle
model <- lm(mpg ~ qsec + am_cat, data = mtcars)
# Afficher le résumé du modèle
summary(model)

# Charger la bibliothèque requise
library(ggplot2)
# Créer un nouveau dataframe pour les prédictions
newdata = data.frame(qsec = seq(min(mtcars$qsec), max(mtcars$qsec), length.out = 100),
                     am_cat = c(rep(0, 100), rep(1, 100)))
newdata$am_cat = as.factor(newdata$am_cat)
# Obtenir les prédictions
newdata$mpg_pred = predict(model, newdata)
# Tracer les données et les lignes de régression
ggplot(data = mtcars, aes(x = qsec, y = mpg, color = am_cat)) +
  geom_point() +
  geom_line(data = newdata, aes(y = mpg_pred)) +
  labs(title = "mpg vs qsec par Type de Transmission",
       x = "Temps au Quart de Mile (qsec)",
       y = "Miles par Gallon (mpg)",
       color = "Type de Transmission") +
  scale_color_discrete(labels = c("Automatique", "Manuel")) +
  theme(text = element_text(size = 16), 
        title = element_text(size = 15), 
        axis.title = element_text(size = 18), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 16), 
        legend.position = "bottom") 

####################################
#Introduction au terme d'interaction
# Ajout du terme d'interaction
model_interaction <- lm(mpg ~ qsec * am_cat, data = mtcars)
# Imprimer le résumé du modèle
summary(model_interaction)

# Créer un graphique à points avec deux lignes d'intersection
ggplot(mtcars, aes(x = qsec, y = mpg, color = as.factor(am_cat))) +
  geom_point() +
  geom_smooth(method ="lm", se = FALSE) + # ajuster des lignes de régression séparées par groupe
  scale_color_discrete(name ="Type de Transmission",
                       labels = c("Automatique", "Manuel")) +
  labs(x ="Temps au quart de mile (secondes)",
       y ="Miles par gallon",
       title ="Lignes de régression séparées ajustées pour les voitures automatiques et manuelles") +
  theme(text = element_text(size = 16),
        title = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

####################################
#Gérer les termes non linéaires
# Créer un ensemble de données quadratique
set.seed(1)
x = seq(-10, 10, by = 0.5)
y = x^2 + rnorm(length(x), sd = 5)
# Mettre dans un dataframe
df = data.frame(x = x, y = y)
# Ajuster une régression linéaire simple sur les données et imprimer le résumé.
lm1 <- lm(y ~ x, data = df)
summary(lm1)

# Ajuster une régression avec un terme quadratique
lm2 <- lm(y ~ x + I(x^2), data = df)
summary(lm2)

# Tracer les données avec les prédictions linéaires et quadratiques
ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color ="blue", linetype ="dashed") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color ="red") +
  labs(title ="Graphique à points avec ajustement linéaire et quadratique",
       x ="",
       y ="") +
  theme(text = element_text(size = 15)) +
  scale_color_discrete(name ="Mode",
                       labels = c("Mode Linéaire", "Mode Quadratique")) +
  annotate("text", x = 0, y = 40, label ="Mode Linéaire", color ="blue") +
  annotate("text", x = 6, y = 80, label ="Mode Quadratique", color ="red")

####################################
#Plus sur la transformation logarithmique
# Ajuster le modèle original
model_original = lm(mpg ~ hp, data = mtcars)
# Ajuster le modèle avec transformation log
mtcars$log_mpg = log(mtcars$mpg)
model_log = lm(log_mpg ~ hp, data = mtcars)
# Prédictions du modèle original
mtcars$pred_original = predict(model_original, newdata = mtcars)
# Prédictions du modèle transformé log (retransformé à l'échelle originale avec exp)
mtcars$pred_log = exp(predict(model_log, newdata = mtcars))
library(tidyr)
library(dplyr)
# Remodeler les données au format long
df_long <- mtcars %>%
  gather(key ="Mode", value ="Prediction", pred_original, pred_log)
# Créer le graphique
ggplot(df_long, aes(x = hp, y = mpg)) +
  geom_point(data = mtcars, aes(x = hp, y = mpg)) +
  geom_line(aes(y = Prediction, color = Mode)) +
  labs(
    x ="Puissance (hp)",
    y ="Miles par gallon (mpg)",
    color ="Mode"
  ) +
  scale_color_manual(values = c("pred_original" ="blue", "pred_log" ="red")) +
  theme(
    legend.position ="bottom",
    text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.text = element_text(size = 16),  # contrôle la taille de la police des étiquettes d'axe
    legend.text = element_text(size = 16)  # contrôle la taille de la police du texte de légende
  )

####################################
#Travailler avec la solution à forme fermée
# Fixer la graine pour la reproductibilité
set.seed(123)
# Générer des données synthétiques
n = 100 # nombre d'observations
x = runif(n, -10, 10) # prédicteurs
beta0 = 2 # intercept
beta1 = 3 # pente
epsilon = rnorm(n, 0, 2) # terme d'erreur aléatoire
y = beta0 + beta1*x + epsilon # variable réponse
# Matrice de conception X
X = cbind(1, x)

# Calculer les estimations de beta
beta_hat = solve(t(X) %*% X) %*% t(X) %*% y
print(beta_hat)

# Ajuster un modèle de régression linéaire pour comparaison
model = lm(y ~ x)
print(coef(model))

####################################
#Gérer la multicollinéarité
# Installer le package si pas déjà installé
if(!require(car)) install.packages('car')
# Charger le package
library(car)
# Ajuster un modèle linéaire
model = lm(mpg ~ hp + wt + disp, data = mtcars)
# Calculer le facteur d'inflation de la variance (VIF)
vif_values = vif(model)
print(vif_values)

####################################
#Gérer l'hétéroscédasticité
# Charger la bibliothèque
library(lmtest)
# Ajuster un modèle de régression linéaire simple sur le jeu de données mtcars
model = lm(mpg ~ wt + hp, data = mtcars)
# Effectuer un test de Breusch-Pagan pour vérifier formellement l'hétéroscédasticité
bptest(model)

####################################
#Introduction à la régression linéaire pénalisée - 
#Travailler avec la régression ridge
# Installer le package si pas déjà installé
if(!require(glmnet)) install.packages('glmnet')
library(glmnet)

# Préparer les données
data(mtcars)
X = as.matrix(mtcars[, -1]) # prédicteurs
y = mtcars[, 1] # réponse

# Ajuster un modèle de régression ridge
set.seed(123) # pour la reproductibilité
ridge_model = glmnet(X, y, alpha = 0)

# Utiliser la validation croisée pour trouver le lambda optimal
cv_ridge = cv.glmnet(X, y, alpha = 0)
best_lambda = cv_ridge$lambda.min
print(best_lambda)

# Ajuster un nouveau modèle de régression ridge avec le lambda optimal
opt_ridge_model = glmnet(X, y, alpha = 0, lambda = best_lambda)
# Obtenir les coefficients
ridge_coefs = coef(opt_ridge_model)[-1]  # retirer l'intercept
print(ridge_coefs)

# Régression des moindres carrés ordinaires
ols_model = lm(mpg ~ ., data = mtcars)
# Obtenir les coefficients
ols_coefs = coef(ols_model)[-1] # retirer l'intercept
print(ols_coefs)

# Comparer graphiquement les coefficients OLS et Ridge
plot(1:length(ols_coefs), ols_coefs, type="b", col="blue", pch=19, xlab="Coefficient", ylab="Valeur", ylim=c(min(ols_coefs, ridge_coefs), max(ols_coefs, ridge_coefs)))
lines(1:length(ridge_coefs), ridge_coefs, type="b", col="red", pch=19)
legend("bottomright", legend=c("OLS", "Ridge"), col=c("blue", "red"), pch=19)

####################################
#Mise en œuvre de la régression lasso
lasso_model = glmnet(X, y, alpha = 1)

# Utiliser la validation croisée pour trouver le lambda optimal
cv_lasso = cv.glmnet(X, y, alpha = 1)
best_lambda = cv_lasso$lambda.min
print(best_lambda)

# Ajuster un nouveau modèle de régression lasso avec le lambda optimal
opt_lasso_model = glmnet(X, y, alpha = 1, lambda = best_lambda)

# Obtenir les coefficients
lasso_coefs = coef(opt_lasso_model)[-1]  # retirer l'intercept
print(lasso_coefs)

# Comparer graphiquement les coefficients OLS, Ridge et Lasso
plot(1:length(ols_coefs), ols_coefs, type="b", col="blue", pch=19, xlab="Coefficient", ylab="Valeur", ylim=c(min(ols_coefs, ridge_coefs, lasso_coefs), max(ols_coefs, ridge_coefs, lasso_coefs)))
lines(1:length(ridge_coefs), ridge_coefs, type="b", col="red", pch=19)
lines(1:length(lasso_coefs), lasso_coefs, type="b", col="green", pch=19)
legend("bottomright", legend=c("OLS", "Ridge", "Lasso"), col=c("blue", "red", "green"), pch=19)

