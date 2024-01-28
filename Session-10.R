# Introduction à la régression logistique - Comprendre la fonction sigmoïde
####################################
# Créer une série de nombres également espacés entre -10 et 10
x = seq(-10, 10, by = 0.1)
# Calculer la valeur de sortie pour chaque nombre en utilisant la fonction sigmoïde
sigmoid = 1 / (1 + exp(-x))
# Tracer la fonction sigmoïde
plot(x, sigmoid, type = "l", lwd = 2,
     main = "Fonction Sigmoïde",
     xlab = "x",
     ylab = "f(x)",
     col = "red")
# Ajouter des lignes de grille
grid()

####################################
#Comparaison de la régression logistique avec la régression linéaire
# installer le package caret si vous ne l'avez pas fait
install.packages("caret")
# charger le package caret
library(caret)
# charger le jeu de données German Credit
data(GermanCredit)
GermanCredit$Class_num = ifelse(GermanCredit$Class == "Bad", 1, 0)

# Modèle de régression linéaire
lm_model = lm(Class_num ~ Duration, data=GermanCredit)
coefs = coefficients(lm_model)
intercept = coefs[1]
slope = coefs[2]

# Tracer les données avec la ligne de régression linéaire
ggplot(GermanCredit,
       aes(Duration, Class_num)) +
  geom_point() +
  geom_abline(intercept=intercept, slope=slope) +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

# Tracer les données avec la ligne de régression logistique
glm_model = glm(Class_num ~ Duration, data=GermanCredit, family=binomial)
summary(glm_model)

ggplot(GermanCredit,
       aes(Duration, Class_num)) +
  geom_point() +
  geom_smooth(
    method = "glm",
    se = FALSE,
    method.args = list(family=binomial)
  ) +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

####################################
#Faire des prédictions avec le modèle de régression logistique
library(tibble)
library(dplyr)
# faire des prédictions
pred_df = tibble(
  Duration = seq(5, 80, 2)
)
pred_df = pred_df %>%
  mutate(
    pred_prob = predict(glm_model, pred_df, type="response")
  )

ggplot() +
  geom_point(data = GermanCredit, aes(Duration, Class_num)) +
  geom_point(data = pred_df, aes(Duration, pred_prob) , color="blue") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

# obtenir le résultat le plus probable
pred_df = pred_df %>%
  mutate(
    most_likely_outcome = round(pred_prob)
  )

ggplot() +
  geom_point(data = GermanCredit, aes(Duration, Class_num)) +
  geom_point(data = pred_df, aes(Duration, pred_prob) , color="blue") +
  geom_point(data = pred_df, aes(Duration, most_likely_outcome) , color="green") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

####################################
#Plus sur les log odds et le ratio de cotes
# calculer les log odds en utilisant les probabilités prédites
pred_df = pred_df %>%
  mutate(
    log_odds = log(pred_prob / (1 - pred_prob))
  )

####################################
#Évaluation d'un modèle de régression logistique
# Créer un nouveau dataframe avec toutes les durées
new_data = data.frame(Duration = GermanCredit$Duration)
# Calculer les classes prédites basées sur les probabilités prédites
predicted_probs = predict(glm_model, new_data, type="response")

# Convertir en résultats binaires
predicted_classes = ifelse(predicted_probs > 0.5, 1, 0)

# Créer une matrice de confusion
conf_matrix = table(predicted = predicted_classes, actual = GermanCredit$Class_num)
conf_matrix

# Précision
accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Précision: ", accuracy))

# Taux d'erreur
error_rate = 1 - accuracy
print(paste("Taux d'erreur: ", error_rate))

# Précision
precision = conf_matrix[2,2] / sum(conf_matrix[2,])
print(paste("Précision: ", precision))

# Rappel / Sensibilité
recall = conf_matrix[2,2] / sum(conf_matrix[,2])
print(paste("Rappel: ", recall))

# Spécificité
specificity = conf_matrix[1,1] / sum(conf_matrix[,1])
print(paste("Spécificité: ", specificity))

library(pROC)
# Calculer la courbe ROC
roc_obj = roc(GermanCredit$Class_num, predicted_probs)
# Tracer la courbe ROC
plot(roc_obj)

# Calculer l'AUC
auc = auc(roc_obj)
print(paste("AUC: ", auc))

####################################
#Gestion d'un ensemble de données déséquilibré - 
#réalisation de sous-échantillonnage et de suréchantillonnage
set.seed(2)
index = sample(1:nrow(GermanCredit), nrow(GermanCredit)*0.7)
train = GermanCredit[index, ]
test = GermanCredit[-index, ]

table(train$Class_num)

# séparer les classes minoritaires et majoritaires
minority_data = train[train$Class_num == 1,]
majority_data = train[train$Class_num == 0,]

# sous-échantillonner la classe majoritaire
undersampled_majority = majority_data[sample(1:nrow(majority_data), nrow(minority_data)),]
# combiner la classe majoritaire sous-échantillonnée et la classe minoritaire
undersampled_data = rbind(minority_data, undersampled_majority)
table(undersampled_data$Class_num)

# suréchantillonner la classe minoritaire
oversampled_minority = minority_data[sample(1:nrow(minority_data), nrow(majority_data), replace = TRUE),]
# combiner la classe majoritaire et la classe minoritaire suréchantillonnée
oversampled_data = rbind(majority_data, oversampled_minority)
table(oversampled_data$Class_num)

# ajuster les modèles de régression logistique sur les données sous-échantillonnées et suréchantillonnées
undersampled_model = glm(Class_num ~ Duration, family = binomial(link = 'logit'), data = undersampled_data)
oversampled_model = glm(Class_num ~ Duration, family = binomial(link = 'logit'), data = oversampled_data)

# obtenir les probabilités prédites sur l'ensemble de test
undersampled_pred = predict(undersampled_model, newdata = test, type = "response")
oversampled_pred = predict(oversampled_model, newdata = test, type = "response")

# appliquer un seuil pour convertir les probabilités en classes binaires
undersampled_pred_class = ifelse(undersampled_pred > 0.5, 1, 0)
oversampled_pred_class = ifelse(oversampled_pred > 0.5, 1, 0)

# calculer la matrice de confusion
undersampled_cm = table(predicted = undersampled_pred_class, actual = test$Class_num)
oversampled_cm = table(predicted = oversampled_pred_class, actual = test$Class_num)
undersampled_cm
oversampled_cm


####################################
#Régression logistique pénalisée
# Créer une matrice de prédicteurs et un vecteur de réponse
# Pour glmnet, nous devons fournir nos données sous forme de matrices/vecteurs
X = model.matrix(Class_num ~ . -1, data = GermanCredit)
y = GermanCredit$Class_num
# Définir une valeur alpha : 0 pour ridge, 1 pour lasso, entre 0 et 1 pour elastic net
alpha_value = 1 # pour lasso
# Exécuter le modèle glmnet
fit = glmnet(X, y, family = "binomial", alpha = alpha_value)

# Tracer les chemins des coefficients
plot(fit, xvar = "lambda", label = TRUE)

####################################
#Extension à la classification multi-classes
library(nnet)
# Convertir gear en facteur
mtcars$gear = as.factor(mtcars$gear)
table(mtcars$gear)

# Ajuster le modèle
multinom_model = multinom(gear ~ mpg + hp + disp, data = mtcars)
# Vue d'ensemble du modèle
summary(multinom_model)

# Faire des prédictions
predicted_gears = predict(multinom_model, newdata = mtcars)
# Afficher la matrice de confusion
table(Predicted = predicted_gears, Actual = mtcars$gear)

