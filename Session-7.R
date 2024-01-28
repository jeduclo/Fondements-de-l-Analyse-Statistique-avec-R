
####################################
#La distribution de Bernoulli
p = 0.6
# Produire un résultat de Bernoulli aléatoire
outcome = rbinom(1, size = 1, prob = p)
print(outcome)

set.seed(8)
rbinom(1, size = 1, prob = p)

# Nombre d'expériences
n = 5
# Générer les résultats correspondants
outcomes = rbinom(n, size = 1, prob = p)
print(outcomes)

# Calculer la moyenne et la variance
mean_bernoulli = p
var_bernoulli = p * (1 - p)
cat("Moyenne:", mean_bernoulli, "\nVariance:", var_bernoulli)

# Nombre de succès
num_successes = sum(outcomes)
# Probabilité empirique de succès
empirical_p = num_successes / n
cat("Nombre de succès:", num_successes, "\nProbabilité empirique de succès:", empirical_p)

n = 1000
num_successes = sum(rbinom(n, size = 1, prob = p))
empirical_p = num_successes / n
cat("Nombre de succès:", num_successes, "\nProbabilité empirique de succès:", empirical_p)


####################################
#Probabilités binomiales pour différentes occurrences de succès
n = 10 # Nombre d'essais
p = 0.5 # Probabilité de succès
binom_probs = dbinom(0:n, n, p)
binom_probs

barplot(binom_probs, names.arg = 0:n, xlab = "Nombre de succès", ylab = "Probabilité", main = "Distribution Binomiale (n = 10, p = 0.5)")

cum_binom_probs <- pbinom(0:n, n, p)
cum_binom_probs

prob_at_least_7_successes = 1 - pbinom(6, n, p)
prob_at_least_7_successes

n = 5
p = 0.8
prob_at_least_4_wins = 1 - pbinom(3, n, p)
prob_at_least_4_wins

prob_at_most_3_wins = pbinom(3, n, p)
prob_at_most_3_wins

prob_at_most_3_wins == 1 - prob_at_least_4_wins

####################################
#Approximation normale d'une distribution binomiale
n = 100
p = 0.5
# Vérifier les conditions pour une approximation normale
n*p > 10
n*p*(1-p) > 10

# Calculer la moyenne et l'écart-type
mu = n*p
mu
std = sqrt(n*p*(1-p))
std

# Calculer P(lower_limit <= X <= upper_limit) en utilisant les scores z
lower_limit = 40
upper_limit = 60
standard_lower_limit = (lower_limit - mu) / std
standard_upper_limit = (upper_limit - mu) / std
standard_lower_limit
standard_upper_limit

# Approximation en utilisant la fonction de distribution cumulée normale standard
pnorm(standard_upper_limit) - pnorm(standard_lower_limit)

# Utiliser la distribution binomiale
pbinom(upper_limit, n, p) - pbinom(lower_limit, n, p)


####################################
#La distribution de Poisson
lambda = 5 # Paramètre de la distribution
pois_probs = dpois(0:15, lambda)
pois_probs

barplot(pois_probs, names.arg = 0:15, xlab = "Nombre d'événements", ylab = "Probabilité", main = "Distribution de Poisson (lambda = 5)")

cum_pois_probs = ppois(0:15, lambda)
cum_pois_probs

barplot(cum_pois_probs, names.arg = 0:15, xlab = "Nombre d'événements", ylab = "Probabilité Cumulative", main = "FDC de la Distribution de Poisson (lambda = 5)")

pois_samples = rpois(100, lambda)
pois_samples

####################################
#Approximation de Poisson à la distribution binomiale
# Paramètres binomiaux
n = 1000
p = 0.01
# Probabilité d'observer 15 succès
binom_prob = dbinom(15, n, p)
binom_prob

lambda_approx = n * p
lambda_approx

pois_approx_prob <- dpois(15, lambda_approx)
pois_approx_prob


####################################
#La distribution géométrique
# Paramètres
p = 0.25 # Probabilité de succès
# Obtenir les probabilités géométriques
geom_probs = dgeom(0:9, p)
geom_probs

barplot(geom_probs, names.arg = 1:10, xlab = "Nombre d'essais", ylab = "Probabilité", main = "Distribution Géométrique (p = 0.25)")

cum_geom_probs = pgeom(0:9, p)
cum_geom_probs

geom_samples = rgeom(100, p)
geom_samples

p = 0.1 # Probabilité de trouver un bug à chaque essai
# Calculer la FDC pour jusqu'à 5 essais
prob_within_5_attempts = pgeom(4, p)
prob_within_5_attempts

sum(dgeom(0:4, p))

mean_attempts <- 1 / p
mean_attempts

geom_probs <- dgeom(0:19, p)
# Créer un graphique en barres des probabilités
barplot(geom_probs, names.arg = 1:20, xlab = "Nombre d'essais", ylab = "Probabilité", main = "Distribution Géométrique (p = 0.1)")


####################################
#La distribution normale
# Paramètres
mu = 0      # Moyenne
sigma = 1   # Écart-type
# Obtenir la densité de probabilité pour différents x
x = seq(-4, 4, by = 0.1)
normal_density = dnorm(x, mu, sigma)

# Tracer la distribution normale
plot(x, normal_density, type = "l", xlab = "x", ylab = "Densité de Probabilité", main = "Distribution Normale (μ = 0, σ = 1)")

# Obtenir les probabilités cumulatives pour différents x
normal_cum_prob <- pnorm(x, mu, sigma)

plot(x, normal_cum_prob, type = "l", xlab = "x", ylab = "Densité de Probabilité Cumulative", main = "Distribution Normale Cumulative (μ = 0, σ = 1)")

# Générer 100 échantillons aléatoires d'une distribution normale avec μ = 0 et σ = 1
normal_samples <- rnorm(100, mu, sigma)

# Trouver le quantile correspondant au 90e percentile
quantile_90 <- qnorm(0.9, mu, sigma)
quantile_90

set.seed(8)
mean_lifespan = 100
sd_lifespan = 10
n = 1000
lifespans = rnorm(n, mean_lifespan, sd_lifespan)

threshold = 120
probability = 1 - pnorm(threshold, mean_lifespan, sd_lifespan)
probability

df <- data.frame(lifespan = lifespans)
df_density <- density(lifespans)
df_shaded <- data.frame(x = df_density$x, y = df_density$y)
df_shaded <- df_shaded[df_shaded$x > threshold,]
ggplot(df, aes(x=lifespan)) +
  geom_density(fill="lightblue") +
  geom_vline(xintercept = threshold, linetype="dashed", color="red") +
  geom_area(data = df_shaded, aes(x=x, y=y), fill="orange", alpha=0.5) +
  theme_minimal() +
  labs(title="Durée de vie des batteries", x="Durée de vie (heures)", y="Densité de Probabilité")

####################################
#La distribution exponentielle
set.seed(8) # Fixer la graine pour la reproductibilité
lambda = 0.01
taille_echantillon = 1000
echantillon_exponentiel = rexp(taille_echantillon, rate = lambda)

seuil = 150
probabilite_au_dessus_seuil = 1 - pexp(seuil, rate = lambda)
cat("Probabilité au-dessus du seuil:", probabilite_au_dessus_seuil, "\n")

# Créer un data frame pour les temps d'attente
temps_attente = seq(0, max(echantillon_exponentiel), length.out = 1000)
valeurs_densite = dexp(temps_attente, rate = lambda)
df = data.frame(temps_attente, valeurs_densite)
# Filtrer les données pour la région ombrée
df_ombre = df[df$temps_attente > seuil,]
# Tracer la PDF de la distribution exponentielle
ggplot(df, aes(x = temps_attente, y = valeurs_densite)) +
  geom_line() +
  geom_area(data = df_ombre, aes(x = temps_attente, y = valeurs_densite), fill = "orange", alpha = 0.5) +
  geom_vline(xintercept = seuil, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Distribution Exponentielle (λ = 0.01)", x = "Temps d'attente", y = "Densité de Probabilité")

####################################
#Génération d'échantillons aléatoires normalement distribués
set.seed(8) # Fixer la graine pour la reproductibilité
# Définir les paramètres de la distribution normale cible
mu = 5
sigma = 2
# Générer des variables aléatoires uniformes
n = 5
echantillon_uniforme = runif(n)
# Calculer les quantiles correspondants pour l'échantillon uniforme en utilisant la CDF inverse (fonction quantile) de la distribution normale
echantillon_normal = qnorm(echantillon_uniforme, mean = mu, sd = sigma)
cat("Échantillon normal:", echantillon_normal, "\n")

echantillon_normal2 = qnorm(echantillon_uniforme, mean = 0, sd = 1)
echantillon_normal2 * sigma + mu


####################################
####################################
####################################
#Comprendre les distributions d'échantillonnage communes
set.seed(8) # Fixer la graine pour la reproductibilité
# Définir les paramètres de la population
moyenne_population = 50
ecart_type_population = 10
taille_population = 100000
# Générer la population en utilisant une distribution normale
population <- rnorm(taille_population, mean = moyenne_population, sd = ecart_type_population)
summary(population)

# Définir la taille de l'échantillon pour chaque tour
taille_echantillon_par_tour = 50
# Fonction pour tirer un échantillon et calculer sa moyenne
obtenir_moyenne_echantillon <- function(population, taille_echantillon_par_tour) {
  echantillon <- sample(population, size = taille_echantillon_par_tour, replace = FALSE)
  return(mean(echantillon))
}

obtenir_moyenne_echantillon(population, taille_echantillon_par_tour)
obtenir_moyenne_echantillon(population, taille_echantillon_par_tour)

# Générer plusieurs tours de moyennes d'échantillon
nombre_tours = 1000 # nombre de tours pour échantillonner
moyennes_echantillon = replicate(nombre_tours, obtenir_moyenne_echantillon(population, taille_echantillon))
summary(moyennes_echantillon)

library(ggplot2)
df_distribution_echantillonnage = data.frame(moyennes_echantillon)
ggplot(df_distribution_echantillonnage, aes(x = moyennes_echantillon)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  geom_density(color = "red", lwd = 1.2) +
  theme_minimal() +
  labs(title = "Distribution d'échantillonnage de la moyenne de l'échantillon",
       x = "Moyenne de l'échantillon",
       y = "Densité")

####################################
#Distributions d'échantillonnage communes
taille_echantillon = 10
mu = 50
sigma = 10
echantillons = rnorm(taille_echantillon, mean = mu, sd = sigma)
echantillons

moyenne_echantillon = mean(echantillons)
ecart_type_echantillon = sd(echantillons)
cat("Moyenne de l'échantillon:", moyenne_echantillon, "\nÉcart-type de l'échantillon:", ecart_type_echantillon, "\n")

alpha = 0.05
t_critique = qt(1 - alpha/2, df = taille_echantillon - 1)  # valeur t pour un test bilatéral avec alpha = 0.05 et df = n - 1
marge_erreur_t = t_critique * (ecart_type_echantillon / sqrt(taille_echantillon))
ci_t = c(moyenne_echantillon - marge_erreur_t, moyenne_echantillon + marge_erreur_t)
ci_t


####################################
#Comprendre les statistiques d'ordre
set.seed(8)
echantillons = rnorm(10, mean = 50, sd = 10)
echantillons

echantillons_tries = sort(echantillons)
echantillons_tries

min_value = echantillons_tries[1]
cat("Valeur minimale:", min_value, "\n")

max_value = echantillons_tries[length(echantillons_tries)]
cat("Valeur maximale:", max_value, "\n")

k = 3
stat_ordre_k = echantillons_tries[k]
cat("Statistique d'ordre k (3ème):", stat_ordre_k, "\n")

valeur_mediane = median(echantillons)
cat("Valeur médiane:", valeur_mediane, "\n")

quartiles = quantile(echantillons, probs = c(0.25, 0.75))
quartiles


####################################
#Calcul de la valeur à risque (VaR)
set.seed(8) # Pour la reproductibilité
# Générer un échantillon aléatoire de rendements quotidiens à partir d'une distribution normale
taille_echantillon = 252  # Nombre de jours de trading dans une année
mu = 0.08                  # Rendement quotidien moyen
sigma = 0.05               # Écart-type des rendements quotidiens
rendements_quotidiens = rnorm(taille_echantillon, mean = mu, sd = sigma)

confidence_level = 0.95
valeur_portefeuille = 1000000  # Valeur du portefeuille en USD
rendements_tries = sort(rendements_quotidiens)
indice_VaR = ceiling(taille_echantillon * (1 - confidence_level))
VaR = rendements_tries[indice_VaR]
montant_VaR = valeur_portefeuille * (1 - (1 + VaR))
cat("VaR:", VaR, "\nMontant VaR:", montant_VaR, "\n")

library(ggplot2)
df_rendements_quotidiens <- data.frame(RendementsQuotidiens = rendements_quotidiens)
# Créer le graphique de densité
graphique_densite <- ggplot(df_rendements_quotidiens, aes(x = RendementsQuotidiens)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = VaR), linetype = "dashed", color = "red") +
  labs(x = "Rendements Quotidiens", y = "Densité", title = "Graphique de densité des rendements quotidiens avec VaR") +
  theme_minimal()
# Ajouter la zone ombrée sous la VaR au graphique de densité
donnees_densite <- ggplot_build(graphique_densite)$data[[1]] %>%
  as.data.frame() %>%
  filter(x < VaR)
graphique_densite +
  geom_ribbon(data = donnees_densite, aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.5)
