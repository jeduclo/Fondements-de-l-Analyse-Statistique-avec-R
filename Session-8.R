install.packages("socviz")
library(socviz)
data(gss_lon)
dim(gss_lon)
glimpse(gss_lon)

####################################
#Introduction au jeu de données de l'Enquête Sociale Générale

#Calcul de la proportion d'échantillon
summary(gss_lon$siblings)

gss2016 = gss_lon %>% filter(year == 2016)
ggplot(gss2016, aes(x = siblings)) +
  geom_bar() +
  labs(title = "Comptage de fréquences des frères et sœurs", x = "Nombre de frères et sœurs", y = "Compte") +
  theme(text = element_text(size = 16))

p_hat = gss2016 %>%
  summarize(prop_2sib = mean(siblings=="2", na.rm=TRUE)) %>%
  pull()
p_hat

####################################
#Calcul de l'intervalle de confiance
install.packages("infer")
library(infer)
gss2016 = gss2016 %>%
  mutate(siblings_two_ind = if_else(siblings=="2","Y","N")) %>%
  filter(!is.na(siblings_two_ind))
bs = gss2016 %>%
  specify(response = siblings_two_ind, success = "Y") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")
bs
ggplot(bs, aes(x = stat)) +
  geom_density() +
  labs(title = "Graphique de densité des proportions d'échantillon", x = "Proportion d'échantillon", y = "Densité") +
  theme(text = element_text(size = 16))

SE = bs %>%
  summarise(sd(stat)) %>%
  pull()
SE

c(p_hat - 2*SE, p_hat + 2*SE)
SE2 = sqrt(p_hat*(1-p_hat)/nrow(gss2016))
c(p_hat - 2*SE2, p_hat + 2*SE2)

####################################
#Test d'hypothèse pour la proportion d'échantillon
gss2016 %>%
  ggplot(aes(x = siblings_two_ind)) +
  geom_bar() +
  labs(title = "Comptage de fréquences des familles avec deux frères et sœurs", x = "A deux frères et sœurs", y = "Compte") +
  theme(text = element_text(size = 16))

p_hat = gss2016 %>%
  summarize(mean(siblings_two_ind=="Y")) %>%
  pull()
p_hat

null = gss2016 %>%
  specify(response = siblings_two_ind, success = "Y") %>%
  hypothesise(null = "point", p = 0.19) %>%
  generate(reps = 500, type = "draw") %>%
  calculate(stat = "prop")
null

ggplot(null, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = p_hat, color = "red") +
  labs(title = "Graphique de densité utilisant le bootstrap", x = "Proportion d'échantillon", y = "Densité") +
  theme(text = element_text(size = 16))

null %>%
  summarise(mean(stat > p_hat)) %>%
  pull()* 2

####################################
#Inférence pour la différence dans les proportions d'échantillon
gss2016 = gss2016 %>%
  mutate(higher_degree = if_else(degree %in% c("Bachelor","Graduate"), "Y", "N"))

table(gss2016$higher_degree)
table(gss2016$sex)

ggplot(gss2016, aes(x = sex, fill=higher_degree)) +
  geom_bar() +
  labs(title = "Comptage de fréquences pour le genre et le diplôme", x = "Genre", y = "Compte") +
  theme(text = element_text(size = 16))

ggplot(gss2016, aes(x = sex, fill=higher_degree)) +
  geom_bar(position = "fill") +
  labs(title = "Proportions d'échantillon pour le genre et le diplôme", x = "Genre", y = "Ratio") +
  theme(text = element_text(size = 16))

p_hats = gss2016 %>%
  group_by(sex) %>%
  summarise(mean(higher_degree=="Y", na.rm=TRUE)) %>%
  pull()
d_hat = diff(p_hats)
d_hat

gss2016 %>%
  specify(response = higher_degree, explanatory = sex, success = "Y") %>%
  hypothesise(null = "independence") %>%
  generate(reps = 1, type = "permute")

null = gss2016 %>%
  specify(higher_degree ~ sex, success = "Y") %>%
  hypothesise(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "diff in props", order = c("Female", "Male"))
null

ggplot(null, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = d_hat, color = "red") +
  labs(x = "Différence dans la proportion d'échantillon (femme - homme)", y = "Compte") +
  theme(text = element_text(size = 16))

null %>%
  summarize(pval = 2 * mean(stat > d_hat)) %>%
  pull()

####################################
#Introduction au tableau de contingence
table(gss2016$degree)

ggplot(gss2016, aes(x = sex, fill=degree)) +
  geom_bar() +
  labs(title = "Comptage de fréquences pour le genre et le diplôme", x = "Genre", y = "Compte") +
  theme(text = element_text(size = 16))

tab = gss2016 %>%
  select(sex, degree) %>%
  table()
tab

####################################
#Application du test du chi-carré pour 
#l'indépendance entre deux variables catégoriques
perm_1 = gss2016 %>%
  specify(degree ~ sex) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1, type = "permute")
perm_1

null_spac = gss2016 %>%
  specify(degree ~ sex) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "Chisq")
null_spac

# Calculer le tableau de fréquence attendu
totaux_lignes = rowSums(tab)
totaux_colonnes = colSums(tab)
total_global = sum(tab)
attendu = outer(totaux_lignes, totaux_colonnes) / total_global
attendu

# Calculer la statistique chi-carré observée
chi_carre_observe = sum((tab - attendu)^2 / attendu)
chi_carre_observe

ggplot(null_spac, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = chi_carre_observe, color = "red") +
  labs(title = "Courbe de densité de la statistique chi-carré bootstrapée", x = "Statistique chi-carré", y = "Densité") +
  theme(text = element_text(size = 16))

null_spac %>%
  summarize(pval = 2 * mean(stat < chi_carre_observe)) %>%
  pull()

####################################
#Construction de l'intervalle de confiance bootstrap
# Calculer l'intervalle de confiance bootstrap pour la médiane
bs %>%
  summarize(
    l = quantile(stat, 0.025),  # Limite inférieure
    u = quantile(stat, 0.975)   # Limite supérieure
  )

# Calculer l'écart-type des statistiques bootstrap
SE = bs %>%
  summarise(sd(stat)) %>%
  pull()
# Médiane observée des miles par gallon dans mtcars
observed_median = median(mtcars$mpg)
# Calculer l'intervalle de confiance à partir de l'écart-type bootstrap
c(observed_median - 2*SE, observed_median + 2*SE)

####################################
#Recentrage d'une distribution bootstrap
# Générer une distribution bootstrap centrée sur une médiane hypothétique
bs = mtcars %>%
  specify(response = mpg) %>%
  hypothesize(null = "point", med = 16) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "median")

# Tracer la densité de la distribution bootstrap avec la médiane observée
ggplot(bs, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = median(mtcars$mpg), color = "red") +
  labs(title = "Courbe de densité de la médiane bootstrapée", x = "Médiane d'échantillon", y = "Densité") +
  theme(text = element_text(size = 16))

####################################
#Introduction au théorème central limite utilisé dans la distribution t
# Calcul de probabilités à l'aide de la fonction de distribution t
x = pt(3, df = 10)
x

y = 1 - x
y

z = 1 - pt(3, df = 100)
z

# Trouver les quantiles pour différents niveaux de confiance
d = qt(0.95, df = 10)
d

e = qt(0.975, df = 10)
e

f = qt(0.975, df = 100)
f

####################################
#Construction de l'intervalle de confiance pour la moyenne 
#de la population utilisant la distribution t
# Calculer la moyenne des miles par gallon dans mtcars
mean(mtcars$mpg)

# Construire un intervalle de confiance à 95% pour la moyenne des miles par gallon
t.test(mtcars$mpg)

####################################
#Réalisation de tests d'hypothèse pour deux moyennes
# Définir deux échantillons
sample1 = c(10, 12, 14, 16, 18)
sample2 = c(15, 17, 19, 21, 23)
# Combinaison des échantillons dans un data frame
data = tibble(
  value = c(sample1, sample2),
  group = factor(rep(c("Groupe 1", "Groupe 2"), each = length(sample1)))
)

# Résultats du bootstrap pour comparer deux moyennes
bootstrap_results = data %>%
  specify(response = value, explanatory = group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("Groupe 1", "Groupe 2"))

# Calculer l'intervalle de confiance pour la différence des moyennes
ci = bootstrap_results %>%
  filter(!is.na(stat)) %>%
  get_confidence_interval(level = 0.95, type = "percentile")

# Résultats du test t pour comparer deux moyennes
t_test_result = t.test(sample1, sample2)
t_test_result

# Test t appliqué au data frame
t_test_result2 = t.test(value ~ group, data = data)
t_test_result2

####################################
#Introduction à l'ANOVA
# Charger le jeu de données de croissance des plantes
data(PlantGrowth)

# Résultats de l'ANOVA pour comparer les moyennes de groupes
anova_results = PlantGrowth %>%
  specify(response = weight, explanatory = group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

# Calculer la valeur p pour l'ANOVA
p_value = anova_results %>%
  get_p_value(obs_stat = anova_results, direction = "right") %>%
  pull()
p_value

