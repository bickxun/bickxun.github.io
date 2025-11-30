library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# 1. Importer les données
data <- read_csv("C:/Users/Victo/OneDrive/Bureau/Etude/ULaval/Session Automne/Outils numériques pour sc.so/Projet/deputes-historique.csv")

# 2. Conversion de la date (adapter si format JJ/MM/AAAA)
data <- data %>%
  mutate(dateNais = ymd(naissance))

# 3. Calculer l'âge actuel
data <- data %>%
  mutate(age = as.numeric(difftime(Sys.Date(), naissance, units = "days")) / 365.25)

# Supprimer les NA
data <- data %>% filter(!is.na(age))

# 4. Écart-type par législature
age_stats <- data %>%
  group_by(legislatureLast) %>%
  summarise(ecart_type = sd(age, na.rm = TRUE), .groups = "drop")

# 5. Regrouper par décennie
data <- data %>%
  mutate(decennie = paste0(floor(age / 10) * 10, "-", floor(age / 10) * 10 + 9))

decennie_stats <- data %>%
  group_by(legislatureLast, decennie) %>%
  summarise(count = n(), .groups = "drop")

# 6. Graphique : écart-type par législature
ggplot(age_stats, aes(x = legislatureLast, y = ecart_type)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "darkred") +
  labs(title = "Écart-type des âges par législature",
       x = "Législature", y = "Écart-type") +
  theme_minimal()

# 7. Graphique : répartition par décennie
ggplot(decennie_stats, aes(x = factor(legislatureLast), y = count, fill = decennie)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Répartition des âges par décennie et législature",
       x = "Législature", y = "Nombre de députés", fill = "Décennie") +
  theme_minimal()