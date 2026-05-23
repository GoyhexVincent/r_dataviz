# Script 2: Analyse du Secteur Hardware et Amont Technologique
# Basé sur le "script tendances globales du marché"

rm(list=ls())
library(yfhist)
library(corrplot)
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Définition des tickers par catégorie
tickers <- c(
  # Benchmark Marché
  "URTH",  # MSCI World
  
  # ETF Sectoriels pour la Tech
  "VGT",   # Vanguard Information Tech (large spectre)
  "SMH",   # VanEck Semiconductor (concentré global)
  "SOXX",  # iShares Semiconductor (diversifié US)
  
  # Grandes Entreprises Établies
  "ASML",  # Lithographie (Pays-Bas)
  "LRCX",  # Gravure (Lam Research)
  "AMAT",  # Équipement (Applied Materials)
  "KLAC",  # Contrôle (KLA Corporation)
  "WDC",   # HDD (Western Digital)
  "SNDK"   # Flash NAND (SanDisk)
)

# Noms pour l'affichage
ticker_names <- c(
  "World", "Vanguard_Tech", "VanEck_Semi", "iShares_Semi",
  "ASML", "Lam_Research", "Applied_Materials", "KLA", "Western_Digital", "SanDisk"
)

# 2. Téléchargement des données
cat("Téléchargement des données pour l'analyse hardware tech...\n")
data_all <- get_data(symbols = tickers, from_date = "2020-01-01")

# 3. Vérification des données téléchargées
cat("\nVérification des données téléchargées :\n")
valid_tickers <- c()
for(ticker in names(data_all)) {
  if(!is.null(data_all[[ticker]]) && "adjclose" %in% colnames(data_all[[ticker]])) {
    n_rows <- nrow(data_all[[ticker]])
    cat("  ✓", ticker, ":", n_rows, "lignes\n")
    valid_tickers <- c(valid_tickers, ticker)
  } else {
    cat("  ✗", ticker, ": données non disponibles\n")
  }
}

# 4. Construction de la matrice des rendements (version robuste)
cat("\nConstruction de la matrice des rendements...\n")

returns_list <- list()
common_dates <- NULL

for(ticker in valid_tickers) {
  df <- data_all[[ticker]]
  prices <- df$adjclose
  returns <- diff(log(prices))
  dates <- df$index[-1]
  
  # Créer un data.frame pour ce ticker
  temp_df <- data.frame(date = dates, returns = returns)
  colnames(temp_df)[2] <- ticker
  
  returns_list[[ticker]] <- temp_df
}

# Fusionner progressivement par date
if(length(returns_list) > 0) {
  merged_returns <- returns_list[[1]]
  
  for(i in 2:length(returns_list)) {
    merged_returns <- merge(merged_returns, returns_list[[i]], by = "date", all = FALSE)
  }
  
  # Extraire la matrice des rendements
  returns_matrix <- as.matrix(merged_returns[, -1])
  returns_dates <- merged_returns$date
  
  # Renommer les colonnes
  colnames(returns_matrix) <- ticker_names[match(colnames(returns_matrix), valid_tickers)]
  
  cat("Matrice finale :", nrow(returns_matrix), "jours x", ncol(returns_matrix), "actifs\n")
  
} else {
  stop("Aucune donnée valide téléchargée")
}

# 5. Création du data.frame pour les analyses
df_returns <- as.data.frame(returns_matrix)
df_returns$date <- returns_dates
df_returns$year <- format(returns_dates, "%Y")

# 6. Matrice de corrélation (période complète)
cor_matrix <- cor(df_returns[, 1:(ncol(df_returns)-2)], use = "pairwise.complete.obs")
cat("\n=== MATRICE DE CORRÉLATION - Hardware Tech (2020-2026) ===\n")
print(round(cor_matrix, 3))

# 7. Visualisation
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7,
         title = "Corrélations : Marché, ETF Tech et Hardware Amont")

# 8. Analyse des corrélations avec le MSCI World (si présent)
world_col <- grep("World", colnames(cor_matrix))
if(length(world_col) > 0) {
  world_cors <- cor_matrix[world_col, ]
  world_cors <- sort(world_cors[names(world_cors) != "World"], decreasing = TRUE)
  
  cat("\n\n=== Corrélations avec le MSCI World (période complète) ===\n")
  print(round(world_cors, 3))
}

# 9. Analyse des performances individuelles (Cumulative Returns)
# Calcul des rendements cumulés (investissement initial de 1)
cumulative_returns <- apply(df_returns[, 1:(ncol(df_returns)-2)], 2, function(x) cumprod(1 + x))

# Conversion en data.frame pour ggplot
df_cumulative <- as.data.frame(cumulative_returns)
df_cumulative$date <- df_returns$date

# Mise au format long pour le graphique
df_cumulative_long <- df_cumulative %>%
  pivot_longer(cols = -date, names_to = "Asset", values_to = "Cumulative_Return")

# Graphique des performances cumulées
p_perf <- ggplot(df_cumulative_long, aes(x = date, y = Cumulative_Return, color = Asset)) +
  geom_line(size = 0.8) +
  labs(title = "Performance Cumulée : Marché, ETF Tech et Acteurs Hardware",
       subtitle = "Hypothèse de réinvestissement (Base 100 à t0)",
       y = "Valeur de l'investissement (Base 1)", x = "Date") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_log10()  # Échelle log pour mieux visualiser les écarts

print(p_perf)

# 10. Résumé des performances
cat("\n\n=== PERFORMANCES SUR LA PÉRIODE ===\n")
final_returns <- tail(df_cumulative, 1)[, 1:(ncol(df_cumulative)-1)]
final_returns <- sort(as.numeric(final_returns), decreasing = TRUE)
names(final_returns) <- colnames(df_cumulative)[1:(ncol(df_cumulative)-1)]

cat("Valeur finale d'un investissement de 1 :\n")
print(round(final_returns, 2))

# 11. Alerte sur les tickers manquants
cat("\n💡 Note importante :\n")
if("SanDisk" %in% colnames(cor_matrix)) {
  cat("  ✓ SanDisk (SNDK) est inclus\n")
} else {
  cat("  ✗ SanDisk (SNDK) n'a pas pu être téléchargé.\n")
  cat("    Raison probable : l'entreprise est récente (spin-off) et n'a pas\n")
  cat("    d'historique complet depuis 2020 sur Yahoo Finance.\n")
  cat("    Solution : réduire la date de début à '2024-01-01' ou la retirer de l'analyse.\n")
}