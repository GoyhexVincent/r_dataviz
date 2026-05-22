rm(list=ls())
library(yfhist)
library(corrplot)
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Définition des 11 secteurs + Monde
sectors <- c(
  "XLK",  # Technology
  "XLV",  # Health Care
  "XLF",  # Financials
  "XLY",  # Consumer Discretionary
  "XLC",  # Communication Services
  "XLI",  # Industrials
  "XLP",  # Consumer Staples
  "XLE",  # Energy
  "XLU",  # Utilities
  "XLRE", # Real Estate
  "XLB",  # Materials
  "URTH",  # MSCI World (marché large)
  "BTC", #Bitcoin
  "GLD", #Gold
  "TLT" #Bonds 20y
)

sector_names <- c(
  "Technology", "Health Care", "Financials", "Consumer Discretionary",
  "Communication", "Industrials", "Consumer Staples", "Energy",
  "Utilities", "Real Estate", "Materials", "World", "Bitcoin","Gold", "Bonds 20y"
)

# 2. Téléchargement
cat("Téléchargement des", length(sectors), "ETF sectoriels...\n")
data_all <- get_data(symbols = sectors, from_date = "2020-01-01")

# 3. Construction de la matrice des rendements
returns_matrix <- NULL
returns_dates <- NULL

for(ticker in names(data_all)) {
  prices <- data_all[[ticker]]$adjclose
  returns <- diff(log(prices))
  
  if(is.null(returns_matrix)) {
    returns_matrix <- returns
    returns_dates <- data_all[[ticker]]$index[-1]
  } else {
    returns_matrix <- cbind(returns_matrix, returns)
  }
}

colnames(returns_matrix) <- sector_names

# 4. Matrice de corrélation (période complète)
cor_matrix <- cor(returns_matrix)
cat("\n=== MATRICE DE CORRÉLATION (2020-2026) ===\n")
print(round(cor_matrix, 3))

# 5. Visualisation période complète
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7,
         title = "Matrice des corrélations sectorielles (2020-2026)")

# ============================================================
# ANALYSE COMPLÉMENTAIRE : CORRÉLATIONS ANNUELLES ET GLISSANTES
# ============================================================

# Créer un data.frame avec les dates et les rendements
df_returns <- as.data.frame(returns_matrix)
df_returns$date <- returns_dates
df_returns$year <- format(returns_dates, "%Y")

# ---------- 1. CORRÉLATIONS ANNUELLES AVEC LE MONDE ----------
cat("\n\n========================================\n")
cat("CORRÉLATIONS ANNUELLES AVEC LE MARCHÉ (World)\n")
cat("========================================\n")

annual_cors <- df_returns %>%
  group_by(year) %>%
  summarise(across(-date, ~ cor(., World, use = "complete.obs"))) %>%
  select(-World)

print(annual_cors)

# Visualisation des corrélations annuelles
annual_long <- annual_cors %>%
  pivot_longer(cols = -year, names_to = "Asset", values_to = "Correlation")

ggplot(annual_long, aes(x = year, y = Correlation, fill = Asset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Corrélations annuelles avec le MSCI World (URTH)",
       subtitle = "Par année civile",
       x = "Année", y = "Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_viridis_d()

# ---------- 2. CORRÉLATIONS GLISSANTES (252 jours) ----------
cat("\n\n========================================\n")
cat("CORRÉLATIONS GLISSANTES - Fenêtre de 252 jours\n")
cat("========================================\n")

window <- 252

if(nrow(df_returns) > window) {
  
  rolling_cor <- data.frame(date = df_returns$date[window:nrow(df_returns)])
  actifs <- sector_names[sector_names != "World"]
  
  for(asset in actifs) {
    rolling_cor[[asset]] <- sapply(window:nrow(df_returns), function(i) {
      window_data <- df_returns[(i-window+1):i, ]
      cor(window_data[[asset]], window_data$World, use = "complete.obs")
    })
  }
  
  rolling_long <- rolling_cor %>%
    pivot_longer(cols = -date, names_to = "Asset", values_to = "Correlation")
  
  p1 <- ggplot(rolling_long, aes(x = date, y = Correlation, color = Asset)) +
    geom_line(size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(title = "Évolution des corrélations avec le MSCI World",
         subtitle = "Fenêtre glissante de 252 jours (1 an)",
         y = "Corrélation", x = "Date") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 6)) +
    guides(color = guide_legend(ncol = 4))
  
  print(p1)
  
  safe_haven <- rolling_long %>%
    filter(Asset %in% c("Bitcoin", "Gold", "Bonds 20y"))
  
  p2 <- ggplot(safe_haven, aes(x = date, y = Correlation, color = Asset)) +
    geom_line(size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Corrélations des actifs 'refuges' avec le marché",
         subtitle = "Bitcoin, Or et Obligations 20 ans - Fenêtre glissante de 252 jours",
         y = "Corrélation avec URTH", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("Bitcoin" = "#F7931A", "Gold" = "#FFD700", "Bonds 20y" = "#1E88E5"))
  
  print(p2)
  
} else {
  cat("Pas assez de données pour la corrélation glissante\n")
}

# ---------- 3. STATISTIQUES RÉSUMÉES ----------
cat("\n\n========================================\n")
cat("STATISTIQUES RÉSUMÉES\n")
cat("========================================\n")

world_cors <- cor_matrix["World", ]
world_cors <- sort(world_cors[names(world_cors) != "World"], decreasing = TRUE)

cat("\nCorrélations avec le MSCI World (période complète) :\n")
print(round(world_cors, 3))

low_cor <- world_cors[abs(world_cors) < 0.3]
if(length(low_cor) > 0) {
  cat("\nActifs avec |corrélation| < 0.3 :\n")
  print(round(low_cor, 3))
} else {
  cat("\nAucun actif avec |corrélation| < 0.3\n")
}

# =============================================
# DÉTECTION DES RUPTURES DE RÉGIME (BREAKPOINTS)
# =============================================

# Installation du package si nécessaire
if (!require(strucchange)) {
  install.packages("strucchange")
  library(strucchange)
}

# -----------------------------------------------------------------
# 1. Fonction pour détecter les ruptures - PARAMÈTRES STRICTS
# -----------------------------------------------------------------
detect_breaks <- function(series, dates, min_segment = 189) {
  # min_segment = 252 jours = 1 an minimum par régime
  
  n <- length(series)
  
  # Il faut au moins 2 segments complets (donc 2 * min_segment)
  if (n < min_segment * 2) {
    return(list(break_dates = NA, nbreaks = 0, model = NULL))
  }
  
  # Calculer la proportion h = min_segment / n
  h <- min_segment / n
  
  # Détection des ruptures (changement de moyenne)
  bp <- tryCatch(
    breakpoints(series ~ 1, data = data.frame(series), 
                h = h, breaks = 3),  # Maximum 3 ruptures seulement
    error = function(e) NULL
  )
  
  if (is.null(bp) || length(bp$breakpoints) == 0) {
    return(list(break_dates = NA, nbreaks = 0, model = NULL))
  }
  
  # Ne garder que les ruptures significatives (espacement minimum)
  break_indices <- bp$breakpoints
  
  # Filtrer : espacer les ruptures d'au moins min_segment jours
  if (length(break_indices) > 1) {
    filtered_indices <- break_indices[1]
    for (idx in break_indices[-1]) {
      if (idx - tail(filtered_indices, 1) >= min_segment) {
        filtered_indices <- c(filtered_indices, idx)
      }
    }
    break_indices <- filtered_indices
  }
  
  break_dates <- dates[break_indices]
  nbreaks <- length(break_indices)
  
  # Limiter à 3 ruptures maximum pour une interprétation claire
  if (nbreaks > 3) {
    break_indices <- break_indices[1:3]
    break_dates <- break_dates[1:3]
    nbreaks <- 3
  }
  
  return(list(break_dates = break_dates, nbreaks = nbreaks, 
              model = bp, indices = break_indices))
}

# -----------------------------------------------------------------
# 2. Appliquer la détection avec PARAMÈTRES STRICTS
# -----------------------------------------------------------------
if (exists("rolling_cor") && nrow(rolling_cor) > 0) {
  
  actifs <- names(rolling_cor)[names(rolling_cor) != "date"]
  break_results <- list()
  
  cat("\n\n========================================\n")
  cat("DÉTECTION DES RUPTURES DE RÉGIME\n")
  cat("========================================\n")
  cat("Paramètres :\n")
  cat("  - Taille minimale d'un régime : 252 jours (1 an)\n")
  cat("  - Nombre maximum de ruptures : 3\n")
  cat("  - Espacement minimum entre ruptures : 252 jours\n\n")
  cat("Analyse des changements structurels dans les corrélations glissantes\n\n")
  
  for (actif in actifs) {
    serie <- rolling_cor[[actif]]
    idx_complet <- which(!is.na(serie))
    
    if (length(idx_complet) < 300) {  # Besoin d'au moins 300 jours (≈1.2 an)
      cat(actif, ": pas assez de données valides\n")
      break_results[[actif]] <- list(break_dates = NA, nbreaks = 0)
      next
    }
    
    serie_clean <- serie[idx_complet]
    dates_clean <- rolling_cor$date[idx_complet]
    
    # Appel CORRECT avec min_segment = 252
    res <- detect_breaks(serie_clean, dates_clean, min_segment = 252)
    break_results[[actif]] <- res
    
    if (res$nbreaks > 0) {
      cat(actif, ":", res$nbreaks, "rupture(s) :", 
          paste(format(res$break_dates, "%Y-%m"), collapse = ", "), "\n")
    } else {
      cat(actif, ": aucune rupture significative\n")
    }
  }
  
  # -----------------------------------------------------------------
  # 3. Visualisation des ruptures
  # -----------------------------------------------------------------
  actifs_interet <- c("Technology", "Energy", "Bitcoin", "Gold", "Bonds 20y")
  actifs_presents <- intersect(actifs_interet, actifs)
  
  if (length(actifs_presents) > 0) {
    
    rolling_subset <- rolling_cor[, c("date", actifs_presents)]
    rolling_long_subset <- rolling_subset %>%
      pivot_longer(cols = -date, names_to = "Asset", values_to = "Correlation")
    
    breaks_df <- data.frame()
    for (a in actifs_presents) {
      if (break_results[[a]]$nbreaks > 0) {
        tmp <- data.frame(Asset = a, 
                          break_date = break_results[[a]]$break_dates,
                          stringsAsFactors = FALSE)
        breaks_df <- rbind(breaks_df, tmp)
      }
    }
    
    if (nrow(breaks_df) > 0) {
      p3 <- ggplot(rolling_long_subset, aes(x = date, y = Correlation, color = Asset)) +
        geom_line(size = 0.8) +
        geom_vline(data = breaks_df, aes(xintercept = break_date), 
                   linetype = "dashed", alpha = 0.5, color = "gray30") +
        geom_point(data = breaks_df, aes(x = break_date, y = 0), 
                   size = 2, color = "red", inherit.aes = FALSE) +
        labs(title = "Corrélations glissantes avec le marché - Ruptures de régime",
             subtitle = "Paramètres stricts : minimum 1 an par régime, maximum 3 ruptures",
             y = "Corrélation (Pearson)", x = "Date") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_color_manual(values = c("Technology" = "#1E88E5", 
                                      "Energy" = "#43A047",
                                      "Bitcoin" = "#F7931A", 
                                      "Gold" = "#FFD700", 
                                      "Bonds 20y" = "#8E24AA"))
      
      print(p3)
    } else {
      cat("\nAucune rupture significative détectée avec ce paramétrage strict.\n")
    }
  }
  
  # Affichage récapitulatif
  cat("\n========================================\n")
  cat("RÉCAPITULATIF DES RUPTURES (PARAMÈTRES STRICTS)\n")
  cat("========================================\n")
  for (a in names(break_results)) {
    nb <- break_results[[a]]$nbreaks
    if (nb > 0) {
      dates_str <- paste(format(break_results[[a]]$break_dates, "%Y-%m"), collapse = ", ")
      cat(sprintf("%-20s : %d rupture(s) : %s\n", a, nb, dates_str))
    } else {
      cat(sprintf("%-20s : aucun changement majeur\n", a))
    }
  }
  
  cat("\n💡 Interprétation :\n")
  cat("   - Seuls les changements DURABLES (>1 an) sont détectés\n")
  cat("   - Maximum 3 ruptures par actif pour une lecture claire\n")
  cat("   - Une rupture signifie un nouveau 'régime' de corrélation\n")
  
} else {
  cat("\nLes corrélations glissantes n'ont pas été calculées.\n")
}