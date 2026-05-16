rm(list=ls())
library(yfhist)
library(corrplot)

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
  "URTH"  # MSCI World (marché large)
)

sector_names <- c(
  "Technology", "Health Care", "Financials", "Consumer Discretionary",
  "Communication", "Industrials", "Consumer Staples", "Energy",
  "Utilities", "Real Estate", "Materials", "World"
)

# 2. Téléchargement
cat("Téléchargement des", length(sectors), "ETF sectoriels...\n")
data_all <- get_data(symbols = sectors, from_date = "2020-01-01")

# 3. Construction de la matrice des rendements
returns_matrix <- NULL

for(ticker in names(data_all)) {
  prices <- data_all[[ticker]]$adjclose
  returns <- diff(log(prices))
  
  if(is.null(returns_matrix)) {
    returns_matrix <- returns
  } else {
    returns_matrix <- cbind(returns_matrix, returns)
  }
}

colnames(returns_matrix) <- sector_names

# 4. Matrice de corrélation
cor_matrix <- cor(returns_matrix)
cat("\n=== MATRICE DE CORRÉLATION ===\n")
print(round(cor_matrix, 3))

# 5. Visualisation
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",  # Regroupe les secteurs similaires automatiquement
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7,
         title = "Matrice des corrélations sectorielles (2020-2026)")