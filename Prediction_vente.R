df <- read.csv("C:/Users/HP/Documents/Portfolio R/Bakery.csv", sep = ";")
View(df)

library(dplyr)
library(prophet)

#df$ds = as.POSIXct(df$ds)

df <- df %>%
  group_by(TransactionNo) %>%
  mutate(quantite_vendue = n()) %>%
  ungroup()
 
View(df)

str(df)
df <- df %>%
  rename(ds = DateTime, y = quantite_vendue)
# format date
# Si ta colonne est du texte au format jj/mm/aaaa
df$ds <- as.Date(df$ds, format = "%d/%m/%Y")


#  les convertir en factor 
df$Daypart <- as.numeric(as.factor(df$Daypart))
df$DayType <- as.numeric(as.factor(df$DayType))

# prophet
# Créer le modèle en activant les saisons

# 1. Créer le modèle Prophet avec saisonnalités
m <- prophet(
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = TRUE
)

# 2. Ajouter les régresseurs externes
m <- add_regressor(m, 'Daypart')
m <- add_regressor(m, 'DayType')


# 4. Ajuster (fit) le modèle sur tes données
m <- fit.prophet(m, df)

future <- make_future_dataframe(m, periods=100, freq = "day")
# DayType : répéter le pattern existant dans df
future$DayType <- rep(df$DayType, length.out = nrow(future))

# Daypart : répéter le pattern existant dans df
future$Daypart <- rep(df$Daypart, length.out = nrow(future))

forecast <- predict(m, future)
View(forecast)
plot(m, forecast)

prophet_plot_components(m, forecast)

# Supposons que votre dataframe d'entraînement s'appelle 'df'
# et votre prévision s'appelle 'forecast'

##### Evaluer le modele
# Filtrer le dataframe de prévision pour les dates d'historique
df_prevision_historique <- forecast[1:nrow(df), ]

# Extraire les valeurs réelles et prédites
y_vrais <- df$y
yhat_prevu <- df_prevision_historique$yhat

# Assurez-vous que les deux vecteurs ont la même longueur
length(y_vrais) == length(yhat_prevu)

mae <- mean(abs(y_vrais - yhat_prevu))
print(paste("MAE :", mae))

rmse <- sqrt(mean((y_vrais - yhat_prevu)^2))
print(paste("RMSE :", rmse))
  