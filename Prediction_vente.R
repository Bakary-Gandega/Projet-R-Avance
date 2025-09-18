df <- read.csv("C:/Users/HP/Documents/Portfolio R/Bakery.csv", sep = ";")
View(df)

library(dplyr)
library(prophet)
library(ggplot2)

#________________________________________________________

df <- df %>%
  group_by(TransactionNo) %>%
  mutate(quantite_vendue = n()) %>%
  ungroup()

str(df)

df$Items <- as.factor(df$Items)
df$Daypart <- as.factor(df$Daypart) 
df$DayType <- as.factor(df$DayType) 
df$DateTime <- as.factor(ventes$DateTime) 
df$Dquantite_vendue <- as.factor(df$quantite_vendue) 

summary(df)

# écart-type, variance et écart-interquartile
sd(df$quantite_vendue)
var(df$quantite_vendue)
IQR(df$quantite_vendue)

# Disribution des quantité de vente (Histogramme)
ggplot(data = df, aes(x=quantite_vendue)) +
  geom_histogram(binwidth = 3, fill= "skyblue", color= "black")

#Disribution des quantité de vente (boite à moustaches)
ggplot(data= df, aes(x= quantite_vendue)) +
  geom_boxplot(fill= "blue") +
  labs(title = "Quantité de vente")


# Distribution des ventes selon le moment de journée
ggplot(data= df, aes(x= Daypart, fill = Daypart)) +
  geom_bar() +
  labs(title = "Distribution des ventes selon le moment de la journée ")+
  theme(legend.position = "y")

#ANALYSE BI-VARIEE


# type de produits par transaction 
ggplot(data= df, aes(x= quantite_vendue, y= DayType)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE)

#Quantité de vente selon le type de jour
ggplot(data= df, aes(x= quantite_vendue, y= DayType)) +
  geom_boxplot() +
  labs(title = "Quantité de ventes par type de jour",
       x= "Quantité de ventes", y= "Type de jour")
#_________________________________________________________

# Modèlisation avec facebook prophet

df <- df %>%
  group_by(TransactionNo) %>%
  mutate(quantite_vendue = n()) %>%
  ungroup()

View(df)

str(df)

df <- df %>%
  rename(ds = DateTime, y = quantite_vendue)

# Convertir la date du format jj/mm/aaaa au format dd/mm/yyyy
df$ds <- as.Date(df$ds, format = "%d/%m/%Y")


#  Convertir les variables categorielles en factor 
df$Daypart <- as.numeric(as.factor(df$Daypart))
df$DayType <- as.numeric(as.factor(df$DayType))


# Créer le modèle Prophet avec saisonnalités (regresseurs internes)
m <- prophet(
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = TRUE
)

# Ajouter les régresseurs externes
m <- add_regressor(m, 'Daypart')
m <- add_regressor(m, 'DayType')


# Ajuster (fit) le modèle sur les données
m <- fit.prophet(m, df)

# Créer la variable future pour y stocker les valeurs predites
future <- make_future_dataframe(m, periods=100, freq = "day")

# DayType : répéter le pattern existant dans df
future$DayType <- rep(df$DayType, length.out = nrow(future))

# Daypart : répéter le pattern existant dans df
future$Daypart <- rep(df$Daypart, length.out = nrow(future))

# Prediction 
forecast <- predict(m, future)
View(forecast)

# Visualisation
plot(m, forecast)
prophet_plot_components(m, forecast)

##### Evaluer le modele

# Filtrer le dataframe de prévision pour les dates d'historique
df_prevision_historique <- forecast[1:nrow(df), ]

# Extraire les valeurs réelles et prédites
y_vrais <- df$y
yhat_prevu <- df_prevision_historique$yhat

# Assurez-vous que les deux vecteurs ont la même longueur
length(y_vrais) == length(yhat_prevu)

mae <- mean(abs(y_vrais - yhat_prevu, na.rm = TRUE))
print(paste("MAE :", mae))

rmse <- sqrt(mean((y_vrais - yhat_prevu)^2, na.rm = TRUE))
print(paste("RMSE :", rmse))
