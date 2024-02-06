ggplot(data_f, aes(x =`Quantity(bucket)` , y = bestzone)) +
  geom_boxplot()

library(plotly)
plot_ly(
  data = data_f,
  y = ~`Quantity(bucket)`,
  x = ~Season,
  type = "box"
)

ggplot(data_f, aes(x = `Quantity(bucket)`, fill = bestzone)) +
  geom_bar(position = position_dodge()) +
  theme_classic()

df <- data.frame(locality = c(1, 2, 3, 1, 2, 3))

# Correspondance entre les codes et les libellés
correspondance <- c("Limbe", "Yaoundé", "Douala")

# Création d'une nouvelle colonne avec les libellés
df$locality_label <- correspondance[df$locality]

# Affichage du dataframe résultant
print(df)


