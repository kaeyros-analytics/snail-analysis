library(readxl)
library(dplyr)


data <- read_xlsx("C:/Users/LENOVO/Downloads/snails_FINAL 2023.xlsx")

str(data)
#edulevel 4 mod au lieu de 3, snailsize, pestiqty, mainactivity

var <- c("locality","Gender", "Age","Marstat", "Edulevel", "Mainact", "Region", "Collectzone",
            "Season", "Month", "Time", "bestzone", "PestiQty", "SnailSize", "snailbreeder")

#Changement de type de colonnes
data_f <- data %>% mutate(across(all_of(var), as.factor))

#suppression de la variable No
data_f <- data_f[,-2]

#Statistiques générales
summary(data_f)

#Données manquantes
data_f <- data_f %>% na.omit()


# for (i in names(data_f)) {
#   if (is.numeric(data_f[[i]])) {
#     hist(data_f[[i]], main = paste("Histogramme de", i), xlab = i)
#
#   }
# }


