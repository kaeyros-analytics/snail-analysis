library(readxl)
library(dplyr)
library(plotly)
library(forcats)
library(Hmisc)
library(shinythemes)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(corrplot)
library(RColorBrewer)
library(magrittr)

root <- getwd()
path_data <- paste(root, "/", "Data", sep="")
file.data <- paste(path_data, "/snails_FINAL 2023.xlsx", sep="")
data <- as.data.frame(readxl::read_excel(file.data))

#str(data)
data[data$PestiQty == 3, "PestiQty"] <- 2
data[data$SnailSize == 3, "SnailSize"] <- 2
data$Mainact[data$Mainact == 6] <- 5
data$Edulevel[data$Edulevel ==4] <-3
data$Month[data$Month =="September"] <-"september"
data$Month[data$Month =="June"] <-"june"
#edulevel 4 mod au lieu de 3, snailsize, pestiqty, mainactivity

var <- c("locality","Gender", "Age","Marstat", "Edulevel", "Mainact", "Region", "Collectzone",
         "Season", "Month", "Time", "bestzone", "SnailSize", "snailbreeder", "PestiQty")

#Changement de type de colonnes
data_f <- data %>% mutate(across(all_of(var), as.factor))
data_f <- data_f[,-2]
data_f$`Quantity(bucket)`[is.na(data_f$`Quantity(bucket)`)] <- round(mean(data_f$`Quantity(bucket)`, na.rm = TRUE))
#data_f <- data_f %>% na.omit()

cor_gender <- c("Male", "Female")
data_f$gender_label <- cor_gender[data_f$Gender]

cor_marstat <- c("Single", "Married", "Separated", "Divorced", "Widow", "Others")
data_f$marstat_label <- cor_marstat[data_f$Marstat]

cor_edulevel <- c("1" = "Primary", "2"="Secondary", "3"="Tertiary")
data_f$edulevel_label <- cor_edulevel[data_f$Edulevel]

cor_mainact <- c("1" = "Farming", "4"="Artisan",
                 "2" = "Trading", "3" = "Civil servant", "5"="Others")
data_f$mainact_label <- cor_mainact[data_f$Mainact]

cor_colzone <- c("1" = "Within the village",
                 "2" = "Out of the village")
data_f$colzone_label <- cor_colzone[data_f$Collectzone]

cor_time <- c("1" = "Morning",
              "2" = "Afternoon", "3" = "Nigth")
data_f$time_label <- cor_time[data_f$Time]

cor_bestzone <- c("1" = "Around houses", "4"="Around farms", "5"="In the forests",
                  "2" = "Around garbage", "3" = "Around toilet", "6"="Others")
data_f$bestzone_label <- cor_bestzone[data_f$bestzone]

cor_pestiqty <- c("1" = "Yes","2" = "No")
data_f$pestqty_label <- cor_pestiqty[data_f$PestiQty]

cor_snailsize <- c("1" = "Yes","2" = "No")
data_f$snailsize_label <- cor_snailsize[data_f$SnailSize]

cor_snailbreeder <- c("1" = "Yes","2" = "No")
data_f$snailbreed_label <- cor_snailbreeder[data_f$snailbreeder]

cor_age <- c("1" = "<30","2" = "30-40", "3"="40-50", "4"="50-60", "5"=">=70")
data_f$age_label <- cor_age[data_f$Age]

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    variable1 = rownames(cormat)[row(cormat)[ut]],
    variable2 = rownames(cormat)[col(cormat)[ut]],
    correlation  =(cormat)[ut],
    p = pmat[ut]
  )
}



