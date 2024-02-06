# Analyses univariées
library(ggplot2)
library(forcats)
 # Variables catégorielles
ggplot(data_f, aes(x = fct_infreq(gender))) +
  geom_bar() + xlab("Gender")+ ylab("Frequency")
  scale_x_discrete(labels = c("1" = "Male", "2"="Female"))+
  theme(panel.background = element_blank())


ggplot(data_f, aes(x = fct_infreq(locality))) +
  geom_bar() + xlab("Locality")

ggplot(data_f, aes(x = fct_infreq(Marstat))) +
  geom_bar() + xlab("Marital status") +
  scale_x_discrete(labels = c("1" = "Single", "4"="Divorced", "6"="Others",
                              "2" = "Married", "3" = "Separated", "5"="Widow"))
ggplot(data_f, aes(x = fct_infreq(Edulevel))) +
  geom_bar() + xlab("Education level") +
  scale_x_discrete(labels = c("1" = "Primary", "2"="Secondary", "3"="Tertiary"))

ggplot(data_f, aes(x = fct_infreq(Mainact))) +
  geom_bar() + xlab("Main activity") +
  scale_x_discrete(labels = c("1" = "Farming", "4"="Artisan",
                              "2" = "Trading", "3" = "Civil servant", "5"="Others"))
ggplot(data_f, aes(x = fct_infreq(Collectzone))) +
  geom_bar() + xlab("Main activity") +
  scale_x_discrete(labels = c("1" = "Within the village",
                              "2" = "Out of the village"))

ggplot(data_f, aes(x = fct_infreq(Season))) +
  geom_bar() + xlab("Season")

ggplot(data_f, aes(x = fct_infreq(Month))) +
  geom_bar() + xlab("Month")

ggplot(data_f, aes(x = fct_infreq(Time))) +
  geom_bar() + xlab("Time") +
  scale_x_discrete(labels = c("1" = "Morning",
                              "2" = "Afternoon", "3" = "Nigth"))

ggplot(data_f, aes(x = fct_infreq(reason_time))) +
  geom_bar() + xlab("Reason Time")

ggplot(data_f, aes(x = fct_infreq(bestzone))) +
  geom_bar() + xlab("Best zone") +
  scale_x_discrete(labels = c("1" = "Around houses", "4"="Around farms", "5"="In the forests",
                              "2" = "Around garbage", "3" = "Around toilet", "6"="Others"))
ggplot(data_f, aes(x = fct_infreq(PestiQty))) +
  geom_bar() + xlab("Pesticides effects on quantity of snails") +
  scale_x_discrete(labels = c("1" = "Yes","2" = "No"))

ggplot(data_f, aes(x = fct_infreq(SnailSize))) +
  geom_bar() + xlab("Pesticides effects on size of snails") +
  scale_x_discrete(labels = c("1" = "Yes","2" = "No"))


 # Variables quantitatives
ggplot(data_f, aes(x = Collectexp)) +
  geom_bar()

ggplot(data_f, aes(x = householdsize)) +
  geom_bar()

ggplot(data_f, aes(x = fct_infreq(`Quantity(bucket)`))) +
  geom_bar() + xlab("Quantity of buckets") +
  scale_x_discrete(labels = c("1" = "One bucket",
                              "2" = "two buckets", "3" = "three buckets"))

ggplot(data_f, aes(x = fct_infreq(Marstat))) +
         geom_bar() + xlab("Marital status") + ylab("Frequency")+
         scale_x_discrete(labels = c("1" = "Single", "4"="Divorced", "6"="Others",
                                     "2" = "Married", "3" = "Separated", "5"="Widow"))+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

