#data cleaning
pacman::p_load(pacman, dplyr, rio, ggplot2, tidyr,scales)
data <- read.csv("~/ProjetCorona/owid-covid-data.csv")


data_maroc <- subset(data, location == "Morocco")
data_maroc$gueris <- data_maroc$total_cases - data_maroc$total_deaths
data_maroc$new_gueris <- data_maroc$new_cases - data_maroc$new_deaths
replace <- c("total_cases", "total_deaths", "new_deaths", "new_cases"  , "total_tests" , "reproduction_rate", "gueris", "new_gueris" )
data_maroc[columns_to_replace] <- lapply(data_maroc[replace], function(x) {
  x[is.na(x)] <- 0
  return(x)
})


data_maroc$date <- as.Date(data_maroc$date)
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
data_maroc <- subset(data_maroc, weekdays(date) == "dimanche")
data_maroc <- data_maroc[, c("total_cases", "new_cases", "total_deaths", "new_deaths", "gueris", "new_gueris", "date" , "total_tests","reproduction_rate" )]

#EX1Q1
data_maroc <- data_maroc[, c("total_cases", "new_cases", "total_deaths", "new_deaths", "gueris", "new_gueris", "date", "total_tests", "reproduction_rate")]
calculate_descriptives <- function(column) {
  min_val <- min(column)
  max_val <- max(column)
  mean_val <- mean(column)
  sd_val <- sd(column)
  return(c(min = min_val, max = max_val, mean = mean_val, sd = sd_val))
}
descriptive_stats <- sapply(data_maroc, calculate_descriptives)
descriptive_stats


#ex1Q2



moyennes_long <- data_maroc %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(
    moyenne_gueris = mean(new_gueris),
    moyenne_morts = mean(new_deaths),
    moyenne_cases = mean(new_cases)
  ) %>%
  gather(
    key = "variable",
    value = "value",
    -year
  )
ggplot(moyennes_long, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Moyennes annuelles des guéris, morts et cas totaux au Maroc",
    x = "Année", y = "Moyenne", fill = "Variable"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()





hist(data_maroc$new_cases,
     xlab = "totale des cas",
     ylab = "fréquence",
     las = 1,
     col="lightslateblue",
)

hist(data_maroc$new_deaths,
     xlab = "le nombre total de la mort",
     ylab = "frequence",
     col="lightblue",
     las = 1)

hist(data_maroc$new_gueris,
     xlab ="le nombre total des guéris",
     ylab = "densité",
     col ="lightblue",
     las = 1)

boxplot(data_maroc$total_cases,
        main = "Boxplot pour Total Cases au Maroc",
        ylab = "Total Cases")

boxplot(data_maroc$total_deaths,
        main = "Boxplot pour Total Deaths au Maroc",
        ylab = "Total Deaths")

boxplot(data_maroc$gueris,
        main ="le nombre totale des guéris",
        ylab = "nombre des guéris")

ggplot(data_maroc, aes(x = date, y = new_deaths)) +
  geom_col(fill = "blue", color = "black") +
  labs(x = "Date", y = "Nouveaux morts", title = "graphe des nouveaux morts en fonction de la date") +
  theme_minimal()

ggplot(data_maroc, aes(x = date, y = total_cases)) +
  geom_col(fill = "red", color = "black") +
  labs(x = "Date", y = "les cas totales", title = "graphe des cas totales en fonction de la date") +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

ggplot(data_maroc, aes(x = date, y = new_cases)) +
  geom_col(fill = "orange", color = "black") +
  labs(x = "Date", y = "nouveaux cas", title = "graphe des nouveaux cas en fonction de la date") +
  theme_minimal()

ggplot(data_maroc, aes(x = date, y = gueris)) +
  geom_col(fill = "purple", color = "black") +
  labs(x = "Date", y = "totale des guéris", title = "graphe des nouveaux morts en fonction de la date") +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

ggplot(data_maroc1, aes(x = total_cases, y = total_deaths)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(x = "Total Cases", y = "Total Deaths", title = "Dispersion des données") +
  theme_minimal()

data_maroc2 <- data_maroc1 %>% select(date, new_cases, new_gueris)
data_long <- gather(data_maroc2, key = "variable", value = "value", -date)
ggplot(data_long, aes(x = date, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "evolution des nouveaux cas et nouveaux guéris au Maroc",
       x = "Date", y = "Nombre",
       color = "Variable") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("new_cases" = "orange", "new_gueris" = "purple"))



nonZero <- function(x) {
  for (i in 2:length(x)) {
    if (x[i] == 0) {
      x[i] <- x[i - 1]
    }
  }
  return(x)
}

data_maroc$total_tests <- nonZero(data_maroc$total_tests)
data_maroc_long <- data_maroc %>%
  select(date, total_cases, total_tests) %>%
  gather(key = "variable", value = "value", -date)
ggplot(data_maroc_long, aes(x = date, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Comparaison entre Total Cases et Total Tests au Maroc",
       x = "Date", y = "Nombre",
       color = "Variable") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("total_cases" = "red", "total_tests"="blue"))




ggplot(data_maroc, aes(x = date, y = reproduction_rate)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 1.5) +
  labs(title = "Évolution du taux de reproduction au Maroc",
       x = "Date", y = "Taux de reproduction") +
  theme_minimal()


data_maroc$taux_mortalite <- data_maroc$total_deaths / data_maroc$total_cases
ggplot(data_maroc, aes(x = date, y = taux_mortalite)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 1.5) +
  labs(title = "Évolution du taux de mortalité au Maroc",
       x = "Date", y = "Taux de mortalité") +
  theme_minimal()


ggplot(data_maroc, aes(x = new_cases, y = new_deaths)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "Régression linéaire entre les nouveaux cas et les nouveaux décès", x = "Nouveaux cas", y = "Nouveaux décès")


#ex2


correlation1 <- cor(data_maroc$total_cases , data_maroc$gueris)
correlation2 <- cor(data_maroc$total_cases , data_maroc$total_deaths)
correlation3 <- cor(data_maroc$new_cases , data_maroc$new_deaths)
correlation4 <- cor(data_maroc$new_cases , data_maroc$new_gueris)


print(correlation1)
print(correlation2)
print(correlation3)
print(correlation4)



# hypothese nulle 

data_maroc_confinement <- subset(data_maroc,date >= as.Date("2020-03-29") & date <= as.Date("2020-07-05"))
data_maroc_apres_confinement <- subset(data_maroc,date >= as.Date("2020-07-05") & date <= as.Date("2020-10-05"))

test <- t.test(data_maroc_confinement$reproduction_rate,data_maroc_apres_confinement$reproduction_rate)
print(test)













