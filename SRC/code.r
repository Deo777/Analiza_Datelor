library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(rsample)

data<-read.csv("D:/UTM/AD/Proiectul Final/Data/dataset.csv")

data <- data %>%
  mutate(winner_categ = case_when(
    winner == home_team ~ "Home",
    winner == away_team ~ "Away",
    winner == "Draw" ~ "Draw",
    TRUE ~ NA_character_
  ))

num_home_wins <- sum(data$home_team == data$winner & data$neutral == "False")
num_home_losses <- sum(data$away_team == data$winner & data$neutral == "False")
num_home_draws <- sum(data$winner == "Draw" & data$neutral == "False")

num_home_wins_neutral <- sum(data$home_team == data$winner & data$neutral == "True")
num_home_losses_neutral <- sum(data$away_team == data$winner & data$neutral == "True")
num_home_draws_neutral <- sum(data$winner == "Draw" & data$neutral == "True")


cat("Numarul de meciuri castigate de echipa gazda:", num_home_wins, "\n")
cat("Numarul de meciuri pierdute de echipa gazda:", num_home_losses, "\n")
cat("Numarul de meciuri la egalitate:", num_home_draws, "\n")

data$winner_categ <- as.factor(data$winner_categ)
data$tournament <- as.factor(data$tournament)
data$neutral <- as.factor(data$neutral)

contingency_table <- table(data$tournament, data$neutral, data$winner_categ)
contingency_df <- as.data.frame(as.table(contingency_table))
colnames(contingency_df) <- c("Tournament", "Neutral", "Winner_Category", "Frequency")

contingency_filtered <- contingency_df[contingency_df$Freq > 300, ]
contingency_filtered_true <- contingency_df[ contingency_df$Freq > 100 & contingency_df$Neutral == "False", ]
contingency_filtered
contingency_filtered_true

print(contingency_df)
print(contingency_filtered)

prop_table <- prop.table(table(data$tournament, data$winner == data$home_team), margin = 1)
prop_table <- as.data.frame(prop_table)

data <- data %>% mutate(home_results = if_else(winner_categ == "Home", 1, 0))

model_logistic <- glm(home_results ~ neutral + tournament + country, data = data, family = "binomial")

summary(model_logistic)

data$predicted_prob <- predict(model_logistic, type = "response")

ggplot(data, aes(x = predicted_prob, y = neutral, color = as.factor(home_results))) +
  geom_point() +
  geom_line() +
  labs(title = "Regresia logistică pentru home_results în funcție de locul desfășurării",
       x = "Probabilitatea de a câștiga",
       y = "Teren Neutru",
       color = "Rezultatul") +
  theme_minimal() +
  xlim(0.25, 1)

ggplot(data, aes(x = predicted_prob, y = neutral, color = winner_categ)) +
  geom_point() +
  geom_line() +
  labs(title = "Regresia logistică pentru winner_categ în funcție de locul desfășurării",
       x = "Probabilitatea de a câștiga",
       y = "Teren Neutru",
       color = "Categorie câștigător") +
  theme_minimal() +
  xlim(0.25, 1)

score_home <- subset(data, home_score <= 15 & neutral == "False")

col_palette <- rainbow(16)

histogram_winning <- ggplot(data = score_home, aes(x = home_score, fill = as.factor(home_score))) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_fill_manual(values = col_palette) +  # Setăm paleta de culori definite mai sus
  labs(title = "Distribuția scorurilor echipei gazde pe teren propriu", x = "Scor", y = "Frecventa") +
  guides(fill = guide_legend(title = "Nr. de goluri"))  # Adăugăm eticheta în legendă

histogram_winning

score_neutral <- subset(data, home_score <= 15 & neutral == "True")

col_palette <- rainbow(16)

histogram_winning <- ggplot(data = score_neutral, aes(x = home_score, fill = as.factor(home_score))) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_fill_manual(values = col_palette) +  # Setăm paleta de culori definite mai sus
  labs(title = "Distribuția scorurilor echipei gazde pe teren neutru", x = "Scor", y = "Frecventa") +
  guides(fill = guide_legend(title = "Nr. de goluri"))  # Adăugăm eticheta în legendă

histogram_winning

terrain_data <- data.frame(
  Terrain = c("Home", "Neutral"),
  Count = c(sum(data$neutral == "False"), sum(data$neutral == "True"))
)

terrain_plot <- ggplot(terrain_data, aes(x = Terrain, y = Count, fill = Terrain)) +
  geom_bar(stat = "identity") +
  labs(title = "Numărul de meciuri jucate pe teren propriu și teren neutru", x = "Tipul de teren", y = "Număr de meciuri") +
  theme_minimal()

print(terrain_plot)

performance_data <- data.frame(
  Type = rep(c("Wins", "Losses", "Draws"), each = 2),
  Terrain = rep(c("Home", "Neutral"), times = 3),
  Count = c(
    num_home_wins, num_home_losses, num_home_draws,
    num_home_wins_neutral, num_home_losses_neutral, num_home_draws_neutral
  )
)

performance_plot <- ggplot(performance_data, aes(x = Terrain, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Performanța echipei pe teren propriu și teren neutru", y = "Număr de meciuri") +
  scale_fill_manual(values = c("blue", "red", "gray")) +  # Culori pentru victorii, înfrângeri, egalități
  theme_minimal()

print(performance_plot)

pie_data_home <- data.frame(
  Type = c("Wins", "Losses", "Draws"),
  Count = c(num_home_wins, num_home_losses, num_home_draws),
  Terrain = rep("Home", 3)
)

pie_data_neutral <- data.frame(
  Type = c("Wins", "Losses", "Draws"),
  Count = c(num_home_wins_neutral, num_home_losses_neutral, num_home_draws_neutral),
  Terrain = rep("Neutral", 3)
)

pie_chart_home <- ggplot(pie_data_home, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Rezultate echipa gazda pe teren propriu", fill = "Rezultat") +
  scale_fill_manual(values = c("blue", "cyan", "gray")) +
  theme_minimal()

pie_chart_neutral <- ggplot(pie_data_neutral, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Rezultate echipa gazda pe teren neutru", fill = "Rezultat") +
  scale_fill_manual(values = c("blue", "cyan", "gray")) +
  theme_minimal()

print(pie_chart_home)
print(pie_chart_neutral)
grid.arrange(pie_chart_home, pie_chart_neutral, ncol = 2)

prag_frecventa <- 500  

competitii_cu_frecventa_mare <- data %>%
  count(tournament) %>%
  filter(n >= prag_frecventa)

ggplot(competitii_cu_frecventa_mare, aes(x = reorder(tournament, -n), y = n)) +
  geom_bar(fill = "blue", stat = "identity") +
  labs(
    title = "Frecvența meciurilor în competiții",
    x = "Competiție",
    y = "Frecvență"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


set.seed(123)
index <- createDataPartition(data$home_results, p = 0.8, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

probabilities <- predict(model_logistic, newdata = test_data, type = "response")

predictions <- ifelse(probabilities > 0.5, 1, 0)

conf_matrix <- table(Actual = test_data$home_results, Predicted = predictions)
print("Matricea de Confuzie:")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Acuratețea modelului este:", accuracy))

data_predict <- data.frame(
  home_results = sample(0:1, 100, replace = TRUE),
  predicted_prob = runif(100)
)

plot_prob <- ggplot(data_predict, aes(x = predicted_prob, y = home_results)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(title = "Probabilitatea de câștig pe teren propriu", x = "Probabilitate prezisă", y = "Câștig pe teren propriu") +
  theme_minimal()

print(plot_prob)

