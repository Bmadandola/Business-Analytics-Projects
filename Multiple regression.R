# Multiple Linear Regression: Biking and Smoking vs Heart Disease

library(ggplot2)
library(ggpubr)

# Load dataset
heart.data <- read.csv("C:/Users/biola/Downloads/heart.data.csv")
summary(heart.data)

# Explore data
hist(heart.data$heart.disease)
plot(heart.disease ~ biking, data = heart.data)
plot(heart.disease ~ smoking, data = heart.data)

# Multiple Linear Regression model
heart.disease.lm <- lm(heart.disease ~ biking + smoking, data = heart.data)
summary(heart.disease.lm)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(heart.disease.lm)
par(mfrow = c(1, 1))

# Create prediction grid
plotting.data <- expand.grid(
  biking = seq(min(heart.data$biking), max(heart.data$biking), length.out = 30),
  smoking = c(min(heart.data$smoking), mean(heart.data$smoking), max(heart.data$smoking))
)

plotting.data$predicted.y <- predict(heart.disease.lm, newdata = plotting.data)
plotting.data$smoking <- round(plotting.data$smoking, 2)
plotting.data$smoking <- as.factor(plotting.data$smoking)

# Plot with prediction lines
heart.plot <- ggplot(heart.data, aes(x = biking, y = heart.disease)) +
  geom_point() +
  geom_line(data = plotting.data, aes(x = biking, y = predicted.y, color = smoking), linewidth = 1.25) +
  theme_bw() +
  labs(
    title = "Rates of Heart Disease vs. Biking and Smoking",
    x = "Biking to Work (% of population)",
    y = "Heart Disease (% of population)",
    color = "Smoking (% of population)"
  ) +
  annotate(geom = "text", x = 30, y = 1.75, 
           label = "= 15 + (-0.2*biking) + (0.178*smoking)")

heart.plot

