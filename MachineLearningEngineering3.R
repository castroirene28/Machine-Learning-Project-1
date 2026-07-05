# Ice Cream Machine Learning Engineering Dashboards

# Package Installation and Loading


required_packages <- c(
  "ggplot2",
  "dplyr",
  "caret",
  "randomForest"
)

for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


# Ice Cream Flavors

icecream_colors <- c(
  Vanilla    = "#FFF8DC",
  Chocolate  = "#8B4513",
  Strawberry = "#FF69B4",
  Mint       = "#98FF98",
  Blueberry  = "#6A5ACD"
)

theme_icecream <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(
        fill = icecream_colors["Vanilla"],
        color = NA
      ),
      panel.background = element_rect(
        fill = icecream_colors["Vanilla"],
        color = NA
      ),
      text = element_text(
        color = icecream_colors["Chocolate"],
        face = "bold"
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 18
      )
    )
}

############################################################
# LOAD DATA
############################################################

data(iris)

cat("\n🍦 DATA LOADED SUCCESSFULLY\n")

############################################################
# TRAIN / TEST SPLIT
############################################################

set.seed(123)

train_index <- createDataPartition(
  iris$Species,
  p = 0.80,
  list = FALSE
)

train_data <- iris[train_index, ]
test_data  <- iris[-train_index, ]


# Machine Learning Model


rf_model <- randomForest(
  Species ~ .,
  data = train_data,
  ntree = 300
)

predictions <- predict(
  rf_model,
  test_data
)

conf_mat <- confusionMatrix(
  predictions,
  test_data$Species
)

accuracy <- conf_mat$overall["Accuracy"]


# Statistical Analysis


stats_summary <- iris %>%
  summarise(
    Mean_Sepal_Length = mean(Sepal.Length),
    SD_Sepal_Length   = sd(Sepal.Length),
    Mean_Petal_Length = mean(Petal.Length),
    SD_Petal_Length   = sd(Petal.Length),
    Mean_Sepal_Width  = mean(Sepal.Width),
    Mean_Petal_Width  = mean(Petal.Width)
  )

print(stats_summary)

############################################################
# BOXPLOT VISUALIZATION
############################################################

box_plot <- ggplot(
  iris,
  aes(
    Species,
    Sepal.Length,
    fill = Species
  )
) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      icecream_colors["Strawberry"],
      icecream_colors["Mint"],
      icecream_colors["Blueberry"]
    )
  ) +
  labs(
    title = " Ice Cream Statistical Analysis",
    x = "Species",
    y = "Sepal Length"
  ) +
  theme_icecream()

print(box_plot)


# Feature Importance

importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[,1]
)

importance_plot <- ggplot(
  importance_df,
  aes(
    reorder(Feature, Importance),
    Importance,
    fill = Feature
  )
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "🍫 Machine Learning Feature Importance",
    x = "",
    y = "Importance"
  ) +
  theme_icecream()

print(importance_plot)

############################################################
# SCATTER PLOT
############################################################

scatter_plot <- ggplot(
  iris,
  aes(
    Sepal.Length,
    Petal.Length,
    color = Species
  )
) +
  geom_point(
    size = 4,
    alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      icecream_colors["Strawberry"],
      icecream_colors["Mint"],
      icecream_colors["Blueberry"]
    )
  ) +
  labs(
    title = " Machine Learning Classification Space"
  ) +
  theme_icecream()

print(scatter_plot)

############################################################
# COMPUTER VISION STYLE HEATMAP
############################################################

set.seed(42)

vision_matrix <- matrix(
  runif(10000),
  nrow = 100,
  ncol = 100
)

image(
  vision_matrix,
  col = colorRampPalette(
    c(
      "#FFF8DC",
      "#FF69B4",
      "#6A5ACD"
    )
  )(100),
  main = "👁️ Computer Vision Heatmap (Ice Cream Theme)",
  axes = FALSE
)


# Edge Dection Style Visualization

edge_matrix <- abs(diff(vision_matrix))

image(
  edge_matrix,
  col = gray.colors(100),
  main = "🔍 Simulated Edge Detection",
  axes = FALSE
)


# Correlation Analysis


cor_matrix <- cor(
  iris[,1:4]
)

print(cor_matrix)

heatmap(
  cor_matrix,
  col = colorRampPalette(
    c(
      "#FFF8DC",
      "#FF69B4",
      "#6A5ACD"
    )
  )(100),
  main = "🍦 Feature Correlation Heatmap"
)

#
# ENGINEERING DASHBOARD OUTPUT


cat("\n")
cat("========================================\n")
cat(" ICE CREAM ML ENGINEERING DASHBOARD\n")
cat("========================================\n")
cat("Algorithm  : Random Forest\n")
cat("Accuracy   :", round(as.numeric(accuracy)*100,2), "%\n")
cat("Trees      :", rf_model$ntree, "\n")
cat("Features   :", ncol(train_data)-1, "\n")
cat("Train Size :", nrow(train_data), "\n")
cat("Test Size  :", nrow(test_data), "\n")
cat("========================================\n")

print(conf_mat)


