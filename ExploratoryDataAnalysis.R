library(ggplot2)

# Target variable distribution
ggplot(df, aes(x = heart_risk)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Heart Disease Risk",
       x = "Heart Risk",
       y = "Count")

# Age distribution
ggplot(df, aes(x = age)) +
  geom_histogram(bins = 30, fill = "lightblue") +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Frequency")


#Smoking vs Heart Risk

library(ggplot2)

ggplot(df, aes(x = smoking, fill = heart_risk)) +
  geom_bar(position = "fill") +
  labs(
    title = "Smoking Status vs Heart Disease Risk",
    x = "Smoking",
    y = "Proportion",
    fill = "Heart Risk"
  )

#normalization

ggplot(df, aes(x = age)) +
  geom_histogram(bins = 30, fill = "lightblue") +
  labs(
    title = "Original Age Distribution",
    x = "Age",
    y = "Frequency"
  )

ggplot(df, aes(x = age_scaled)) +
  geom_histogram(bins = 30, fill = "lightyellow") +
  labs(
    title = "Standardized Age Distribution",
    x = "Scaled Age (Z-score)",
    y = "Frequency"
  )


