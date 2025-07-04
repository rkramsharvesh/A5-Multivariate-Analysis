# Load required libraries
library(tidyverse)

# 1. Load the data
pizza <- read.csv("D:/Masters/VCU/Classes/SCMA/R/A5/pizza_data.csv")

# 2. Run the conjoint regression model
conjoint_model <- lm(ranking ~ ., data = pizza)

# 3. View summary of the model (part-worth utilities)
summary(conjoint_model)

# 4. Extract coefficients
coefs <- coef(conjoint_model)
coef_df <- data.frame(term = names(coefs), estimate = as.numeric(coefs))

# 5. Remove intercept
coef_df <- coef_df %>% filter(term != "(Intercept)")

# 6. Separate into attribute and level
coef_df <- coef_df %>%
  separate(term, into = c("attribute", "level"), sep = "(?<=[a-z])(?=[A-Z0-9])", extra = "merge", fill = "right")

# 7. Calculate attribute importance as range of part-worths
importance_df <- coef_df %>%
  group_by(attribute) %>%
  summarise(range = max(estimate) - min(estimate)) %>%
  mutate(importance = round(range / sum(range) * 100, 1))

# 8. Bar plot for attribute importance
ggplot(importance_df, aes(x = reorder(attribute, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Attribute Importance from Conjoint Analysis",
       x = "Attribute", y = "Importance (%)") +
  theme_minimal()
