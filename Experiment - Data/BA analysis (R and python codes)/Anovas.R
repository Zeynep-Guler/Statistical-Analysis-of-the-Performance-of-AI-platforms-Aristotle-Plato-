#ANOVAS

library(dplyr)
library(tidyr)
attach(data_cleaned)
# Perform ANOVA for each variable

anova_ai_usage <- aov(AI_Usage_Frequency ~ Treatment_Randomizer, data = data_cleaned)
anova_ai_knowledge <- aov(AI_Knowledge_Rating ~ Treatment_Randomizer, data = data_cleaned)
anova_privacy_concern <- aov(Digital_Privacy_Concern ~ Treatment_Randomizer, data = data_cleaned)
anova_tech_openess <- aov(Tech_Adoption_Openess ~ Treatment_Randomizer, data = data_cleaned)

# Print summaries of ANOVA results
cat("ANOVA for AI Usage Frequency:\n")
summary(anova_ai_usage)

cat("\nANOVA for AI Knowledge Rating:\n")
summary(anova_ai_knowledge)

cat("\nANOVA for Digital Privacy Concern:\n")
summary(anova_privacy_concern)

cat("\nANOVA for Tech Adoption Openness:\n")
summary(anova_tech_openess)


library(ggplot2)

# Boxplots for each variable
variables <- c("AI_Usage_Frequency", "AI_Knowledge_Rating", 
               "Digital_Privacy_Concern", "Tech_Adoption_Openess")

for (var in variables) {
  ggplot(data_cleaned, aes(x = Treatment_Randomizer, y = .data[[var]])) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("Boxplot of", var, "by Treatment Group"),
         x = "Treatment Group",
         y = var) +
    ggsave(paste0("boxplot_", var, ".png")) # Save the plot
}
