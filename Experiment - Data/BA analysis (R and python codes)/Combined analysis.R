# Combined analysis file# Load necessary library
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(car)

#DATA CLEANUP

# Read the data
file_path <- '/Users/monikakaczorowska/Desktop/DSBA/semester 1/Business Analytics/project/11.19.data.csv'
data <- read.csv(file_path, stringsAsFactors = FALSE, na.strings=c("","NA"))
data$Q3_1 <- as.numeric(data$Q3_1)
data$Q6_1 <- as.numeric(data$Q6_1)
data$Q7_1 <- as.numeric(data$Q7_1)
data$Q8_1 <- as.numeric(data$Q8_1)
data$Q9_1 <- as.numeric(data$Q9_1)
data$Q15_1 <- as.numeric(data$Q15_1)
data$Q18_1 <- as.numeric(data$Q18_1)
data$Q19_1 <- as.numeric(data$Q19_1)

# Step 1: Remove rows where the "Status" column indicates a preview
data_cleaned <- data %>%
  filter(Status != "Survey Preview")


data_cleaned <- data %>%
  filter(Progress == 100)



# Step 3: Select only question columns and the treatment randomizer column
# Keep only question columns and the randomizer column explicitly
columns_to_keep <- c(question_cols, "FL_7_DO")
data_cleaned <- data_cleaned %>%
  select(all_of(columns_to_keep))
data_cleaned <- data_cleaned %>% select(-Q_DataPolicyViolations)


# Step 2: Drop rows where any column has no data
data_cleaned <- data_cleaned %>% drop_na()


# Step 4: Rename columns to be more intuitive
colnames(data_cleaned) <- c("Age", "Gender", "AI_Usage_Frequency", "AI_Knowledge_Rating", 
                            "Digital_Privacy_Concern", "Tech_Adoption_Openess",
                            "Trust_AI_Privacy_Pre", "Decision_Confidence_Pre", 
                            "Trust_AI_Privacy_Post", "Decision_Confidence_Post", "Treatment_Randomizer")


# Save the cleaned data
write.csv(data_cleaned, "cleaned_data.csv", row.names = FALSE)

data_cleaned$Treatment_Randomizer <- factor(data_cleaned$Treatment_Randomizer, 
                                            levels = c("Treatment-Neutral", "Treatment-Negative", "Treatment-Positive"))


data_cleaned$pre_trust_segment <- cut(data_cleaned$Trust_AI_Privacy_Pre, 
                                      breaks = c(-Inf, 3, 6, Inf), 
                                      labels = c("low", "medium", "high"))


data_cleaned$pre_confidence_segment <- cut(data_cleaned$Decision_Confidence_Pre, 
                                           breaks = c(-Inf, 3, 6, Inf), 
                                           labels = c("low", "medium", "high"))


# Define dependent variables
data_cleaned$Trust_Change <- data_cleaned$Trust_AI_Privacy_Post - data_cleaned$Trust_AI_Privacy_Pre
data_cleaned$Confidence_Change <- data_cleaned$Decision_Confidence_Post - data_cleaned$Decision_Confidence_Pre

write.csv(data_cleaned,"dataCleaned.csv", row.names = FALSE)

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


# Model 1: Just Treatment
model1 <- lm(Trust_Change ~ Treatment_Randomizer, data = data_cleaned)

# Model 2: Add Pre-trust segment covariate
model2 <- lm(Trust_Change ~ Treatment_Randomizer + pre_trust_segment, data = data_cleaned)

# Model 3: Add unbalanced covariate (Tech Adoption Openness)
model3 <- lm(Trust_Change ~ Treatment_Randomizer + pre_trust_segment + Tech_Adoption_Openess, data = data_cleaned)

# Model 4: Adds all other covariates
model4 <- lm(Trust_Change ~ Treatment_Randomizer + pre_trust_segment + Tech_Adoption_Openess + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern +pre_confidence_segment, data = data_cleaned)

# Model 5: Interaction between Treatment and Pre-trust segment
model5 <- lm(Trust_Change ~ Treatment_Randomizer * pre_trust_segment + Tech_Adoption_Openess, data = data_cleaned)

# Model 6: All covariates
model6 <- lm(Trust_Change ~ Treatment_Randomizer * pre_trust_segment + Tech_Adoption_Openess +
               AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern +
               pre_confidence_segment, data = data_cleaned)


summary (model1)
summary(model4)
# Comparing models
AIC(model1, model2, model3, model4, model5, model6)
# VIF checks
vif(model4)
vif(model6, type = "predictor")



# Force stargazer to include only the correct terms for each model
stargazer(model1, model2, model3, model4, model5,
          type = "text",
          title = "Regression Results: Trust Change",
          dep.var.labels = c("Trust Change"),
          covariate.labels = c(
            # Treatment effects
            "Treatment: Negative",
            "Treatment: Positive",
            # Pre-trust segment effects
            "Pre-trust Segment: Medium",
            "Pre-trust Segment: High",
            # Unbalanced covariate
            "Tech Adoption Openness",
            # Interaction terms for Model 4 and 5
            "Treatment x Pre-trust: Negative x Medium",
            "Treatment x Pre-trust: Positive x Medium",
            "Treatment x Pre-trust: Negative x High",
            "Treatment x Pre-trust: Positive x High",
            # Additional covariates (Model 5 only)
            "AI Usage Frequency",
            "AI Knowledge Rating",
            "Digital Privacy Concern",
            "Pre-confidence Segment: Medium",
            "Pre-confidence Segment: High"
          ),
          keep.stat = c("n", "rsq", "adj.rsq"),
          notes = "p-values: ***<0.01, **<0.05, *<0.10")


