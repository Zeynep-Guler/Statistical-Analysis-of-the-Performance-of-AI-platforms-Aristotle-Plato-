# Load necessary libraries
library(stargazer)
install.packages('car')
library(car)

# Model 1: Just Treatment
model1 <- lm(Trust_Change ~ Treatment_Randomizer, data = data_cleaned)

# Model 2: Add Pre-trust segment covariate
model2 <- lm(Trust_Change ~ Treatment_Randomizer + pre_trust_segment, data = data_cleaned)

# Model 3: Add unbalanced covariate (Tech Adoption Openness)
model3 <- lm(Trust_Change ~ Treatment_Randomizer + pre_trust_segment + Tech_Adoption_Openess, data = data_cleaned)

# Model 4: Interaction between Treatment and Pre-trust segment
model4 <- lm(Trust_Change ~ Treatment_Randomizer * pre_trust_segment + Tech_Adoption_Openess, data = data_cleaned)

# Model 5: All covariates
model5 <- lm(Trust_Change ~ Treatment_Randomizer * pre_trust_segment + Tech_Adoption_Openess +
               AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern +
               pre_confidence_segment, data = data_cleaned)



# Example: Comparing models
AIC(model1, model2, model3, model4, model5)
# VIF check
vif(model5, type = "predictor")


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

