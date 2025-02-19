# analysis 2 - interaction effects

attach(data_cleaned)



#change in TRUST,  interaction + tech adoption (unbalanced covariate)
modelChangeTrustInteraction <- lm(Trust_Change ~ Treatment_Randomizer * pre_trust_segment + Tech_Adoption_Openess, data = data_cleaned)
summary(modelChangeTrustInteraction)

#change in TRUST,  interaction + covariates
modelChangeTrustInteractionCovariates <- lm(Trust_Change ~ Treatment_Randomizer * pre_trust_segment + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern + Tech_Adoption_Openess  + pre_confidence_segment, data = data_cleaned)
summary(modelChangeTrustInteractionCovariates)


# Example: Comparing models
AIC(modelChangeTrustInteraction, modelChangeTrustInteractionCovariates)
# VIF check
vif(modelChangeTrustInteractionCovariates, type = "predictor")



#change in Confidence, interaction tech adoption (unbalanced covariate)
modelChangeConfidenceInteraction <- lm(Confidence_Change ~ Treatment_Randomizer * pre_confidence_segment  + Tech_Adoption_Openess,data = data_cleaned)
summary(modelChangeConfidenceInteraction)


#change in Confidence, interaction + covariates
modelChangeConfidenceInteractionCovariates <- lm(Confidence_Change ~ Treatment_Randomizer * pre_confidence_segment + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern + Tech_Adoption_Openess + pre_trust_segment, data = data_cleaned)
summary(modelChangeConfidenceInteractionCovariates)


# Example: Comparing models
AIC(modelChangeConfidenceInteraction, modelChangeConfidenceInteractionCovariates)
# VIF check
vif(modelChangeConfidenceInteractionCovariates, type = "predictor")
