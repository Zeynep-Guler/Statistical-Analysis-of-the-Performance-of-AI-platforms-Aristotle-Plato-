
# analysis
# 
# 1-Change in trust with treatment dummies
# 

attach(data_cleaned)
library(car)

# Define dependent variables
data_cleaned$Trust_Change <- Trust_AI_Privacy_Post - Trust_AI_Privacy_Pre
data_cleaned$Confidence_Change <- Decision_Confidence_Post - Decision_Confidence_Pre

# 1. CHANGE IN TRUST --------------------------------------------------------

# Change in TRUST, treatment only + Tech Adoption(un balanced variable)
modelChangeTrust0 <- lm(Trust_Change ~ Treatment_Randomizer + Tech_Adoption_Openess, data = data_cleaned)
summary(modelChangeTrust0)

# Change in TRUST, Tech Adoption + AI Usage
modelChangeTrust1 <- lm(Trust_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency, data = data_cleaned)
summary(modelChangeTrust1)

# Change in TRUST, Tech Adoption + AI Usage + AI Knowledge
modelChangeTrust2 <- lm(Trust_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency + AI_Knowledge_Rating, data = data_cleaned)
summary(modelChangeTrust2)

# Change in TRUST, Tech Adoption + AI Usage + AI Knowledge + Digital Privacy
modelChangeTrust3 <- lm(Trust_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern, data = data_cleaned)
summary(modelChangeTrust3)

# Change in TRUST, Tech Adoption + AI Usage + AI Knowledge + Digital Privacy + Pre Treatment Confidence Segment
modelChangeTrust4 <- lm(Trust_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern + pre_confidence_segment, data = data_cleaned)
summary(modelChangeTrust4)

# Change in TRUST, Tech Adoption + ALL covariates (except Gender and Age)
modelChangeTrust5 <- lm(Trust_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern + pre_confidence_segment + pre_trust_segment , data = data_cleaned)
summary(modelChangeTrust5)

# Comparing models
AIC(modelChangeTrust0, modelChangeTrust1, modelChangeTrust2, modelChangeTrust3, modelChangeTrust4, modelChangeTrust5)

# VIF check
vif(modelChangeTrust5)




# 2. CHANGE IN CONFIDENCE ---------------------------------------------------

# Change in CONFIDENCE, treatment only + Tech Adoption
modelChangeConfidence0 <- lm(Confidence_Change ~ Treatment_Randomizer + Tech_Adoption_Openess, data = data_cleaned)
summary(modelChangeConfidence0)

# Change in CONFIDENCE, Tech Adoption + AI Usage
modelChangeConfidence1 <- lm(Confidence_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency, data = data_cleaned)
summary(modelChangeConfidence1)

# Change in CONFIDENCE, Tech Adoption + AI Usage + AI Knowledge
modelChangeConfidence2 <- lm(Confidence_Change ~ Tech_Adoption_Openess + Treatment_Randomizer  + AI_Usage_Frequency + AI_Knowledge_Rating, data = data_cleaned)
summary(modelChangeConfidence2)

# Change in CONFIDENCE, Tech Adoption + AI Usage + AI Knowledge + Digital Privacy
modelChangeConfidence3 <- lm(Confidence_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern, data = data_cleaned)
summary(modelChangeConfidence3)

# Change in CONFIDENCE, Tech Adoption + AI Usage + AI Knowledge + Digital Privacy + Pre-Treatment Trust
modelChangeConfidence4 <- lm(Confidence_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern + pre_trust_segment, data = data_cleaned)
summary(modelChangeConfidence4)

# Change in CONFIDENCE, Tech Adoption + ALL covariates (except Gender and Age)
modelChangeConfidence5 <- lm(Confidence_Change ~ Tech_Adoption_Openess + Treatment_Randomizer + AI_Usage_Frequency + AI_Knowledge_Rating + Digital_Privacy_Concern + pre_trust_segment + pre_confidence_segment, data = data_cleaned)
summary(modelChangeConfidence5)

# Comparing models
AIC(modelChangeConfidence0, modelChangeConfidence1, modelChangeConfidence2, modelChangeConfidence3, modelChangeConfidence4, modelChangeConfidence5)

# VIF check
vif(modelChangeConfidence5)
