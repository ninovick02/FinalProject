library(plumber)
library(tidyverse)

# Loading saved fits and workflows
load("savedrf.RData")
load("savedtree.RData")

# Loading Data Set
diabetes_data <- df <- read_csv("Data/diabetes_binary_health_indicators_BRFSS2015.csv")|>
  mutate(across(-c(BMI, GenHlth, MentHlth, PhysHlth, Sex, Age, Education, Income),
                ~factor(.x, levels = c(0, 1), labels = c("No", "Yes")))) |>
  mutate(GenHlth = ordered(GenHlth, levels = c(1, 2, 3, 4, 5), label=c("excellent", "very good", "good", "fair", "poor"))) |>
  mutate(Sex = factor(Sex, levels = c(0, 1), labels = c("female", "male"))) |>
  mutate(Age = ordered(Age,
                       levels = 1:13, 
                       labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older"))) |>
  mutate(Education = ordered(Education, levels = 1:6,
                             labels = c("Never attended school or only kindergarten", "Elementary", "Some high school", "High school graduate", "Some college", "College graduate"))) |>
  mutate(Income = ordered(Income, levels = 1:8, 
                          labels = c("<$10,000", "$10,000-$15,000", "$15,000-$20,000", "$20,000-$25,000", "$25,000-$35,000", "$35,000-$50,000", "$50,000-$75,000", ">$75,000")))|>
  mutate(
    Education = fct_collapse(Education,
                             "Up to some high school" = c("Never attended school or only kindergarten", "Elementary", "Some high school")),
    Income = fct_collapse(Income, "<$20,000" = c("<$10,000", "$10,000-$15,000", "$15,000-$20,000")))

# Fitting Best model using saved workflow
final_tree_model <- tree_final_wf |> fit(diabetes_data)

# Creating default list for predictors
defaults <- lapply(names(df), function(x){
  if(is.numeric(df[[x]])){
    mean(df[[x]])
  }else{
    temp <- unique(df[[x]])
    temp[which.max(tabulate(match(df[[x]], temp)))]
  }
})
names(defaults) = names(df)

#* @apiTitle Diabetes Fits
#* @apiDescription Plumber example description.

#* Predict diabetes probability
#* Parameters in order of Model Importance
#* @param BMI Body Mass Index
#* @param GenHlth General Health (excellent, very good, good, fair, poor)
#* @param Age Age in 5 year intervals (18-24, 25-29, 30-24, ..., 75-79, 80 or older)
#* @param HighBP High Blood Pressure (Yes, No)
#* @param PhysHlth Number of physical illness or injury days in the past 30 days (1-30)
#* @param HighChol High cholesterol (Yes, No)
#* @param Income Annual Income (<$20,000, $20,000-$25,000, $25,000-$35,000, $35,000-$50,000, $50,000-$75,000, >$75,000)
#* @param MentHlth Number of poor mental health days in the past 30 days (1-30)
#* @param DiffWalk Difficulty walking or climbing stairs (Yes, No)
#* @param Education Highest education level (Up to some high school, High school graduate, Some college, College graduate)
#* @param HeartDiseaseorAttack Prior coronary heart disease or myocardial infarction (Yes, No)
#* @param PhysActivity Physical activity in the past 30 days (Yes, No)
#* @param Sex Sex (female, male)
#* @param Fruits Consume fruit 1 or more times per day (Yes, No)
#* @param Veggies Consume vegetables 1 or more times per day (Yes, No)
#* @param Smoker Smoked at least 100 cigarettes (Yes, No)
#* @param Stroke Prior Stroke (Yes, No)
#* @param NoDocbcCost Could not see doctor in the last 12 months due to cost (Yes, No)
#* @param HvyAlcoholConsump Heavy Alcohol Consumption (Yes, No) *men > 13 per week, women > 6 per week
#* @param AnyHealthcare Health Care (Yes, No)
#* @param CholCheck Cholesterol checked in the last 5 years (Yes, No)
#* @get /pred
function(
    BMI                  = defaults$BMI,
    GenHlth              = defaults$GenHlth,
    Age                  = defaults$Age,
    HighBP               = defaults$HighBP,
    PhysHlth             = defaults$PhysHlth,
    HighChol             = defaults$HighChol,
    Income               = defaults$Income, 
    MentHlth             = defaults$MentHlth,
    DiffWalk             = defaults$DiffWalk,
    Education            = defaults$Education,
    HeartDiseaseorAttack = defaults$HeartDiseaseorAttack,
    PhysActivity         = defaults$PhysActivity,
    Sex                  = defaults$Sex,
    Fruits               = defaults$Fruits,
    Smoker               = defaults$Smoker,
    Veggies              = defaults$Veggies,
    Stroke               = defaults$Stroke,
    NoDocbcCost          = defaults$NoDocbcCost,
    HvyAlcoholConsump    = defaults$HvyAlcoholConsump,
    AnyHealthcare        = defaults$AnyHealthcare,
    CholCheck            = defaults$CholCheck
) {
  # In order of df
  newdata <- data.frame(
    HighBP               = as.numeric(HighBP),
    HighChol             = as.numeric(HighChol),
    CholCheck            = as.numeric(CholCheck),
    BMI                  = as.numeric(BMI),
    Smoker               = as.numeric(Smoker),
    Stroke               = as.numeric(Stroke),
    HeartDiseaseorAttack = as.numeric(HeartDiseaseorAttack),
    PhysActivity         = as.numeric(PhysActivity),
    Fruits               = as.numeric(Fruits),
    Veggies              = as.numeric(Veggies),
    HvyAlcoholConsump    = as.numeric(HvyAlcoholConsump),
    AnyHealthcare        = as.numeric(AnyHealthcare),
    NoDocbcCost          = as.numeric(NoDocbcCost),
    GenHlth              = as.numeric(GenHlth),
    MentHlth             = as.numeric(MentHlth),
    PhysHlth             = as.numeric(PhysHlth),
    DiffWalk             = as.numeric(DiffWalk),
    Sex                  = as.numeric(Sex),
    Age                  = as.numeric(Age),
    Education            = as.numeric(Education),
    Income               = as.numeric(Income)
  )
  
  # Return class and probabilities
  list(
    "Prediction" = predict(final_rf_model, newdata),
    "Prob_of_Pred"  = predict(final_rf_model, newdata, type="prob")
  )
}

# Examples:
# http://127.0.0.1:8000/pred?BMI=30&Age=50&Smoker=1
# http://127.0.0.1:8000/pred?HighBP=1&HighChol=1&GenHlth=4
# http://127.0.0.1:8000/pred

#* Info Endpoint
#* @get /info
function(){
  list(
    name = "Naomi Novick",
    github_page = ""
  )
}



#* Plot confusion Matrix
#* @serializer png
#* @get /conf_mat
function() {
  preds <- predict(final_tree_model, diabetes_data)
  diabetes_data$pred <- preds$.pred_class
  cm <- yardstick::conf_mat(data = diabetes_data, truth = Diabetes_binary, estimate = pred)
  autoplot(cm, type="heatmap") +
    labs(title = "Confusion Matrix of All Data Using Model") +
    theme(plot.title = element_text(hjust = .5))
}

