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

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
