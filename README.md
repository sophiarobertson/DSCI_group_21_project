# dsci-100-project_template
Template project repository for DSCI-100

library(repr)
library(tidyverse)
library(tidymodels)
options(repr.matrix.max.rows = 6)
source('tests.R')
source("cleanup.R")
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
url <- "http://roycekimmons.com/system/generate_data.php?dataset=exams&n=1000"
exam_data <- read_csv(url)
colnames(exam_data) <- c("gender", "race_ethnicity", "parental_level_of_education", "lunch", "test_preparation_course", 
                         "math_score", "reading_score", "writing_score")
exam_data1<- exam_data |>
mutate(parental_level_of_education = as_factor(parental_level_of_education)) |>
mutate("mean_score" = (math_score + reading_score + writing_score)/3) |>
select(parental_level_of_education, math_score, reading_score, writing_score, mean_score)

exam_data1

exam_graph <- exam_data1 |>
ggplot(aes(x = math_score, y = reading_score, color = parental_level_of_education)) +
geom_point(alpha = 0.5) +
labs(color = "Parental level of education", y = "Reading Score", x = "Math Score")

exam_graph

#exam_graph <- exam_data1 |>
#ggplot(aes(x = mean_score, y = fct_reorder(parental_level_of_education, mean_score, .desc = TRUE), 
#           fill = parental_level_of_education)) +
#geom_bar(stat = "identity") + 
#labs(x = "Average Score", y = "Parental Level of Education",  fill = "type")

data_split <- initial_split(exam_data1, prop = 0.75, strata = parental_level_of_education)
data_train <- training(data_split)
data_test <- testing(data_split)

data_recipe <- recipe(parental_level_of_education ~ math_score + reading_score + writing_score, data = data_train) |>
step_scale(all_predictors()) |>
step_center(all_predictors())
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1234) 
options(repr.plot.height = 5, repr.plot.width = 6)

knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |>
set_engine("kknn") |>
set_mode("classification")

k_vals <- tibble(neighbors = seq(from = 1, to = 10))
vfold <- vfold_cv(data_train, v = 5, strata = parental_level_of_education)
recipe <- recipe(parental_level_of_education ~., data = data_train)

knn_results <- workflow() |>
add_recipe(recipe) |>
add_model(knn_spec)|>
tune_grid(resamples = vfold, grid = k_vals) |>
collect_metrics()

accuracies <- knn_results |>
filter(.metric == "accuracy")

accuracies

cross_val_plot <- ggplot(accuracies, aes(x = neighbors, y = mean)) +
geom_point() +
geom_line() +
labs(x = "Neighbors", y = "Accuracy Estimate")

cross_val_plot
