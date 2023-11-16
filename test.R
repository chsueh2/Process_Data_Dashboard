source(here("00 preloads.R"))


data("iris")

df <- iris %>% 
  mutate(Species = factor(Species)) %>% 
  relocate(Species)

# split train/test sets
set.seed(2022)

trainIndex <- createDataPartition(df$Species, p = 0.7, list = FALSE)
df_train <- df[trainIndex, ]
df_test <- df[-trainIndex, ]


# initiate a list to save the results
fittings <- list()


# model 1 -----------------------------------------------------------------

# fit_model 1
# configure the model to use
name <- "Multinomial Logistic Regression"
method <-  "multinom"
tuneGrid <- expand.grid(decay  = 1:15)

# fit the model
fitting <- fit_model(
  Species ~ ., df_train, df_test, method = method, name = name,
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
  tuneGrid = tuneGrid,
  trace = FALSE
)

# save the fittings
fittings <- append(fittings, list(fitting))

# plot hyperparameter
plot_modelinfo(fitting$fit)

plot_modelinfo(fittings[[1]]$fit)
fittings[[1]]$result

summary(fittings[[1]]$fit)


# model 2 -----------------------------------------------------------------

# fit_model 2
# configure the model to use
name <- "KNN Euclidean"
method <-  "knn"
tuneGrid <- expand.grid(k = 1:15)

# fit the model
fitting <- fit_model(
  Species ~ ., df_train, df_test, method = method, name = name,
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
  tuneGrid = tuneGrid
)

# save the fittings
fittings <- append(fittings, list(fitting))

# plot hyperparameter
plot_modelinfo(fitting$fit)

plot_modelinfo(fittings[[2]]$fit)
fittings[[2]]$result




# model 3 -----------------------------------------------------------------

# fit_model 3
# configure the model to use
name <- "Random Forests"
method <-  "rf"
tuneGrid <- expand.grid(mtry = 1:15)

# fit the model
fitting <- fit_model(
  Species ~ ., df_train, df_test, method = method, name = name,
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3),
  tuneGrid = tuneGrid
)

# save the fittings
fittings <- append(fittings, list(fitting))

# plot hyperparameter
plot_modelinfo(fitting$fit)

plot_modelinfo(fittings[[3]]$fit)
fittings[[3]]$result




# summary and prediction --------------------------------------------------

df <- tibble()
for(i in 1:3){
  df <- bind_rows(df, fittings[[i]]$result)
}

predict(
  fittings[[3]]$fit,
  newdata = data.frame(
    Sepal.Length = 1,
    Sepal.Width = 1,
    Petal.Length = 1,
    Petal.Width = 1))





