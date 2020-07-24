# Note: this process could take a few minutes

# Install packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

install.packages(c("rmarkdown","knitr"))
library(rmarkdown)
library(knitr)

# Download data
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Create edx and validation sets
# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# RMSE function

RMSE <- function(true_ratings, predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}

# Table for edx set model
# Movie and user histogram

Movies <- edx$movieId 
hist(Movies)

Users <- edx$userId
hist(Users)

# Rating average

mu_hat <- mean(edx$rating)
mu_hat

# Simple model RMSE

naive_rmse <- RMSE(edx$rating, mu_hat)
naive_rmse

# Table creation

rmse_results_edx <- data_frame(method = "Edx Dataset", RMSE = naive_rmse)

# Movie Effect Model

mu <- mean(edx$rating)
movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + edx %>% left_join(movie_avgs, by = "movieId") %>% .$b_i
model_1_rmse <- RMSE (predicted_ratings, edx$rating)
rmse_results_edx <- bind_rows(rmse_results_edx, data_frame(method = "Movie Effect Model", RMSE = model_1_rmse))
rmse_results_edx %>% knitr::kable() 

# Movie + User Effects Model

user_avgs <- edx %>% left_join(movie_avgs, by = "movieId") %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i)) 
predicted_ratings <- edx %>% left_join(movie_avgs, by = "movieId") %>%  left_join(user_avgs, by = "userId") %>% mutate(pred = mu + b_u + b_i) %>% .$pred
model_2_rmse <- RMSE(predicted_ratings, edx$rating)
rmse_results_edx <- bind_rows(rmse_results_edx, data_frame(method = "Movie + User Effects Model", RMSE = model_2_rmse))
rmse_results_edx %>% knitr::kable() 

# Table for validation set model
# Rating average

mu_hat <- mean(validation$rating)
mu_hat

# Simple model RMSE

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse

# Table creation

rmse_results_validation <- data_frame(method = "Validation Dataset", RMSE = naive_rmse)

# Movie Effect Model

mu <- mean(validation$rating)
movie_avgs <- validation %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + validation %>% left_join(movie_avgs, by = "movieId") %>% .$b_i
model_1_rmse <- RMSE (predicted_ratings, validation$rating)
rmse_results_validation <- bind_rows(rmse_results_validation, data_frame(method = "Movie Effect Model", RMSE = model_1_rmse))
rmse_results_validation %>% knitr::kable() 

# Movie + User Effects Model

user_avgs <- validation %>% left_join(movie_avgs, by = "movieId") %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i)) 
predicted_ratings <- validation %>% left_join(movie_avgs, by = "movieId") %>%  left_join(user_avgs, by = "userId") %>% mutate(pred = mu + b_u + b_i) %>% .$pred
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results_validation <- bind_rows(rmse_results_validation, data_frame(method = "Movie + User Effects Model", RMSE = model_2_rmse))
rmse_results_validation %>% knitr::kable() 
