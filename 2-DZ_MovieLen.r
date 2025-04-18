library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

# set working dir
setwd("C:\\Users\\user\\Desktop\\Harvard_R\\CapStone\\MovieLensRecommendation")

#####################################################################
# Note: the following code is provided by edx course and comment off during my testing
# Create edx set, validation set
# use saveRDS to save files for later process
#####################################################################
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#dl <- tempfile()
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                      col.names = c("userId", "movieId", "rating", "timestamp"))
# Save ratings object at my laptop file size around 46MB
#saveRDS(ratings, "ratings.rda")

#movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
#colnames(movies) <- c("movieId", "title", "genres")
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))
#saveRDS(movies, "movies.rda")

#movielens <- left_join(ratings, movies, by = "movieId")
#saveRDS(movielens, "movielens.rda")

# Validation set will be 10% of MovieLens data
#set.seed(1)
#movielens_model<-readRDS("movielens.rda")
#test_index <- createDataPartition(y = movielens_model$rating, times = 1, p = 0.1, list = FALSE)
#edx <- movielens_model[-test_index,]
#temp <- movielens_model[test_index,]

# Make sure userId and movieId in validation set are also in edx set
#validation <- temp %>% 
#  semi_join(edx, by = "movieId") %>%
#  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
#removed <- anti_join(temp, validation)
#edx <- rbind(edx, removed)

#saveRDS(edx, "edx.rda")
#saveRDS(validation, "validation.rda")

#rm(dl, ratings, movies, test_index, temp, movielens, removed)
#gc()

#####################################################################
# MovieLens Reccomender System Project
#  use readRDS to open previously saved files 
#####################################################################

edx_dat <- readRDS("edx.rda")
validation_dat <-readRDS("validation.rda")

# --- Descriptive Analysis ----
# data type glimpse
glimpse(edx_dat)
head(edx_dat)

# Distinct number of user and movieId
edx_dat %>%
   summarize(n_users = n_distinct(userId),
             n_movies = n_distinct(movieId))

# Avg rating and SD
round(mean(edx_dat$rating), digits = 4)
round(sd(edx_dat$rating), digits = 4)

# Some movies have more ratings than others
edx_dat %>% 
   dplyr::count(movieId) %>% 
   ggplot(aes(n)) + 
   geom_histogram(bins = 30, color = "black") + 
   scale_x_log10() + 
   ggtitle("Movies")

# Some users rate more frequently than others
edx_dat %>%
   dplyr::count(userId) %>% 
   ggplot(aes(n)) + 
   geom_histogram(bins = 30, color = "black") + 
   scale_x_log10() +
   ggtitle("Users")

#  Top 5 most rated movies
top5 <- edx_dat %>%
   dplyr::count(movieId) %>%
   top_n(5) %>%
   pull(movieId)
top5
                
# Top 5 frequent users
top5_u <- edx_dat %>%
   dplyr::count(userId) %>%
   top_n(5) %>%
   pull(userId)
top5_u

# Top 5 mostly rated Movies rating given by top 5 frequent users
tab <- edx_dat %>% 
   filter(movieId %in% top5) %>% 
   filter(userId %in% top5_u) %>%
   select(userId, title, rating) %>% 
   spread(title, rating) 
print(tab)

# Top 20 most rated movies and their Titles
t<-edx_dat %>%
   group_by(title) %>%
   summarise(count = n()) %>%
   arrange(desc(count)) %>%
   head(n=20) 
print.data.frame(t)


# ratings distribution histogram
edx_dat %>%
   ggplot(aes(rating)) +
   geom_histogram(binwidth = 0.1)+
   labs(title="Histogram of Rating")


# mean rating distribution per title
edx_dat %>%
   group_by(title) %>%
   summarise(mean = mean(rating)) %>%
   ggplot(aes(mean)) +
   theme_classic()  +
   geom_histogram(bins=12) +
   labs(title = "Mean Distribution per Title",
        x = "Mean",
        y = "Frequency")

# View of all unique genres
unique_genres_list <- str_extract_all(unique(edx_dat$genres), "[^|]+") %>%
   unlist() %>%unique()
unique_genres_list


# --- Analysis part ---
options(digits = 4)
# The RMSE function that will be used in this project is:
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Split into train 0.75 and test set 0.25 of dataset
set.seed(1) 
test_index <- createDataPartition(edx_dat$rating, times = 1, p = 0.25, list = FALSE)
test <- edx_dat[test_index,]
train <- edx_dat [-test_index,]

# --- 1. Native Mean Avg Model ---
# Calculate the average of all movies
mu_hat <- mean(edx_dat$rating)
mu_hat  #3.512

# Predict the RMSE on the validation set
rmse_mean <- RMSE(validation_dat$rating, mu_hat) #1.06065

# Creating a results dataframe
results <- data.frame(model="Naive Mean Avg Model", RMSE=rmse_mean)
print.data.frame(results)

# --- 2. Movie Avg Model ---
# Calculate the average by movie
movie_avgs <- train %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))

# Compute the predicted ratings on validation dataset
rmse_movie_model <- test  %>%
   left_join(movie_avgs, by='movieId') %>%
   mutate(pred = mu_hat + b_i) %>%
   pull(pred)

rmse_movie_model_result <- RMSE(test$rating, rmse_movie_model)

# Adding row to the results
results <- results %>% add_row(model="Movie-Based Model", RMSE=rmse_movie_model_result)
print.data.frame(results)


# --- 3. Movie User Avg Model ---
# Calculate the average by user
user_avgs <- train %>%
   left_join(movie_avgs, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu_hat - b_i))

# Compute the predicted ratings on validation dataset
rmse_movie_user_model <- testt %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   mutate(pred = mu_hat + b_i + b_u) %>%
   pull(pred)

rmse_movie_user_model_result <- RMSE(test$rating, rmse_movie_user_model)

# Adding row to the results
results <- results %>% add_row(model="Movie+User Based Model", RMSE=rmse_movie_user_model_result)
print.data.frame(results)


# --- 4 Movie User Genre Avg Model ---
# calculate genre bias
genre_avgs <- train %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   group_by(genres) %>%
   summarize(b_u_g = mean(rating - mu_hat - b_i - b_u))


# Compute the predicted ratings on validation dataset
rmse_movie_user_genre_model <- test %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   left_join(genre_avgs, by='genres') %>%
   mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
   pull(pred)

rmse_movie_user_genre_model_result <- RMSE(test$rating,rmse_movie_user_genre_model)

# Adding row to the results
results <- results %>% add_row(model="Movie+User+Genre Based Model", RMSE=rmse_movie_user_genre_model_result)
print.data.frame(results)


# --- 5 Regularized Movie Model  ---
set.seed(1)
lambdas <- seq(0, 10, 0.1)

# Compute the predicted ratings on validation dataset using different lambda
rmses <- sapply(lambdas, function(lambda) {
   
  # Calculate the average by user
   b_i <- train %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
   
   # Compute the predicted ratings on validation dataset
   predicted_ratings <- test %>%
      left_join(b_i, by='movieId') %>%
      mutate(pred = mu_hat + b_i) %>%
      pull(pred)
   
   # Predict the RMSE on the validation set
      return(RMSE(test$rating, predicted_ratings))
})

# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

# RMSE
rmse_regularized_movie_model <- min(rmses)

# Adding row to the results
results <- results %>% add_row(model="Regularized Movie-Based Model", RMSE=rmse_regularized_movie_model)
print.data.frame(results)

# Plot RMSE vs Lambda
data.frame(lam = lambdas, rmse=rmses)%>% 
   ggplot(aes(x=lambdas, y=rmses)) +
     geom_line()
     labs(title = "RMSEs vs Lambdas - Regularized Movie Model")

     
# --- 6 Regularized Movie + User Model ---
set.seed(1)

rmses <- sapply(lambdas, function(lambda) {

   # Calculate the average by movie
   b_i <- edx_dat %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
   
   # Calculate the average by user
   b_u <- edx_dat %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
   
   # Compute the predicted ratings on validation dataset
   predicted_ratings <- validation_dat %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      mutate(pred = mu_hat + b_i + b_u) %>%
      pull(pred)
   
   # Predict the RMSE on the validation set
   return(RMSE(validation_dat$rating, predicted_ratings))
})

# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

# Predict the RMSE on the validation set
rmse_regularized_movie_user_model <- min(rmses)

# Adding the results to the results dataset
results <- results %>% add_row(model="Regularized Movie+User Based Model", RMSE=rmse_regularized_movie_user_model)
print.data.frame(results)


# # --- 7 Regularized Movie User Genres Model ---
# set.seed(1)
# lambdas <- seq(0, 10, 0.1)
# 
# # Compute the predicted ratings on validation dataset using different values of lambda
# rmses <- sapply(lambdas, function(lambda) {
# 
#    # Calculate the average by movie
#    b_i <- edx_dat %>%
#       group_by(movieId) %>%
#       summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
#    
#    # Calculate the average by user
#    b_u <- edx_dat %>%
#       left_join(b_i, by='movieId') %>%
#       group_by(userId) %>%
#       summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
#    # Calculate genre bias
#     b_g <- edx_dat %>%
#       left_join(b_i, by='movieId') %>%
#       left_join(b_u, by='userId') %>%
#       group_by(genres) %>%
#       summarize(b_g = sum(rating - b_i - mu_hat - b_u) / (n() + lambda))
#    
#    # Compute the predicted ratings on validation dataset
#    predicted_ratings <- validation_dat s%>%
#       left_join(b_i, by='movieId') %>%
#       left_join(b_u, by='userId') %>%
#       left_join(b_g, by='genres') %>%
#       mutate(pred = mu_hat + b_i + b_u + b_g) %>%
#       pull(pred)
#    
#    # Predict the RMSE on the validation set
#    return(RMSE(validation_dat$rating, predicted_ratings))
# })
# 
# # Get the lambda value that minimize the RMSE
# min_lambda <- lambdas[which.min(rmses)]
# 
# # Predict the RMSE on the validation set
# rmse_regularized_movie_user_genre_model <- min(rmses)
# 
# # Adding the results to the results dataset
# results <- results %>% add_row(model="Regularized Movie+User+Genre Based Model", RMSE=rmse_regularized_movie_user_genre_model)
# print.data.frame(results)
