# Title: "PH125.9X - Capstone: MovieLens Project"
# Author: "Mohammed Al-Areqi"
# Date: "21-Nov-23"

## Overview & Preparation ##

# Install all needed packages for the project if not installed

if(!require(dplyr))
  install.packages("dplyr", repos = "http://cran.us.r-project.org") 
if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org") 
if(!require(kableExtra)) 
  install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(hablar))
  install.packages("hablar", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) 
  install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(scales))
  install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(stringr))
  install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) 
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr))
  install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")

# Load all needed libraries for the project.

library(dplyr)
library(tidyverse)
library(kableExtra)
library(hablar)
library(tidyr)
library(scales)
library(stringr)
library(ggplot2)
library(knitr)
library(caret)

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

# MovieLens 10M data set:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file),
                                   fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file),
                                  fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
# Edx traning set will contain 90% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating,
                                  times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx training set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## Data Structure ##

# A quick display of the data
head(edx) %>% 
  kable( digits = 4, format = "pipe")

# Summary and structure of the data
str(edx, vec.len = 2)

# Number of unique values for user, movidId, title, genre, and rating in edx set
edx %>% summarize(user_unique    = n_distinct(userId),
                  movieId_unique = n_distinct(movieId),
                  title_unique   = n_distinct(title),
                  genre_unique   = n_distinct(genres),
                  rating_unique  = n_distinct(rating)) %>% 
  kable( digits = 4, format = "pipe")

# Find the duplicate in title column when grouping both movieId and title in edx set 
# Both movieId and title supposed to give the same unique values
edx %>% group_by(movieId, title) %>%
  summarize(n = n(), rating_avg = mean(rating)) %>% 
  find_duplicates(title) %>% 
  kable( digits = 4, format = "pipe")

# movieId `64997` to `34048` has the same title "War of the Worlds (2005)".
# the code below will replace all all movieId 64997 with 34048.
edx %>% mutate(movieId = ifelse(movieId == 64997, 34048, movieId)) %>%
  summarize(user_unique    = n_distinct(userId),
            movieId_unique = n_distinct(movieId),
            title_unique   = n_distinct(title),
            genre_unique   = n_distinct(genres),
            rating_unique  = n_distinct(rating)) %>% 
  kable( digits = 4, format = "pipe")

## Analysis ##
### Rating Analysis ###

# Movie rating distribution from 0.5 to 5
edx %>% group_by(rating) %>%
  summarize( count = n()) %>%
  ggplot(aes(rating, count)) + 
  geom_col() + 
  scale_y_continuous(labels = comma, breaks =  seq(500000, 3000000, 500000))+
  scale_x_continuous(breaks=seq(0.5,5,0.5))+ 
  ggtitle("Counts per Rating")

### Movie Analysis ###

# Distribution for the count of rating per movie
edx %>% group_by(movieId) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  scale_y_continuous(breaks=seq(100,800,100)) +
  ggtitle("Count of rating per Movie") +
  xlab("Times Movie rated") +
  ylab("Movie count")

### User Analysis ###

# Distribution for the count of rating per user
edx %>% group_by(userId) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Count of rating per User") +
  xlab("Times User rated") +
  ylab("User count")

### Genre Analysis ###

# Note: Very slow.. 
# Count of movie ratings per genre
max_genres_per_user <-edx %>%  
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>% 
  arrange(desc(count)) %>%
  mutate(percent = percent(count/sum(count), accuracy = .01)) 
max_genres_per_user %>% kable( digits = 4, format = "pipe")

# Distribution for count of movie ratings per genre
max_genres_per_user %>%
  ggplot(aes(x = count, y = reorder(genres, -count)))+
  geom_col() + 
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ggtitle("Count of rating per genre") +
  xlab("Count") +
  ylab("Genres")

### Time Analysis ###

# Distribution for the average rating of the year when movie was released
edx %>% mutate(year_released = str_extract(title,"[(]\\d{4}[)]")) %>%
  mutate(year_released = as.numeric(str_replace_all(year_released, "[//(//)]", ""))) %>% 
  group_by(year_released) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(year_released, avg_rating)) +
  geom_point() +
  geom_smooth() + 
  ggtitle("Average rating VS Year Movie released")

## Data Separation ##

# split edx into a training set 90% of the data and temp set 10% of the data
set.seed(1) 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1,
                                  list = FALSE)
edx_train_set <- edx[-test_index,]
edx_temp <- edx[test_index,]

# Make sure movieId and userId in edx_test_set are also in edx_train_set
edx_test_set <- edx_temp %>% 
  semi_join(edx_train_set, by = "movieId") %>%
  semi_join(edx_train_set, by = "userId")

# Add rows removed from edx_test_set back into edx_train_set
removed <- anti_join(edx_temp, edx_test_set)
edx_train_set <- rbind(edx_train_set, removed)

# Remove unnecessary names
rm(test_index, edx_temp,removed)

## Data Modeling ##

#RMSE function to test the prediction with the test set
RMSE <- function(test_ratings , predict_ratings){
  sqrt(mean((test_ratings - predict_ratings)^2))
}

### Average Rating Model ###

# First model overall mean rating using the edx training set
mu <- mean(edx_train_set$rating, na.rm = TRUE)
mu

# Predict rating in edx test set with overall mean only and run the RMSE
first_rmse <- RMSE(edx_test_set$rating, mu)
results <- tibble(Method = "Just the average(mu only)" ,
                  RMSE = first_rmse,
                  Target = ifelse(first_rmse < 0.86490, "Hit", "Away"))
results %>% kable( digits = 4, format = "pipe")

### Movie Effect Model ###

#Second model movie effect b_i. Average rating per movie using the edx training set
movie_effect <-  edx_train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = mean(rating-mu))
movie_effect

# Predict rating in edx test set by adding the movie effect b_i and run the RMSE
y_hat_movie <- mu+edx_test_set %>% 
  left_join(movie_effect, by = "movieId") %>%
  pull(b_i)

second_rmse <- RMSE(y_hat_movie, edx_test_set$rating)

#Adding second model to the results
results <- bind_rows (results, tibble(Method = "movie effect" ,
                                      RMSE = second_rmse,
                                      Target = ifelse(second_rmse < 0.86490,
                                                      "Hit", "Away")))
results %>% kable( digits = 4, format = "pipe")

### User Effect Model ###

#Third model user effect b_u. Average rating per user using the edx training set
user_effect <- edx_train_set %>% 
  left_join(movie_effect, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating-mu-b_i))
user_effect

# Predict rating in edx test set by adding the movie b_i and user b_u effect and run the RMSE
y_hat_movie_user <- edx_test_set %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>% 
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
third_rmse <- RMSE( y_hat_movie_user, edx_test_set$rating)

#Adding third model to the results
results <- bind_rows (results, tibble(Method = "movie and user effect" ,
                                      RMSE = third_rmse,
                                      Target = ifelse(third_rmse < 0.86490,
                                                      "Hit", "Away")))
results %>% kable( digits = 4, format = "pipe")

### Time Effect Model ###

#Before starting the model we would need to extract the year the movie was
#released from the edx training set
edx_train_set <- edx_train_set %>%
  mutate(year_released = str_extract(title,"[(]\\d{4}[)]")) %>%
  mutate(year_released = as.numeric(str_replace_all(year_released, "[//(//)]", "")))

#Forth model time effect (y_i). Average rating per year when the movie was released
time_effect <- edx_train_set %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>%
  group_by(year_released) %>%
  summarize(y_i = mean(rating-mu-b_i-b_u))
time_effect

# add movie year_released column to the edx test set to match the edx training set
edx_test_set <- edx_test_set %>%
  mutate(year_released = str_extract(title,"[(]\\d{4}[)]")) %>%
  mutate(year_released = as.numeric(str_replace_all(year_released, "[//(//)]", "")))

# Predict rating in edx test set by adding the movie b_i, user b_u, and 
#year released y_i effect and run the RMSE
y_hat_movie_user_time <- edx_test_set %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>% 
  left_join(time_effect, by = "year_released") %>% 
  mutate(pred = mu + b_i + b_u + y_i) %>%
  .$pred
forth_rmse <- RMSE( y_hat_movie_user_time, edx_test_set$rating)

#Adding forth model to the results
results <- bind_rows (results,
                      tibble(Method = "movie, user, and time effect" ,
                             RMSE = forth_rmse,
                             Target = ifelse(forth_rmse < 0.86490,
                                             "Hit","Away")))
results %>% kable( digits = 4, format = "pipe")

### Genre Effect Model ###

# Fifth model genre effect (g). Average rating per distinct genre using training set
# Very slow but needed to separate the genres
genres_effect <- edx_train_set %>%
  left_join(movie_effect, by ="movieId") %>%
  left_join(user_effect, by = "userId") %>%
  left_join(time_effect, by = "year_released") %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>% 
  summarize(g = mean(rating- mu - b_i - b_u - y_i))
genres_effect

# Separating edx test set genres to match the edx training set.
# It will be slow but we can't run the model without modifying test set
edx_test_set_date_genres <- edx_test_set %>% 
  separate_rows(genres, sep = "\\|")

# Predict rating in edx test set by adding the movie b_i, user b_u, 
#year released y_i, and genre g effect and run the RMSE
y_hat_movie_user_time_genres <- edx_test_set_date_genres %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>% 
  left_join(time_effect, by = "year_released") %>%
  left_join(genres_effect, by = "genres") %>% 
  mutate(pred = mu + b_i + b_u + y_i + g) %>%
  .$pred
fifth_rmse <- RMSE( y_hat_movie_user_time_genres, edx_test_set_date_genres$rating)

#Adding fifth model to the results
results <- bind_rows (results,
                      tibble(Method = "movie, user, time effect, and genres" ,
                             RMSE = fifth_rmse,
                             Target = ifelse(fifth_rmse < 0.86490,
                                             "Hit", "Away")))
results %>% kable( digits = 4, format = "pipe")

## Results ##

# Add a movie release date column to Final holdout data to match the edx training set.
final_holdout_test_date <- final_holdout_test %>% 
  mutate(year_released = str_extract(title,"[(]\\d{4}[)]")) %>%
  mutate(year_released = as.numeric(str_replace_all(year_released, "[//(//)]", "")))

# Separate genres in Final holdout data to match the edx training set.
# Note: This will take some time.
final_holdout_test_date_genres <- final_holdout_test_date %>% 
  separate_rows(genres, sep = "\\|")

# Predict rating in Final holdout data set by adding the movie b_i, user b_u, 
#year released y_i, and genre g effect and run the RMSE
y_hat_movie_user_time_genres_final <- final_holdout_test_date_genres %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(user_effect, by = "userId") %>% 
  left_join(time_effect, by = "year_released") %>%
  left_join(genres_effect, by = "genres") %>% 
  mutate(pred = mu + b_i + b_u + y_i + g) %>%
  .$pred
final_rmse <- RMSE( y_hat_movie_user_time_genres_final,
                    final_holdout_test_date_genres$rating)

#Running final model to the results
results <- bind_rows (results,
                      tibble(Method = "final validation" ,
                             RMSE = final_rmse ,
                             Target = ifelse(final_rmse < 0.86490,
                                             "Hit", "Away")))
results %>% kable( digits = 6, format = "pipe")
