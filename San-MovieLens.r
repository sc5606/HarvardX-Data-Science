if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(devtools)) install.packages("devtools")
if(!require(sqldf)) install.packages("sqldf")
#if(!require(kableExtra) install.packages("kableExtra")
#devtools::install_github("haozhu233/kableExtra")
library(devtools)
devtools::install_github("collectivemedia/tictoc")
library(tictoc)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(knitr)
library(dplyr)
library(caret)
library(knitr)
#library(kableExtra)

# Show the environment used for this project
print("Envrionment Information:")
version
# Get the edx and validataion data set from either Google Drive or One Drive
tic("Loading edx data set...")
edx <- readRDS("data/edx.rds")
toc()

# Information about edx data set
class(edx)
glimpse(edx)

# Summary information about edx data set
summary(edx)

tic("Loading validation data set...")
validation <- readRDS("data/validation.rds")
toc()
# Loading edx data set...: 17.495 sec elapsed

# Loading validation data set...: 5.383 sec elapsed

# Information about validation data set
class(validation)
glimpse(validation)


####### Understanding Data set and Analysis #######


# Distinct Movies in the edx data set

# using sqldf
tic("Distinct Movies in edx data set -Using sqldf")
sqldf("select count(distinct(movieId)) from edx")
toc()
# Distinct Movies in edx data set -Using sqldf: 17.997 sec elapsed

# using R
tic("Distinct Movies in edx data set -Using R")
n_distinct(edx$movieId)
toc()
# Distinct Movies in edx data set -Using R: 0.465 sec elapsed

# Distinct Users in the edx data set

# using sqldf
sqldf("select count(distinct(userId)) from edx")

# using R
n_distinct(edx$userId)

# Distinct Users and Movies in the edx data set
edx %>% summarize(unique_users = n_distinct(userId), unique_movies = n_distinct(movieId))

# using sqldf to acheive the same. Note SQLDF is slow

sqldf("select count(distinct(userId)) unique_users, count(distinct(movieId)) unique_movies from edx")

# Distinct Genre's in the data set
n_distinct(edx$genres)

# Number of Ratings per Genre. **** Very Very Time Consuming

tic("Number of Ratings per Genre...") 
edx %>% separate_rows(genres, sep = "\\|") %>%
     group_by(genres) %>%
     summarize(count = n()) %>%
     arrange(desc(count))
toc()


# Getting eds data into edx_by_genre by splitting genre into separate row
# this is far better than above in execution

tic("Number of Ratings per Genre. Method 2...") 

tic("Step#1 - Creating edx_by_genre data set per genre")
edx_by_genre <- edx %>% separate_rows(genres, sep = "\\|")
toc()

tic("Step#2 - Distinct Genres from edx_by_genre set")
data.frame(table(edx_by_genre$genres))
toc()

toc()

tic("Distinct Genre's")
n_distinct(edx_by_genre$genres)
toc()

# Number of Unique Movies per Genre

tic("Number of Unique Movies per Genre")
edx_by_genre %>% group_by(genres) %>%
    summarize(count = n_distinct(movieId)) %>%
    arrange(desc(count))
toc()

# SQL Version:
tic("SQL Version:")
sqldf("select genres, count(distinct movieId) tot from edx_by_genre group by genres order by tot desc")
toc()

# sqldf("select sum(a.tot) from (select count(distinct movieId) tot, genres from edx_by_genre group by genres) a ")

# Which movie has the greatest number of ratings?
tic("Which movie has the greatest number of ratings?")
edx %>% group_by(movieId, title) %>%
     summarize(count = n()) %>%
     arrange(desc(count))
toc()
# Which movie has the greatest number of ratings?: 1.388 sec elapsed

tic("SQL: Which movie has the greatest number of ratings?")
sqldf("select count(1), title from edx group by title order by 1 desc limit 10")
toc()
# SQL: Which movie has the greatest number of ratings?: 18.27 sec elapsed


## Distribution of Ratings...

edx %>% 
	group_by(rating) %>% 
	summarize(count = n()) %>% 
	select(Rating = rating, Number_of_Movies = count) %>% 
	arrange(desc(Rating))

# Plot the Distribution of Ratings

edx %>%
	ggplot(aes(rating)) + 
	geom_histogram(binwidth = .25, color = "black") +
	scale_x_discrete(limits = c(seq(.5, 5, .5))) +
	scale_y_continuous(breaks = c(seq(0, 2500000, 500000))) +
	ggtitle("Distribution of Ratings...") +
	theme(plot.title = element_text(hjust = 0.5)) +
	labs(x = "Rating") +
	labs(y = "# of Ratings") + 
	labs(caption = "(based on data from edx...)")


# Plot the Distribution of Ratings with the cut and fill
edx %>% 
	ggplot(aes(rating, fill = cut(rating, 100))) + 
	geom_histogram(binwidth = .20, color = "black") +
	scale_x_discrete(limits = c(seq(.5, 5, .5))) +
	scale_y_continuous(breaks = c(seq(0, 2500000, 500000))) +
	geom_vline(xintercept = mean(edx$rating), col = "red", linetype = "dashed") +
	ggtitle("Distribution of Ratings...") +
	theme(plot.title = element_text(hjust = 0.5)) +
	labs(x = "Rating") +
	labs(y = "# of Ratings") + 
	labs(caption = "(based on data from edx...)")


# Number of ratings per movie...

edx %>%
	count(movieId) %>%
	ggplot(aes(n)) +
	geom_histogram(bins = 30, color = "black") + 
	scale_x_log10() + 
	xlab("Number of Ratings") +
	ylab("Number of Movies") +
	ggtitle("Number of Ratings per Movie...") +
	theme(plot.title = element_text(hjust = 0.5)) +
	labs(caption = "(scale_x_log10...)")

# as you can see there are quite a few movies rated very few times. 
# Lets see the movies which has less than 10 ratings

edx %>% 
	group_by(title) %>% 
	summarize(count = n()) %>% 
	filter(count <= 10) %>%
	left_join(edx, by = "title") %>%
	select(Movie = title, Rating = rating, Number_Of_Ratings = count) %>%
	arrange(Number_Of_Ratings, desc(Rating)) %>%
	slice(1:50) %>%
	knitr::kable() 

# Number of Ratings given by Users

edx %>%
	group_by(userId) %>%
	summarize(number_of_ratings = n()) %>%
	ggplot(aes(number_of_ratings)) +
	geom_histogram(bins = 10, color = "black") + 
	scale_x_log10() +
	xlab("Number of Ratings") +
	ylab("Number of Users") +
	ggtitle("Number of Ratings given by Users") +
	theme(plot.title = element_text(hjust = 0.5)) +
	labs(caption = "(scale_x_log10...)")

# Mean Movie Ratings given by Users

edx %>%
	group_by(userId) %>%
  	filter(n() >= 100) %>%
  	summarize(b_u = mean(rating)) %>%
  	ggplot(aes(b_u)) +
  	geom_histogram(bins = 30, color = "black") +
  	xlab("Mean Rating") +
  	ylab("Number of Users") +
  	ggtitle("Mean Movie Ratings given by Users") +
  	scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
	theme(plot.title = element_text(hjust = 0.5)) +
	labs(caption = "(based on data from edx...)")


### Modelling ###

## Model#1: Average Movie Rating

# Creating RMSE Function
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Mean Rating of edx data set
tic("Mean Rating of edx data set")
mu <- mean(edx$rating)
# [1] 3.512465
toc()

# Naive RMSE of validataion data set

tic("Naive RMSE of validataion data set")
naive_rmse <- RMSE(validation$rating, mu)
toc()
naive_rmse
# [1] 1.061202

# Persist prediction results 

tic("Persist prediction results ")
rmse_results <- data_frame(method = "Model#1: Average Movie Rating", RMSE = naive_rmse)
rmse_results %>% knitr::kable()
#|method                      |     RMSE|
#|:---------------------------|--------:|
#|Model: Average Movie Rating | 1.061202|
toc()

# Model#2: Movie Effect

# Substract mu from movie rating -getting b_i
tic("Substract mu from movie rating -getting b_i")
movie_avgs <- edx %>%
	group_by(movieId) %>%
	summarize(b_i = mean(rating - mu))
toc()

# Generate a plot with computed b_i
tic("Generate a plot with computed b_i")
movie_avgs %>% 
	qplot(b_i, geom = "histogram", bins = 10, data = ., color = I("black"), ylab = "Number of Movies", main = "Number of Movies with computed b_i") + 
	theme(plot.title = element_text(hjust = 0.5))	
toc()

# Validate with validation data set
tic("Validate with validation data set")
predicted_ratings <- mu + validation %>%
	left_join(movie_avgs, by = 'movieId') %>%
	pull(b_i)

toc()

# Persist prediction results for Model#1 - Movie Effect Model

tic("Persist prediction results for Model#1 - Movie Effect Model")

movie_effect_rmse <- RMSE(predicted_ratings, validation$rating)

# Appending the results 
rmse_results <- 
	bind_rows(rmse_results, 
		  data_frame(method = "Model#2: Movie Effect", 
                               RMSE = movie_effect_rmse)
		 )

rmse_results %>% knitr::kable()
#|method                      |      RMSE|
#|:---------------------------|---------:|
#|Model: Average Movie Rating | 1.0612018|
#|Model: Movie Effect         | 0.9439087|

toc()

## Model#3: Movie and User Effect
tic("Model#3: Movie and User Effect")

# Users those have rated more than 100 movies
user_avgs <- edx %>%
	left_join(movie_avgs, by = 'movieId') %>%
	group_by(userId) %>%
	filter(n() >= 100) %>%
	summarize(b_u = mean(rating - mu - b_i))

# Plot the results

user_avgs %>%
	qplot(b_u, geom = "histogram", bins = 30, data = ., color = I("black"), ylab = "Number of Movies", main = "Users that have rated >= 100 Movies") + 
	theme(plot.title = element_text(hjust = 0.5))	

user_avgs <- edx %>%
	left_join(movie_avgs, by = 'movieId') %>%
	group_by(userId) %>%
	summarize(b_u = mean(rating - mu - b_i))

# Validate with validation data set
tic("Validate with validation data set")
predicted_ratings <- validation %>%
	left_join(movie_avgs, by = 'movieId') %>%
	left_join(user_avgs, by = 'userId') %>%
	mutate(pred = mu + b_i + b_u) %>%
	pull(pred)

toc()

# Persist prediction results for Model#3 - Movie and User Effect Model

tic("Persist prediction results for Model#1 - Movie and User Effect Model")
movie_user_effect_rmse <- RMSE(predicted_ratings, validation$rating)

# Appending the results 
rmse_results <- 
	bind_rows(rmse_results, 
		  data_frame(method = "Model#3: Movie and User Effect", 
                               RMSE = movie_user_effect_rmse)
		 )

rmse_results %>% knitr::kable()

#|method                       |      RMSE|
#|:----------------------------|---------:|
#|Model: Average Movie Rating  | 1.0612018|
#|Model: Movie Effect          | 0.9439087|
#|Model: Movie and User Effect | 0.8653488|

toc()


## Model: Regularization: Movie and User Effect
tic("Model: Regularization: Movie and User Effect")

# Using lambda tuning parameters
lambdas <- seq(0, 10, 0.25)

# Iterate for each lambda paramter and find b_i, b_u, predictions and validations

rmses <- sapply(lambdas, function(i){
	# Calculate the mean of ratings from the edx training set
	mu <- mean(edx$rating)

	# Adjust mean by movie effect and penalize low number on ratings
	# tic("Finding b_i")
	b_i <- edx %>%
		group_by(movieId) %>%
		summarize(b_i = sum(rating - mu)/(n() + i))
	# toc()

	# ajdust mean by user and movie effect and penalize low number of ratings
	# tic("Finding b_u")
	b_u <- edx %>%
		left_join(b_i, by = "movieId") %>%
		group_by(userId) %>%
		summarize(b_u = sum(rating - b_i - mu)/(n() + i))
	# toc()

	# Finding Predicted_ratings
	# tic("Finding Predicted_ratings")
	predicted_ratings <- validation %>%
		left_join(b_i, by = "movieId") %>%
		left_join(b_u, by = "userId") %>%
		mutate(prediction = mu + b_i + b_u) %>%
		pull(prediction)
	# toc()

	# Return RMSE
	# tic("Return RMSE")
	return(RMSE(predicted_ratings, validation$rating))
	# toc()
})

toc()

# Print rmses values
# rmses

# Plot the results

qplot(lambdas, rmses)

# Which is the Optimal lambda 
optimal_lambda <- lambdas[which.min(rmses)]

# Print Optimal Lambda
optimal_lambda
# [1] 5.25

# Print the minimum RMSE value
min(rmses)
# [1] 0.864817

# Appending the results 
rmse_results <- bind_rows(rmse_results,
	data_frame(method = "Model: Regularization: Movie and User Effect",
	RMSE = min(rmses)))

# Print the RMSE's obtained from all the Models
rmse_results %>% knitr::kable()
#|method                                       |      RMSE|
#|:--------------------------------------------|---------:|
#|Model: Average Movie Rating                  | 1.0612018|
#|Model: Movie Effect                          | 0.9439087|
#|Model: Movie and User Effect                 | 0.8653488|
#|Model: Regularization: Movie and User Effect | 0.8648170|

# Lowest RMSE is our Reqularization Model.

min(rmse_results$RMSE)
#[1] 0.864817
