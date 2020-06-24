## Used Libraries
library("recommenderlab")
library("ggplot2")
## The dataset used was from MovieLens, and is publicly available at 
## http://grouplens.org/datasets/movielens/
##The dataset is embedded into the RStudio IDE
data(MovieLense)
class(MovieLense)
##The dime returns the dimension of the matrix, number of columns and rows – users and ratings
dim(MovieLense)
slotNames(MovieLense)
class(MovieLense@data)
dim(MovieLense@data)
##Ratings are presented as integers and should be changed to factors.
vector_ratings <- as.vector(MovieLense@data)
##Visualise number of ratings rated by users from 0 to 5
table_ratings <- table(vector_ratings)
table_ratings
vector_ratings
##Remove rows that contain value 0 as they are useless for the project.
vector_ratings <- vector_ratings[vector_ratings != 0]
##Ratings from 1 to 5 will be saved saved to factors.
vector_ratings <- factor(vector_ratings)
##Visualize the distribution of the ratings.
qplot(vector_ratings) + ggtitle("Distribution of the ratings")
##count views for each film
views_per_film <- colCounts(MovieLense)
##Create dataframe of views
table_views <- data.frame(film = names(views_per_film), 
views = views_per_film) 
##Extract the first 6 top film
ggplot(table_views[1:6, ], aes(x = film, y = views)) +
    geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle("Number of views of the top films")
## Sort by number of views
table_views <- table_views[order(table_views$views, decreasing = TRUE), ] 
## Identify the top-rated films by computing the average rating of each of them
average_ratings <- colMeans(MovieLense)
qplot(average_ratings) + stat_bin(binwidth = 0.1) +
    ggtitle("Distribution of the average film rating")
average_ratings_relevant <- average_ratings[views_per_film > 100] ## films with over 100 ratings
qplot(average_ratings_relevant) + stat_bin(binwidth = 0.1) +
    ggtitle(paste("Distribution of the relevant average ratings"))
image(MovieLense, main = "Heatmap of the rating matrix") ## hard to read-too many dimensions
image(MovieLense[1:10, 1:15], main = "Heatmap of the first 10 rows and 15 columns")
## Determine the minimum number of films per user (top 1%).
min_n_films <- quantile(rowCounts(MovieLense), 0.99)
## Determine the minimum number of users per film (top 1%).
min_n_users <- quantile(colCounts(MovieLense), 0.99)
min_n_films
min_n_users
## Select the users and films matching these criteria.
image(MovieLense[rowCounts(MovieLense) > min_n_films,
                 +                  colCounts(MovieLense) > min_n_users], main = "Heatmap of the top users and films")

## Data Preparation
## Select the most relevant data by defining the minimum number of users per rated film as 50 and the minimum views number per film as 100:
ratings_films <- MovieLense[rowCounts(MovieLense) > 50,
                            +                              colCounts(MovieLense) > 100] 
ratings_films

## Visualize the top matrix
## Visualize the top 2 percent of users and films in the new matrix of the most relevant data:
min_films <- quantile(rowCounts(ratings_films), 0.98)
min_users <- quantile(colCounts(ratings_films), 0.98)
min_films <- quantile(rowCounts(ratings_films), 0.98)
min_users <- quantile(colCounts(ratings_films), 0.98)
image(ratings_films[rowCounts(ratings_films) > min_films,
                    +                      colCounts(ratings_films) > min_users], main = "Heatmap of the top users and films")

## Normalizing data
average_ratings_per_user <- rowMeans(ratings_films)
qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1)  +
    ggtitle("Distribution of the average rating per user")
##Normalize the data by setting  the average rating of each user to 0. As a quick check, I calculate the average rating by users, and it is equal to 0, as expected:
ratings_films_norm <- normalize(ratings_films)
sum(rowMeans(ratings_films_norm) > 0.00001)
# Visualize the normalized matrix
image(ratings_films_norm[rowCounts(ratings_films_norm) > min_films,colCounts(ratings_films_norm) > min_users], main = "Heatmap of the top users and films")

## Binarizing data – Defining two matrices depending on the context (we will only present in this project the first approach, the item-based filtering.
##Define a matrix equal to 1 if the user rated the movie, and 0 otherwise. In this case, the information about the rating is lost.
## 1st option: define a matrix equal to 1 if the movie has been watched
ratings_films_watched <- binarize(ratings_films, minRating = 1)
##Visualize a 5% portion of each of binarized matrices.
min_films_binary <- quantile(rowCounts(ratings_films), 0.95)
min_users_binary <- quantile(colCounts(ratings_films), 0.95)
image(ratings_films_watched[rowCounts(ratings_films) > min_films_binary,colCounts(ratings_films) > min_users_binary], main = "Heatmap of the top users and films")
##Define a matrix having 1 if the rating is above or equal to a definite threshold (3), and 0 otherwise. In this case, giving a bad rating to a movie is equivalent to not having rated it.
ratings_films_good <- binarize(ratings_films, minRating = 3)
image(ratings_films_good[rowCounts(ratings_films) > min_films_binary, colCounts(ratings_films) > min_users_binary], main = "Heatmap of the top users and films")
 
## Defining training/test sets respectively 80% and 20% each
which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_films), replace = TRUE, prob = c(0.8, 0.2))
head(which_train)
recc_data_train <- ratings_films[which_train, ]
recc_data_test <- ratings_films[!which_train, ]

which_set <- sample(x = 1:5, size = nrow(ratings_films), replace = TRUE) ##taking into consideration ##the first five users of the dataset
for(i_model in 1:5) {
    which_train <- which_set == i_model
    recc_data_train <- ratings_films[which_train, ]
    recc_data_test <- ratings_films[!which_train, ]

}
##Exploring Similarity Data
# Determine the similarity among the first 5 users
# Create similarity matrix
similarity_users <- similarity(MovieLense[1:5, ], method = "cosine", 
which = "users")
class(similarity_users)
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")
# Determine the similarity among the first 5 items
similarity_items <- similarity(MovieLense[, 1:5], method = "cosine", which = "items")
as.matrix(similarity_items)



##Building the recommender model 
##with default parameters of IBCF
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters
##k is the number of items to compute the similarities among them in the first step. Then, for each ##item, the algorithm identifies its k most similar items and stores the number. Method is a Cosine ##similarity function.
recc_model <- Recommender(data = recc_data_train, method = "IBCF",parameter = list(k = 30))
recc_model
class(recc_model)
model_details <- getModel(recc_model)
model_details$description
model_details$k
## this contains a similarity matrix
class(model_details$sim)
dim(model_details$sim)
n_items_top <- 20
image(model_details$sim[1:n_items_top, 1:n_items_top],main = "Heatmap of the first rows and columns")
##each row contains only k (30) elements that are greater than 0.
model_details$k
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)
col_sums <- colSums(model_details$sim > 0)
qplot(col_sums) + stat_bin(binwidth = 1) + 
ggtitle("Distribution of the column count")
##See the films with the most similar elements:
which_max <- order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]
## Setting the number of items to recommend to each user
n_recommended <- 6 
##Applying recommender system on the dataset:
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
recc_predicted
class(recc_predicted)
slotNames(recc_predicted)
recc_predicted@items[[1]]
##Exploring the results of the recommendations for the first user:
recc_user_1 <- recc_predicted@items[[1]]
films_user_1 <- recc_predicted@itemLabels[recc_user_1]
films_user_1
##Define a matrix with the recommendations for each user. visualize the recommendations for the ##first four users:
recc_matrix <- sapply(recc_predicted@items, function(x){ colnames(ratings_films)[x] })
dim(recc_matrix)
recc_matrix[, 1:4] ##visualize the recommendations for the first four users:
number_of_items <- factor(table(recc_matrix))
chart_title <- "Distribution of the number of items for IBCF"
qplot(number_of_items) + 
    ggtitle(chart_title)
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(names(number_of_items_top),number_of_items_top)
table_top

##Evaluating the Recommender Systems 
##Splitting the data, for training and test
ratings_films <- MovieLense[rowCounts(MovieLense) > 50, colCounts(MovieLense)>100]
ratings_films
percentage_training <- 0.8
min(rowCounts(ratings_films))
##number of items to generate recommendations
items_to_keep <- 15 
##threshold with the minimum rating that is considered good
rating_threshold <- 3
##number of times to run evaluation
n_eval <- 1
eval_sets <- evaluationScheme(data = ratings_films, 
method = "split",
train = percentage_training, 
given = items_to_keep, 
goodRating = rating_threshold, 
k = n_eval) 
eval_sets
##training set
getData(eval_sets, "train")
##set with the items used to test the recommendations
getData(eval_sets, "unknown")
##Visualize the unknown items by the users, which varies a lot.
qplot(rowCounts(getData(eval_sets, "unknown"))) + geom_histogram(binwidth = 10) + 
    ggtitle("unknown items by the users")
##Using cross-validation to validate models - Using 4-fold approach, we get four sets of the same size 448
n_fold <- 4
##Evaluating the ratings
model_to_evaluate <- "IBCF"
model_parameters <- NULL
eval_recommender <- Recommender(data = getData(eval_sets, "train"),method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 10
eval_prediction <- predict(object = eval_recommender, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")
class(eval_prediction)
qplot(rowCounts(eval_prediction)) + geom_histogram(binwidth = 10) +
    ggtitle("Distribution of films per user")
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = TRUE)
head(eval_accuracy)
qplot(eval_accuracy[, "RMSE"]) + geom_histogram(binwidth = 0.1) +
    ggtitle("Distribution of the RMSE by user")
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = FALSE)
##Evaluating the recommendations
eval_accuracy
results <- evaluate(x = eval_sets, method = model_to_evaluate, n = seq(10, 100, 10))
class(results)
head(getConfusionMatrix(results)[[1]])
columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)
plot(results, annotate = TRUE, main = "ROC curve")
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")
eval_accuracy<-calcPredictionAccuracy(x=eval_prediction,data= getData(eval_sets,"unknown"),byUser=TRUE)
head(eval_accuracy)
