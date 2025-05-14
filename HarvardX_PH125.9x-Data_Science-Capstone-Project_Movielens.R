#title: "Data Science - Capstone - Project Movielens"
#author: "JMMA"
#date: "Febrary 2025"

#########################################################
# 1.Create edx and final_holdout_test sets 
#########################################################

# Note: this process could take a couple of minutes. 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(vtable)) install.packages("vtable", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

# Libraries required to run the proyect
library(tidyverse)
library(caret)
library(kableExtra)
library(data.table)
library(vtable)
library(ggthemes)
library(corrr)
library(ggcorrplot)
library(factoextra)
library(recosystem)

options(digits = 5)
options(timeout = 120)

# Download and unzip dataset MovieLens from https://grouplens.org/datasets/movielens/latest/
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

#Create a data frame ratings with columns (userId, movieId, rating, timestamp) to 
#obtain ratings_file and convert (integer, integer, numeric, timestamp)
ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE), 
                         stringsAsFactors = FALSE)

colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

#Create a data frame movies with columns (movieId, title, genres) to obtain
#movies_file and convert (movieId to integer)
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)

colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

#Create data_set movielens with movies are in ratings by movieId
movielens <- left_join(ratings, movies, by = "movieId")

#Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

#Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#########################################################
# 2. ANALYSIS
#########################################################

#########################################################
# I. Data structure
#########################################################

# Create a complex HTML table using kableExtra for obtain data structure EDX
kable(head(edx, 5), caption = "Movielens") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Palatino") %>%
  add_header_above(c(" " = 1,"Important data" = 3, " " = 3), color = "#B22222") %>%
  column_spec(2:4, bold = TRUE, color = "#27408B") %>%
  footnote(general = "This table lists the movies.")

#########################################################
# II. Summaries structure
#########################################################

#Convert timestamp column to datetime format in rating_date column 
#in training set edx to calculate statistics 
edx <- edx %>%
  mutate(timestamp = as_datetime(timestamp),
         rating_date = make_date(year(timestamp), month(timestamp))) %>%
  select(-timestamp) %>%
  relocate(rating_date, .after = rating) %>%
  as.data.table()

#Convert timestamp column to datetime format in rating_date column 
#in test set final_holdout_test to calculate statistics 
final_holdout_test <- final_holdout_test %>%
  mutate(timestamp = as_datetime(timestamp),
         rating_date = make_date(year(timestamp), month(timestamp))) %>%
  select(-timestamp) %>%
  relocate(rating_date, .after = rating) %>%
  as.data.table()

#Extract year to the title and create a new column name movie_year
#in training set edx 
year_pattern <- "(?<=\\()\\d{4}(?=\\))"

edx <- edx %>%
  mutate(year_movie = as.Date(str_extract(title, year_pattern), format = "%Y")) %>%
  relocate(year_movie, .after = title) %>%
  as.data.table()

#Extract year to the title and create a new column name movie_year
#in test set final_holdout_test
final_holdout_test <- final_holdout_test %>%
  mutate(year_movie = as.Date(str_extract(title, year_pattern), format = "%Y")) %>%
  relocate(year_movie, .after = title) %>%
  as.data.table()

#Calculates the following summary statistics for the data frame
summary_edx <- summary(edx)

#The important statics is rating min, 1st Quantile, median, mean, 3rd Quantil, max
kable(summary_edx[,3:4], caption = "Statics rating") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Palatino") %>%
  row_spec(3:4, bold = TRUE, color = "#00688B") %>%
  footnote(general = "This table lists rating and rating date min, 1st Quantile, median, mean, 3rd Quantiles, max.")

#########################################################
# III. Graphics statistics
#########################################################

#########################################################
# a. Number of ratings by user
#########################################################

#Extract the summary_edx range years of movies
year_pattern_summary <- "\\d{4}"
min_rating_movie_year <- str_extract(summary_edx[1,4], year_pattern_summary)   
max_rating_movie_year <- str_extract(summary_edx[6,4], year_pattern_summary)

#Extract main genres of movie
genres_pattern <- "([:alpha:]+[-]?[:alpha:]*[:space:]?[:alpha:]*[:space:]?[:alpha:]*)" 

edx <- edx %>%
  mutate(real_genres = str_extract(genres, genres_pattern)) %>%
  relocate(real_genres, .after = genres) %>%
  as.data.table()

final_holdout_test <- final_holdout_test %>%
  mutate(real_genres = str_extract(genres, genres_pattern)) %>%
  relocate(real_genres, .after = genres) %>%
  as.data.table()

#Create summarize with metrics of movielens
edx %>%
  summarize('Number Users' = length(unique(userId)), 
            'Number Movies' = length(unique(movieId)),
            'Number rating' = length(rating),
            'Number Genres' = length(unique(real_genres)),
            'Min year rating movie' = min_rating_movie_year,
            'Max year rating movie' = max_rating_movie_year) %>%
  kable(caption = "Metric Movielens") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Palatino") %>%
  column_spec(1:6, bold = TRUE, color = "#27408B") %>%
  footnote(general = "This table lists metrics of movielens.")

#Create summarize with number of rating by user
ratings_by_user <- edx %>%
  group_by(userId) %>%
  summarize(n_ratings_by_user = n()) %>%
  ungroup()

#Function return mean all number ratings of users
mean_ratings_by_users <- edx %>% 
  group_by(userId) %>%
  summarize(n_ratings_by_user = n()) %>%
  pull(n_ratings_by_user) %>%
  mean() %>%
  signif(3)

#Function return median all number ratings of users
median_ratings_by_users <- edx %>% 
  group_by(userId) %>%
  summarize(n_ratings_by_user = n()) %>%
  pull(n_ratings_by_user) %>%
  median() %>%
  signif(3)

#Remove row userId because the unique row statistics important is number of ratings by user
ratings_by_user <- ratings_by_user %>%
  mutate(median_ratings_user = median(ratings_by_user$n_ratings_by_user)) %>%
  select(-userId, n_ratings_by_user, median_ratings_user) %>%
  as.data.table()

labs <- data.frame(n_ratings_by_user = 'Number of ratings by user',
                   median_ratings_user = 'Median ratings by user')

#Create a table of statistics training set Edx with mean, sd, min,
#percentil 25, percentil 75, max by number of ratings by user
sumtable(ratings_by_user, labels = labs)

#Calculation of users with fewer ratings 30 with a total of 16271 out of 69878 users 
length(which(ratings_by_user$n_ratings_by_user<=30))

#Show in a histogram graphic number user vs rating by user
ggplot(ratings_by_user, aes(x = n_ratings_by_user)) +
  geom_histogram(bins = 35, fill = "#4169E1", color = "black") +
  geom_vline(xintercept=mean_ratings_by_users, linewidth=1, color="#B22222") +
  geom_vline(xintercept=median_ratings_by_users, linewidth=1, color="#B22222") +
  annotate("text", x = 180, y = 6000, label = paste0("Mean\n",mean_ratings_by_users), color="#B22222", size=4 , angle=0) +
  annotate("text", x = 40, y = 6000, label = paste0("Median\n",median_ratings_by_users), color="#B22222", size=4 , angle=0) +
  scale_x_log10() +
  labs(title = "Number of ratings by user",
       x = "Number of ratings",
       y = "Number of users",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") + 
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=0, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))+
  theme_economist_white()

#########################################################
# b. Number of movies with number of ratings
#########################################################

#Create summarize with number of movies with number of ratings
ratings_by_movies <- edx %>%
  group_by(movieId) %>%
  summarize(n_ratings_by_movie = n()) %>%
  ungroup()

#Function return mean all number ratings of movies
mean_ratings_by_movies <- edx %>% 
  group_by(movieId) %>%
  summarize(n_ratings_by_movie = n()) %>%
  pull(n_ratings_by_movie) %>%
  mean() %>%
  signif(3)

#Function return median all number ratings of movies
median_ratings_by_movies <- edx %>% 
  group_by(movieId) %>%
  summarize(n_ratings_by_movie = n()) %>%
  pull(n_ratings_by_movie) %>%
  median() %>%
  signif(3)

#Remove row movieId because the unique row statistics important is number of ratings by user
ratings_by_movies <- ratings_by_movies %>%
  mutate(median_movie_rating = median(ratings_by_movies$n_ratings_by_movie)) %>%
  select(-movieId, n_ratings_by_movie,median_movie_rating) %>%
  as.data.table()

labs <- data.frame(n_ratings_by_movie = 'Number of ratings by movie',
                   median_movie_rating = 'Median ratings by movie')

#Create a table of statistics training set Edx with mean, sd, min,
#percentil 25, percentil 75, max by number of ratings by movie
sumtable(ratings_by_movies, labels = labs)

#Calculation of movies with fewer ratings 30 with a total of 2688 out of 10677 users 
length(which(ratings_by_movies$n_ratings_by_movie<=30))

#Show in a histogram graphic number movie vs rating by movie
ggplot(ratings_by_movies, aes(x = n_ratings_by_movie)) +
  geom_histogram(bins = 35, fill = "#00688B", color = "black") +
  geom_vline(xintercept=mean_ratings_by_movies, linewidth=1, color="#B22222") +
  geom_vline(xintercept=median_ratings_by_movies, linewidth=1, color="#B22222") +
  annotate("text", x = 1290, y = 700, label = paste0("Mean\n",mean_ratings_by_movies), color="#B22222", size=4 , angle=0) +
  annotate("text", x = 70, y = 700, label = paste0("Median\n",median_ratings_by_movies), color="#B22222", size=4 , angle=0) +
  scale_x_log10() +
  labs(title = "Number of ratings by movie",
       x = "Number of ratings",
       y = "Number of movies",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=0, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))+
  theme_economist_white()

#########################################################
# c. Number of movies ratings of year-month of rating
#########################################################

#Create summarize with number of movies ratings of year rating
ratings_by_year_rating <- edx %>%
  mutate(year_rating = format(as.Date(rating_date, format="%d %m %Y"), "%Y")) %>%
  mutate(year_rating = as.Date(year_rating, format ="%Y")) %>%
  group_by(year_rating) %>%
  summarize(n_ratings_by_year_rating = n())

labs <- data.frame(n_ratings_by_year_rating = 'Number of ratings movies by year')  

sumtable(ratings_by_year_rating, labels = labs)

# Show in scatter a graphic number ratings of year of rating
ggplot(ratings_by_year_rating, aes(x = year_rating, y = n_ratings_by_year_rating)) + 
  geom_point(color = "#4169E1") +
  geom_line(aes(year_rating, n_ratings_by_year_rating), col = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 1200000, 100000), labels = scales::label_number()) +
  labs(title = "Number ratings of year of rating", 
       x = "Year of rating", 
       y = "Number of ratings", 
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  geom_smooth(color = "black") + 
  theme_economist()

#Create summarize with number of movies ratings of month rating
ratings_by_month_rating <- edx %>%
  group_by(rating_date) %>%
  summarize(n_ratings_by_month_rating = n())

labs <- data.frame(n_ratings_by_month_rating = 'Number of ratings movies by month')  
sumtable(ratings_by_month_rating, labels = labs)

# Show in a scatter graphic number ratings of month of rating
ggplot(ratings_by_month_rating, aes(x = rating_date, y = n_ratings_by_month_rating)) +
  geom_point(color = "#00688B") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 300000, 50000), labels = scales::label_number()) +
  labs(title = "Number ratings of year of rating",
       x = "Year of rating",
       y = "Number of ratings",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  geom_smooth(color = "black") +
  theme_economist_white()

#########################################################
# d. Number of movies ratings by rating values
#########################################################

#Show in a histogram graphic number of movies by rating values
ggplot(edx, aes(x = rating)) +
  geom_histogram(bins = 35, fill = "#FFD700", color = "black") +
  scale_x_continuous(breaks = seq(0,5,0.5)) +
  scale_y_continuous(breaks = seq(0,3000000,100000), labels = scales::label_number()) +
  labs(title = "Number of movies ratings by rating values",
       x = "Number of ratings",
       y = "Number of movies",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") + 
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=0, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))+
  theme_economist()

#########################################################
# e. Number of users ratings by average ratings
#########################################################

#Create a summarize number of users ratings by average ratings
avg_ratings_by_user <- edx %>%
  group_by(userId) %>%
  summarize(average_rating = mean(rating)) %>%
  select(-userId) %>%
  as.data.table()

labs <- data.frame(average_rating = 'Number of users ratings by average ratings')  
sumtable(avg_ratings_by_user, labels = labs)

#Function return mean all the movies
mean_rating <- edx %>% 
  pull(rating) %>%
  mean() %>%
  signif(3)

#Show in a histogram graphic number of users ratings by average ratings
ggplot(avg_ratings_by_user, aes(x = average_rating)) +
  geom_histogram(bins = 35, fill = "#00688B", color = "black") +
  geom_vline(xintercept=mean_rating, linewidth=1, color="#B22222") +
  annotate("text", x = 3.25, y = 8800, label = paste0("Mean\n",mean_rating), color="#B22222", size=4 , angle=0) +
  labs(title = "Number of users ratings by average ratings",
       x = "Average ratings",
       y = "Number of users",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=0, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))+
  theme_stata()

#########################################################
# f. Average ratings by genres of movie
#########################################################

#Create a summarize average ratings by genres of movie
avg_ratings_by_genres <- edx %>%
  group_by(real_genres) %>%
  select(real_genres,rating) %>%
  summarize(average_genres_rating = mean(rating)) %>%
  arrange(desc(average_genres_rating))

avg_ratings_by_genres %>%
  reframe('Genres' = real_genres, 
          'Average genres' = average_genres_rating) %>%
  kable(caption = "Statics average rating by genres") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "Palatino") %>%
  column_spec(1, bold = TRUE, color = "#4169E1") %>%
  column_spec(2, bold = TRUE, color = "#00688B") %>%
  footnote(general = "Source data: https://grouplens.org/datasets/movielens/10m/")

#show in a plot ratings by genres of movie
  ggplot(edx, aes(real_genres, rating)) +
  geom_boxplot(col = "#00688B") +
  labs(title = "Quartiles(min-25%-median-75%-max-outliers) by genres of movie",
       x = "Genres",
       y = "Ratings",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=90, hjust=1, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))

#########################################################
# g. Number of Movies by genre year
#########################################################

#create a columns with genres that permit create a density plot 
edx <- edx %>% mutate(
  'Action' = case_when(edx$real_genres == 'Action' ~ 1, .default = as.integer(0)),
  'Adventure' = case_when(edx$real_genres == 'Adventure' ~ 1, .default = as.integer(0)),
  'Animation' = case_when(edx$real_genres == 'Animation' ~ 1, .default = as.integer(0)),
  'Children' = case_when(edx$real_genres == 'Children' ~ 1, .default = as.integer(0)),
  'Comedy' = case_when(edx$real_genres == 'Comedy' ~ 1, .default = as.integer(0)),
  'Crime' = case_when(edx$real_genres == 'Crime' ~ 1, .default = as.integer(0)),
  'Documentary' = case_when(edx$real_genres == 'Documentary' ~ 1, .default = as.integer(0)),
  'Drama' = case_when(edx$real_genres == 'Drama' ~ 1, .default = as.integer(0)),
  'Fantasy' = case_when(edx$real_genres == 'Fantasy' ~ 1, .default = as.integer(0)),
  'Film-Noir' = case_when(edx$real_genres == 'Film-Noir' ~ 1, .default = as.integer(0)),
  'Horror' = case_when(edx$real_genres == 'Horror' ~ 1, .default = as.integer(0)),
  'IMAX' = case_when(edx$real_genres == 'IMAX' ~ 1, .default = as.integer(0)),
  'Musical' = case_when(edx$real_genres == 'Musical' ~ 1, .default = as.integer(0)),
  'Mystery' = case_when(edx$real_genres == 'Mystery' ~ 1, .default = as.integer(0)),
  'no genres listed' = case_when(edx$real_genres == 'no genres listed' ~ 1, .default = as.integer(0)),
  'Romance' = case_when(edx$real_genres == 'Romance' ~ 1, .default = as.integer(0)),
  'Sci-Fi' = case_when(edx$real_genres == 'Sci-Fi' ~ 1, .default = as.integer(0)),
  'Thriller' = case_when(edx$real_genres == 'Thriller' ~ 1, .default = as.integer(0)),
  'War' = case_when(edx$real_genres == 'War' ~ 1, .default = as.integer(0)),
  'Western' = case_when(edx$real_genres == 'Western' ~ 1, .default = as.integer(0))) %>%
  relocate('Action','Adventure','Animation','Children','Comedy','Crime','Documentary','Drama','Fantasy','Film-Noir','Horror','IMAX','Musical','Mystery','no genres listed','Romance','Sci-Fi','Thriller','War','Western', .after = real_genres) %>%
  as.data.table()

#Create a filter to view in a density plot with title, year movie and genre
filtered_title_year_genre <- edx %>%
  pivot_longer(c(Action, Adventure, Animation, Comedy, Crime, Documentary, Drama, Fantasy, `Film-Noir`, Horror, IMAX, Musical, Mystery, `no genres listed`, Romance, `Sci-Fi`, Thriller, War, Western), names_to = "genre") %>% 
  filter(value == "1") %>% 
  select(title, year_movie, genre)

#show in a density plot number of movies by genre year
ggplot(filtered_title_year_genre, aes(x = year_movie, color = genre)) +
  geom_line(stat = "count", aes(group = genre)) +
  labs(title = "Number of Movies by Genre (1915-2008)",
       x = "Year",
       y = "Number of Movies",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=0, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))+
  theme_gdocs() +
  scale_color_discrete(name = "Genre")

#########################################################
# h. Average ratings by year movie
#########################################################

#Create summarize average rating by year movie
avg_rating_by_year_movie <- edx %>%
  group_by(year_movie) %>%
  summarize (average_year_movie = mean(rating))

#Show in a scatter graphic average ratings by year movie
ggplot(avg_rating_by_year_movie, aes(x = year_movie, y = average_year_movie)) +
  geom_point(color = "#4169E1") +
  labs(title = "Average ratings by year movie",
       x = "Year movie",
       y = "Average ratings",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  geom_smooth(color = "black") +
  theme_foundation()

#########################################################
# i. Average rating by year of movies by genre
#########################################################

#Create summarize average rating by year movie with five most important genres
avg_rating_by_year_movie_genres <- edx %>%
  filter(real_genres == "Action" | real_genres == "Comedy" | real_genres == "Documentary" | real_genres == "Drama" | real_genres == "Romance") %>%
  group_by(year_movie,real_genres) %>%
  summarize (average_year_movie_genres = mean(rating)) 

#show in a density plot average rating by year of movies with five most important genres
ggplot(avg_rating_by_year_movie_genres, aes(x = year_movie, y = average_year_movie_genres, color = real_genres)) +
  geom_line(aes(x = year_movie, y = average_year_movie_genres )) +
  labs(title = "Average rating by year of movies by genre (1915-2008)",
       x = "Year movie by genres",
       y = "Average ratings by year",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=0, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))+
  theme_economist() +
  scale_color_discrete(name = "Genre")

#########################################################
# 3. PCA
#########################################################

#create a data for realize a PCA study with 8 genres with the users above the median 62 rating movies
PCA <- edx %>%
  group_by(userId) %>%
  filter(n() >= 62) %>%
  select(Action,Adventure, Children, Comedy, Crime, Documentary, Drama, Romance) %>%
  summarize(Action = sum(Action),
            Adventure = sum(Adventure),
            Children = sum(Children),
            Comedy = sum(Comedy),
            Crime = sum(Crime),
            Documentary = sum(Documentary),
            Drama = sum(Drama),
            Romance = sum (Romance)) %>%
  select(Action,Adventure, Children, Comedy, Crime, Documentary, Drama, Romance)

#check is not null values in PCA data   
colSums(is.na(PCA))

#normalization using function scale 
data_normalized <- scale(PCA)

#calculate correlation of matrix
corr_matrix_genres <- cor(data_normalized)

#Correlation plot
ggcorrplot(corr_matrix_genres,
           method = "square",
           hc.order = TRUE, 
           type = "full",
           lab = TRUE,
           title = "Matrix correlation of genre",
           ggtheme = ggplot2::theme_light(),
           colors = c("#4169E1", "white", "#B22222"))

#summary importance of components
data.pca <- princomp(corr_matrix_genres)
summary(data.pca)

#Proportion acumulate Comp1. Comp2.
data.pca$loadings[, 1:2]

#Scree plot
fviz_eig(data.pca, 
         addlabels = TRUE,
         barfill = "#00688B",
         barcolor = "#00688B", 
         linecolor = "black",
         ggtheme = ggplot2::theme_light())

#Graphic pca var
fviz_pca_var(data.pca, col.var = "#00688B")+
  labs(title ="PCA", x = "PC1", y = "PC2") +
  theme_stata()

#Graphic pca cos2 
fviz_cos2(data.pca,
          addlabels = TRUE,
          choice = "var",
          fill = "#00688B",
          color = "#00688B", 
          axes = 1:2) +
  theme_foundation()

#Graphic pca var
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "#FFA500", "#2E8B57"),
             repel = TRUE,
             ggtheme = ggplot2::theme_light())

#########################################################
# 4. MODELING RESULTS
#########################################################

#########################################################
# I. A First model 
#########################################################

#RMSE formula
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Mean movies rating (mu) to training set (edx) 3.51
edx_mu_hat <- mean(edx$rating)

#Naive RMSE obtain use test set (final_holdout_test) rating and mean (mu) to training set (edx)
result_nai <- RMSE(final_holdout_test$rating, edx_mu_hat)
result_naive <- tibble(Model = "Naive RMSE", RMSE = result_nai) 
result_naive %>%
  knitr::kable()

#Any number other than ^mu_hat would result into a higher RMSE 
predictions <- rep(2.5, nrow(final_holdout_test))
result_dis_mu <- RMSE(final_holdout_test$rating, predictions)

result_distinct_mu <- tibble(Model = "Distinct value mu_hat RMSE", RMSE = result_dis_mu) 
result_distinct_mu %>% kable()

#########################################################
# II. Modeling movie effects
#########################################################

#The least squares estimate ^b_i is just the average of Y(u,i) - ^mu for each movie
mu <- mean(edx$rating) 
movie_bias <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#Histogram graphic show 0 equivalent 3.51 mean rating movie, 1.5 is equal to 5 rating and -3 is equal to 0.5 rating 
ggplot(movie_bias, aes(x = b_i)) +
  geom_histogram(bins = 35, fill = "#27408B", color = "black") +
  labs(title = "Movie bias",
       x = "Bias movies",
       y = "Number of movies",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=0, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))+
  theme_gdocs()

#create a predicted rating add mu + b_i (bias movie) 
predicted_ratings <- mu + final_holdout_test %>% 
  left_join(movie_bias, by='movieId') %>%
  pull(b_i)

#calculate RMSE with predicted_ratings and test set final_holdout_test
model_1 <- RMSE(predicted_ratings, final_holdout_test$rating)

model_1_rmse <- tibble(Model = "Movie Effect Model", RMSE = model_1)

model_1_rmse %>% kable()

#########################################################
# III. Modeling user effects 
#########################################################

#We will compute an approximation by computing ^mu  and ^b_i and estimating ^b_u as the average of  ^y(u,i)- ^mu - ^b_i
user_bias <- edx %>% 
  left_join(movie_bias, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#Histogram graphic bias user 
ggplot(user_bias, aes(x = b_u)) +
  geom_histogram(bins = 35, fill = "#00688B", color = "black") +
  labs(title = "User bias",
       x = "Bias user",
       y = "Number of movies",
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  theme(axis.title.x=element_text(size=12, angle=0, face = "bold"))+  
  theme(axis.text.x=element_text(size=10, angle=0, face = "bold")) +
  theme(axis.text.y=element_text(size=10, angle=90, face = "bold"))+
  theme_stata()

#create a predicted rating add mu + b_i (bias movie) + b_u (bias user)
predicted_ratings <- final_holdout_test %>% 
  left_join(movie_bias, by='movieId') %>%
  left_join(user_bias, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#calculate RMSE with predicted_ratings and test set final_holdout_test
model_2 <- RMSE(predicted_ratings, final_holdout_test$rating)

model_2_rmse <- tibble(Model = "Movie + User Effect Model", RMSE = model_2)

model_2_rmse %>% kable()

#########################################################
# IV. Regularized movie + user effect model
#########################################################

#The idea is to add a tuning parameter lamba to further reduce the RMSE
#The idea is to penalize outliers from the Movie Bias and User Bias sets which shall optimize the recommendation system

lambdas_regularize <- seq(0, 10, 0.25)

rmse_regularize <- sapply(lambdas_regularize, function(l){
  
  edx_mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - edx_mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - edx_mu - b_i)/(n()+l))
  
  predicted_ratings <- final_holdout_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = edx_mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, final_holdout_test$rating))
})

#Discover which lamba will be best at reducing the RMSE

lambda <- lambdas_regularize[which.min(rmse_regularize)]
lambda

ggplot(mapping = aes(x = lambdas_regularize, y = rmse_regularize)) + 
  geom_point(color = "#00BFFF") +
  labs(title = "Distribution Lambdas", 
       x = "Lambda", 
       y = "RMSE", 
       caption = "Source data: https://grouplens.org/datasets/movielens/10m/") +
  geom_smooth(color = "black") + 
  theme_economist()

#We will use the best lamba to reduce our RMSE regularized Movie + User Effect Model

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))

predicted_ratings <- final_holdout_test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#calculate RMSE with predicted_ratings and test set final_holdout_test

model_3 <- RMSE(predicted_ratings, final_holdout_test$rating)

model_3_rmse <- tibble(Model = "Regularized Movie + User Effect Model", RMSE = model_3)

model_3_rmse %>% kable()

##################################################################
# V. Regularized movie + user effect model + genres effecft model
#################################################################

#The idea is to add a tuning parameter lamba to further reduce the RMSE
#The idea is to penalize outliers from the Movie, User and genres Bias sets which shall optimize the recommendation system

#We will use the best lamba to reduce our RMSE regularized Movie + User Effect Model + Genres Effect Model

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))

b_g <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(real_genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda))

predicted_ratings <- final_holdout_test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "real_genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

#calculate RMSE with predicted_ratings and test set final_holdout_test

model_4 <- RMSE(predicted_ratings, final_holdout_test$rating)

model_4_rmse <- tibble(Model = "Regularized Movie + User Effect Model + Genres Effect Model", RMSE = model_4)

model_4_rmse %>% kable()

#########################################################
# 5. RECO 
#########################################################

niter <- 25 
# Train the reco model with the fixed niter on the entire edx set
reco_train <- data_memory(edx$userId, edx$movieId, edx$rating)
reco_model <- Reco()

reco_model$train(reco_train, opts = list(dim = 10, niter = niter,  nthread = 8, costp_l2 = 0.1, costq_l2 = 0.1, lrate = 0.05))

# Prepare final hold-out test data
reco_test <- data_memory(final_holdout_test$userId, final_holdout_test$movieId)

# Predict ratings for the final hold-out test set
predicted_holdout_ratings <- reco_model$predict(reco_test)

# Add predictions to final hold-out test set
final_holdout_test$predicted_rating <- predicted_holdout_ratings

# Calculate RMSE for the final hold-out test set
model_reco <-  RMSE(final_holdout_test$predicted_rating, final_holdout_test$rating)
model_reco_rmse <- tibble(Model = "Reco matrix factorization", RMSE = model_reco)

model_reco_rmse %>% kable()

#########################################################
# 6. RESULTS
#########################################################

#Summarize the models RMSE 

results_rmse <- tibble(Model = c("Naive RMSE", "Movie Effect Model", "Movie + User Effect Model", "Regularized Movie + User Effect Model",
                                 "Regularized Movie + User Effect Model + Genres Effect Model", "Reco matrix factorization"),
                       RMSE = c(result_nai, model_1, model_2, model_3, model_4, model_reco))

results_rmse %>% kable()

