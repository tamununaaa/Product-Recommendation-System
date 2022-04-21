###########################################
# LOAD REQUIRED LIBRARIES
###########################################
library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)


###########################################
# LOAD DATA FILES INTO VARIABLES
###########################################
books <- fread('books.csv')
ratings <- fread('ratings.csv')
book_tags <- fread('book_tags.csv')
tags <- fread('tags.csv')

#SHOW HEAD() OF THE FILES
head(books)
head(ratings)
head(book_tags)
head(tags)


###########################################
# CLEAN THE DATA
###########################################

#REMOVE DUPLICATE RATINGS
ratings[, N := .N, .(user_id, book_id)]
cat('Number of duplicate ratings: ', nrow(ratings[N > 1]))
ratings <- ratings[N == 1]

#REMOVE USERS WHO RATED LESS THAN 3 BOOKS
ratings[, N := .N, .(user_id)]
cat('Number of users who rated fewer than 3 books: ', uniqueN(ratings[N <= 2, user_id]))
ratings <- ratings[N > 2]


###########################################
# CREATE SUBSET OF DATA
###########################################
set.seed(1)
user_fraction <- 0.2
users <- unique(ratings$user_id)
sample_users <- sample(users, round(user_fraction * length(users)))
cat('Number of ratings (before): ', nrow(ratings))
ratings <- ratings[user_id %in% sample_users]
cat('Number of ratings (after): ', nrow(ratings))


###########################################
# EXPLORATORY ANALYSIS
###########################################

#WHAT IS THE DISTRIBUTION OF RATINGS?
ratings %>% 
  ggplot(aes(x = rating, fill = factor(rating))) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = FALSE)

#NUMBER OF RATINGS PER USER
ratings %>% 
  group_by(user_id) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_bar(fill = "cadetblue3", color = "grey20") + coord_cartesian(c(3, 50))

#DISTRIBUTION OF MEAN USER RATINGS
ratings %>% 
  group_by(user_id) %>% 
  summarize(mean_user_rating = mean(rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  geom_histogram(fill = "cadetblue3", color = "grey20")

#DISTRIBUTION OF GENRES
genres <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", 
                         "Classics", "Comics", "Contemporary", "Cookbooks", "Crime", "Ebooks", 
                         "Fantasy", "Fiction", "Gay and Lesbian", "Graphic Novels", "Historical Fiction", 
                         "History", "Horror", "Humor and Comedy", "Manga", "Memoir", "Music", "Mystery", 
                         "Nonfiction", "Paranormal", "Philosophy", "Poetry", "Psychology", "Religion", 
                         "Romance", "Science", "Science Fiction", "Self Help", "Suspense", "Spirituality", 
                         "Sports", "Thriller", "Travel", "Young Adult"))

exclude_genres <- c("fiction", "nonfiction", "ebooks", "contemporary")
genres <- setdiff(genres, exclude_genres)
available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]
tmp <- book_tags %>% 
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n / sumN) %>%
  arrange(-percentage) %>%
  left_join(tags, by = "tag_id")
tmp %>% 
  ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + geom_bar(stat = "identity") + 
  coord_flip() + scale_fill_distiller(palette = 'YlOrRd') + labs(y = 'Percentage', x = 'Genre')

#TOP 10 RATED BOOKS
books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

#TOP 10 POPULAR BOOKS
books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, 
            options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


###########################################
# CORRELATION AMONG VARIABLES
###########################################

#WHAT INFLUENCES A BOOK'S RATINGS?
tmp <- books %>% 
  select(one_of(c("books_count","original_publication_year","ratings_count", 
                  "work_ratings_count", "work_text_reviews_count", "average_rating"))) %>% 
  as.matrix()
corrplot(cor(tmp, use = 'pairwise.complete.obs'), type = "lower")

#WHAT IS THE RELATION BETWEEN NUMBER OF RATINGS AND THE AVERAGE RATING?
get_cor <- function(df){
  m <- cor(df$x,df$y, use="pairwise.complete.obs");
  eq <- substitute(italic(r) == cor, list(cor = format(m, digits = 2)))
  as.character(as.expression(eq));                 
}
books %>% 
  filter(ratings_count < 1e+5) %>% 
  ggplot(aes(ratings_count, average_rating)) + stat_bin_hex(bins = 50) + 
  scale_fill_distiller(palette = "Spectral") + 
  stat_smooth(method = "lm", color = "orchid", size = 2) +
  annotate("text", x = 85000, y = 2.7, label = get_cor(data.frame(x = books$ratings_count, y = books$average_rating)), parse = TRUE, color = "orchid", size = 7)


###########################################
# COLLABORATIVE FILTERING
###########################################

#RESTRUCTURE DATA
dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)
ratingmat <- as.matrix(ratingmat)
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]
dim(ratingmat)

#FIND SIMILAR USERS
current_user <- "17329"
rated_items <- which(!is.na((as.data.frame(ratingmat[current_user, ]))))
selected_users <- names(which(apply(!is.na(ratingmat[ ,rated_items]), 1, sum) >= 2))
head(selected_users, 40)

user1 <- data.frame(item=colnames(ratingmat),rating=ratingmat[current_user,]) %>% filter(!is.na(rating))
user2 <- data.frame(item=colnames(ratingmat),rating=ratingmat["1339",]) %>% filter(!is.na(rating))
tmp<-merge(user1, user2, by="item")
tmp
cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs")
user2 <- data.frame(item = colnames(ratingmat), rating = ratingmat["21877", ]) %>% filter(!is.na(rating))
tmp <- merge(user1, user2, by="item")
tmp
cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs")

rmat <- ratingmat[selected_users, ]
user_mean_ratings <- rowMeans(rmat,na.rm=T)
rmat <- rmat - user_mean_ratings

similarities <- cor(t(rmat[rownames(rmat)!=current_user, ]), rmat[current_user, ], use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
head(res, 40)


###########################################
# MAKE PREDICTIONS
###########################################

similar_users <- names(res[1:4])
similar_users_ratings <- data.frame(item = rep(colnames(rmat), length(similar_users)), rating = c(t(as.data.frame(rmat[similar_users,])))) %>% filter(!is.na(rating))
current_user_ratings <- data.frame(item = colnames(rmat), rating = rmat[current_user,]) %>% filter(!is.na(rating))
predictions <- similar_users_ratings %>% 
  filter(!(item %in% current_user_ratings$item)) %>% 
  group_by(item) %>% summarize(mean_rating = mean(rating))
predictions %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


###########################################
# RECOMMENDATIONS
###########################################

predictions %>% 
  arrange(-mean_rating) %>% 
  top_n(5, wt = mean_rating) %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

ratingmat0 <- ratingmat
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
rm(ratingmat0)
gc()

real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

model <- Recommender(real_ratings, method = "UBCF", param = list(method = "pearson", nn = 4))
prediction <- predict(model, real_ratings[current_user, ], type = "ratings")

as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))  


