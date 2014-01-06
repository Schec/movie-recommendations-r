library(ggplot2)
library(plyr)
library(Hmisc)
library(xtable)

# read in the data

reviews <- read.delim2('C:/Documents and Settings/schechter/Desktop/rfinal/ml-100k/u.data', header=FALSE, col.names=c('user.id','item.id','rating','timestamp'))

items <- read.delim2('C:/Documents and Settings/schechter/Desktop/rfinal/ml-100k/u.item', header=FALSE, sep = "|", col.names=c('movie.id', 'movie.title', 'release.date', 'video.release.date', 'IMDb.URL', 'Unknown',  'Action', 'Adventure', 'Animation', 'Childrens', 'Comedy', 'Crime', 'Documentary', 'Drama', 'Fantasy', 'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci-Fi', 'Thriller', 'War', 'Western'))

users <- read.delim2('C:/Documents and Settings/schechter/Desktop/rfinal/ml-100k/u.user', header=FALSE, sep='|', col.names=c('user.id', 'age', 'gender', 'occupation', 'zip.code'))


# Exploration steps

# Summarize reviews per user
# Summarize average rating per user
# Summarize reviews per movie
# Summarize average rating per movie
# Summarize movies/reviews per genre.  Summarize genres per movie.

# Summarize reviews per user

user.review.counts <- count(reviews, "user.id")
colnames(user.review.counts)[2] <- "reviews"
qplot(reviews, data=user.review.counts, geom="histogram", binwidth=10, fill=..count.., main="Number of Reviews per User")

# Every user has made at least 10 reviews (data set specified)
# Greatest number of users have made 10-20 reviews
# Some "power users" have made more than 600!

graphics.off()

# Summarize average rating per user
user.review.averages <- ddply(reviews,~user.id,summarise,mean=mean(rating),sd=sd(rating))
qplot(mean, data=user.review.averages, geom="histogram", binwidth=0.1, fill=..count.., main="Average User Review")

# Most users give an average score of 3-4 stars
# There's a distribution of average review - some users just tend to score movies higher than others - we will have to control for this when comparing reviews across users

graphics.off()

# Summarize reviews per movie
movie.review.counts <- count(reviews, "item.id")
colnames(user.review.counts)[2] <- "reviews"
qplot(freq, data=movie.review.counts, geom="histogram", binwidth=10, fill = ..count.., main="Number of Reviews per Movie")

# Again, we see a long tail with most movies having fewer than 10 reviews
# One movie way down at the end there...

max(movie.review.counts$freq)

graphics.off()

# Again, we see a long tail with most movies having fewer than 10 reviews
# One movie way down at the end there...

# find movie with most reviews
most.reviewed.movie <- movie.review.counts[movie.review.counts$freq == max(movie.review.counts$freq),]

# lookup most reviewed movie by title
items[items$movie.id == most.reviewed.movie$item.id,]$movie.title

# So the movie witht the most reviews is Star Wars with 538 reviews

# We would expect a positive correlation between the number of movie reviews and how good a movie is.  Let's take a look.

movie.review.averages <- ddply(reviews,~item.id,summarise,mean=mean(rating),sd=sd(rating),freq=length(item.id))
qplot(mean, data=movie.review.averages, geom="histogram", binwidth=0.1, fill=..count.., main="Average Movie Review")


plot(movie.review.averages$mean, movie.review.averages$freq, main="Popularity and Average Review",
   xlab="Average Review", ylab="Number of Reviews", pch=19)
abline(lm(movie.review.averages$freq~movie.review.averages$mean), col="sky blue", lwd=3) # regression line (y~x)

# So we see that only the higher rated movies have huge numbers of reviews, as expected

graphics.off()


######## Building Our Recommendation System ########

## I'm going to do a neighborhood collaborative factoring model
## This involves finding similarities among movies, and then finding a recommendation list based on neighboring movies, by similarity score, given a first movie
## You can only calculate similarity scores for two movies based on the users that have rated them both, so..

# Find common support (common_reviewers) for every pair of movies (adapted from beer example)
common.reviewers.by.id <- function(movie.id.1, movie.id.2){
    reviews1 <- subset(reviews, item.id == movie.id.1)
    reviews2 <- subset(reviews, item.id == movie.id.2)
    common.reviewers <- intersect(reviews1$user.id, reviews2$user.id)

    if (length(common.reviewers) == 0){
        NA
    } else {
        common.reviewers
    }
}


# Choose Star Wars ID 50 and whichever movie is id 100 to test function
movie.review.counts[movie.review.counts$item.id == 50,]$freq # number of reviewers of movie 50
movie.review.counts[movie.review.counts$item.id == 100,]$freq  # number of reviewers of movie 100
common.reviewers.by.id(50,100)   # common reviewers' ids
length(common.reviewers.by.id(50,100))    # number of common reviewers (common support)

# Let's find out how much common support their is for each pair of movies
movie.list <- items$movie.id
length(movie.list)

movie.pairs <- expand.grid(movie.id.1=movie.list, movie.id.2=movie.list)
movie.pairs <- subset(movie.pairs, movie.id.1!=movie.id.2)
dim(movie.pairs)
## THAT'S A LOT OF PAIRS!  Since calculating more than 2,000,000 similarity scores is too much for my computer to handle, I'm going to limit my recommendation system to only movies with at least 50 reviews

movie.list <-  which(movie.review.counts$freq > 50)
length(movie.list) #593 movies
movie.pairs <- expand.grid(movie.id.1=movie.list, movie.id.2=movie.list)
movie.pairs <- subset(movie.pairs, movie.id.1!=movie.id.2)
dim(movie.pairs)


#common.supports <- ddply(movie.pairs, .(movie.id.1, movie.id.2), function(x){
    #movie.id.1 <- x$movie.id.1
    #movie.id.2 <- x$movie.id.2
    #h <- common.reviewers.by.id(movie.id.1, movie.id.2)
    #c("support" <- length(h))}, .progress = "text")

#write.csv(common.supports, "common_supports.csv", row.names = FALSE)

common.supports <- read.csv('C:/Documents and Settings/schechter/Desktop/rfinal/common_supports.csv', header=TRUE)
colnames(common.supports)[3] <- "Support"
# Let's look at the average common support among our movie pairs
mean(common.supports$Support)
min(common.supports$Support)
max(common.supports$Support)
qplot(Support, data=common.supports, geom="histogram", binwidth=10, fill=..count.., main="Common Support Among Movie Pairs")
graphics.off()

# We need to calculate a similarity score for each pair
# The beer example weighted different rated attributes against one another, but we only have one score
# My AC209 class showed me a way to do with with Pearson coefficient
# We can't just compare two user's reviews, because some reviewers generally give lower reviews than others
# Instead, we'll compare (user's review) - (user's average review) for the common users
# We need user's average review in the same dataframe as user's review for all of our recommendation system restaurants
# We'll recalculate the average user's review for just the movies in our subset


small.reviews <- reviews[is.element(reviews$item.id, movie.list),]
dim(reviews)
dim(small.reviews)

# We'll recalculate average rating per user on the small dataset
user.review.averages <- ddply(small.reviews,~user.id,summarise,mean=mean(rating),sd=sd(rating))
qplot(mean, data=user.review.averages, geom="histogram", binwidth=0.1, fill=..count.., main="Average Rating on Small Dataset")
small.reviews <- merge(small.reviews, user.review.averages, by="user.id")
dim(small.reviews)

# I'm going to use a pearson correlation coefficient as my value for similarity
pearson.sim <- function(movie.1.reviews, movie.2.reviews, n.common){
    if (n.common <= 4){
        NA
    }
    else {
        diff1 <- movie.1.reviews$rating - movie.1.reviews$mean
        diff2 <- movie.2.reviews$rating - movie.2.reviews$mean
        P <- rcorr(diff1, diff2, type="pearson")[3]
        P$P[1,2]
    }
}

calc.similarity <- function(movie.id.1, movie.id.2) {
    common.users <- common.reviewers.by.id(movie.id.1, movie.id.2)
    movie.1.reviews <- small.reviews[small.reviews$item.id == movie.id.1,]
    movie.2.reviews <- small.reviews[small.reviews$item.id == movie.id.2,]
    movie.1.reviews <- subset(movie.1.reviews, is.element(user.id, common.users) == TRUE)
    movie.2.reviews <- subset(movie.2.reviews, is.element(user.id, common.users) == TRUE)
    # reviews already sorted by user.id, so no reason to sort
    pearson.sim(movie.1.reviews, movie.2.reviews, length(common.users))
}

calc.similarity(50,100)
calc.similarity(90,100)

# The following applies the calc.similarity function to every pair of movies.  It takes a very long time to run, which is why I have commented it here.  You can see that I wrote to file when I ran it,and that I read the data calculated by this function in from a csv file below.

#similarities <- ddply(movie.pairs, .(movie.id.1, movie.id.2), function(x){
#    movie.id.1 <- x$movie.id.1
#    movie.id.2 <- x$movie.id.2
#    h <- calc.similarity(movie.id.1, movie.id.2)
#    c("similarity" <- h)}, .progress = "text")

#write.csv(similarities, "similarities.csv", row.names = FALSE)

similarities <- read.csv('C:/Documents and Settings/schechter/Desktop/rfinal/similarities.csv', header=TRUE)

# create one table with common supports and similarities together for every movie pair
# rename the columns for the merge
colnames(common.supports)[3] <- "n.common"
colnames(similarities)[3] <- "pearson"

similarities <- merge(common.supports, similarities)

# shrunken similarity - regularizes the estimates to corral in estimates based on little data, reduce overall varianc
similarities$shrunk <- similarities$n.common*similarities$pearson / (similarities$n.common +3)

# sort the data so that taking the first N gives the most similar movies
similarities <- similarities[order(-similarities$shrunk),]


k.nearest <- function(movie.id.1,k){
    similarities[which(similarities$movie.id.1 == movie.id.1),][1:k,]
}

k.nearest(10,4)

# To use the recommendation system I'm going to need to be able to look up movies based on search terms

items[grep('You',items$movie.title),2]

# make this case-insensitive
tolower('You')
tolower(items$movie.title)[1:10]
items[grep(tolower('You'),tolower(items$movie.title)),2]

# remember when earlier i reduced the dataset  to only movies with at least 60 reviews?  This is really part of that.
small.items <-  items[unique(similarities$movie.id.1),]
dim(items)
dim(small.items)


# the following function looks up movie titles based on search term input from user
get.movie.title <- function(x){
    cat("Please enter a search term\n ")
    pattern <- scan("stdin", what = character(),  n=1, quiet = TRUE)
    suggestions <- small.items[grep(tolower(pattern),tolower(small.items$movie.title)),2]
    if (length(suggestions) > 1){
        cat("Please choose from the below list by entering the corresponding numeric value\n")
        print(suggestions)
        selection <- scan("stdin", what = integer(),  n=1, quiet = TRUE)
    } else if (length(suggestions) == 1) {
        selection <-  1
    } else {
        cat("I'm sorry.  No movies matched your search.  ")
        selection <- 0
        get.movie.title()
    }
    suggestions[selection]
}

get.movie.title()

# functions to translate titles to ids and ids to titles
title.to.id <- function(title){
    items[match(title,items$movie.title),1]
}

id.to.title <- function(id){
    items[match(id,items$movie.id),2]
}

# vectorizing these functions
title.to.id <- Vectorize(title.to.id)
id.to.title <- Vectorize(id.to.title)

make.recommendations <- function(k=10){
    title <- get.movie.title()
    id <- title.to.id(title)
    print(sprintf("Recommendations for movie %s:",title))
    k.nearest <- k.nearest(id,k)
    id.to.title(k.nearest$movie.id.2)
}

make.recommendations()

## Some of these recommendations aren't what I'm looking for.  I want to limit to only recommendations in the same genre!

genres <- colnames(items)[7:ncol(items)]

id.to.genres <- function(id){
    genre.vector <- items[match(id,items$movie.id),7:ncol(items)]
    genres[genre.vector == 1]
}

id.to.genres(50)

# one movie can have multiple genres - I'm letting the user choose one
choose.genre <- function(genres){
    cat("We found the following genres for your id.  Please select one.")
    scan("stdin", what = integer(),  n=1, quiet = TRUE)
}

get.movie.genre <- function(id){
    suggestions <- id.to.genres(id)
    if (length(suggestions) > 1){
        cat("Please choose from the below list by entering the corresponding numeric value\n")
        print(suggestions)
        selection <- scan("stdin", what = integer(),  n=1, quiet = TRUE)
    } else if (length(suggestions) == 1) {
        selection <-  1
    } else {
        cat("I'm sorry.  We do not have genre information for this film.")
        selection <- 0
        }
    suggestions[selection]
}

## I create lists of the movies in each genre from the 0/1 encoding we have now
genre.lists <-  items[,7:ncol(items)]*items$movie.id
for (i in 1:ncol(genre.lists)){
    assign(colnames(genre.lists)[i],unique(genre.lists[,i]))
}


k.nearest.by.genre <- function(movie.id.1,k,genre){
    a <- similarities[which(similarities$movie.id.1 == movie.id.1),]
    b <- a[which(a$movie.id.2 %in% get(genre)),]
    b[1:k,]
}

make.recommendations.by.genre <- function(k=10){
    title <- get.movie.title()
    id <- title.to.id(title)
    genre <- get.movie.genre(id)
    print(sprintf("Recommendations for movie %s:",title))
    k.nearest <- k.nearest.by.genre(id,k,genre)
    if (length(k.nearest) == 0){
        cat("We're sorry.  We have no same-genre recommendations for this film.")
    } else{
    id.to.title(k.nearest$movie.id.2)
}
}

make.recommendations.by.genre()


# Example of making a nice table
"StarWars" <-  make.recommendations()
xtable(data.frame(StarWars))

"StarWarsG" <-  make.recommendations.by.genre()
xtable(data.frame(StarWarsG))
