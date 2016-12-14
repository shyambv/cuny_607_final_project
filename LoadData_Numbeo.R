# Data 607 Final Project
# Fall 2016
# Load Data File

#--------------Packages--------------#

if(!require("RCurl", character.only = TRUE, quietly = TRUE)) {
    install.packages("RCurl")
    library("RCurl", character.only = TRUE)
}

if(!require("jsonlite", character.only = TRUE, quietly = TRUE)) {
    install.packages("jsonlite")
    library("jsonlite", character.only = TRUE)
}

# For web scraping
if(!require("rvest", character.only = TRUE, quietly = TRUE)) {
    install.packages("rvest")
    library("rvest", character.only = TRUE)
}

#--------------Get City Data--------------#

#Get city listings in the US from the Numbeo API

numbeo.url <- "https://www.numbeo.com/api/"
numbeo.key <- "api_key=vjd4t3rcp6nc50"

numbeo.query.term <- "cities?"
numbeo.query.search <- "&country=United%20States"

json_file <- getURL(paste0(numbeo.url, numbeo.query.term, numbeo.key, numbeo.query.search))

cities <- data.frame(fromJSON(json_file,simplifyDataFrame= TRUE))

colnames(cities) <- gsub("cities.", "", colnames(cities), perl = TRUE)

#--------------Get City Indexes--------------#

#Get city indexes from the Numbeo API

numbeo.query.term = "indices?"

for (city in 1:nrow(cities)) {

    tmp_list <- fromJSON(
        getURL(
            paste0(
                numbeo.url, numbeo.query.term, numbeo.key,
                numbeo.query.search,"&city_id=",
                cities$city_id[city])
        )
    )

    n <- names(tmp_list)
    for (i in 1:length(n)) {
        cities[city, n[i]] <- tmp_list[[i]]
    }
}

#--------------Rank City Indexes--------------#

city_rank <- subset(cities, select = c(4, 2, 3, 5, order(names(cities[,6:ncol(cities)]))+5))
city_rank$name <- NULL
city_rank$safety_index <- NULL

neg_val <- c(9, 13)

for (i in 5:ncol(city_rank)) {
    city_rank[,colnames(city_rank[i])] <- rank(
        ifelse(i %in% neg_val,-1, 1)*city_rank[,colnames(city_rank[i])],
        na.last = "keep",
        ties.method = "average")
}

# Resolve NAs by coercing value to city with nearest lat & long
for (j in 5:ncol(city_rank)) {
    tmp <- subset(city_rank, !is.na(city_rank[j]), select = c(3, 4, j))
    for (k in 1:nrow(city_rank)) {
        min_dist <- Inf
        tmp_ind <- 0
        if (is.na(city_rank[k,j])) {
            for (l in 1:nrow(tmp)) {
                if (sqrt((city_rank[k,3] - tmp[l,1])^2 + (city_rank[k,4] - tmp[l,2])^2) < min_dist) {
                    min_dist <- sqrt((city_rank[k,3] - tmp[l,1])^2 + (city_rank[k,4] - tmp[l,2])^2)
                    tmp_ind <- tmp[l,3]
                }
            }
            city_rank[k,j] <- tmp_ind
        }
    }
}