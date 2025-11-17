library(jsonlite)
library(dplyr)

# Get all JSON files from data/annotations
annotation_files <- list.files("data/annotations", 
                               pattern = "\\.json$", 
                               full.names = TRUE)

# Count annotations per character across all files
df <- data.frame(
  trait = character(),
  action = character(),
  assessment = character(),
  rating = numeric(),
  name_post_annotation = character(),
  movie_name = character(),
  stringsAsFactors = FALSE
)

for (file in annotation_files) {
  movie = gsub("data/annotations/", "", gsub(".json", "", file))
  
  movie_df <- data.frame(
    trait = "",
    action = "",
    assessment = "",
    rating = NA,
    name_post_annotation = "",
    movie_name = movie,
    stringsAsFactors = FALSE
  )
  
  data <- fromJSON(file)
  
  for (char_name in names(data)) {
 
    #take data from char with most annotations
    if(nrow(data[[char_name]]) >= nrow(movie_df)){
      movie_df = data[[char_name]]
      movie_df["name_post_annotation"] = char_name
      movie_df["movie_name"] = movie
    }
    # if(tolower(movie) == "beavis and butt head do america"){
    #   temp = movie_df
    # }
  }

  df = dplyr::bind_rows(df, movie_df)
  
}
rm(movie_df, annotation_files)

#average rating by name, trait, and movie file, keep n
df <- df %>%
  dplyr::group_by(name_post_annotation, trait, movie_name) %>%
  dplyr::summarise(
    average_rating = mean(rating, na.rm = TRUE),
    n = n()
  ) %>%
  dplyr::ungroup()

# trait column to wide format
df <- df %>%
  tidyr::pivot_wider(
    names_from = trait,
    values_from = c(average_rating, n),
    names_sep = "_"
  )

# read data/kaggle_data.csv
kaggle_data <- read.csv("data/kaggle_data.csv", stringsAsFactors = FALSE)

df$movie_name <- sapply(df$movie_name, function(x) {
  strsplit(x, "_")[[1]][1]
})

# merge with df by name and movie_name
df <- df %>%
  dplyr::left_join(kaggle_data, by = c("movie_name" = "movie_name"))

df["average_rating_NA"] <- df["n_NA"] <- NULL

#write to file
write.csv(df, "data/data.csv", row.names = FALSE)


#which movie names are not in df
kaggle_movie_names <- unique(kaggle_data$movie_name)
df_movie_names <- unique(df$movie_name)
missing_movie_names <- setdiff(kaggle_movie_names, df_movie_names)
print(missing_movie_names)
