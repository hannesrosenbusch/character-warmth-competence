library(brms)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bridgesampling)
library(BayesFactor)
library(tidyverse)


# read data
df <- read.csv("data/data.csv")


# explore vars ------------------------------------------------------------

# histograms
summary(df$average_rating_warmth)
summary(df$average_rating_competence)

# summary n
summary(df$n_warmth)
summary(df$n_competence)

# show max/min name_post_annotations
df_temp <- df[df$n_warmth > 20, ]
max_warmth <- df_temp[which.max(df_temp$average_rating_warmth), "name_post_annotation"]
min_warmth <- df_temp[which.min(df_temp$average_rating_warmth), "name_post_annotation"]

#same for competence trait
df_temp <- df[df$n_competence > 20, ]
max_competence <- df_temp[which.max(df_temp$average_rating_competence), "name_post_annotation"]
min_competence <- df_temp[which.min(df_temp$average_rating_competence), "name_post_annotation"]
df_temp[which.min(df_temp$average_rating_competence), "movie_name"]

# correlations
cor.test(df$imdb_rating_decimal, df$average_rating_competence)
cor.test(df$imdb_rating_decimal, df$average_rating_warmth)
cor.test(df$average_rating_warmth, df$average_rating_competence)

# standardize --------------------------------------------------

#standardize dv s
df$imdb_rating_decimal <- scale(df$imdb_rating_decimal)[,1]

#standardize iv s
df$average_rating_warmth <- scale(df$average_rating_warmth)[,1]
df$average_rating_competence <- scale(df$average_rating_competence)[,1]

genre_cols <- c("Romance", "Crime", "Drama", "Thriller", "Comedy", "Action", "Adventure", "Sci.Fi", "Mystery", "Horror", "Fantasy", "Biography")


# avg warmth comp of genres --------------------------------------------------

# Pivot genres
df_plot <- df %>%
  pivot_longer(
    cols = all_of(genre_cols),
    names_to = "genre",
    values_to = "is_genre"
  ) %>%
  filter(is_genre == 1)


plot_trait_by_genre <- function(df, trait_col, trait_name) {
  # Calculate mean per genre for ordering
  genre_order <- df %>%
    group_by(genre) %>%
    summarise(mean_rating = mean(.data[[trait_col]], na.rm = TRUE)) %>%
    arrange(mean_rating)
  
  # Summary stats for plotting mean ± SE
  df_summary <- df %>%
    group_by(genre) %>%
    summarise(
      mean_rating = mean(.data[[trait_col]], na.rm = TRUE),
      se_rating = sd(.data[[trait_col]], na.rm = TRUE)/sqrt(n()),
      .groups = "drop"
    )
  
  # Violin plot
  ggplot(df, aes(x = factor(genre, levels = genre_order$genre), y = .data[[trait_col]])) +
    geom_violin(trim = FALSE, alpha = 0.5, fill = "skyblue") +
    geom_point(data = df_summary, aes(y = mean_rating), color = "black", size = 1) +
    geom_errorbar(data = df_summary,
                  aes(y = NULL, ymin = mean_rating - se_rating, ymax = mean_rating + se_rating),
                  width = 0.2, color = "black") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = paste("Average", trait_name, "by Genre"),
      x = "",
      y = paste(trait_name, "Rating")
    )
}

plot_trait_by_genre(df_plot, "average_rating_competence", "Competence")
plot_trait_by_genre(df_plot, "average_rating_warmth", "Warmth")

# Bayesian ANOVA for Warmth
bf_warmth <- lmBF(
  formula = average_rating_warmth ~ Drama + Thriller + Comedy + Action + Crime + Romance + Adventure + Sci.Fi + Mystery + Horror + Fantasy + Biography,
  data = df[!is.na(df$average_rating_warmth), ],
  whichModels = "all",
  progress = FALSE
)
print(bf_warmth)

# Bayesian ANOVA for competence
bf_competence <- lmBF(
  formula = average_rating_competence ~ Drama + Thriller + Comedy + Action + Crime + Romance + Adventure + Sci.Fi + Mystery + Horror + Fantasy + Biography,
  data = df[!is.na(df$average_rating_competence), ],
  whichModels = "all",
  progress = FALSE
)
print(bf_competence)


# main effects ----------------------------------------------------------------
my_priors <- c(prior(normal(0, 1), class = "b"))

# # single pred warmth
lm_warmth <- brm(
  formula = imdb_rating_decimal ~ average_rating_warmth,
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123
)
print(lm_warmth)
bayes_factor(lm_warmth, lm_null)

# #competence
lm_competence <- brm(
  formula = imdb_rating_decimal ~ average_rating_competence,
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123)
print(lm_competence)
bayes_factor(lm_competence, lm_null)


# PREP MULTIPLE MEMBERSHIP -----------------------------------------------------

# For each movie, get genres that are positive; only get three genres per movie as all genres leads to convergence errors
library(dplyr)
library(purrr)

set.seed(42)

movies_limited <- df %>%
  rowwise() %>%
  mutate(
    # list of genres this movie belongs to
    genres_list = list(genre_cols[which(c_across(all_of(genre_cols)) == 1)]),
    # limit to max 3 genres by random sampling
    genres_sample = list(sample(genres_list, size = min(3, length(genres_list))))
  ) %>%
  ungroup()

# Create g1, g2, g3 and w1, w2, w3 columns
movies_final <- movies_limited %>%
  mutate(
    g1 = map_chr(genres_sample, ~ .x[1] %||% NA_character_),
    g2 = map_chr(genres_sample, ~ .x[2] %||% NA_character_),
    g3 = map_chr(genres_sample, ~ .x[3] %||% NA_character_),
    w1 = map_dbl(genres_sample, ~ if(length(.x) == 1) 1 else if(length(.x) == 2) 0.5 else 1/3),
    w2 = map_dbl(genres_sample, ~ if(length(.x) == 1) 0 else if(length(.x) == 2) 0.5 else 1/3),
    w3 = map_dbl(genres_sample, ~ if(length(.x) <= 2) 0 else 1/3)
  ) %>%
  select(-genres_list, -genres_sample)




# ---- Fit multiple membership model WARMTH----


mm_fit_null <- brm(
  imdb_rating_decimal ~ average_rating_warmth +
    (1 | mm(g1, g2, weights = cbind(w1, w2))),
  data = movies_final,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 4000,
  save_pars = save_pars(all = TRUE)
)


mm_fit <- brm(
  imdb_rating_decimal ~ average_rating_warmth +
    (1 + average_rating_warmth | mm(g1, g2, weights = cbind(w1, w2))),
  data = movies_final,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 4000,
  save_pars = save_pars(all = TRUE)
)

bayes_factor(mm_fit, mm_fit_null)

# Get posterior draws
posterior <- as_draws_df(mm_fit)

# Extract the population-level slope
beta_warmth <- posterior %>% select(b_average_rating_warmth)

# Extract random effects (genre-specific slope deviations)
ranef_df <- ranef(mm_fit)$mm  # list of arrays: intercepts and slopes


# Example: get slope mean + 95% CI for each genre
# extract the slope deviations matrix
slope_mat <- ranef_df[, , "average_rating_warmth"]

# create data frame
genre_slopes <- tibble(
  genre = rownames(slope_mat),
  slope_mean = slope_mat[, "Estimate"] + fixef(mm_fit)["average_rating_warmth", "Estimate"],
  slope_lower = slope_mat[, "Q2.5"] + fixef(mm_fit)["average_rating_warmth", "Estimate"],
  slope_upper = slope_mat[, "Q97.5"] + fixef(mm_fit)["average_rating_warmth", "Estimate"]
)

ggplot(genre_slopes, aes(x = reorder(genre, slope_mean), y = slope_mean)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = slope_lower, ymax = slope_upper), width = 0.2, color = "steelblue") +
  coord_flip() +
  labs(
    x = "Genre",
    y = "Slope of Warmth on IMDb Rating",
  ) +
  theme_minimal(base_size = 14)

# ---- Fit multiple membership model Competence----

mm_fit_null_comp <- brm(
  imdb_rating_decimal ~ average_rating_competence +
    (1 | mm(g1, g2, weights = cbind(w1, w2))),
  data = movies_final,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 4000,
  save_pars = save_pars(all = TRUE)
)


mm_fit_comp <- brm(
  imdb_rating_decimal ~ average_rating_competence +
    (1 + average_rating_competence | mm(g1, g2, weights = cbind(w1, w2))),
  data = movies_final,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 4000,
  save_pars = save_pars(all = TRUE)
)

bayes_factor(mm_fit_comp, mm_fit_null_comp)



#plot simple correlation

warmth_effects = list()
for(g in genre_cols){
  bf_full = correlationBF(df[df[[g]] == 1, "average_rating_warmth"], df[df[[g]] == 1, "imdb_rating_decimal"])
  bf = as.vector(bf_full)
  posterior_samples <- posterior(bf_full, iterations = 10000)
  corr <- mean(posterior_samples[, "rho"])
  ci <- quantile(posterior_samples[, "rho"], probs = c(0.025, 0.975))
  warmth_effects[[g]] = list(bf = bf, corr = corr, ci = ci)
}

competence_effects = list()
for(g in genre_cols){
  bf_full = correlationBF(df[df[[g]] == 1, "average_rating_competence"], df[df[[g]] == 1, "imdb_rating_decimal"])
  bf = as.vector(bf_full)
  posterior_samples <- posterior(bf_full, iterations = 10000)
  corr <- mean(posterior_samples[, "rho"])
  ci <- quantile(posterior_samples[, "rho"], probs = c(0.025, 0.975))
  competence_effects[[g]] = list(bf = bf, corr = corr, ci = ci)
}

#plot results
effects_df <- data.frame(
  genre = character(),
  trait = character(),
  bf = numeric(),
  corr = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  stringsAsFactors = FALSE
)

for(g in genre_cols){
  effects_df <- rbind(effects_df, data.frame(
    genre = g,
    trait = "Warmth",
    bf = warmth_effects[[g]]$bf,
    corr = warmth_effects[[g]]$corr,
    ci_lower = warmth_effects[[g]]$ci[1],
    ci_upper = warmth_effects[[g]]$ci[2]
  ))
  
  effects_df <- rbind(effects_df, data.frame(
    genre = g,
    trait = "Competence",
    bf = competence_effects[[g]]$bf,
    corr = competence_effects[[g]]$corr,
    ci_lower = competence_effects[[g]]$ci[1],
    ci_upper = competence_effects[[g]]$ci[2]
  ))
}
library(ggplot2)

# Warmth plot
warmth_df <- effects_df %>% 
  filter(trait == "Warmth") %>%
  arrange(corr) %>%                       # sort by correlation
  mutate(genre = factor(genre, levels = genre))  # set factor order

ggplot(warmth_df, aes(x = genre, y = corr)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = element_blank(),
    x = "",
    y = "Correlation with IMDb Rating"
  ) +
  theme_minimal() +
  coord_flip() +
  ylim(c(-0.5, 0.5))

# Competence plot
competence_df <- effects_df %>% 
  filter(trait == "Competence") %>%
  arrange(corr) %>%
  mutate(genre = factor(genre, levels = genre))

ggplot(competence_df, aes(x = genre, y = corr)) +
  geom_point(size = 2, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
  labs(
    title = element_blank(),
    x = "",
    y = "Correlation with IMDb Rating"
  ) +
  theme_minimal() +
  coord_flip() +
  ylim(c(-0.5, 0.5))

