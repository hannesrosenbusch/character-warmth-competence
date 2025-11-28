library(brms)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bridgesampling)
library(BayesFactor)
library(tidyverse)
library(patchwork)

# HELPER FUNCTIONS --------------------------------------------------------

to_apa_scientific <- function(x, digits = 3) {
  s <- format(x, scientific = TRUE, digits = digits)
  parts <- strsplit(s, "e")[[1]]
  coeff <- parts[1]
  exp <- as.integer(parts[2])
  paste0(coeff, " × 10^", exp)
}

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
    geom_violin(trim = FALSE, alpha = 0.5, fill = "lightgray") +
    geom_point(data = df_summary, aes(y = mean_rating), color = "black", size = 1) +
    geom_errorbar(data = df_summary,
                  aes(y = NULL, ymin = mean_rating - se_rating, ymax = mean_rating + se_rating),
                  width = 0.2, color = "black") +
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size = 14)) +
    labs(
      x = "",
      y = paste(trait_name, "Rating")
    )
}

descriptives_plot <- function(data, var1, var2, var3) {
  
  # Map old column names to pretty labels
  old_names <- c(var1, var2, var3)
  new_names <- c("Warmth", "Competence", "IMDb Rating")
  name_map <- setNames(new_names, old_names)
  
  # Long format for violin/jitter
  df_long <- data %>%
    select(name_post_annotation, n_warmth, n_competence, all_of(old_names)) %>%
    rename_with(~ name_map[.x], .cols = all_of(old_names)) %>%
    pivot_longer(cols = all_of(new_names),
                 names_to = "variable",
                 values_to = "value")
  
  df_long$variable <- factor(df_long$variable,
                             levels = c("IMDb Rating", "Warmth", "Competence"))
  

  df_filtered <- df_long %>%
    mutate(
      restriction_ok =
        case_when(
          variable == "Warmth" ~ n_warmth > 20,
          variable == "Competence" ~ n_competence > 20,
          variable == "IMDb Rating" ~ TRUE,  # or add restriction if needed
          TRUE ~ TRUE
        )
    ) %>%
    filter(restriction_ok)
  
  # compute min/max labels *only among restricted rows*
  label_df <- df_filtered %>%
    group_by(variable) %>%
    summarise(
      min_value = min(value, na.rm = TRUE),
      max_value = max(value, na.rm = TRUE),
      min_name = name_post_annotation[which.min(value)],
      max_name = name_post_annotation[which.max(value)]
    ) %>%
    pivot_longer(cols = c(min_value, max_value, min_name, max_name),
                 names_to = c("type", ".value"),
                 names_pattern = "(min|max)_(.*)") %>%
    rename(value = value, name = name)
  

  p_violin <- ggplot(df_long, aes(x = variable, y = value)) +
    geom_violin(fill = "white", color = "black") +
    geom_jitter(width = 0.2, alpha = 0.1, size = 1) +
    geom_text(
      data = label_df,
      aes(label = name, y = value),
      vjust = -0.3,
      size = 3,
      fontface = "bold"
    ) +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    theme(
      text = element_text(size = 14),
      legend.position = "none",
      axis.title = element_blank()
    ) +
    scale_y_continuous(breaks = seq(-2.5, 10, by = 2.5))
  
  genre_cols <- c("Drama", "Thriller", "Comedy", "Action", "Crime",
                  "Romance", "Adventure", "Sci.Fi", "Mystery",
                  "Horror", "Fantasy", "Biography")
  
  genre_counts <- data %>%
    select(all_of(genre_cols)) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    pivot_longer(cols = everything(),
                 names_to = "Genre",
                 values_to = "Count")
  
  p_genre <- ggplot(genre_counts, aes(x = reorder(Genre, -Count), y = Count)) +
    geom_bar(stat = "identity", color = "black", fill = "white") +
    theme_bw() +
    labs(x = NULL, y = NULL) +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Combine the two plots
  p_violin + p_genre + patchwork::plot_layout(widths = c(1, 1))
}


get_coef_mean <- function(fit, coef_name) {
  
  draws_list = as_draws(fit)
  # Check if coefficient exists
  if (!coef_name %in% names(draws_list[[1]])) {
    stop(paste("Coefficient", coef_name, "not found in draws"))
  }
  
  # Combine all chains
  all_draws <- unlist(lapply(draws_list, function(chain) chain[[coef_name]]))
  
  # Compute mean
  mean(all_draws)
}

# read data ---------------------------------------------------------------

df <- read.csv("data/data.csv")
my_priors <- c(prior(normal(0, 1), class = "b"))
set.seed(42)

# explore vars ------------------------------------------------------------

# histograms
summary(df$average_rating_warmth)
sd(df$average_rating_warmth, na.rm = T)
summary(df$average_rating_competence)
sd(df$average_rating_competence, na.rm = T)
summary(df$imdb_rating_decimal)
needed_later = sd(df$imdb_rating_decimal, na.rm = T)

# summary n
mean(df$n_warmth, na.rm = T)
mean(df$n_competence, na.rm = T)

# correlations
cor.test(df$imdb_rating_decimal, df$average_rating_competence)
cor.test(df$imdb_rating_decimal, df$average_rating_warmth)
cor.test(df$average_rating_warmth, df$average_rating_competence)

#count number of times that multiple people were chosen as protagonist
sum(grepl("and", df$name_post_annotation))

png("figures/descriptives_plot.png", width = 10, height = 5, units = "in", res = 300)
descriptives_plot(df, 
                  var1 = "average_rating_warmth",
                  var2 = "average_rating_competence",
                  var3 = "imdb_rating_decimal")
dev.off()

# standardize --------------------------------------------------

#standardize dv s
df$imdb_rating_decimal <- scale(df$imdb_rating_decimal)[,1]

#standardize iv s
df$average_rating_warmth_unscaled <- df$average_rating_warmth
df$average_rating_competence_unscaled <- df$average_rating_competence
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

a = plot_trait_by_genre(df_plot, "average_rating_warmth_unscaled", "Warmth")
b = plot_trait_by_genre(df_plot, "average_rating_competence_unscaled", "Competence")
png("figures/genre_traits.png", width = 10, height = 6, units = "in", res = 300)
a + b
dev.off()

# compare traits across genres --------------------------------------------

#warmth
null_warmth <- brm(average_rating_warmth ~ 1,
                    data = df[!is.na(df$average_rating_warmth), ],  
                    family = gaussian(),   # numeric outcome
                    chains = 4,
                    iter = 4000,
                    cores = 4,
                    seed = 42)

warmth_across_genres <- brm(average_rating_warmth ~ Drama + Thriller + Comedy + Action + Crime + Romance + Adventure + Sci.Fi + Mystery + Horror + Fantasy + Biography,
                                data = df[!is.na(df$average_rating_warmth), ],  
                                family = gaussian(),   # numeric outcome
                                chains = 4,
                                iter = 4000,
                                cores = 4,
                                prior = my_priors,
                                seed = 42)
bf1 = bayes_factor(warmth_across_genres, null_warmth)
to_apa_scientific(bf1)

#competence
null_competence <- brm(average_rating_competence ~ 1,
  data = df[!is.na(df$average_rating_competence), ],  
  family = gaussian(),   # numeric outcome
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 42)

competence_across_genres <- brm(average_rating_competence ~ Drama + Thriller + Comedy + Action + Crime + Romance + Adventure + Sci.Fi + Mystery + Horror + Fantasy + Biography,
  data = df[!is.na(df$average_rating_competence), ],  
  family = gaussian(),   # numeric outcome
  chains = 4,
  iter = 4000,
  cores = 4,
  prior = my_priors,
  seed = 42)

bf2 = bayes_factor(competence_across_genres, null_competence)
to_apa_scientific(bf2)

# main effects ----------------------------------------------------------------

#lm null

lm_null <- brm(
  formula = imdb_rating_decimal ~ 1,
  data = df,
  family = gaussian(),   # numeric outcome
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 42
)

# # single pred warmth
lm_warmth <- brm(
  formula = imdb_rating_decimal ~ average_rating_warmth,
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 42
)

bf3 = bayes_factor(lm_warmth, lm_null)
to_apa_scientific(bf3)
print(lm_warmth)
#extract coeff
# compute unstandardized effect size
needed_later *get_coef_mean(lm_warmth, "b_average_rating_warmth")


# #competence
lm_competence <- brm(
  formula = imdb_rating_decimal ~ average_rating_competence,
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 42)

bf4 = bayes_factor(lm_competence, lm_null)
to_apa_scientific(bf4)
print(lm_competence)
#compute unstandardized effect size
needed_later * get_coef_mean(lm_competence, "b_average_rating_competence")


# PREP MULTIPLE MEMBERSHIP -----------------------------------------------------

# For each movie, get genres that are positive; only get three genres per movie as all genres leads to convergence errors
library(dplyr)
library(purrr)

set.seed(42)

#check how many movies have more than 3 genres
mean(rowSums(df[!is.na(df$average_rating_warmth),  genre_cols]) > 3)

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


mm_null_warmth <- brm(
  imdb_rating_decimal ~ average_rating_warmth +
    (1 | mm(g1, g2, weights = cbind(w1, w2))),
  data = movies_final,
  family = gaussian(),
  chains = 4,
  cores = 4,
  seed = 42,
  iter = 4000,
  save_pars = save_pars(all = TRUE)
)


mm_warmth <- brm(
  imdb_rating_decimal ~ average_rating_warmth +
    (1 + average_rating_warmth | mm(g1, g2, weights = cbind(w1, w2))),
  data = movies_final,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 4000,
  prior = my_priors,
  seed = 42,
  control = list(adapt_delta = 0.9),
  save_pars = save_pars(all = TRUE)
)

bf5 = bayes_factor(mm_warmth, mm_null_warmth)
bf5

# # Get posterior draws
# posterior <- as_draws_df(mm_fit)
# 
# # Extract the population-level slope
# beta_warmth <- posterior %>% select(b_average_rating_warmth)
# 
# # Extract random effects (genre-specific slope deviations)
# ranef_df <- ranef(mm_fit)$mm  # list of arrays: intercepts and slopes
# 
# 
# # Example: get slope mean + 95% CI for each genre
# # extract the slope deviations matrix
# slope_mat <- ranef_df[, , "average_rating_warmth"]
# 
# # create data frame
# genre_slopes <- tibble(
#   genre = rownames(slope_mat),
#   slope_mean = slope_mat[, "Estimate"] + fixef(mm_fit)["average_rating_warmth", "Estimate"],
#   slope_lower = slope_mat[, "Q2.5"] + fixef(mm_fit)["average_rating_warmth", "Estimate"],
#   slope_upper = slope_mat[, "Q97.5"] + fixef(mm_fit)["average_rating_warmth", "Estimate"]
# )
# 
# ggplot(genre_slopes, aes(x = reorder(genre, slope_mean), y = slope_mean)) +
#   geom_point(size = 3, color = "steelblue") +
#   geom_errorbar(aes(ymin = slope_lower, ymax = slope_upper), width = 0.2, color = "steelblue") +
#   coord_flip() +
#   labs(
#     x = "Genre",
#     y = "Slope of Warmth on IMDb Rating",
#   ) +
#   theme_minimal(base_size = 14)

# ---- Fit multiple membership model Competence----

mm_null_comp <- brm(
  imdb_rating_decimal ~ average_rating_competence +
    (1 | mm(g1, g2, weights = cbind(w1, w2))),
  data = movies_final,
  family = gaussian(),
  chains = 4,
  cores = 4,
  seed = 42,
  iter = 4000,
  save_pars = save_pars(all = TRUE)
)


mm_competence <- brm(
  imdb_rating_decimal ~ average_rating_competence +
    (1 + average_rating_competence | mm(g1, g2, weights = cbind(w1, w2))),
  data = movies_final,
  family = gaussian(),
  chains = 4,
  cores = 4,
  prior = my_priors,
  seed = 42,
  iter = 4000,
  save_pars = save_pars(all = TRUE)
)

bf6 = bayes_factor(mm_competence, mm_null_comp)
bf6


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

#check Rhat and ESS of every model. save in c() and print
model_list = list(
  lm_warmth,
  lm_competence,
  mm_warmth,
  mm_competence
)

for(m in model_list){
  vals = rhat(m)
  if(any(vals[!is.na(vals)]>1.01)){
    print(paste("Warning: Rhat > 1.01 in model", deparse(substitute(m))))
  }
}
