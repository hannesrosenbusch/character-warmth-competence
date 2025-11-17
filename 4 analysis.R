library(brms)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bridgesampling)
library(BayesFactor)

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


# avg warmth comp of genres --------------------------------------------------

library(tidyverse)

library(tidyverse)

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


# main hypotheses ----------------------------------------------------------------
my_priors <- c(prior(normal(0, 1), class = "b"))

# single pred warmth
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

#warmth + genre
lm_warmth_genre <- brm(
  formula = imdb_rating_decimal ~ average_rating_warmth + Drama + Thriller + Comedy + Action + Crime + Romance + Adventure + Sci.Fi + Mystery + Horror +Fantasy + Biography, 
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123
)

#competence
lm_competence <- brm(
  formula = imdb_rating_decimal ~ average_rating_competence, 
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123)

#competence + genre
lm_competence_genre <- brm(
  formula = imdb_rating_decimal ~ average_rating_competence + Drama + Thriller + Comedy + Action + Crime + Romance + Adventure + Sci.Fi + Mystery + Horror +Fantasy + Biography, 
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123
)



#include interaction warmth
lm_warmth_interaction <- brm(
  formula = imdb_rating_decimal ~ 
    average_rating_warmth * (Drama + Thriller + Comedy + Action + Crime + Romance + Adventure + Sci.Fi + Mystery + Horror +Fantasy + Biography),
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123)

#include interaction competence
lm_competence_interaction <- brm(
  formula = imdb_rating_decimal ~ 
    average_rating_competence * (Drama + Thriller + Comedy + Action + Crime + Romance + Adventure + Sci.Fi + Mystery + Horror +Fantasy + Biography),
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123)



# Model comparisons -------------------------------------------------------

#does genre add value
bf(bridge_sampler(lm_warmth_genre), bridge_sampler(lm_warmth)) #evidence ratio in favor of first model
bf(bridge_sampler(lm_competence_genre), bridge_sampler(lm_competence))

#does warmth*genre interaction add value?
bf(bridge_sampler(lm_warmth_interaction), bridge_sampler(lm_warmth_genre)) #evidence ratio in favor of first model

#does competence*genre interaction add value?
bf(bridge_sampler(lm_competence_interaction), bridge_sampler(lm_competence_genre))



# plot simple effects -----------------------------------------------------

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
effects_df$genre <- factor(effects_df$genre, levels = genre_order$genre)
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
  geom_point(size = 2, color = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "steelblue") +
  labs(
    title = element_blank(),
    x = "",
    y = "Correlation with IMDb Rating"
  ) +
  theme_minimal() +
  coord_flip() +
  ylim(c(-0.5, 0.5))
