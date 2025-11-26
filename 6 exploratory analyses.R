#gender
df <- read.csv("data/data.csv")
gender = read.csv("data/protagonist_gender.csv")

#merge on imdb_id
df = merge(df, gender, by = "imdb_id")

#if gender value lower includes female, set it to Female
df$gender <- tolower(df$gender)
for(i in 1:nrow(df)){
  if(grepl("female", df$gender[i])){
    df$gender[i] = "female"
  }
}

#descriptive of variable gender
table(df$gender)

#remove rows that arent lower() == "male" or "female"
df = df[grepl("male", df$gender),]

#standardize imdb_rating_decimal
df$imdb_rating_decimal <- scale(df$imdb_rating_decimal)

my_priors <- c(
  prior(normal(0, 1), class = "b")  # prior for coefficients
)

#base model
lm_competence <- brm(
  formula = imdb_rating_decimal ~ average_rating_competence + gender,
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123)

print(lm_competence)

#add interaction
lm_competence_interaction <- brm(
  formula = imdb_rating_decimal ~ average_rating_competence * gender,
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123)
print(lm_competence_interaction)
bayes_factor(lm_competence_interaction, lm_competence)

#check simple effects
plot(conditional_effects(lm_competence_interaction), points = TRUE)


#same for average_rating_warmth
lm_warmth = brm(
  formula = imdb_rating_decimal ~ average_rating_warmth + gender,
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123)
print(lm_warmth)

lm_warmth_interaction = brm(
  formula = imdb_rating_decimal ~ average_rating_competence * gender,
  data = df,
  family = gaussian(),   # numeric outcome
  prior = my_priors,
  chains = 4,
  iter = 4000,
  cores = 4,
  seed = 123)
bayes_factor(lm_warmth_interaction, lm_warmth)

plot(conditional_effects(lm_competence_interaction), points = TRUE)

