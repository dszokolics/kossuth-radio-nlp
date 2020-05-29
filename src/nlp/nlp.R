library(tidytext)
library(tidyverse)
library(purrr)
library(topicmodels)
library(quanteda)
library(quanteda.textmodels)
library(hunspell)
library(RcppRoll)
library(zoo)

Sys.setlocale("LC_ALL", 'Hungarian_Hungary.1250')
Sys.setenv(DICPATH = "data/utils/hu_HU/")

# Read the files
data <- read.csv("data/processed/almost_tidy_text_cl.csv",
                 stringsAsFactors=FALSE,
                 encoding="UTF-8")

names <- read.csv("data/processed/subjects_categorized.csv",
                  stringsAsFactors=FALSE,
                  encoding="UTF-8")

# Basic cleaning on the names
names <- names %>%
  filter(count > 15, category != "") %>%
  mutate(name = ifelse(alias == "", str_remove(name, ','), alias)) %>%
  select(-alias) %>%
  mutate(name = str_replace_all(name, c("o" = "\\u0151", "u" = "\\u0171")))

# Create a name column with words which are possibly names: this will speed up feature generation
data <- data %>%
  filter(str_length(text) > 15) %>%
  mutate(names = lapply(str_extract_all(text,
                                        "(?<=^|\\s)[A-ZÁÉÍÖ\\u0150ÚÜ\\u0170]{1}[a-záéíóö\\u0151úü\\u0171]+"),
                        function(x) paste(x, collapse = ' ')))

# Create one column for each subject
for (row in 1:nrow(names)){
  name <- names[row, 'name']
  version_1 <- names[row, 'version_1']
  version_2 <- names[row, 'version_2']
  data <- data %>%
    mutate(!!name := agrepl(name, names, fixed = FALSE, max.distance = 0.1))
}

# Load stopword list and sentiment dictionary
stopwords <-read.csv("data/utils/stopwords-hu.txt",
                     stringsAsFactors=FALSE,
                     encoding="UTF-8")

sentiments <- read.csv("data/utils/sentiments-hu.csv",
                       stringsAsFactors = FALSE,
                       encoding = "UTF-8")

# Get sentiment for each line and a flag if one name presents in it
varnames <- names$name
tokenized <- data %>%
  filter_at(vars(varnames), any_vars(.)) %>%
  unnest_tokens(word, text) %>%
  filter(str_length(word) > 2) %>%
  anti_join(stopwords) %>%
  left_join(sentiments)

# What are the most common sentimented words?
sentimented_words <- tokenized %>%
  filter(sentiment != 0) %>%
  group_by(word, sentiment) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

# Aggregate to paragraph level
d2 <- tokenized %>%
  group_by(date, paragraph) %>%
  mutate(sentiment = sum(sentiment, na.rm = TRUE)) %>%
  summarise_at(vars(c(varnames, sentiment)), max) %>%
  ungroup()

# Create a tidy table where each row represents one subject's mentions on a given date
mentions <- d2 %>%
  select(-paragraph, -sentiment) %>%
  group_by(date) %>%
  summarise_at(vars(names$name), sum) %>%
  ungroup() %>%
  pivot_longer(-date, "subject", values_to = "mention") %>%
  filter(mention > 0)

# Get sentiments for the subjects
subjects <- d2 %>%
  mutate_at(vars(varnames), .funs = ~ . * sentiment) %>%
  filter(sentiment != 0) %>%
  group_by(date) %>%
  summarise_at(vars(names$name), sum) %>%
  ungroup() %>%
  pivot_longer(-date, "subject", values_to = "sentiment") %>%
  filter(sentiment != 0) %>%
  full_join(mentions) %>%
  replace_na(list(sentiment = 0)) %>%
  arrange(date)

# Count the mentions for each subject and compare it with the reference counts from the website
all_mentions <- subjects %>%
  mutate(mention = ifelse(mention > 0, 1, 0)) %>%
  group_by(subject) %>%
  summarise(mention = sum(mention), sentiment = sum(sentiment)) %>%
  ungroup() %>%
  full_join(select(names, name, count), by = c("subject"="name")) %>%
  mutate(mentions_found = mention / count)

# Daily mentions and sentiments towards each "party"
parties <- subjects %>%
  left_join(select(names, name, category), by = c("subject" = "name")) %>%
  group_by(date, category) %>%
  summarise(sentiment = sum(sentiment), mention = sum(mention)) %>%
  ungroup() %>%
  mutate(sentiment_avg = sentiment/mention)

# Wordfish
# Stem words in LibreOffice
words_to_stem <- tokenized %>%
  select(word) %>%
  unique()

# I can't set up the stemmer internally, so I'll export the words, stem it, and import the results.
# I used a Libre Office macro for stemming.
# I think I have some problem with my system's language setting, because I always have
# problems with anything language or encoding related.
words_to_stem %>%
  write.table("data/processed/words_to_stem.txt",
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE,
              fileEncoding = "iso-8859-2")

words_stemmed <- read.csv("data/processed/words_stemmed_2.txt",
                          encoding = "iso-8859-2",
                          header = FALSE,
                          col.names = "text") %>%
  mutate(word = word(text, 1),
         stemmed = word(text, -1)) %>%
  select(-text) %>%
  mutate(word = str_replace_all(word, c("o" = "\\u0151", "u" = "\\u0171")),
         stemmed = str_replace_all(stemmed, c("o" = "\\u0151", "u" = "\\u0171")))

# Add stemmed words to the tokenized data
tokens_stemmed <- tokenized %>%
  left_join(words_stemmed) %>%
  filter(!is.na(stemmed), is.na(as.numeric(stemmed))) %>%
  anti_join(select(unnest_tokens(names, word, name), word),
            by = c("stemmed" = "word"))

# Free up some space
rm("data", "d2", "tokenized", "words_to_stem")


### Party sentiments
# Basic smoothed plot
parties %>%
  ggplot(aes(x=date, y=sentiment_avg, group=category, color=category)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k=100), se = FALSE)

# Calculate sentiments for each groups and add rolling mean for smoothing
parties_m <- parties %>%
  mutate(year_month = as.yearmon(date)) %>%
  group_by(category, year_month) %>%
  summarize(sentiment_avg = mean(sentiment_avg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sentiment_sm = ave(sentiment_avg,
                             category,
                             FUN = function(x) roll_mean(x, 5, fill = NA,
                                                          weights = c(1,2,3,2,1)))) %>%
  mutate(sentiment_sm = ifelse(is.na(sentiment_sm),
                               sentiment_avg,
                               sentiment_sm)) %>%
  filter(year_month < "1990-12") %>%
  group_by(year_month) %>%
  mutate(sentiment_monthly = mean(sentiment_sm)) %>%
  ungroup() %>%
  mutate(sentiment_dev = sentiment_sm - sentiment_monthly)

# Plot sentiments for groups
parties_m %>%
  ggplot(aes(x = year_month, y = sentiment_sm, group = category, color = category)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  xlab("Date") +
  ylab("Sentiments (smoothed)") +
  ggtitle("Sentiment towards political groups") +
  geom_vline(xintercept = as.yearmon("1988-08-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-07-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1988-11-24"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1989-10-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-05-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  annotate("text",
           x = c(as.yearmon("1988-08-31"),
                 as.yearmon("1990-07-31"),
                 as.yearmon("1989-01-01"),
                 as.yearmon("1989-10-23"),
                 as.yearmon("1990-05-23")),
           y = c(0.7, 0.6, 0.65, 0.7, 0.65),
           label = c("Hajdú, István",
                     "Gombár, Csaba",
                     "Németh, Miklós",
                     "Proclamation of the republic",
                     "Antall, József")) +
  labs(color = "Group")

# Redo the previous one but with demeaned sentiments
parties_m %>%
  ggplot(aes(x = year_month, y = sentiment_dev, group = category, color = category)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  xlab("Date") +
  ylab("Sentiments (smoothed)") +
  ggtitle("Relative sentiments for the groups") +
  geom_vline(xintercept = as.yearmon("1988-08-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-07-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1988-11-24"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1989-10-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-05-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  annotate("text",
           x = c(as.yearmon("1988-08-31"),
                 as.yearmon("1990-07-31"),
                 as.yearmon("1989-01-01"),
                 as.yearmon("1989-10-23"),
                 as.yearmon("1990-05-23")),
           y = c(0.35, 0.35, 0.25, 0.3, 0.25),
           label = c("Hajdú, István",
                     "Gombár, Csaba",
                     "Németh, Miklós",
                     "Proclamation of the republic",
                     "Antall, József")) +
  labs(color = "Group")


### Word counts

wordcounts <- tokens_stemmed %>%
  select(date, paragraph, stemmed) %>%
  distinct() %>%
  group_by(stemmed) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(n > 10) %>%
  arrange(desc(n))

# Visualize word counts
wordcounts %>%
  head(15) %>%
  ggplot(aes(x = reorder(stemmed, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  ggtitle("Word counts") +
  ylab("Occurences") +
  xlab("word") +
  scale_x_discrete(labels = c("economy", "tells", "negotiation",
                              "minister of foreign affairs", "government", "question",
                              "Soviet Union", "holds/believes", "country", "politics",
                              "leader", "America", "Hungarian", "Soviet", "president"))
  

### Further data processing

# What I'm doing here is basically pivoting all my boolean columns
# and creating one row for each occurence of each subject
stemmed_long <- list()
i <- 1
for(name in varnames){
  stemmed_long[[i]] <- data.frame(tokens_stemmed %>%
                                    select(date, stemmed, sentiment, name) %>%
                                    filter(!!as.name(name)) %>%
                                    mutate(subject = name) %>%
                                    select(-!!as.name(name)))
  i <- i + 1
}

stemmed_long <- do.call("rbind", stemmed_long)


# Get top words for each group
stemmed_long %>%
  left_join(select(names, name, category), by = c("subject" = "name")) %>%
  select(category, stemmed) %>%
  group_by(category, stemmed) %>%
  summarize(n = n()) %>%
  top_n(15, n) %>%
  ungroup() %>%
  arrange(category, -n) %>%
  ggplot(aes( reorder(stemmed, n), n, fill = factor(category))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ category, scales = "free") +
  coord_flip()

  
### Topic analysis
term_matrix <- tokens_stemmed %>%
  anti_join(filter(wordcounts, n > 2000)) %>%
  group_by(date, paragraph, stemmed) %>%
  summarise(n = n()) %>%
  mutate(date_p = paste(date, paragraph)) %>%
  cast_dtm(date_p, stemmed, n)

# LDA
paragraph_lda <- LDA(term_matrix, k = 6, control = list(seed = 1234))
paragraph_topics <- tidy(paragraph_lda, matrix = "beta")

# Get top 15 words per topic
top_terms <- paragraph_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualize them
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  xlab("Terms") +
  ylab("Beta") +
  ggtitle("LDA results")


### Wordfish

# Function for creating document frequency matrices
get_dfm <- function(input_df, date_min, date_max){
  input_df %>%
    filter(date >= date_min, date < date_max) %>%
    select(-sentiment) %>%
    left_join(select(names, name, category), by = c("subject" = "name")) %>%
    anti_join(filter(wordcounts, n > 2000)) %>%
    mutate(year_month = as.yearmon(date)) %>%
    group_by(category, year_month) %>%
    summarise(text = paste(stemmed, collapse = " ")) %>%
    ungroup() %>%
    corpus() %>%
    dfm(remove_numbers = TRUE) %>%
    dfm_trim(min_docfreq = 15)
}

# Build a wordfish model on the whole dataset
dfm <- get_dfm(stemmed_long, "1980-01-01", "1991-01-01")

text_model <- dfm %>%
  textmodel_wordfish()

# Get word data
model_word_data <- data.frame(beta = text_model$beta,
                              psi = text_model$psi,
                              features = text_model$features) %>%
  arrange(beta)

# Get group data
model_party_data <- data.frame(alpha = text_model$alpha,
                               theta = text_model$theta,
                               party = docvars(dfm, "category"),
                               year_month = docvars(dfm, "year_month"))

# Visualize group data
model_party_data %>%
  ggplot(aes(x = year_month,
             y = theta,
             color = party,
             group = party,
             label = year_month)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  xlab("Date") +
  ylab("Scale (theta)") +
  ggtitle("Wordfish scaling") +
  geom_vline(xintercept = as.yearmon("1988-08-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-07-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1988-11-24"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1989-10-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-05-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  labs(color = "Group")

# Get top and bottom 10 words
model_word_data %>%
  head(10) %>%
  mutate(sign = "Negative") %>%
  rbind(model_word_data %>% tail(10) %>% mutate(sign = "Positive")) %>%
  mutate(beta = abs(beta)) %>%
  ggplot(aes(x = reorder(features, beta), y = beta, fill = sign)) +
  geom_col() +
  xlab("Beta") +
  ylab("Features") +
  ggtitle("Wordfish word coefficients") +
  coord_flip()


# Build a wordfish model based on only local subjects
dfm_local <- stemmed_long %>%
  left_join(select(names, name, foreigner), by = c("subject" = "name")) %>%
  filter(is.na(foreigner)) %>%
  select(-foreigner) %>%
  get_dfm("1980-01-01", "1991-01-01")

text_model_local <- dfm_local %>%
  textmodel_wordfish()

# Get word data
model_word_data_local <- data.frame(beta = text_model_local$beta,
                              psi = text_model_local$psi,
                              features = text_model_local$features) %>%
  arrange(beta)

# Get group data
model_party_data_local <- data.frame(alpha = text_model_local$alpha,
                               theta = text_model_local$theta,
                               party = docvars(dfm_local, "category"),
                               year_month = docvars(dfm_local, "year_month"))

# Visualize group data
model_party_data_local %>%
  ggplot(aes(x = year_month,
             y = theta,
             color = party,
             group = party,
             label = year_month)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  xlab("Date") +
  ylab("Scale (theta)") +
  ggtitle("Wordfish scaling") +
  geom_vline(xintercept = as.yearmon("1988-08-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-07-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1988-11-24"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1989-10-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-05-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  labs(color = "Group")

# Get top and bottom 10 words
model_word_data_local %>%
  head(10) %>%
  mutate(sign = "Negative") %>%
  rbind(model_word_data %>% tail(10) %>% mutate(sign = "Positive")) %>%
  mutate(beta = abs(beta)) %>%
  ggplot(aes(x = reorder(features, beta), y = beta, fill = sign)) +
  geom_col() +
  xlab("Beta") +
  ylab("Features") +
  ggtitle("Wordfish word coefficients") +
  coord_flip()


# Create a wordfish model with only the sentimented words
dfm_sentiment <- stemmed_long %>%
  filter(!is.na(sentiment)) %>%
  get_dfm("1980-01-01", "1991-01-01")

text_model_sentiment <- dfm_sentiment %>%
  textmodel_wordfish()

model_word_data_sentiment <- data.frame(beta = text_model_sentiment$beta,
                                        psi = text_model_sentiment$psi,
                                        features = text_model_sentiment$features) %>%
  arrange(beta)

model_party_data_sentiment <- data.frame(alpha = text_model_sentiment$alpha,
                                         theta = text_model_sentiment$theta,
                                         party = docvars(dfm_sentiment, "category"),
                                         year_month = docvars(dfm_sentiment, "year_month"))

model_party_data_sentiment %>%
  ggplot(aes(x = year_month,
             y = theta,
             color = party,
             group = party,
             label = year_month)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  xlab("Date") +
  ylab("Scale (theta)") +
  ggtitle("Wordfish scaling for sentimented words") +
  geom_vline(xintercept = as.yearmon("1988-08-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-07-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1988-11-24"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1989-10-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.yearmon("1990-05-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  labs(color = "Group")

# Get top and bottom 10 words
model_word_data_sentiment %>%
  head(10) %>%
  ggplot(aes(x = reorder(features, beta), y = beta)) +
  geom_col(fill = "red") +
  ylab("Beta") +
  ggtitle("Wordfish word coefficients") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  labs(subtitle = "Positive") +
  scale_x_discrete(labels = c("Satan", "funeral", "solidarity", "deficit",
                              "repressive", "noble", "misunderstanding", "conciliatory",
                              "loyal", "reconciliation"))

# Get top and bottom 10 words
model_word_data_sentiment %>%
  tail(10) %>%
  ggplot(aes(x = reorder(features, beta), y = beta)) +
  geom_col(fill = "blue") +
  ylab("Beta") +
  ggtitle(" ") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  labs(subtitle = "Negative") +
  scale_x_discrete(labels = c("offensive", "slump", "evil", "abuse", "unsolved",
                              "responsible", "devil", "uncleared", "like", "dogmatic"))




# Create a wordfish model with only the sentimented words and only local subjects
dfm_sentiment_l <- stemmed_long %>%
  left_join(select(names, name, foreigner), by = c("subject" = "name")) %>%
  filter(is.na(foreigner)) %>%
  select(-foreigner) %>%
  filter(!is.na(sentiment)) %>%
  get_dfm("1980-01-01", "1991-01-01")

text_model_sentiment_l <- dfm_sentiment_l %>%
  textmodel_wordfish()

model_word_data_sentiment_l <- data.frame(beta = text_model_sentiment_l$beta,
                                        psi = text_model_sentiment_l$psi,
                                        features = text_model_sentiment_l$features) %>%
  arrange(beta)

model_party_data_sentiment_l <- data.frame(alpha = text_model_sentiment_l$alpha,
                                         theta = text_model_sentiment_l$theta,
                                         party = docvars(dfm_sentiment_l, "category") %>% head(107),
                                         year_month = docvars(dfm_sentiment_l, "year_month") %>% head(107))

model_party_data_sentiment_l %>%
  ggplot(aes(x = year_month,
             y = theta,
             color = party,
             group = party,
             label = year_month)) +
  geom_line() +
  geom_point()


### Sentiment analysis
# On all the words
tokens_stemmed %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarize(sentiment_avg = sum(sentiment, na.rm = TRUE) / sum(!is.na(sentiment))) %>%
  ungroup() %>%
  mutate(sentiment_sm = roll_mean(sentiment_avg, 9, fill = NA,
                                  weights = c(1,2,3,4,5,4,3,2,1))) %>%
  ggplot(aes(x = date, y = sentiment_sm)) +
  geom_point(color = rgb(210, 210, 190, maxColorValue = 255)) +
  geom_smooth(aes(x = date, y = sentiment_sm)) +
  theme_minimal() +
  xlab("Date") +
  ylab("Sentiments (smoothed)") +
  ggtitle("General sentiment over time") +
  geom_vline(xintercept = as.Date("1988-08-31"), linetype = 2, color = rgb(.2,.2,.2)) +
  geom_vline(xintercept = as.Date("1990-07-31"), linetype = 2, color = rgb(.2,.2,.2)) +
  geom_vline(xintercept = as.Date("1988-11-24"), linetype = 3, color = rgb(.2,.2,.2)) +
  geom_vline(xintercept = as.Date("1989-10-23"), linetype = 3, color = rgb(.2,.2,.2)) +
  geom_vline(xintercept = as.Date("1990-05-23"), linetype = 3, color = rgb(.2,.2,.2)) +
  annotate("text",
           x = c(as.Date("1988-08-31"),
                 as.Date("1990-07-31"),
                 as.Date("1989-01-01"),
                 as.Date("1989-10-23"),
                 as.Date("1990-05-23")),
           y = c(0.6, 0.5, 0.55, 0.6, 0.55),
           label = c("Hajdú, István",
                     "Gombár, Csaba",
                     "Németh, Miklós",
                     "Proclamation of the republic",
                     "Antall, József"))

# Absolute sentiment
tokens_stemmed %>%
  mutate(date = as.Date(date), sentiment = abs(sentiment)) %>%
  group_by(date) %>%
  summarize(sentiment_avg = sum(sentiment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sentiment_sm = roll_mean(sentiment_avg, 9, fill = NA,
                                  weights = c(1,2,3,4,5,4,3,2,1))) %>%
  ggplot(aes(x = date, y = sentiment_sm)) +
  geom_point(color = rgb(210, 210, 190, maxColorValue = 255)) +
  geom_smooth(aes(x = date, y = sentiment_sm)) +
  theme_minimal() +
  xlab("Date") +
  ylab("Total sentiments (smoothed)") +
  ggtitle("Total sentiment over time") +
  geom_vline(xintercept = as.Date("1988-08-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.Date("1990-07-31"), linetype = 2, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.Date("1988-11-24"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.Date("1989-10-23"), linetype = 3, color = rgb(.5,.5,.5)) +
  geom_vline(xintercept = as.Date("1990-05-23"), linetype = 3, color = rgb(.5,.5,.5))


tokens_stemmed %>%
  mutate(date = as.Date(date), sentiment = abs(sentiment)) %>%
  group_by(date) %>%
  summarize(sentiment_avg = sum(sentiment, na.rm = TRUE)) %>%
  ungroup() %>%
  View()

