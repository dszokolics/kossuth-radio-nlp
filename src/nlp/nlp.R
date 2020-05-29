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
  select(-alias, -foreigner) %>%
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

tokens_stemmed <- tokenized %>%
  left_join(words_stemmed) %>%
  filter(!is.na(stemmed), is.na(as.numeric(stemmed))) %>%
  anti_join(select(unnest_tokens(names, word, name), word),
            by = c("stemmed" = "word"))

rm("data", "d2", "tokenized", "words_to_stem")

# Party sentiments
parties %>%
  ggplot(aes(x=date, y=sentiment_avg, group=category, color=category)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k=100), se = FALSE)

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







# Word counts
wordcounts <- tokens_stemmed %>%
  select(date, paragraph, stemmed) %>%
  distinct() %>%
  group_by(stemmed) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(n > 10) %>%
  arrange(desc(n))

wordcounts %>%
  head(50) %>%
  ggplot(aes(x = reorder(stemmed, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()


tokens_stemmed %>%
  select(-document, -paragraph, -names, -sentiment, -word) %>%
  pivot_longer(cols = -c(date, stemmed), names_to = "subject", values_to = "occur") %>%
  filter(occur, !mapply(agrepl, stemmed, subject)) %>%
  left_join(select(names, name, category), by = c("subject" = "name"))  # %>%
  group_by(category, stemmed) %>%
  top_n(15, n) %>%
  ungroup() %>%
  arrange(category, -n) %>%
  ggplot(aes(stemmed, n, fill = factor(category))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ category, scales = "free") +
  coord_flip()

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

  
# Topic analysis
term_matrix <- tokens_stemmed %>%
  anti_join(filter(wordcounts, n > 2000)) %>%
  group_by(date, paragraph, stemmed) %>%
  summarise(n = n()) %>%
  mutate(date_p = paste(date, paragraph)) %>%
  cast_dtm(date_p, stemmed, n)

paragraph_lda <- LDA(term_matrix, k = 6, control = list(seed = 1234))
paragraph_topics <- tidy(paragraph_lda, matrix = "beta")

top_terms <- paragraph_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()




# Create wordfish models
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

dfm <- get_dfm(stemmed_long, "1980-01-01", "1991-01-01")

text_model <- dfm %>%
  textmodel_wordfish()

model_word_data <- data.frame(beta = text_model$beta,
                              psi = text_model$psi,
                              features = text_model$features) %>%
  arrange(beta)

model_party_data <- data.frame(alpha = text_model$alpha,
                               theta = text_model$theta,
                               party = docvars(dfm, "category"),
                               year_month = docvars(dfm, "year_month"))

model_party_data %>%
  ggplot(aes(x = year_month,
             y = theta,
             color = party,
             group = party,
             label = year_month)) +
  geom_line() +
  geom_point()

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
  geom_line() +
  geom_point()


dfm_1 <- get_dfm(tokens_stemmed, "1987-01-01", "1988-09-01")

text_model_1 <- dfm_1 %>%
  textmodel_wordfish()

model_word_data_1 <- data.frame(beta = text_model_1$beta,
                                psi = text_model_1$psi,
                                features = text_model_1$features) %>%
  arrange(beta)

model_party_data_1 <- data.frame(alpha = text_model_1$alpha,
                                 theta = text_model_1$theta,
                                 party = docvars(dfm_1, "category"),
                                 year_month = docvars(dfm_1, "year_month"))

model_party_data_1 %>%
  ggplot(aes(x = year_month,
             y = theta,
             color = party,
             group = party,
             label = year_month)) +
  geom_line() +
  geom_point()


dfm_2 <- get_dfm(tokens_stemmed, "1988-09-01", "1990-08-01")

text_model_2 <- dfm_2 %>%
  textmodel_wordfish()

model_word_data_2 <- data.frame(beta = text_model_2$beta,
                                psi = text_model_2$psi,
                                features = text_model_2$features) %>%
  arrange(beta)

model_party_data_2 <- data.frame(alpha = text_model_2$alpha,
                                 theta = text_model_2$theta,
                                 party = docvars(dfm_2, "category"),
                                 year_month = docvars(dfm_2, "year_month"))

model_party_data_2 %>%
  ggplot(aes(x = year_month,
             y = theta,
             color = party,
             group = party,
             label = year_month)) +
  geom_line() +
  geom_point()


dfm_3 <- get_dfm(tokens_stemmed, "1990-08-01", "1991-01-01")

text_model_3 <- dfm_3 %>%
  textmodel_wordfish()

model_word_data_3 <- data.frame(beta = text_model_3$beta,
                                psi = text_model_3$psi,
                                features = text_model_3$features) %>%
  arrange(beta)

model_party_data_3 <- data.frame(alpha = text_model_3$alpha,
                                 theta = text_model_3$theta,
                                 party = docvars(dfm_3, "category"),
                                 year_month = docvars(dfm_3, "year_month"))

model_party_data_3 %>%
  ggplot(aes(x = year_month,
             y = theta,
             color = party,
             group = party,
             label = year_month)) +
  geom_line() +
  geom_point()


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
  ylab("Sentiments smoothed") +
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
           label = c("Hajdú, János",
                     "Gombár, Csaba",
                     "Németh, Miklós",
                     "Proclamation of republic",
                     "Antall, József"))

