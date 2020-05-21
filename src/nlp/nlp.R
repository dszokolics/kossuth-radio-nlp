library(tidytext)
library(tidyverse)
library(purrr)

Sys.setlocale("LC_ALL", 'Hungarian_Hungary.1250')
# Sys.setenv(LANG = "HU")

# Read the files
data <- read.csv("data/processed/almost_tidy_text_200.csv",
                 stringsAsFactors=FALSE,
                 encoding="UTF-8",
                 nrow=50000)

names <- read.csv("data/processed/subjects_categorized.csv",
                  stringsAsFactors=FALSE,
                  encoding="UTF-8")

# Generate two verions of the name: Firstname Lastname, Lastname Firstname
names <- names %>%
  filter(count > 50) %>%
  mutate(name = ifelse(alias == "", str_remove(name, ','), alias)) %>%
  select(-alias)

# Create a name column with words which are possibly names: this will speed up feature generation
data <- data %>%
  filter(str_length(text) > 15) %>%
  mutate(names = lapply(str_extract_all(text,
                                        "(?<=^|\\s)[A-ZÁÉÍÖ\\u0150ÚÜ\\u0170]{1}[a-záéíóö\\u0151úü\\u0170]+"),
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
varnames <- c(names$name, "sentiment")
d2 <- data %>%
  unnest_tokens(word, text) %>%
  filter(str_length(word) > 2) %>%
  anti_join(stopwords) %>%
  left_join(sentiments) %>%
  group_by(date, paragraph) %>%
  mutate(sentiment = sum(sentiment, na.rm = TRUE)) %>%
  summarise_at(vars(varnames), max) %>%
  ungroup()

mentions <- d2 %>%
  select(-paragraph, -sentiment) %>%
  group_by(date) %>%
  summarise_at(vars(names$name), sum) %>%
  ungroup() %>%
  pivot_longer(-date, "subject", values_to = "mention") %>%
  filter(mention > 0)

# Get sentiments for the subjects
name_cols_end <- dim(d2)[2]-1
subject_names <- names$name
subjects <- d2 %>%
  filter(rowSums(.[3:name_cols_end]) > 0) %>%
  mutate_at(vars(subject_names), .funs = ~ . * sentiment) %>%
  filter(sentiment != 0) %>%
  group_by(date) %>%
  summarise_at(vars(names$name), sum) %>%
  ungroup() %>%
  pivot_longer(-date, "subject", values_to = "sentiment") %>%
  filter(sentiment != 0) %>%
  full_join(mentions) %>%
  replace_na(list(sentiment = 0)) %>%
  arrange(date)

all_mentions <- subjects %>%
  mutate(mention = ifelse(mention > 0, 1, 0)) %>%
  group_by(subject) %>%
  summarise(mention = sum(mention), sentiment = sum(sentiment)) %>%
  ungroup() %>%
  full_join(select(names, name, count), by = c("subject"="name")) %>%
  mutate(mentions_found = mention / count)

parties <- subjects %>%
  left_join(select(names, name, category), by = c("subject" = "name")) %>%
  group_by(date, category) %>%
  summarise(sentiment = sum(sentiment), mention = sum(mention)) %>%
  ungroup()
