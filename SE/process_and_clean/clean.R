# library
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(lme4)

# import a key
stimuli_df <- read.table("Cloze_SpEng_n122_133itemmeasures.txt",header = TRUE,sep = "",
                         quote = "\"",stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8") %>% 
  select("InnerNumber","Item","Itemtype") 

# import data
claude_raw <- read_csv("../llm_data/llm_anthropic-claude-opus-4.6.csv")
mistral_raw <- read_csv("../llm_data/llm_mistralai-ministral-8b-2512.csv")
gpt_raw <- read_csv("../llm_data/llm_Openai GPT OSS 120B.csv")

# clean NAs
claude_clean <- na.omit(claude_raw)
mistral_clean <- na.omit(mistral_raw)
gpt_clean <- na.omit(gpt_raw)

# assign information and clean
# claude
claude_merged <- claude_clean %>%
  inner_join(stimuli_df %>% select(InnerNumber, Item), by = "Item") %>% 
  select(ID, InnerNumber, Item, Itemtype, Completion) %>% 
  mutate(
     # remove % and everything after, lowercase, drop punctuation and spaces
     cloze_clean = Completion %>%
     str_replace("%.*$", "") %>%
     str_to_lower() %>%
     str_replace_all("[[:punct:][:space:]]+", ""))

# mistral
mistral_merged <- mistral_clean %>%
  inner_join(stimuli_df %>% select(InnerNumber, Item), by = "Item") %>% 
  select(ID, InnerNumber, Item, Itemtype, Completion) %>% 
  mutate(
    # remove % and everything after, lowercase, drop punctuation and spaces
    cloze_clean = Completion %>%
      str_replace("%.*$", "") %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:][:space:]]+", ""))

# gpt
gpt_merged <- gpt_clean %>%
  inner_join(stimuli_df %>% select(InnerNumber, Item), by = "Item") %>% 
  select(ID, InnerNumber, Item, Itemtype, Completion) %>% 
  mutate(
    # remove % and everything after, lowercase, drop punctuation and spaces
    cloze_clean = Completion %>%
      str_replace("%.*$", "") %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:][:space:]]+", ""))

# save to csv
#write_csv(claude_merged, "claude_clean.csv")
#write_csv(mistral_merged, "mistral_clean.csv")
#write_csv(gpt_merged, "gpt_clean.csv")


# compute cloze probs for each llm
# claude
cloze_probs_claude <- claude_merged %>%
  group_by(InnerNumber, Item, Itemtype, cloze_clean) %>%
  summarise(n = n_distinct(ID), .groups = "drop_last") %>%
  mutate(total = sum(n),
         cloze_probability = n / total) %>%
  ungroup()
# mistral
cloze_probs_mistral <- mistral_merged %>%
  group_by(InnerNumber, Item, Itemtype, cloze_clean) %>%
  summarise(n = n_distinct(ID), .groups = "drop_last") %>%
  mutate(total = sum(n),
         cloze_probability = n / total) %>%
  ungroup()
# gpt
cloze_probs_gpt <- gpt_merged %>%
  group_by(InnerNumber, Item, Itemtype, cloze_clean) %>%
  summarise(n = n_distinct(ID), .groups = "drop_last") %>%
  mutate(total = sum(n),
         cloze_probability = n / total) %>%
  ungroup()

# merge llms' cloze probs
cloze_probs_llms <- bind_rows(
  cloze_probs_claude  %>% mutate(participant = "claude"),
  cloze_probs_mistral %>% mutate(participant = "mistral"),
  cloze_probs_gpt     %>% mutate(participant = "gpt")
)

#write_csv(cloze_probs_llms, "cloze_probs_llms.csv")

# choose the highest cloze probs for each item
highest_cloze_llms <- cloze_probs_llms %>%
  group_by(InnerNumber, Item, Itemtype, participant) %>%
  slice_max(cloze_probability, n = 1, with_ties = FALSE) 

colSums(is.na(highest_cloze_human))

# import human cloze probs
cloze_probs_human <- read_excel("../human_data/Cloze_SpEng_n122_133cloze_prob.xlsx") %>%
  slice_head(n = nrow(.) - 2) %>%
  mutate(participant = "human") %>%
  rename(cloze_clean = cloze_clean_wtrans)

# choose the highest cloze probs for each item
highest_cloze_human <- cloze_probs_human %>%
  group_by(InnerNumber, Item, Itemtype, participant) %>%
  slice_max(cloze_probability, n = 1, with_ties = FALSE) %>%
  select(InnerNumber, Item, Itemtype, n, total, cloze_clean, cloze_probability)

# merge llms and human
highest_cloze_human$InnerNumber <- as.numeric(highest_cloze_human$InnerNumber)
highest_cloze_llms$InnerNumber <- as.numeric(highest_cloze_llms$InnerNumber)
highest_cloze_all <- bind_rows(highest_cloze_human, highest_cloze_llms)

write_csv(highest_cloze_all, "highest_cloze_all.csv")
