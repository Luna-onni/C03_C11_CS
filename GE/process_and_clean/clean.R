# library
library(tidyverse)

# import a key
stimuli_df <- read.csv("../human_data/CodeSwitchingStimuli.csv") %>% 
  rename(Item = item)

# import data
claude_raw <- read_csv("../llm_data/llm_anthropic-claude-opus-4.6.csv")
mistral_raw <- read_csv("../llm_data/llm_mistralai-ministral-8b-2512.csv")
gpt_raw <- read_csv("../llm_data/llm_Openai GPT OSS 120B.csv")

# clean NAs
#colSums(is.na(gpt_raw))
claude_clean <- na.omit(claude_raw)
mistral_clean <- na.omit(mistral_raw)
gpt_clean <- na.omit(gpt_raw)

# assign information and clean
# claude
claude_merged <- left_join(claude_clean, stimuli_unique, by = "Item") %>% 
  #filter(inner_number != "filler21") %>% 
  select(ID, inner_number, Itemtype, Item, Completion, Best_completion, Unexpected_word, Sentence_length, Sentence_words) %>% 
  mutate(
  # G if first character is uppercase, else E
     Language = if_else(!is.na(Completion) & str_detect(Completion, "^[[:upper:]]"), "G", "E"),
     # empty column (name as written)
     English_tranlsation = NA_character_,
     # remove % and everything after, lowercase, drop punctuation and spaces
     cloze_clean = Completion %>%
     str_replace("%.*$", "") %>%
     str_to_lower() %>%
     str_replace_all("[[:punct:][:space:]]+", ""),
     # empty columns
     exclusion_explanation = NA_character_,
     exclusion_liberal = NA_character_,
     conservation_exclusion = NA_character_,
     comments = NA_character_
)

# mistral
mistral_merged <- left_join(mistral_clean, stimuli_unique, by = "Item") %>% 
  #filter(inner_number != "filler21") %>% 
  select(ID, inner_number, Itemtype, Item, Completion, Best_completion, Unexpected_word, Sentence_length, Sentence_words) %>% 
  mutate(
    # G if first character is uppercase, else E
    Language = if_else(!is.na(Completion) & str_detect(Completion, "^[[:upper:]]"), "G", "E"),
    # empty column (name as written)
    English_tranlsation = NA_character_,
    # remove % and everything after, lowercase, drop punctuation and spaces
    cloze_clean = Completion %>%
      str_replace("%.*$", "") %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:][:space:]]+", ""),
    # empty columns
    exclusion_explanation = NA_character_,
    exclusion_liberal = NA_character_,
    conservation_exclusion = NA_character_,
    comments = NA_character_
  )

# gpt
gpt_merged <- left_join(gpt_clean, stimuli_unique, by = "Item") %>% 
  #filter(inner_number != "filler21") %>% 
  select(ID, inner_number, Itemtype, Item, Completion, Best_completion, Unexpected_word, Sentence_length, Sentence_words) %>% 
  mutate(
    # G if first character is uppercase, else E
    Language = if_else(!is.na(Completion) & str_detect(Completion, "^[[:upper:]]"), "G", "E"),
    # empty column (name as written)
    English_tranlsation = NA_character_,
    # remove % and everything after, lowercase, drop punctuation and spaces
    cloze_clean = Completion %>%
      str_replace("%.*$", "") %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:][:space:]]+", ""),
    # empty columns
    exclusion_explanation = NA_character_,
    exclusion_liberal = NA_character_,
    conservation_exclusion = NA_character_,
    comments = NA_character_
  )

# save to csv
#write_csv(claude_merged, "claude_clean.csv")
#write_csv(mistral_merged, "mistral_clean.csv")
#write_csv(gpt_merged, "gpt_clean.csv")


# compute cloze probs for each llm
# claude
cloze_probs_claude <- claude_merged %>%
  group_by(inner_number, Item, Itemtype, cloze_clean) %>%
  summarise(n = n_distinct(ID), .groups = "drop_last") %>%
  mutate(total = sum(n),
         cloze_probability = n / total) %>%
  ungroup()
# mistral
cloze_probs_mistral <- mistral_merged %>%
  group_by(inner_number, Item, Itemtype, cloze_clean) %>%
  summarise(n = n_distinct(ID), .groups = "drop_last") %>%
  mutate(total = sum(n),
         cloze_probability = n / total) %>%
  ungroup()
# gpt
cloze_probs_gpt <- gpt_merged %>%
  group_by(inner_number, Item, Itemtype, cloze_clean) %>%
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
  group_by(inner_number, Item, Itemtype, participant) %>%
  slice_max(cloze_probability, n = 1, with_ties = FALSE) %>%
  select(inner_number, Item, Itemtype, cloze_clean, cloze_probability)

# lme
modelNull <- lmer(cloze_probability ~ 1 + (1 | inner_number),
                  data = highest_cloze)

model <- lmer(cloze_probability ~ Itemtype + (1 | inner_number),
              data = highest_cloze)

summary(model)
anova(modelNull, model)

# plot
