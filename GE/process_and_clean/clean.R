# library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)

# set working path
setwd("/Users/zliu/Documents/C03_C11/GE")
# load a key file
stimuli_df <- read.csv("human_data/CodeSwitchingStimuli.csv") %>% 
  rename(Item = item)
stimuli_unique <- stimuli_df %>%
  distinct(Item, .keep_all = TRUE)

# load data
#claude_raw <- read_csv("llm_data/llm_anthropic-claude-opus-4.6.csv")
#mistral_raw <- read_csv("llm_data/llm_mistralai-ministral-8b-2512.csv")
gpt_t0_raw <- read_csv("llm_data/llm_gpt-oss-120b_r_low_t0.csv")
gpt_t2_raw <- read_csv("llm_data/llm_gpt-oss-120b_r_low_t2.csv")
gpt_t4_raw <- read_csv("llm_data/llm_gpt-oss-120b_r_low_t4.csv")
gpt_t6_raw <- read_csv("llm_data/llm_gpt-oss-120b_r_low_t6.csv")
gpt_t8_raw <- read_csv("llm_data/llm_gpt-oss-120b_r_low_t8.csv")
gpt_t10_raw <- read_csv("llm_data/llm_gpt-oss-120b_r_low_t10.csv")
gpt_t12_raw <- read_csv("llm_data/llm_gpt-oss-120b_r_low_t12.csv")
gpt_t14_raw <- read_csv("llm_data/llm_gpt-oss-120b_r_low_t14.csv")

# clean NAs
clean <- function(df) {
  df %>%
    mutate(
      Completion = ifelse(
        is.na(Completion) | tolower(trimws(Completion)) == "none", # trimws() remove the space before and after a string
        NA_character_, # if fit the condition, transfer to real "NA", if not, keep the original value in "Completion"
        Completion)
      ) %>%
    filter(!is.na(Completion))
}

gpt_t0_clean <- clean(gpt_t0_raw)
gpt_t2_clean <- clean(gpt_t2_raw)
gpt_t4_clean <- clean(gpt_t4_raw)
gpt_t6_clean <- clean(gpt_t6_raw)
gpt_t8_clean <- clean(gpt_t8_raw)
gpt_t10_clean <- clean(gpt_t10_raw)
gpt_t12_clean <- clean(gpt_t12_raw)
gpt_t14_clean <- clean(gpt_t14_raw)

# assign information and clean
process_dataset <- function(data, stimuli_unique) {
  data %>%
    left_join(stimuli_unique, by = "Item") %>%
    filter(inner_number != "filler21") %>%
    select(
      ID, inner_number, Itemtype, Item, Completion, Best_completion, Unexpected_word, Sentence_length, Sentence_words) %>%
    mutate(
      # G if first character is uppercase, else E
      Language = if_else(
        !is.na(Completion) & str_detect(Completion, "^[[:upper:]]"),
        "G", "E"),
      
      # Empty column
      English_tranlsation = NA_character_,
      
      # Clean completion
      cloze_clean = Completion %>%
        str_replace("%.*$", "") %>%
        str_to_lower() %>%
        str_replace_all("[[:punct:][:space:]]+", ""),
      
      # Empty columns
      exclusion_explanation = NA_character_,
      exclusion_liberal = NA_character_,
      conservation_exclusion = NA_character_,
      comments = NA_character_
    )
}
gpt_t0_merged <- process_dataset(gpt_t0_clean, stimuli_unique)
gpt_t2_merged <- process_dataset(gpt_t2_clean, stimuli_unique)
gpt_t4_merged <- process_dataset(gpt_t4_clean, stimuli_unique)
gpt_t6_merged <- process_dataset(gpt_t6_clean, stimuli_unique)
gpt_t8_merged <- process_dataset(gpt_t8_clean, stimuli_unique)
gpt_t10_merged <- process_dataset(gpt_t10_clean, stimuli_unique)
gpt_t12_merged <- process_dataset(gpt_t12_clean, stimuli_unique)
gpt_t14_merged <- process_dataset(gpt_t14_clean, stimuli_unique)


# compute cloze probs for each llm
compute_cloze_probs <- function(data) {
  
  data %>%
    group_by(inner_number, Item, Itemtype, cloze_clean) %>%
    summarise(
      n = n_distinct(ID),
      .groups = "drop_last"
    ) %>%
    mutate(
      total = sum(n),
      cloze_probability = n / total
    ) %>%
    ungroup()
}

cloze_probs_gpt_t0 <- compute_cloze_probs(gpt_t0_merged)
cloze_probs_gpt_t2 <- compute_cloze_probs(gpt_t2_merged)
cloze_probs_gpt_t4 <- compute_cloze_probs(gpt_t4_merged)
cloze_probs_gpt_t6 <- compute_cloze_probs(gpt_t6_merged)
cloze_probs_gpt_t8 <- compute_cloze_probs(gpt_t8_merged)
cloze_probs_gpt_t10 <- compute_cloze_probs(gpt_t10_merged)
cloze_probs_gpt_t12 <- compute_cloze_probs(gpt_t12_merged)
cloze_probs_gpt_t14 <- compute_cloze_probs(gpt_t14_merged)

# merge gpt's cloze probs
cloze_probs_gpt <- bind_rows(cloze_probs_gpt_t0 %>% mutate(participant = "gpt_t0"),
                             cloze_probs_gpt_t2 %>% mutate(participant = "gpt_t2"),
                             cloze_probs_gpt_t4 %>% mutate(participant = "gpt_t4"),
                             cloze_probs_gpt_t6 %>% mutate(participant = "gpt_t6"),
                             cloze_probs_gpt_t8 %>% mutate(participant = "gpt_t8"),
                             cloze_probs_gpt_t10 %>% mutate(participant = "gpt_t10"),
                             cloze_probs_gpt_t12 %>% mutate(participant = "gpt_t12"),
                             cloze_probs_gpt_t14 %>% mutate(participant = "gpt_t14"))


#write_csv(cloze_probs_gpt, "cloze_probs_gpt.csv")


#write_csv(cloze_probs_llms, "cloze_probs_llms.csv")

# choose the highest cloze probs for each item

highest_cloze_gpt <- cloze_probs_gpt %>%
  group_by(inner_number, Item, Itemtype, participant) %>%
  slice_max(cloze_probability, n = 1, with_ties = FALSE) %>%
  select(inner_number, Item, Itemtype, cloze_clean, cloze_probability)

# import human cloze probs
cloze_probs_human <- read_csv("human_data/cloze_probability.csv") %>% 
  select(-1) %>% 
  mutate(participant = "human")
# choose the highest cloze probs for each item
highest_cloze_human <- cloze_probs_human %>%
  group_by(inner_number, Item, Itemtype, participant) %>%
  slice_max(cloze_probability, n = 1, with_ties = FALSE) %>%
  select(inner_number, Item, Itemtype, cloze_clean, cloze_probability)

# merge llms and human
#highest_cloze_human$inner_number <- as.numeric(highest_cloze_human$inner_number)
#highest_cloze_llms$inner_number <- as.numeric(highest_cloze_llms$inner_number)
#highest_cloze_all <- bind_rows(highest_cloze_human, highest_cloze_llms)

#write_csv(highest_cloze_all, "highest_cloze_all.csv")

# merge gpt and human
highest_cloze_human$inner_number <- as.numeric(highest_cloze_human$inner_number)
highest_cloze_gpt$inner_number <- as.numeric(highest_cloze_gpt$inner_number)
highest_cloze_human_gpt <- bind_rows(highest_cloze_human, highest_cloze_gpt)

write_csv(highest_cloze_human_gpt, "process_and_clean/highest_cloze_human_gpt.csv")