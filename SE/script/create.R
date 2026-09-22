library(tidyverse)
library(readr)
library(readxl)

df <- read_excel("/Users/zliu/Downloads/demographics_osf.xlsx")

df_kept <- df %>%
  filter(Kept == 1) %>%
  select("ID","Gender","Education","Age","Country","State","OtherLang","SpanishVariety","EnglishVariety")

write_csv(df_kept, "personas.csv")