setwd("C:/Users/Clément/Desktop/Work/projet_env/data")

library(tidyverse)
library(data.table)

df <- fread("FDS_G_2047_2010.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)

unique(df$V7)

colnames(df) = tolower(c(df[1,]))
df = df[-1,]

df = df %>%
  filter(frdom == "METRO") %>%
  filter(!(dep %in% c("............","971","972","973","974","2A","2B","ZA","ZB","ZC","ZD","ZM","ZN","ZP","97"))) %>%
  filter(com != "............") %>%
  filter(g_2047_lib_dim3 == "Production brute standard (millier d'euros)") %>%
  filter(g_2047_lib_dim2 == "Ensemble") %>%
  filter(g_2047_lib_dim1 == "Ensemble des exploitations (hors pacages collectifs)") %>%
  arrange(com) %>%
  mutate(
    valeur = as.numeric(valeur)
  )

table(is.na(df$valeur))

unique(df$dep)
