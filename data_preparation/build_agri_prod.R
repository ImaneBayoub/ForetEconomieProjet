base_agri2010 <- fread("FDS_G_2047_2010.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)
base_agri2000 <- fread("FDS_G_2047_2000.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)
base_agri1988 <- fread("FDS_G_2047_1988.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)


colnames(agri2010) = tolower(c(agri2010[1,]))
agri2010 = agri2010[-1,]

# unique(agri2010$g_2047_lib_dim3)

agri2010 = agri2010 %>%
  filter(frdom == "METRO") %>%
  filter(!(dep %in% c("............","971","972","973","974","2A","2B","ZA","ZB","ZC","ZD","ZM","ZN","ZP","97"))) %>%
  filter(com != "............") %>%
  filter(g_2047_lib_dim3 == "Production brute standard (millier d'euros)"|
           g_2047_lib_dim3 == "Superficie agricole utilisÃ©e (ha)" ) %>%
  filter(g_2047_lib_dim2 == "Ensemble") %>%
  filter(g_2047_lib_dim1 == "Ensemble des exploitations (hors pacages collectifs)") %>%
  arrange(com) %>%
  group_by(com) %>%
  mutate(
    valeur = as.numeric(valeur)
  ) %>%
  filter(!is.na(valeur)) %>%
  mutate(n_count = n()) %>%
  filter(n_count == 2) %>%
  pivot_wider(  #il y a deux ligne par commune: la production et la surface utilisée,
                #chacune dans une colonne nommée valeur.
    names_from = g_2047_lib_dim3,
    values_from = valeur
  ) %>%
  summarise(
    superficie = max(`Superficie agricole utilisÃ©e (ha)`, na.rm = TRUE),
    production = max(`Production brute standard (millier d'euros)`, na.rm = TRUE)
  ) %>%
  mutate(ratio_prod_surface = production / superficie)

#summary(lm(ratio_prod_surface~ production + superficie, data = agri2010))


agri2000 = read_parquet("base_agri2000.parquet")

colnames(agri2000) = tolower(c(agri2000[1,]))
agri2000 = agri2000[-1,]

unique(agri2000$g_2047_lib_dim3)

agri2000 = agri2000 %>%
  filter(frdom == "METRO") %>%
  filter(!(dep %in% c("............","971","972","973","974","2A","2B","ZA","ZB","ZC","ZD","ZM","ZN","ZP","97"))) %>%
  filter(com != "............") %>%
  filter(g_2047_lib_dim3 == "Production brute standard (millier d'euros)"|
           g_2047_lib_dim3 == "Superficie agricole utilisÃ©e (ha)" ) %>%
  filter(g_2047_lib_dim2 == "Ensemble") %>%
  filter(g_2047_lib_dim1 == "Ensemble des exploitations (hors pacages collectifs)") %>%
  arrange(com)%>%
  group_by(com) %>%
  mutate(
    valeur = as.numeric(valeur),
    g_2047_lib_dim3 = ifelse(g_2047_lib_dim3 == "Production brute standard (millier d'euros)","Production","Surface")
  ) %>%
  filter(!is.na(valeur)) %>%
  mutate(n_count = n()) %>%
  filter(n_count == 2) %>%
  pivot_wider(  #il y a deux ligne par commune: la production et la surface utilisée,
    #chacune dans une colonne nommée valeur.
    names_from = g_2047_lib_dim3,
    values_from = valeur
  ) %>%
  summarise(
    superficie = max(Surface, na.rm = TRUE),
    production = max(Production, na.rm = TRUE)
  ) %>%
  mutate(ratio_prod_surface = production / superficie)



agri1988 = read_parquet("base_agri1988.parquet")

colnames(agri1988) = tolower(c(agri1988[1,]))
agri1988 = agri1988[-1,]

agri1988 = agri1988 %>%
  filter(frdom == "METRO") %>%
  filter(!(dep %in% c("............","971","972","973","974","2A","2B","ZA","ZB","ZC","ZD","ZM","ZN","ZP","97"))) %>%
  filter(com != "............") %>%
  filter(g_2047_lib_dim3 == "Production brute standard (millier d'euros)"|
           g_2047_lib_dim3 == "Superficie agricole utilisÃ©e (ha)" ) %>%
  filter(g_2047_lib_dim2 == "Ensemble") %>%
  filter(g_2047_lib_dim1 == "Ensemble des exploitations (hors pacages collectifs)") %>%
  arrange(com)%>%
  group_by(com) %>%
  mutate(
    valeur = as.numeric(valeur),
    g_2047_lib_dim3 = ifelse(g_2047_lib_dim3 == "Production brute standard (millier d'euros)","Production","Surface")
  ) %>%
  filter(!is.na(valeur)) %>%
  mutate(n_count = n()) %>%
  filter(n_count == 2) %>%
  pivot_wider(  #il y a deux ligne par commune: la production et la surface utilisée,
    #chacune dans une colonne nommée valeur.
    names_from = g_2047_lib_dim3,
    values_from = valeur
  ) %>%
  summarise(
    superficie = max(Surface, na.rm = TRUE),
    production = max(Production, na.rm = TRUE)
  ) %>%
  mutate(ratio_prod_surface = production / superficie)

# write_parquet("agri2010.parquet",x = agri2010)
# write_parquet("agri2000.parquet",x = agri2000)
# write_parquet("agri1988.parquet",x = agri1988)