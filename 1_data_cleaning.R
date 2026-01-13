
#data
#base_agri2010 <- fread("FDS_G_2047_2010.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)
#base_agri2000 <- fread("FDS_G_2047_2000.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)
#base_agri1988 <- fread("FDS_G_2047_1988.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)
#base_agri1979 <- fread("FDS_G_2047_1979.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)
#base_agri1970 <- fread("FDS_G_2047_1970.txt", sep = ";", encoding = "Latin-1", fill = TRUE, header = FALSE)

#write_parquet("base_agri2010.parquet",x = base_agri2010)
#write_parquet("base_agri2000.parquet",x = base_agri2000)
#write_parquet("base_agri1988.parquet",x = base_agri1988)
#write_parquet("base_agri1979.parquet",x = base_agri1979)
#write_parquet("base_agri1970.parquet",x = base_agri1970)

agri2010 = read_parquet("base_agri2010.parquet")
agri2000 = read_parquet("base_agri2000.parquet")
agri1988 = read_parquet("base_agri1988.parquet")
agri1979 = read_parquet("base_agri1979.parquet")
agri1970 = read_parquet("base_agri1970.parquet")



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

#>
#>
#>
#>
#>
#>
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





# write.csv("agri2010.csv",x = agri2010)
# write.csv("agri2000.csv",x = agri2000)
# write.csv("agri1988.csv",x = agri1988)

# write_parquet("agri2010.parquet",x = agri2010)
# write_parquet("agri2000.parquet",x = agri2000)
# write_parquet("agri1988.parquet",x = agri1988)


# Forêts
clc <- read.csv("clc.csv", sep=";")
clc = clc[-(1:3),] %>%
  rename(
    cog_commune_2010 = Code.Insee.de.la.commune,
    annee = Millésime,
    label = Base.de.données......label,
    agrofor = Superficie.du.poste.244...Territoires.agroforestiers..en.ha.,
    for_feu = Superficie.du.poste.311...Forêts.de.feuillus..en.ha.,
    for_con = Superficie.du.poste.312...Forêts.de.conifères..en.ha.,
    for_mel = Superficie.du.poste.313...Forêts.mélangées..en.ha.,
    for_arb = Superficie.du.poste.324...Forêt.et.végétation.arbustive.en.mutation..en.ha.
  ) %>%
  filter(
    (label != "CLC 2000") &
      (label != "CLC 2012") &
      (label != "CLC 2006")
  ) %>%
  select(-"label") %>%
  mutate(across(!cog_commune_2010, ~ as.numeric(.)),
         sum_hectar = rowSums(across(!c(cog_commune_2010, annee))),
         prop_foret = rowSums(across(c(agrofor,for_feu,for_con,for_mel,for_arb)))/sum_hectar,
         prop_foret_alt = rowSums(across(c(for_feu,for_con,for_mel)))/sum_hectar
  ) %>%
  select(c("cog_commune_2010", "annee","agrofor","for_feu","for_con","for_mel",
           "for_arb","sum_hectar","prop_foret","prop_foret_alt")) 


clc2018 = clc %>%
  filter(annee == 2018) %>%
  select(c("cog_commune_2010","prop_foret","prop_foret_alt")) %>%
  rename(#prop_foret_2018 = prop_foret,
         #prop_foret_alt_2018 = prop_foret_alt,
         id = cog_commune_2010 )

clc2012  = clc %>% filter(annee == 2012) %>%
  select(c("cog_commune_2010","prop_foret","prop_foret_alt")) %>%
  rename(#prop_foret_2012 = prop_foret,
         id = cog_commune_2010 )

clc2006  = clc %>% filter(annee == 2006) %>%
  select(c("cog_commune_2010","prop_foret","prop_foret_alt")) %>%
  rename(#prop_foret_2010 = prop_foret,
         id = cog_commune_2010 )

clc2000 = clc %>% filter(annee == 2000) %>%
  select(c("cog_commune_2010","prop_foret","prop_foret_alt")) %>%
  rename(#prop_foret_2000 = prop_foret,
         id = cog_commune_2010)

clc1990  = clc %>% filter(annee == 1990) %>%
  select(c("cog_commune_2010","prop_foret","prop_foret_alt")) %>%
  rename(#prop_foret_1990 = prop_foret,
         #prop_foret_alt_1990 = prop_foret_alt,
         id = cog_commune_2010 )


hist(clc2018$prop_foret)

#
foret = clc1990 %>%
  rename(prop_foret_1990 = prop_foret) %>%
  left_join(clc2000, by="id", suffix = c("","_2000")) %>%
  left_join(clc2006, by="id", suffix = c("","_2006")) %>%
  left_join(clc2012, by="id", suffix = c("","_2012")) %>%
  left_join(clc2018, by="id", suffix = c("","_2018")) 



#write.csv(x = foret, "foret.csv")