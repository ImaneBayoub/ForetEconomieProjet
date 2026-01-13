library(dplyr)

twfe_data = read.csv("ASS.csv")

set.seed(123)

twfe_data <- twfe_data %>%
  group_by(id) %>%
  filter(n() == 3) %>%
  filter(time !=3) 

df = twfe_data %>%
  select(-"X") %>%
  rename(
    "D" = prop_foret_alt ,
    "Y" = ratio_prod_surface 
  ) %>%
  pivot_wider(names_from = time,
                               values_from = c(D,Y),
              names_sep = "",
              id_cols = id) %>%
  mutate(
    delta_D = D2 - D1,
    #delta_D = ifelse(abs(delta_D) < 0.01,0,delta_D),
    delta_Y = Y2 - Y1,
    S = as.integer(delta_D != 0)
  ) %>%
  ungroup()



# E1: First, one estimates E(∆Y |D1, S = 0) using a non-parametric regression of ∆Yi on Di,1 among stayers.

stayers <- df %>% filter(S == 0)
np_fit <- loess(
  delta_Y ~ D1,
  data = stayers,
  span = 0.75,
  degree = 1
)

# E2: its predicted outcome evolution given its baseline treatment, according to the non-parametric regression estimated among stayers

switchers <- df %>%
  filter(S == 1) %>%
  mutate(
    delta_Y_hat = predict(np_fit, newdata = data.frame(D1 = D1))
  )

# E3: Third, one let

switchers = switchers %>%
  mutate(
    numerateur = delta_Y - delta_Y_hat,
    sig = numerateur/delta_D,
    error = is.na(sig)
  ) %>%
  filter(error == F)

delta_1 = mean(switchers$sig)



#
# Switcher = 0 ==> -2.890112e+13
# Switcher = abs() < 1%: 2.38 
# Switcher = abs() < 1.5%: 0.84 
# Switcher = abs() < 5%: -2.288738
