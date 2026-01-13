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

# E2 switcher_up et switcher_down
switchers <- df %>%
  filter(S == 1)

switcher_up = switchers %>% filter(delta_D > 0) %>%
  mutate(
    delta_Y_hat = predict(np_fit, newdata = data.frame(D1 = D1)),
    error = is.na(delta_Y_hat),
    numerateur = delta_Y - delta_Y_hat
  ) %>%
  filter(error == F)

sig_up = mean(switcher_up$numerateur)/mean(switcher_up$delta_D)

switcher_down = switchers %>% filter(delta_D < 0) %>%
  mutate(
    delta_Y_hat = predict(np_fit, newdata = data.frame(D1 = D1)),
    error = is.na(delta_Y_hat),
    numerateur = delta_Y - delta_Y_hat
  ) %>%
  filter(error == F)

sig_down = mean(switcher_down$numerateur)/mean(switcher_down$delta_D)

prop_up = nrow(switcher_up)/nrow(switchers)
prop_down = nrow(switcher_down)/nrow(switchers)

weight_switcher = prop_up*mean(switcher_up$delta_D) / ((prop_up*mean(switcher_up$delta_D)) - (prop_down*mean(switcher_down$delta_D)))
# (0.3988152*0.03655502)/((0.3988152*0.03655502)-(0.6011848*-0.03976733))
# E3: Third, one let




delta_2_hat = weight_switcher*sig_up + (1-weight_switcher)*sig_down

