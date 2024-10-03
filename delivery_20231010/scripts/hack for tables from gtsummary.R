# Hack tbl_summary to prep data for a percentage histogram
p_hist <- ee %>%
  group_by(Arb_PersonId, Intervention) %>%
  count() %>%
  ungroup() %>%
  mutate(n = factor(n),) %>%
  select(-Arb_PersonId) %>%
  tbl_summary(by = Intervention,
              digits = everything() ~ c(2,2),
              statistic = everything() ~ "{p}") %>%
  modify_header(label ~ "Characteristic",
                all_stat_cols() ~ "{level}") %>%
  as_tibble() %>%
  drop_na() %>%
  pivot_longer(cols = c(Control, Intervention), values_to = "Percent", names_to = "Intervention") %>%
  mutate(Percent = as.numeric(Percent)) %>%
  mutate(Characteristic = factor(as.numeric(Characteristic)))

p_hist %>%
  ggplot(., aes(x = Characteristic, y = Percent, color = Intervention, fill = Intervention)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  theme_minimal() +
  xlab("Number of visits per patient")
