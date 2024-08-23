# Count WPVs A:D and E:H %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Counts the number of WPVs by different criteria
# *** Drops NA weights to count, but may be more useful to drop Weight_kgs after
# censoring the visits that are classified as intervention that took place before
# before control.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Count WPVs A:D and E:G to determine the overlap ------------------------------
# How many E-G are included in the definition for A-D
# n.b. the chain of commands is drop_na(Weight), but this is only for counting
# purposes. Transmute() creates three new variables, and discards the rest,
# mutate converts the cells that represent counts because of rowSums().
# Could also create by mutate and ifelse(WPV_CC > 1 | WPC_ICD > 1 | WPV_OBHPI)
print(
  data.frame(
    as.list(
      visits %>%
        drop_na(Weight) %>%
        filter(Eligible == 1) %>%
        transmute(WPV_AD = ifelse(WPV_CC == 1 | WPV_ICD == 1 | WPV_OBHPI == 1, 1, 0),
                  WPV_EG = ifelse(WPV_WMQ == 1 |WPV_PW_flow == 1 | WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1, 1, 0),
                  WPV_AG = ifelse(WPV_CC == 1 | WPV_ICD == 1 | WPV_OBHPI == 1 | WPV_WMQ == 1 |WPV_PW_flow == 1 | 
                                    WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1, 1, 0)) %>%
        colSums() 
    )
  ) %>%
    mutate(Overlap = (WPV_AD + WPV_EG) - WPV_AG,
           EG_Only = WPV_EG - Overlap)
)

      
# Corroborate with filter ------------------------------------------------------
# For 20220606 data, shows 639 encounters that meet criteria for WPV_AD and 
# WPV_EG. 41 meet criteria from E:G only (PW_flow (E), WMQ (E), VisitType (F), 
# Smart set (G).

# The number of visits criteria A-D
visits %>% 
  drop_na(Weight) %>% 
  filter(Eligible == 1) %>% 
  filter(WPV_CC == 1 | WPV_ICD == 1 | WPV_OBHPI == 1) %>% 
  nrow()

# The number of visits criteria E-H
visits %>% 
  drop_na(Weight) %>% 
  filter(Eligible == 1) %>% 
  filter(WPV_WMQ == 1 |WPV_PW_flow == 1 | WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1) %>% 
  nrow()

# The number of visits in criteria A-D and E-H, this is the overlap.
visits %>% 
  drop_na(Weight) %>% 
  filter(Eligible == 1) %>% 
  filter(WPV_CC == 1 | WPV_ICD == 1 | WPV_OBHPI == 1,
         WPV_WMQ ==1 | WPV_PW_flow == 1 | WPV_IP == 1 | WPV_TH == 1 | WPV_smart == 1) %>% 
  nrow()


