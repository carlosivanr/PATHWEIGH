# Contrasts for a lmer model
library("lme4")
library("emmeans")
library("multcomp")
data("sleepstudy")
head(sleepstudy)




# Assign each row to one of 4 groups
subject_groups <- sleepstudy %>%
  distinct(Subject) %>%
  { set.seed(123); . } %>%  
  mutate(Group = sample(rep(1:4, length.out = n())))  %>%
    mutate(Group1 = ifelse(Group == 1, 1, 0),
           Group2 = ifelse(Group == 2, 1, 0),
           Group3 = ifelse(Group == 3, 1, 0), 
           Group4 = ifelse(Group == 4, 1, 0))

# Merge groups back into the main data frame
sleepstudy %<>% 
  left_join(., subject_groups, by = "Subject")

# Set a reference for Group
sleepstudy %<>%
  mutate(Group = factor(Group, levels = c(1,2,3,4)))

sleepstudy$Group

# Fit a model with categorical variables
tbl_regression(lmerTest::lmer(Reaction ~ Days + Group + (1| Subject), data = sleepstudy )) %>%
  as_kable()

# Fit a model with dummy variables and check output
fit <- lmerTest::lmer(Reaction ~ Days + Group2 + Group3 + Group4 + (1| Subject), data = sleepstudy)

tbl_regression(fit) %>% 
  as_kable()

# Define the contrast matrix, where each row represents a comparison
# The comparisons are based on the order of the coefficients including
# the intercept, where the intercept represents group1
contrast_matrix <- rbind(
  "Group3 vs Group2" = c(0, 0, -1,  1,  0),
  "Group4 vs Group2" = c(0, 0, -1,  0,  1),
  "Group4 vs Group3" = c(0, 0,  0, -1,  1)
)

# Apply the contrasts using glht:
glht_res <- glht(fit, linfct = contrast_matrix)

# Display the summary with test statistics and adjusted p-values:
summary(glht_res)


# Now try to add a second variable
# Assign each row to one of 2 groups
subject_sex <- sleepstudy %>%
  distinct(Subject) %>%
  { set.seed(123); . } %>%  
  mutate(Sex = sample(rep(1:2, length.out = n())))  %>%
    mutate(Male = ifelse(Sex == 1, 1, 0),
           Female = ifelse(Sex == 2, 1, 0))

# Merge groups back into the main data frame
sleepstudy %<>% 
  left_join(., subject_sex, by = "Subject")

sleepstudy
################# LEFT OFF HERE #########################
# Want to try adding an interaction term to see how to get
# comparisons of different groups