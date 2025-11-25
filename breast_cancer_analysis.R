###############################################
# PACKAGES
###############################################
library(tidyverse)
library(ggplot2)
library(naniar)
library(visdat)
library(logistf)
library(tidyr)
library(stringr)


###############################################
# LOAD DATA
###############################################
df <- read_csv("R PROJECT - Sheet1 (1).csv")

###############################################
# BASIC CLEANING
###############################################

# Menopause state
df <- df %>%
  mutate(
    menopause_state = case_when(
      menopause_state %in% c("pre-menopausal", "Pre-menopausal",
                             "pre menopausal", "Premenopausal",
                             "premenopausal") ~ "Pre-menopausal",
      menopause_state %in% c("post-menopausal", "Post-menopausal",
                             "post menopausal", "Postmenopausal",
                             "postmenopausal", "menopausal",
                             "Post-manopausal") ~ "Post-menopausal",
      menopause_state %in% c("na", "Nil", "-", "") ~ NA_character_,
      TRUE ~ as.character(menopause_state)
    ),
    menopause_state = factor(menopause_state)
  )

# Toxicity clean (binary)
df <- df %>%
  mutate(
    tox_clean = case_when(
      adjuvant_chemo_toxicity %in% c("Yes","yes") ~ "Yes",
      adjuvant_chemo_toxicity %in% c("No","Nil","no") ~ "No",
      TRUE ~ NA_character_
    ),
    tox_clean = factor(tox_clean, levels = c("No","Yes"))
  )

# Age numeric + age group
df <- df %>%
  mutate(
    age = as.numeric(age),
    age_group = cut(
      age,
      breaks = c(0, 40, 60, Inf),
      labels = c("<40","40–59","≥60"),
      right = FALSE
    )
  )

###############################################
# CLEAN REGIMEN GROUPS (choose your main column)
###############################################

# Here I assume drug_regimen_adjuvant is the main column.
# If you prefer drug_regimen, replace it below.

reg <- tolower(df$drug_regimen_adjuvant)

map_reg <- function(x){
  if (is.na(x) | x %in% c("na","-","nan","")) return("Missing")
  if (x %in% c("ec","ac","ec> paclitaxel","doxorubicin",
               "ac, paclitaxel","docetaxel, tamoxifen",
               "ec x5, paclitaxel x6","ec x1",
               "ec x1, paclitaxel x1","cef"))
    return("AC/EC-based")
  if (grepl("paclitaxel", x) | grepl("docetaxel", x))
    return("Taxane-based")
  if (grepl("cmf", x)) return("CMF")
  if (grepl("xeloda", x)) return("Capecitabine")
  if (grepl("tamoxifen", x) & !grepl("xeloda", x)) return("Hormonal")
  return("Other")
}

df$regimen_clean <- sapply(reg, map_reg)
df$regimen_clean <- factor(df$regimen_clean)

###############################################
# TNM + AJCC STAGE
###############################################
df <- df %>%
  mutate(
    tnm_stage = str_replace_all(tnm_stage, "\\s+", ""),
    T_stage = str_extract(tnm_stage, "T[0-9]"),
    N_stage = str_extract(tnm_stage, "N[0-9]"),
    M_stage = str_extract(tnm_stage, "M[0-9]")
  ) %>%
  mutate(
    T_stage = case_when(
      T_stage %in% c("T1","t1") ~ "T1",
      T_stage %in% c("T2","t2") ~ "T2",
      T_stage %in% c("T3","t3") ~ "T3",
      T_stage %in% c("T4","t4") ~ "T4",
      TRUE ~ NA_character_
    ),
    N_stage = case_when(
      N_stage %in% c("N0","n0") ~ "N0",
      N_stage %in% c("N1","n1") ~ "N1",
      N_stage %in% c("N2","n2") ~ "N2",
      N_stage %in% c("N3","n3") ~ "N3",
      TRUE ~ NA_character_
    ),
    M_stage = case_when(
      M_stage %in% c("M0","m0") ~ "M0",
      M_stage %in% c("M1","m1") ~ "M1",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    AJCC_stage = case_when(
      M_stage == "M1" ~ "Stage IV",
      N_stage %in% c("N2","N3") ~ "Stage III",
      T_stage == "T3" & N_stage %in% c("N0","N1") ~ "Stage II",
      T_stage %in% c("T1","T2") & N_stage == "N1" ~ "Stage II",
      T_stage %in% c("T1","T2") & N_stage == "N0" ~ "Stage I",
      TRUE ~ NA_character_
    ),
    AJCC_stage = factor(AJCC_stage,
                        levels = c("Stage I","Stage II","Stage III","Stage IV"))
  )

###############################################
# TOXICITY DATASET (COMPLETE CASES)
###############################################
df_tox <- df %>% filter(!is.na(tox_clean))

###############################################
# 1. SUMMARY TABLES
###############################################

# Overall toxicity
overall <- df %>%
  summarise(
    Total = n(),
    Toxicity_Available = sum(!is.na(tox_clean)),
    Toxicity_Yes = sum(tox_clean == "Yes", na.rm = TRUE),
    Toxicity_No = sum(tox_clean == "No", na.rm = TRUE),
    Toxicity_Missing = sum(is.na(tox_clean))
  )

# Toxicity by regimen
by_regimen <- df_tox %>%
  group_by(regimen_clean) %>%
  summarise(
    Total = n(),
    Yes = sum(tox_clean == "Yes"),
    No = sum(tox_clean == "No"),
    .groups = "drop"
  )

# 3. Toxicity by AJCC stage
by_stage <- df_tox %>%
  group_by(AJCC_stage) %>%
  summarise(
    Total = n(),
    Yes = sum(tox_clean == "Yes"),
    No = sum(tox_clean == "No"),
    .groups = "drop"
  )

# 4. Toxicity by menopause
by_meno <- df_tox %>%
  group_by(menopause_state) %>%
  summarise(
    Total = n(),
    Yes = sum(tox_clean == "Yes"),
    No = sum(tox_clean == "No"),
    .groups = "drop"
  )

list(
  Overall_Toxicity = overall,
  Toxicity_by_Regimen = by_regimen,
  Toxicity_by_AJCC = by_stage,
  Toxicity_by_Menopause = by_meno
)

###############################################
# 2. FISHER TESTS / LOGISTIC MODELS
###############################################

# AJCC (Stage III vs IV only)
tox_ajcc <- table(df_tox$AJCC_stage, df_tox$tox_clean)
tox_ajcc_small <- tox_ajcc[c("Stage III","Stage IV"), c("No","Yes")]
fisher.test(tox_ajcc_small)

# Menopause vs toxicity
fisher.test(table(df_tox$menopause_state, df_tox$tox_clean))

# Firth logistic for menopause (if separation)
model_firth <- logistf(tox_clean ~ menopause_state, data = df_tox)
summary(model_firth)

# Age vs toxicity
model_age <- glm(tox_clean ~ age,
                 data = df_tox,
                 family = binomial)
summary(model_age)

###############################################
# 3. KEY PLOTS
###############################################

# Age distribution
ggplot(df, aes(age)) +
  geom_histogram(bins = 20) +
  labs(title="Age Distribution", x="Age", y="Count")

# Menopause distribution
ggplot(df, aes(menopause_state)) +
  geom_bar() +
  labs(title="Menopause State Distribution")

# T / N / M / AJCC Stage
ggplot(df, aes(T_stage)) + geom_bar() + labs(title="T Stage Distribution")
ggplot(df, aes(N_stage)) + geom_bar() + labs(title="N Stage Distribution")
ggplot(df, aes(M_stage)) + geom_bar() + labs(title="M Stage Distribution")
ggplot(df, aes(AJCC_stage)) + geom_bar() + labs(title="AJCC Stage Distribution")

# Regimen frequencies
ggplot(df, aes(regimen_clean)) +
  geom_bar() +
  labs(title="Chemotherapy Regimen Frequency",
       x="Regimen", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Toxicity overall
ggplot(df_tox, aes(tox_clean)) +
  geom_bar() +
  labs(title="Overall Toxicity", x="Toxicity")

# Toxicity by regimen
ggplot(df_tox, aes(regimen_clean, fill = tox_clean)) +
  geom_bar(position = "fill") +
  labs(title="Toxicity by Chemotherapy Regimen",
       y="Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Toxicity by AJCC
ggplot(df_tox, aes(AJCC_stage, fill = tox_clean)) +
  geom_bar(position = "fill") +
  labs(title="Toxicity by AJCC Stage",
       y="Proportion")

# Toxicity by menopause
ggplot(df_tox, aes(menopause_state, fill = tox_clean)) +
  geom_bar(position = "fill") +
  labs(title="Toxicity by Menopause State",
       y="Proportion")

###############################################
# 4. MISSING DATA
###############################################

# Visdat heatmap
vis_miss(df)

# Percentage missing per variable
df_missing <- df %>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "MissingPercent")

df_missing %>%
  mutate(Variable = stringr::str_wrap(Variable, width = 20)) %>%
  ggplot(aes(x = MissingPercent,
             y = reorder(Variable, MissingPercent))) +
  geom_segment(aes(x = 0, xend = MissingPercent,
                   y = Variable, yend = Variable),
               color = "grey70") +
  geom_point(size = 2) +
  labs(title = "Missing Data (%) per Variable",
       x = "% Missing", y = "Variable") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# Highlight variables with >20% missing
df_missing %>%
  filter(MissingPercent > 20) %>%
  ggplot(aes(MissingPercent, reorder(Variable, MissingPercent))) +
  geom_segment(aes(x = 0, xend = MissingPercent,
                   y = Variable, yend = Variable),
               color = "grey70") +
  geom_point(size = 2) +
  labs(title = "Variables With >20% Missing",
       x = "% Missing", y = "Variable") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

#Break missing data plot into 6 different panels to enhance visibility
# Compute % missing per variable
df_missing <- df %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "Variable",
               values_to = "MissingPercent") %>%
  arrange(desc(MissingPercent)) %>%
  mutate(Variable = str_wrap(Variable, width = 18)) %>%   # wrap long names
  mutate(group = ntile(row_number(), 6))                   # split into 6 panels
# change 6 → 4 if needed

# Faceted missingness plot
ggplot(df_missing,
       aes(x = MissingPercent,
           y = reorder(Variable, MissingPercent))) +
  geom_segment(aes(x = 0,
                   xend = MissingPercent,
                   y = Variable,
                   yend = Variable),
               color = "grey70") +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Missing Data (%) per Variable",
    x = "% Missing",
    y = "Variable"
  ) +
  facet_wrap(~ group,
             scales = "free_y",
             ncol = 3) +             # 3 columns like your example
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    plot.title = element_text(size = 16, face = "bold")
  )

