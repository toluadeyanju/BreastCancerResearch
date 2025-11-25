# Install if needed
# install.packages("tidyverse")

library(tidyverse)

# Load the CSV
df <- read_csv("R PROJECT - Sheet1 (1).csv")

# View structure
glimpse(df)
# Convert categorical variables to factors
df <- df %>%
  mutate(across(where(is.character), as.factor))

# Check missing values
colSums(is.na(df))
table(df$menopause_state)
table(df$histology)
table(df$stage)
table(df$adjuvant_chemo_toxicity)
prop.table(table(df$adjuvant_chemo_toxicity))
df <- df %>%
  mutate(adjuvant_chemo_toxicity = case_when(
    adjuvant_chemo_toxicity %in% c("Yes", "yes") ~ "Yes",
    adjuvant_chemo_toxicity %in% c("No", "Nil") ~ "No",
    adjuvant_chemo_toxicity %in% c("-", "----------", "na", "") ~ NA_character_,
    TRUE ~ as.character(adjuvant_chemo_toxicity)
  )) %>%
  mutate(adjuvant_chemo_toxicity = as.factor(adjuvant_chemo_toxicity))
table(df$adjuvant_chemo_toxicity, useNA = "ifany")
prop.table(table(df$adjuvant_chemo_toxicity))



df <- df %>%
  mutate(menopause_state = case_when(
    menopause_state %in% c("pre-menopausal", "Pre-menopausal", "pre menopausal",
                           "Premenopausal", "premenopausal") ~ "Pre-menopausal",
    
    menopause_state %in% c("post-menopausal", "Post-menopausal", "post menopausal",
                           "Postmenopausal", "postmenopausal") ~ "Post-menopausal",
    
    menopause_state %in% c("-", "na", "Nil", "") ~ NA_character_,
    
    TRUE ~ as.character(menopause_state)
  )) %>%
  mutate(menopause_state = as.factor(menopause_state))
table(df$menopause_state, useNA = "ifany")

df <- df %>%
  mutate(menopause_state = case_when(
    
    # Pre-menopausal category
    menopause_state %in% c("Pre-menopausal", "pre-menopausal",
                           "pre menopausal", "Premenopausal", 
                           "premenopausal") ~ "Pre-menopausal",
    
    # Post-menopausal category (all variants)
    menopause_state %in% c("Post-menopausal", "post-menopausal",
                           "post menopausal", "Postmenopausal", 
                           "postmenopausal", "menopausal", "Post-manopausal") ~ "Post-menopausal",
    
    # Missing / invalid values
    menopause_state %in% c("na", "Nil", "-", "") ~ NA_character_,
    
    TRUE ~ as.character(menopause_state)
  )) %>%
  mutate(menopause_state = as.factor(menopause_state))

table(df$menopause_state, useNA = "ifany")

df <- df %>%
  mutate(T_stage = case_when(
    T_stage %in% c("T1", "t1", "1", "T 1", "T-1") ~ "T1",
    T_stage %in% c("T2", "t2", "2", "T 2", "T-2") ~ "T2",
    T_stage %in% c("T3", "t3", "3") ~ "T3",
    T_stage %in% c("T4", "t4", "4") ~ "T4",
    
    T_stage %in% c("-", "na", "Nil", "", "unknown") ~ NA_character_,
    TRUE ~ as.character(T_stage)
  )) %>%
  mutate(T_stage = factor(T_stage, levels = c("T1","T2","T3","T4")))


names(df)
library(tidyverse)

df <- df %>%
  mutate(tnm_stage = str_replace_all(tnm_stage, "\\s+", "")) %>%  # remove spaces
  mutate(
    T_stage = str_extract(tnm_stage, "T[0-9]"),
    N_stage = str_extract(tnm_stage, "N[0-9]"),
    M_stage = str_extract(tnm_stage, "M[0-9]")
  )
df <- df %>%
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
  )
df$T_stage <- factor(df$T_stage, levels = c("T1","T2","T3","T4"))
df$N_stage <- factor(df$N_stage, levels = c("N0","N1","N2","N3"))
df$M_stage <- factor(df$M_stage, levels = c("M0","M1"))
table(df$T_stage, useNA = "ifany")
table(df$N_stage, useNA = "ifany")
table(df$M_stage, useNA = "ifany")


df <- df %>%
  mutate(
    AJCC_stage = case_when(
      M_stage == "M1" ~ "Stage IV",
      N_stage %in% c("N2", "N3") ~ "Stage III",
      T_stage %in% c("T3") & N_stage %in% c("N0","N1") ~ "Stage II",
      T_stage %in% c("T1","T2") & N_stage == "N1" ~ "Stage II",
      T_stage %in% c("T1","T2") & N_stage == "N0" ~ "Stage I",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(AJCC_stage = factor(AJCC_stage, 
                             levels = c("Stage I","Stage II","Stage III","Stage IV")))

table(df$AJCC_stage, useNA = "ifany")
table(df$AJCC_stage, df$adjuvant_chemo_toxicity, useNA = "ifany")


tox_stage <- table(df$AJCC_stage, df$adjuvant_chemo_toxicity)

# Reduce to stages with actual data (Stage III & IV)
tox_stage_small <- tox_stage[c("Stage III","Stage IV"), c("No","Yes")]

tox_stage_small

fisher.test(tox_stage_small)

df_complete <- df %>% filter(!is.na(adjuvant_chemo_toxicity))

df_complete <- df %>% 
  filter(!is.na(adjuvant_chemo_toxicity))
nrow(df_complete)
colSums(is.na(df_complete[, c("age", "menopause_state", "AJCC_stage", 
                              "T_stage", "N_stage", "M_stage",
                              "histology")]))

colSums(is.na(df_complete[, c("age",
                              "menopause_state",
                              "AJCC_stage",
                              "T_stage",
                              "N_stage",
                              "M_stage")]))
model_age <- glm(adjuvant_chemo_toxicity ~ age,
                 data = df_complete,
                 family = binomial)

summary(model_age)

model_meno <- glm(adjuvant_chemo_toxicity ~ menopause_state,
                  data = df_complete,
                  family = binomial)

summary(model_meno)


table(df_complete$menopause_state, df_complete$adjuvant_chemo_toxicity)


table(df_complete$menopause_state, df_complete$adjuvant_chemo_toxicity)

fisher.test(table(df_complete$menopause_state, 
                  df_complete$adjuvant_chemo_toxicity))

install.packages("logistf")
install.packages("logistf", type = "source")

library(logistf)

model_firth <- logistf(adjuvant_chemo_toxicity ~ menopause_state,
                       data = df_complete)
summary(model_firth)




tox_ajcc <- table(df_complete$AJCC_stage,
                  df_complete$adjuvant_chemo_toxicity)

tox_ajcc
tox_ajcc_small <- tox_ajcc[c("Stage III", "Stage IV"), c("No", "Yes")]
tox_ajcc_small
fisher.test(tox_ajcc_small)

grep("chemo", names(df), value = TRUE, ignore.case = TRUE)

table(df$adjuvant_chemo, useNA = "ifany")

names(df)
table(df$drug_regimen, useNA = "ifany")


df <- df %>%
  mutate(regimen_clean = case_when(
    drug_regimen %in% c("AC","EC","Doxorubicin",
                        "AC+ Tamoxifen","AC, CMF",
                        "EC X 4, Docetaxel X 6",
                        "ECX8, Docetaxel x 1",
                        "EC + Docetaxel + Vinorelbin",
                        "CEF") ~ "AC/EC-based",
    
    drug_regimen %in% c("Docetaxel") ~ "Taxane-based",
    
    drug_regimen %in% c("CMF") ~ "CMF",
    
    drug_regimen %in% c("Xeloda","Tamoxifen>Xeloda") ~ "Capecitabine/Xeloda",
    
    drug_regimen %in% c("-", "na", NA) ~ "Missing",
    
    TRUE ~ "Other"
  ))

table(df$regimen_clean, useNA="ifany")
df_regimen <- df %>%
  filter(!is.na(adjuvant_chemo_toxicity),
         regimen_clean != "Missing")
tox_regimen <- table(df_regimen$regimen_clean,
                     df_regimen$adjuvant_chemo_toxicity)
tox_regimen

fisher.test(tox_regimen)
library(ggplot2)

df_regimen %>%
  ggplot(aes(regimen_clean, fill = adjuvant_chemo_toxicity)) +
  geom_bar(position = "fill") +
  labs(title = "Chemotherapy Toxicity by Regimen",
       y = "Proportion")


library(dplyr)
library(knitr)

# 1. Overall toxicity summary
overall <- df %>%
  summarise(
    Total = n(),
    Toxicity_Available = sum(!is.na(adjuvant_chemo_toxicity)),
    Toxicity_Yes = sum(adjuvant_chemo_toxicity == "Yes", na.rm = TRUE),
    Toxicity_No = sum(adjuvant_chemo_toxicity == "No", na.rm = TRUE),
    Toxicity_Missing = sum(is.na(adjuvant_chemo_toxicity))
  )

# 2. Toxicity by chemotherapy regimen (cleaned)
by_regimen <- df %>%
  filter(!is.na(adjuvant_chemo_toxicity)) %>%
  group_by(regimen_clean) %>%
  summarise(
    Total = n(),
    Yes = sum(adjuvant_chemo_toxicity == "Yes"),
    No = sum(adjuvant_chemo_toxicity == "No"),
    .groups = "drop"
  )

# 3. Toxicity by AJCC sta
3. Toxicity by AJCC stage
by_stage <- df %>%
  filter(!is.na(adjuvant_chemo_toxicity)) %>%
  group_by(AJCC_stage) %>%
  summarise(
    Total = n(),
    Yes = sum(adjuvant_chemo_toxicity == "Yes"),
    No = sum(adjuvant_chemo_toxicity == "No"),
    .groups = "drop"
  )

# 4. Toxicity by menopause state
by_meno <- df %>%
  filter(!is.na(adjuvant_chemo_toxicity)) %>%
  group_by(menopause_state) %>%
  summarise(
    Total = n(),
    Yes = sum(adjuvant_chemo_toxicity == "Yes"),
    No = sum(adjuvant_chemo_toxicity == "No"),
    .groups = "drop"
  )

# Print all tables
list(
  Overall_Toxicity = overall,
  Toxicity_by_Regimen = by_regimen,
  Toxicity_by_AJCC = by_stage,
  Toxicity_by_Menopause = by_meno
)









install.packages("naniar")
install.packages("visdat")



###############################################
# LOAD PACKAGES
###############################################
library(tidyverse)
library(ggplot2)
library(naniar)       # Missing data heatmap
library(visdat)       # Missing data visualization
library(dplyr)


###############################################
# LOAD DATA
###############################################
df <- read_csv("R PROJECT - Sheet1 (1).csv")


###############################################
# CLEAN TOXICITY VARIABLE
###############################################
df <- df %>%
  mutate(
    tox_clean = case_when(
      adjuvant_chemo_toxicity %in% c("Yes","yes") ~ "Yes",
      adjuvant_chemo_toxicity %in% c("No","Nil","no") ~ "No",
      TRUE ~ NA_character_
    )
  )


###############################################
# CLEAN REGIMEN GROUPS
###############################################
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


###############################################
# AGE GROUPS
###############################################
df <- df %>%
  mutate(age = as.numeric(age),
         age_group = cut(age,
                         breaks = c(0,40,60,100),
                         labels = c("<40","40–59","≥60")))


###############################################
# === DEMOGRAPHIC PLOTS ===
###############################################

# Age histogram
ggplot(df, aes(age)) +
  geom_histogram(bins = 20, fill="skyblue") +
  labs(title="Age Distribution", x="Age", y="Count")

# Age by menopause (boxplot)
ggplot(df, aes(menopause_state, age, fill=menopause_state)) +
  geom_boxplot() +
  labs(title="Age by Menopause State")

# Menopause distribution
ggplot(df, aes(menopause_state)) +
  geom_bar(fill="steelblue") +
  labs(title="Menopause State Distribution")


###############################################
# === TUMOR STAGING PLOTS ===
###############################################
df <- df %>%
  mutate(tnm_stage = str_replace_all(tnm_stage, "\\s+", "")) %>%
  mutate(
    T_stage = str_extract(tnm_stage, "T[0-9]"),
    N_stage = str_extract(tnm_stage, "N[0-9]"),
    M_stage = str_extract(tnm_stage, "M[0-9]")
  )

df <- df %>%
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
  )



df <- df %>%
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
  )

df <- df %>%
  mutate(
    AJCC_stage = case_when(
      M_stage == "M1" ~ "Stage IV",
      N_stage %in% c("N2","N3") ~ "Stage III",
      T_stage == "T3" & N_stage %in% c("N0","N1") ~ "Stage II",
      T_stage %in% c("T1","T2") & N_stage == "N1" ~ "Stage II",
      T_stage %in% c("T1","T2") & N_stage == "N0" ~ "Stage I",
      TRUE ~ NA_character_
    )
  )

df <- df %>%
  mutate(
    AJCC_stage = case_when(
      M_stage == "M1" ~ "Stage IV",
      N_stage %in% c("N2","N3") ~ "Stage III",
      T_stage == "T3" & N_stage %in% c("N0","N1") ~ "Stage II",
      T_stage %in% c("T1","T2") & N_stage == "N1" ~ "Stage II",
      T_stage %in% c("T1","T2") & N_stage == "N0" ~ "Stage I",
      TRUE ~ NA_character_
    )
  )

table(df$AJCC_stage, useNA="ifany")

# T stage
ggplot(df, aes(T_stage)) +
  geom_bar(fill="tomato") +
  labs(title="T Stage Distribution")

# N stage
ggplot(df, aes(N_stage)) +
  geom_bar(fill="darkorange") +
  labs(title="N Stage Distribution")

# M stage
ggplot(df, aes(M_stage)) +
  geom_bar(fill="purple") +
  labs(title="M Stage Distribution")

# AJCC Stage
ggplot(df, aes(AJCC_stage)) +
  geom_bar(fill="darkgreen") +
  labs(title="AJCC Stage Distribution")


###############################################
# === TREATMENT PLOTS ===
###############################################

# Chemotherapy regimen distribution
ggplot(df, aes(regimen_clean)) +
  geom_bar(fill="darkcyan") +
  labs(title="Chemotherapy Regimen Frequency",
       x="Regimen", y="Count") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


###############################################
# === TOXICITY PLOTS ===
###############################################

df_tox <- df %>% filter(!is.na(tox_clean))

# Toxicity overall
ggplot(df_tox, aes(tox_clean)) +
  geom_bar(fill="firebrick") +
  labs(title="Overall Toxicity", x="Toxicity")

# Toxicity by regimen
ggplot(df_tox, aes(regimen_clean, fill=tox_clean)) +
  geom_bar(position="fill") +
  labs(title="Toxicity by Chemotherapy Regimen",
       y="Proportion") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Toxicity by AJCC stage
ggplot(df_tox, aes(AJCC_stage, fill=tox_clean)) +
  geom_bar(position="fill") +
  labs(title="Toxicity by AJCC Stage",
       y="Proportion")

# Toxicity by menopause state
ggplot(df_tox, aes(menopause_state, fill=tox_clean)) +
  geom_bar(position="fill") +
  labs(title="Toxicity by Menopause State",
       y="Proportion")


###############################################
# === MISSING DATA VISUALIZATION ===
###############################################

# Missingness heatmap
vis_miss(df)

# Bar chart of missing percentage
df %>% 
  summarise(across(everything(), ~mean(is.na(.))*100)) %>%
  pivot_longer(everything(), names_to="Variable", values_to="MissingPercent") %>%
  ggplot(aes(reorder(Variable, MissingPercent), MissingPercent)) +
  geom_col(fill="gray50") +
  coord_flip() +
  labs(title="Missing Data (%) per Variable",
       y="% Missing", x="Variable")


###############################################
# END OF MASTER SCRIPT
###############################################

options(repr.plot.width = 14, repr.plot.height = 18)




df %>% 
  summarise(across(everything(), ~mean(is.na(.))*100)) %>% 
  pivot_longer(everything(), names_to="Variable", values_to="MissingPercent") %>% 
  ggplot(aes(x = MissingPercent, y = reorder(Variable, MissingPercent))) +
  geom_segment(aes(x=0, xend=MissingPercent, y=Variable, yend=Variable),
               color="grey") +
  geom_point(size=2, color="steelblue") +
  labs(title="Missing Data (%) per Variable", 
       x="% Missing", y="Variable") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=6),
        plot.title = element_text(size=14))



df %>% 
  summarise(across(everything(), ~mean(is.na(.))*100)) %>% 
  pivot_longer(everything(), names_to="Variable", values_to="MissingPercent") %>%
  ggplot(aes(x = MissingPercent, y = reorder(Variable, MissingPercent))) +
  geom_segment(aes(x=0, xend=MissingPercent, y=Variable, yend=Variable),
               color="grey") +
  geom_point(size=2, color="steelblue") +
  labs(title="Missing Data (%) per Variable", 
       x="% Missing", y="Variable") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=5),
        plot.title = element_text(size=14))



df_missing %>%
  mutate(Variable = stringr::str_wrap(Variable, width = 20)) %>%
  ggplot(aes(x = MissingPercent,
             y = reorder(Variable, MissingPercent))) +
  geom_segment(aes(x = 0, xend = MissingPercent,
                   y = Variable, yend = Variable),
               color = "grey70") +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Missing Data (%) per Variable",
    x = "% Missing", y = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5),
    plot.title = element_text(size = 16, face = "bold")
  )


library(dplyr)
library(ggplot2)

df_missing <- df %>% 
  summarise(across(everything(), ~mean(is.na(.))*100)) %>% 
  pivot_longer(everything(), names_to="Variable", values_to="MissingPercent") %>%
  mutate(group = ntile(row_number(), 6))  # split into 4 panels

ggplot(df_missing, aes(x = MissingPercent,
                       y = reorder(Variable, MissingPercent))) +
  geom_segment(aes(x=0, xend=MissingPercent, y=Variable, yend=Variable),
               color = "grey70") +
  geom_point(size = 2, color = "steelblue") +
  labs(title="Missing Data (%) per Variable",
       x="% Missing", y="Variable") +
  facet_wrap(~ group, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=6))



df_missing %>%
  mutate(Variable = stringr::str_wrap(Variable, width = 20)) %>%
  ggplot(aes(x = MissingPercent,
             y = reorder(Variable, MissingPercent))) +
  geom_segment(aes(x = 0, xend = MissingPercent,
                   y = Variable, yend = Variable),
               color = "grey70") +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Missing Data (%) per Variable",
       x = "% Missing", y = "Variable") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5))


df_missing %>%
  filter(MissingPercent > 20) %>%
  ggplot(aes(MissingPercent, reorder(Variable, MissingPercent))) +
  geom_segment(aes(x=0, xend=MissingPercent, y=Variable, yend=Variable),
               color="grey70") +
  geom_point(size=2, color="firebrick") +
  labs(title="Variables With >20% Missing", x="% Missing", y="Variable") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=6))


model_age <- glm(tox_clean ~ age, data=df_complete, family=binomial)
summary(model_age)

df <- df %>%
  mutate(
    tox_clean = case_when(
      adjuvant_chemo_toxicity %in% c("Yes","yes") ~ "Yes",
      adjuvant_chemo_toxicity %in% c("No","Nil","no") ~ "No",
      TRUE ~ NA_character_
    )
  )

df_complete <- df %>% filter(!is.na(tox_clean))
model_age <- glm(tox_clean ~ age,
                 data = df_complete,
                 family = binomial)
summary(model_age)
