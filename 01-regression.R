# In class exercise
# Regressions from Smith et al. 2024

library(tidyverse)

#### Wrangling data ####
# Figure 3 regression predictors
map <- read_csv("data/datappt_MAP2.csv") |> 
  select(-1, -X)
prior_ppt <- read_csv("data/datappt_pryr.csv") |> 
  select(-1)
map_cv <- read_csv("data/Dryad_submission_datappt_cv2.csv") |> 
  select(-1)
ai <- read_csv("data/datappt_ai3.csv") |> 
  select(-1)
sand <- read_csv("data/Dryad_submission_datapptsand2.csv") |> 
  select(-1)
graminoid <- read_csv("data/Dryad_submission_gram.ave.csv") |> 
  select(-1)
richness <- read_csv("data/Dryad_submission_datappt_rich2.csv") |> 
  select(-1)

# Figure 4 drought severity
drought_severity <- read_csv("data/data.Drtsite.csv") |> 
  select(-1)

# Create full dataset from above
data_all <- read_csv("data/data.hab.csv") |> 
  select(-1) |> 
  rename(habitat_type = habitat.type) |> 
  left_join(map) |> 
  relocate(precip, .after = Sig) |> 
  left_join(prior_ppt |> 
              select(site_code, drtpct_map730)) |> 
  left_join(map_cv |> 
              select(site_code, cv_ppt_inter)) |> 
  left_join(ai |> 
              select(site_code, logAI)) |> 
  left_join(sand|> 
              select(site_code, sand_mean)) |> 
  left_join(graminoid |> 
              select(site_code, prop.ave)) |> 
  left_join(richness |> 
              select(site_code, average.richness)) |> 
  left_join(drought_severity|> 
              select(site_code, mean_drt2)) |> 
  rename(DR = mean_DS3, # Drought response
         MAP = precip,
         prev_precip = drtpct_map730,
         MAP_cv = cv_ppt_inter,
         lnAridity = logAI,
         gram_prop = prop.ave,
         rich_mean = average.richness,
         DS = mean_drt2) # Drought severity



# Run individual regressions
m_map <- lm(DR ~ MAP, data = data_all)
summary(m_map)

# Do your results match what is reported in the text?

# Challenge, how would you make a singular, facetted plot for Fig. 3?

# Inverse texture hypothesis

m_inv_tex <- lm(DR ~ MAP * sand_mean, data = data_all)
summary(m_inv_tex)

# Is there statistical support for the inverse texture hypothesis?

# Replicate Fig 4

m_ds <- lm(DR ~ DS, data = data_all)
summary(m_ds)


# Replicate Table S8
m_multiple <- lm(DR ~ DS + MAP + prev_precip + gram_prop + MAP_cv + sand_mean,
                 data = data_all)
summary(m_multiple)
