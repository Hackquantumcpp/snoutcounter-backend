library(tidyverse)
library(rstan)
library(rstanarm)
library(janitor)
library(rsample) # rsample in tidymodels
library(DescTools)
library(progressr)
library(locpol)
library(broom.mixed)

options(mc.cores = parallel::detectCores(logical = FALSE))

# Get banned pollsters

setwd("../")
source("banned_pollsters.R")
setwd(paste0(getwd(), "/senate_primary_2026/"))

setwd("../../")

ratings <- read_csv("ratings/pollster_ratings_silver.csv") %>% janitor::clean_names()

ratings_24 <- read_csv("ratings/pollster_ratings_silver_2024.csv") %>% janitor::clean_names()

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ9QMspmsUAnmpt-SA_anpnJ-st5zOuH6NN5MB9Mt1UzXv1le5_4PQDdfikYJ6RMIPfEjARVXyMapaT/pub?output=csv"
  
polls <- read_csv(url)

setwd(paste0(getwd(), "/data/"))

write_csv(polls, "senate_primary_2026_polls.csv")

setwd("../R/senate_primary_2026")

polls <- polls %>% filter(!(pollster %in% banned_pollsters)) %>%
  filter(is.na(population) == FALSE) %>% filter(is.na(end_date) == FALSE)

polls <- polls %>% filter(state == "Michigan") %>% filter(
  subpopulation == "D"
)

cands <- c("Mallory Ann McMorrow", "Abdul El-Sayed", "Haley Stevens")

cand_cols <- c("haley_stevens_dem", "mallory_ann_mc_morrow_dem",
               "abdul_el_sayed_dem")

tracking_polls_pipeline <- function(data_frame) {
  df <- data_frame %>% filter(tracking == TRUE)
  pollsters <- as.vector(df %>% distinct(pollster))$pollster
  
  df_tracking <- tibble()
  
  for (p in pollsters) {
    df_pollst <- df %>% filter(pollster == p) %>%
      rowwise() %>%
      mutate(interval = start_date %--% end_date) %>%
      ungroup() %>%
      arrange(desc(end_date))
    
    if (dim(df_pollst)[1] == 1) {
      df_tracking <- bind_rows(df_tracking, df_pollst)
      next
    }
    
    ptr <- 1
    
    while (ptr <= dim(df_pollst)[1]) {
      interv_metric <- df_pollst$interval[ptr]
      
      df_pollst <- df_pollst %>% filter(
        interval == interv_metric | !(int_overlaps(interval, interv_metric) == TRUE)
      )
      
      ptr <- ptr + 1
    }
    
    df_tracking <- bind_rows(df_tracking, df_pollst)
  }
  
  return(df_tracking)
}

polls_in_window <- function(data_frame, date, pid) {
  df <- data_frame # Copy data frame
  
  thres = date - 14
  df <- df %>% filter(poll_spon_id == pid & end_date >= thres)
  return(max(dim(df)[1], 1)) ## REMEMBER, IMPORTANT, NOT ZERO INDEXED IN R!!!
}

poll_avg <- function(data_frame, date) {
  # Copy data frame, filter for all those less than given date
  df <- data_frame %>% filter(end_date <= date)
  
  ### Sample size weights
  size_cap <- 2000
  
  df_nullsampsize <- df %>% filter(is.na(sample_size) == TRUE)
  
  impute_sample_size <- function(data_frame, data_frame_nullsampsize, pollster, mode) {
    df <- data_frame # Copy data farme
    df_pollst <- df %>% filter(pollster == pollster)
    df_mode <- df %>% filter(mode == mode)
    
    if (nrow(df_pollst) != 0) {
      return(median(df_pollst$sample_size))
    }
    else if (nrow(df_mode) != 0) {
      return (median(df_mode$sample_size))
    }
    else {
      return (median(df$sample_size))
    }
  }
  
  impute_sample_size_dfnullsampsize <- function(pollster, mode) {
    return(impute_sample_size(df %>% select(pollster, mode, sample_size), df_nullsampesize, pollster, mode))
  }
  
  df <- df %>% filter(is.na(sample_size) == FALSE)
  df <- df %>% mutate(sample_size_winsr = pmin(sample_size, size_cap))
  df <- df %>% mutate(sample_size_winsr = Winsorize(sample_size_winsr, val = quantile(sample_size_winsr, probs = c(0.025, 0.975), na.rm = FALSE)))
  
  df_nullsampsize <- df_nullsampsize %>% rowwise() %>%
    mutate(sample_size_winsr = impute_sample_size_dfnullsampsize(pollster, mode)) %>%
    ungroup()
  
  df <- bind_rows(df, df_nullsampsize)
  
  df <- df %>% mutate(sample_size_weight = sqrt(pmin(sample_size_winsr, size_cap)) / sqrt(median(pmin(sample_size_winsr, size_cap))))
  
  # Quick wrangling
  df$sponsors[df$pollster == "CNN/SSRS"] <- "CNN"
  df$pollster[df$pollster == "CNN/SSRS"] <- "SSRS"
  df <- df %>% mutate(
    pollster_ratname = recode(pollster,
                              "Quantus Insights" = "Quantus Polls and News",
                              "University of California Berkeley Institute of Governmental Studies" = "University of California Berkeley",
                              "University of Southern California/California State University Long Beach Center for Urban Politics and Policy/Cal Poly Pomona" = "University of Southern California",
                              "University of California Irvine School of Social Ecology/Truedot" = "Truedot",
                              "University of California Berkeley Jack Citrin Center for Public Opinion Research/Possibility Lab/TrueDot" = "Truedot",
                              "University of California Berkeley Jack Citrin Center for Public Opinion Research/TrueDot" = "Truedot"
    )
  )
  
  ### Quality weights
  df_25 <- df %>% filter(end_date < ymd("2026-01-14")) %>% left_join(ratings_24 %>% rename(pollster_ratname = pollster),
                                                                     join_by(pollster_ratname))
  
  df_26 <- df %>% filter(end_date >= ymd("2026-01-14")) %>% left_join(ratings %>% rename(pollster_ratname = pollster),
                                                                      join_by(pollster_ratname))
  df <- bind_rows(df_25, df_26)
  
  df <- df %>%
    filter(
      !(pollster_ratname %in% (ratings %>% filter(grade == "F@@16") %>% select(pollster)))
    ) %>%
    mutate(
      predictive_plus_minus = coalesce(predictive_plus_minus, 5),
      # quality_weight = if_else(predictive_plus_minus < 0.5, exp(-predictive_plus_minus/1.3), 0.2)
      quality_weight = if_else(predictive_plus_minus <= 1, sqrt(1/2.4 * (1 - predictive_plus_minus)) + 0.2, 0.2)    
    )
  
  pid_in_window <- function(end_date, pid) {
    return(polls_in_window(df, end_date, pid))
  }
  
  pid_in_window <- function(end_date, pid) {
    return(polls_in_window(df, end_date, pid))
  }
  
  ### Multiple polls in short window weights
  df <- df %>% group_by(pollster) %>%
    mutate(poll_spon_id = cur_group_id()) %>%
    ungroup()
  df <- df %>% rowwise() %>% mutate(zone_flood_weight = 1 / sqrt(pid_in_window(end_date, poll_spon_id))) %>%
    ungroup()
  
  ### Recency weight
  window <- 30
  df <- df %>% mutate(recency_weight = 0.1^(as.numeric(date - end_date, units = "days")/window))
  
  ## Partisan downweight
  partisan_dw <- 0.8
  df <- df %>% mutate(
    partisan_downweight = if_else(partisan == "NA", 1, partisan_dw)
  )
  
  ## Internals downweight
  internals_dw <- 0.5 / partisan_dw
  df <- df %>% mutate(
    internals_downweight = if_else(internal == FALSE, 1, internals_dw)
  )
  
  ### Bring it all together
  df <- df %>% mutate(total_weight = sample_size_weight * recency_weight * quality_weight * zone_flood_weight * partisan_downweight * internals_downweight)
  df$total_weight <- df$total_weight / sum(df$total_weight)
  
  # Drop columns from ratings data frame
  df <- df %>% select(-predictive_plus_minus, -mean_reverted_bias, -number_of_polls, -cat, -grade)
  
  return(df)
}

avg_over_time <- function(data_frame) {
  df <- data_frame # Copy data frame
  
  df_avg <- tibble()
  
  avg_oneday <- function(date, cand) {
    df_weights <- poll_avg(data_frame, date) %>% filter(is.na(.[[cand]]) == FALSE)
    df_weights$total_weight <- df_weights$total_weight / sum(df_weights$total_weight)
    cand_avg <- sum(df_weights$total_weight * df_weights[[cand]])
    cand_sd <- sqrt(sum(df_weights$total_weight * (df_weights[[cand]] - cand_avg)^2))
    
    return(
      list(
        avg = cand_avg,
        sd = cand_sd,
        cand = cand
      )
    )
  }
  
  for (cand in cand_cols) {
    
    print(cand)
    
    df_cand <- tibble()
    df_only_cand <- df_weights %>% filter(is.na(df_weights[[cand]]) == FALSE)
    date_interv <- seq(pmax(min(df_only_cand$end_date, na.rm = TRUE), ymd("2025-01-01")), today(), by = "day")
    
    with_progress({
      p <- progressor(along = date_interv)
        
      df_cand <- bind_cols(
        tibble(end_date = date_interv),
        map_dfr(date_interv, function(d) {
          p()
          avg_oneday(d, cand)
        })
      )
    })
    
    if (nrow(df_avg) == 0) {
      df_avg <- df_cand
    }
    
    else {
      df_avg <- bind_rows(df_avg, df_cand)
    }
  }
  
  return(df_avg)
}

#### With adjustments
avg_over_time_w_adj <- function(data_frame, model, cand) {
  options(warn = -1)
  
  df <- data_frame # Copy data frame
  
  fixed <- tidy(model, effects = "fixed", conf.int = TRUE, conf.level = 0.95)
  randomeff <- tidy(model, effects = "ran_vals", conf.int = TRUE, conf.level = 0.95)
  
  df_avg <- tibble()
  
  avg_oneday <- function(date, cand) {
    df_weights <- poll_avg(data_frame, date) %>% filter(is.na(.[[cand]]) == FALSE)
    
    ## All other adjustments
    
    np_a <- ranef(model)$partisan["NA", 1]
    nospon_a <- ranef(model)$sponsor_candidate["NA", 1]
    
    df_weights <- df_weights %>%
      left_join( (rownames_to_column(ranef(model)$pollster) %>% rename(pollster = rowname, house_effect = "(Intercept)") %>% mutate(house_effect = -1 * house_effect)), join_by(pollster)) %>%  
      left_join( (rownames_to_column(ranef(model)$mode) %>% rename(mode = rowname, mode_adj = "(Intercept)") %>% mutate(mode_adj = -1 * mode_adj)), join_by(mode)) %>%
      # left_join((rownames_to_column(ranef(fit)$population) %>% rename(population = rowname, pop_adj = "(Intercept)") %>% mutate(population = as.character(population), pop_adj = pop_a - pop_adj)), join_by(population))%>%
      left_join( (rownames_to_column(ranef(model)$partisan) %>% rename(partisan = rowname, partisan_adj = "(Intercept)") %>% mutate(partisan_adj = np_a - partisan_adj)), join_by(partisan)) %>%
      left_join( (rownames_to_column(ranef(model)$sponsor_candidate) %>% rename(sponsor_candidate = rowname, sponsorcand_adj = "(Intercept)") %>% mutate(sponsorcand_adj = nospon_a - sponsorcand_adj)), join_by(sponsor_candidate))
    
    if(date == ymd("2025-08-02")) { ## Debug
      View(df_weights)
    }
    
    df_weights[[cand]] <- df_weights[[cand]] + df_weights$house_effect + df_weights$mode_adj + df_weights$partisan_adj + df_weights$sponsorcand_adj
    
    df_weights$total_weight <- df_weights$total_weight / sum(df_weights$total_weight)
    cand_avg <- sum(df_weights$total_weight * df_weights[[cand]])
    cand_sd <- sqrt(sum(df_weights$total_weight * (df_weights[[cand]] - cand_avg)^2))
    
    return(
      list(
        avg = cand_avg,
        sd = cand_sd,
        cand = cand
      )
    )
  }
  
  df_cand <- tibble()
  df_only_cand <- df_weights %>% filter(is.na(df_weights[[cand]]) == FALSE)
  date_interv <- seq(pmax(min(df_only_cand$end_date, na.rm = TRUE), ymd("2025-08-01")), today(), by = "day")
    
  with_progress({
    p <- progressor(along = date_interv)
      
    df_cand <- bind_cols(
      tibble(end_date = date_interv),
      map_dfr(date_interv, function(d) {
      p()
        suppressMessages({
          avg_oneday(d, cand)
        })
      })
    )
  })
  
  options(warn = 0)
  
  return(df_cand)
}

polls <- polls %>% mutate(end_date = ymd(end_date), start_date = ymd(start_date)) %>%
  group_by(pollster, sponsors, sponsor_candidate, start_date, end_date) %>%
  mutate(poll_id = cur_group_id()) %>%
  ungroup()

polls <- polls %>% pivot_wider(
  names_from = c(candidate_name, party),
  values_from = pct
) %>% janitor::clean_names()

# polls_tracking <- tracking_polls_pipeline(polls) %>% select(-interval) # Drop interval column

# polls <- polls %>% filter(tracking == "FALSE")

# polls <- bind_rows(polls, polls_tracking)

polls <- polls %>% arrange(pollster) %>%
  mutate(mode = replace_na(mode, "Unknown"))

question_dup <- polls %>%
  group_by(poll_id) %>%
  filter(n() > 1) %>%
  ungroup()

# question_ids_excl <- c(217016, 218250, 219208, 222699, 211099, 0)

# polls <- polls %>% filter(!(question_id %in% question_ids_excl))

polls <- polls %>% 
  mutate(population = recode(population, "LV" = "b", "RV" = "c", "A" = "e")) %>% 
  arrange(population) %>% 
  distinct(poll_id, .keep_all = TRUE) %>% 
  mutate(population = recode(population, "b" = "LV", "c" = "RV", "e" = "A"))

polls <- polls %>% mutate(partisan = replace_na(partisan, "NA"))  %>%
  arrange(end_date)

## Exclude hypothetical polls
## Perhaps later we can re-include them
polls <- polls %>% filter(
  is.na(pete_buttigieg_dem) == TRUE
) %>% filter(
  is.na(gretchen_whitmer_dem) == TRUE
) %>% mutate(
  sponsors = replace_na(sponsors, "NA"),
  sponsor_candidate = replace_na(sponsor_candidate, "NA"),
  partisan = replace_na(partisan, "NA")
)

df_weights <- poll_avg(polls, today())

df_avg <- avg_over_time(polls)

ggplot(df_avg, aes(x = end_date, y = avg, group = cand, color = cand)) + geom_line(
  size = 1
)

df_allavg <- tibble()

## Haley Stevens
cand_name = "haley_stevens_dem"
  
print(cand_name)

polls_cand <- polls %>% left_join(df_avg %>% filter(cand == cand_name) %>%
                               select(end_date, avg), join_by(end_date))
  
polls_cand <- polls_cand %>% mutate(
    partisan = replace_na(partisan, "NA"),
    sponsor_candidate = replace_na(sponsor_candidate, "NA")
)
  
polls_cand <- polls_cand %>% rename(cand = !!cand_name)
polls_cand$cand <- as.numeric(polls_cand$cand)

fit <- stan_glmer(cand ~ 0 + (1 | pollster) + (1 | mode) +
                    (1 | sponsor_candidate) + (1 | partisan) +
                      avg,
                    family = gaussian(),
                    data = polls_cand,
                    prior = normal(0, 1, autoscale = TRUE),
                    prior_covariance = decov(scale = 0.50),
                    adapt_delta = 0.99,
                    refresh = 100,
                    seed = 1010)
  
print(fit)
print(summary(fit))
print(fixef(fit))
print(ranef(fit))
  
df_stevens <- avg_over_time_w_adj(polls, fit, "haley_stevens_dem")

df_allavg <- df_stevens

## Mallory McMorrow
cand_name = "mallory_ann_mc_morrow_dem"

print(cand_name)

polls_cand <- polls %>% left_join(df_avg %>% filter(cand == cand_name) %>%
                                    select(end_date, avg), join_by(end_date))

polls_cand <- polls_cand %>% mutate(
  partisan = replace_na(partisan, "NA"),
  sponsor_candidate = replace_na(sponsor_candidate, "NA")
)

polls_cand <- polls_cand %>% rename(cand = !!cand_name)
polls_cand$cand <- as.numeric(polls_cand$cand)


fit <- stan_glmer(cand ~ 0 + (1 | pollster) + (1 | mode) +
                    (1 | sponsor_candidate) + (1 | partisan) +
                    avg,
                  family = gaussian(),
                  data = polls_cand,
                  prior = normal(0, 1, autoscale = TRUE),
                  prior_covariance = decov(scale = 0.50),
                  adapt_delta = 0.99,
                  refresh = 100,
                  seed = 1010)

print(fit)
print(summary(fit))
print(fixef(fit))
print(ranef(fit))

df_mcmorrow <- avg_over_time_w_adj(polls, fit, "mallory_ann_mc_morrow_dem")

df_allavg <- bind_rows(df_allavg, df_mcmorrow)

## Abdul el-Sayed
cand_name = "abdul_el_sayed_dem"

print(cand_name)

polls_cand <- polls %>% left_join(df_avg %>% filter(cand == cand_name) %>%
                                    select(end_date, avg), join_by(end_date))

polls_cand <- polls_cand %>% mutate(
  partisan = replace_na(partisan, "NA")
)

polls_cand <- polls_cand %>% rename(cand = !!cand_name)
polls_cand$cand <- as.numeric(polls_cand$cand)

fit <- stan_glmer(cand ~ 0 + (1 | pollster) + (1 | mode) +
                    (1 | sponsor_candidate) + (1 | partisan) +
                    # factor(minor_cands_in) + 
                    # factor(steyer_in) +
                    # factor(swalwell_in) + factor(mahan_in) +
                    # factor(langford_in) + factor(cloobeck_in) + 
                    # factor(calderon_in) + 
                    avg,
                  family = gaussian(),
                  data = polls_cand,
                  prior = normal(0, 1, autoscale = TRUE),
                  prior_covariance = decov(scale = 0.50),
                  adapt_delta = 0.99,
                  refresh = 100,
                  seed = 1010)

print(fit)
print(summary(fit))
print(fixef(fit))
print(ranef(fit))

df_elsayed <- avg_over_time_w_adj(polls, fit, "abdul_el_sayed_dem")

df_allavg <- bind_rows(df_allavg, df_elsayed)

ggplot(df_allavg, aes(x = end_date, y = avg, group = cand, color = cand)) + geom_line(
  size = 1)

df_allavg <- df_allavg %>% mutate(
  upper_ci = avg + 1.96*sd,
  lower_ci = pmax(avg - 1.96*sd, 0)
)

df_mi_avg <- df_allavg %>% pivot_wider(
  names_from = cand,
  values_from = c(avg, sd, upper_ci, lower_ci)
)

setwd("../../averages/")

write_csv(df_ca_avg, 'mi_senate_primary_2026.csv')

setwd("../R/")

# Polls dataset - display table

avg_today <- poll_avg(polls, today())

setwd("../transformed_tables/")

write_csv(avg_today, 'mi_senate_primary_2026_transf.csv')

setwd("../R/senate_primary_2026")