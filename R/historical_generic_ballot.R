library(tidyverse)
library(rstan)
library(rstanarm)
library(janitor)
library(rsample) # rsample in tidymodels
library(DescTools)
library(progressr)

options(mc.cores = parallel::detectCores(logical = FALSE))

# Get banned pollsters
source("banned_pollsters.R")

setwd("../")

ratings <- read_csv("ratings/pollster_ratings_silver.csv") %>% janitor::clean_names()

ratings_24 <- read_csv("ratings/pollster_ratings_silver_2024.csv") %>% janitor::clean_names()

filepath <- "data/fte/generic_ballot_polls_historical.csv"

polls <- read_csv(filepath)

setwd(paste0(getwd(), "/R/"))

polls <- polls %>% filter(!(display_name %in% banned_pollsters))

year <- 2018

polls <- polls %>% filter(cycle == year)

labor_day <- ymd("2018-09-03")

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
  size_cap <- 5000
  df_nullsampsize <- df %>% filter(is.na(sample_size) == TRUE)
  
  impute_sample_size <- function(data_frame, data_frame_nullsampsize, pollster, mode) {
    df <- data_frame # Copy data frame
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
  
  ### Quality weights
  df <- df %>%
    mutate(
      pollscore = coalesce(pollscore, 5),
      # quality_weight = if_else(predictive_plus_minus < 0.5, exp(-predictive_plus_minus/1.3), 0.2)
      quality_weight = if_else(pollscore <= 1, sqrt(1/2.4 * (1 - pollscore)) + 0.2, 0.2)    
    )
  
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
  if (date < labor_day) {
    window <- 30
  }
  else {
    delta <- as.numeric(date - labor_day, units = "days")
    window <- max(-(7/30)*delta + 30, 21)
  }
  df <- df %>% mutate(recency_weight = 0.1^(as.numeric(date - end_date, units = "days")/window))
  
  ## Partisan downweight
  partisan_dw <- 0.8
  df <- df %>% mutate(
    partisan_downweight = if_else(partisan == "NA", 1, partisan_dw)
  )
  
  ### Bring it all together
  df <- df %>% mutate(total_weight = sample_size_weight * quality_weight * zone_flood_weight * recency_weight * partisan_downweight)
  df$total_weight <- df$total_weight / sum(df$total_weight)
  
  return(df)
}

avg_over_time <- function(data_frame) {
  df <- data_frame # Copy data frame
  
  #date_interv <- seq(ymd("2025-01-03"), today(), by = "day")
  
  date_interv <- seq(min(df$end_date), max(df$end_date), by = "day")
  
  print(interactive())
  
  df_avg <- tibble(
    end_date = date_interv
  )
  
  avg_oneday <- function(date) {
    df_weights <- poll_avg(data_frame, date)
    rep_avg <- sum(df_weights$total_weight * df_weights$rep)
    dem_avg <- sum(df_weights$total_weight * df_weights$dem)
    net_avg <- sum(df_weights$total_weight * df_weights$net)
    
    rep_sd <- sqrt(sum(df_weights$total_weight * (df_weights$rep - rep_avg)^2))
    dem_sd <- sqrt(sum(df_weights$total_weight * (df_weights$dem - dem_avg)^2))
    net_sd <- sqrt(sum(df_weights$total_weight * (df_weights$net - net_avg)^2))
    
    return(list(dem = dem_avg, 
                rep = rep_avg, 
                net = net_avg,
                dem_std = dem_sd,
                rep_std = rep_sd,
                net_std = net_sd))
  }
  
  with_progress({
    p <- progressor(along = df_avg$end_date)
    
    df_avg <- bind_cols(
      df_avg,
      map_dfr(df_avg$end_date, function(d) {
        p()
        avg_oneday(d)
      })
    )
  })
  
  
  df_avg <- df_avg %>% mutate(
    dem_lower_ci = dem - 1.96*dem_std,
    dem_upper_ci = dem + 1.96*dem_std,
    rep_lower_ci = rep - 1.96*rep_std,
    rep_upper_ci = rep + 1.96*rep_std,
    net_lower_ci = net - 1.96*net_std,
    net_upper_ci = net + 1.96*net_std
  )
  
  return(df_avg)
}

polls <- polls %>% mutate(end_date = mdy(end_date), 
                          start_date = mdy(start_date),
                          tracking = if_else(is.na(tracking), FALSE, TRUE),
                          population = toupper(population))

polls_tracking <- tracking_polls_pipeline(polls) %>% select(-interval) # Drop interval column

polls <- polls %>% filter(tracking == "FALSE")

polls <- bind_rows(polls, polls_tracking)

polls <- polls %>% arrange(pollster) %>%
  rename(mode = methodology) %>%
  mutate(mode = replace_na(mode, "Unknown"))

polls <- polls %>% 
  mutate(population = recode(population, "LV" = "b", "RV" = "c", "A" = "e")) %>% 
  arrange(population) %>% 
  distinct(poll_id, .keep_all = TRUE) %>% 
  mutate(population = recode(population, "b" = "LV", "c" = "RV", "e" = "A"))

polls <- polls %>% mutate(net = rep - dem, partisan = replace_na(partisan, "NA")) 

# polls <- poll_avg(polls, today())

df_avg <- avg_over_time(polls)

# df_avg <- df_avg %>% mutate(lagged_net = lag(net, 1))

polls <- polls %>% left_join(df_avg %>% select(end_date, dem, rep, net) %>% rename(dem_avg = dem, rep_avg = rep)
                             , join_by(end_date)) %>%
  rename(net = net.x, net_avg = net.y)


## Democratic adjustments
fit <- stan_glmer(dem ~ 0 + (1 | pollster) + (1 | partisan) + (1 | population) +
                    (1 | mode) + dem_avg,
                  family = gaussian(),
                  data = polls,
                  prior = normal(0, 1, autoscale = TRUE),
                  prior_covariance = decov(scale = 0.50),
                  adapt_delta = 0.99,
                  refresh = 100,
                  seed = 1010)

print(fit)
print(summary(fit))
print(fixef(fit))
print(ranef(fit))

## For now we want to convert to RV due to likely voter samples being less
## reliable at this point in time; come Labor Day we want to switch
## to converting to LV.
## TODO: Edit to account for the Labor Day switch!
pop_a <- ranef(fit)$population[2, 1]
np_a <- ranef(fit)$partisan[2, 1]

polls <- polls %>% select(-net_avg) # Drop net avg

polls <- polls %>%
  left_join( (rownames_to_column(ranef(fit)$pollster) %>% rename(pollster = rowname, house_effect = "(Intercept)") %>% mutate(house_effect = -1 * house_effect)), join_by(pollster)) %>%  
  left_join( (rownames_to_column(ranef(fit)$mode) %>% rename(mode = rowname, mode_adj = "(Intercept)") %>% mutate(mode_adj = -1 * mode_adj)), join_by(mode)) %>%
  left_join((rownames_to_column(ranef(fit)$population) %>% rename(population = rowname, pop_adj = "(Intercept)") %>% mutate(population = as.character(population), pop_adj = pop_a - pop_adj)), join_by(population))%>%
  left_join( (rownames_to_column(ranef(fit)$partisan) %>% rename(partisan = rowname, partisan_adj = "(Intercept)") %>% mutate(partisan_adj = np_a - partisan_adj)), join_by(partisan))

polls_og <- polls %>% arrange(end_date)

polls <- polls %>% mutate(
  dem = dem + house_effect + mode_adj + partisan_adj + pop_adj
) %>% arrange(end_date)

polls <- polls %>% select(-house_effect, -mode_adj, -partisan_adj, -pop_adj)


## Republican adjustments
fit <- stan_glmer(rep ~ 0 + (1 | pollster) + (1 | partisan) + (1 | population) +
                    (1 | mode) + rep_avg,
                  family = gaussian(),
                  data = polls,
                  prior = normal(0, 1, autoscale = TRUE),
                  prior_covariance = decov(scale = 0.50),
                  adapt_delta = 0.99,
                  refresh = 100,
                  seed = 1010)

print(fit)
print(summary(fit))
print(fixef(fit))
print(ranef(fit))

pop_a <- ranef(fit)$population[2, 1]
np_a <- ranef(fit)$partisan[2, 1]

polls <- polls %>%
  left_join( (rownames_to_column(ranef(fit)$pollster) %>% rename(pollster = rowname, house_effect = "(Intercept)") %>% mutate(house_effect = -1 * house_effect)), join_by(pollster)) %>%  
  left_join( (rownames_to_column(ranef(fit)$mode) %>% rename(mode = rowname, mode_adj = "(Intercept)") %>% mutate(mode_adj = -1 * mode_adj)), join_by(mode)) %>%
  left_join((rownames_to_column(ranef(fit)$population) %>% rename(population = rowname, pop_adj = "(Intercept)") %>% mutate(population = as.character(population), pop_adj = pop_a - pop_adj)), join_by(population))%>%
  left_join( (rownames_to_column(ranef(fit)$partisan) %>% rename(partisan = rowname, partisan_adj = "(Intercept)") %>% mutate(partisan_adj = np_a - partisan_adj)), join_by(partisan))

polls <- polls %>% mutate(
  rep = rep + house_effect + mode_adj + partisan_adj + pop_adj
) %>% arrange(end_date)

polls <- polls %>% mutate(
  net = rep - dem
)

# today_avg = poll_avg(polls, today())
generic_ballot_avg <- avg_over_time(polls)

ggplot(
  generic_ballot_avg, aes(x = end_date)
) + geom_line(size = 1, mapping = aes(y = rep, color = "Republicans")) +
  geom_line(size = 1, mapping = aes(y = dem, color = "Democrats")) +
  scale_color_manual(
    name = "Legend",
    values = c("Republicans" = "red", "Democrats" = "blue")
  ) +
  geom_ribbon(aes(ymin = rep_lower_ci, ymax = rep_upper_ci), fill = "#fa928e",
              alpha = 0.4) +
  geom_ribbon(aes(ymin = dem_lower_ci, ymax = dem_upper_ci), fill = "#8e96fa",
              alpha = 0.4) +
  geom_point(data = polls, mapping = aes(y = rep), color = "red", alpha = 0.2) +
  geom_point(data = polls, mapping = aes(y = dem), color = "blue", alpha = 0.2) +
  labs(
    x = "Date",
    y = "%",
    title = paste("Generic Ballot", year)
  )

ggplot(
  generic_ballot_avg, mapping = aes(x = end_date, y = net)
) + geom_line(color = "purple", size = 1) + 
  geom_ribbon(aes(ymin = net_lower_ci, ymax = net_upper_ci), fill = "#c39af5", alpha = 0.4) +
  geom_point(data = polls, mapping = aes(y = net), color = "purple", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  labs(
    x = "Date",
    y = "Rep-Dem Spread %",
    title = "Generic Ballot Spread"
  )

setwd("../averages/historical/")

write_csv(generic_ballot_avg, paste0('historical_generic_ballot_', year, '.csv'))

setwd("../../R/")