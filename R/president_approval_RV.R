library(tidyverse)
library(rstan)
library(rstanarm)
library(janitor)
library(rsample) # rsample in tidymodels
library(progress)
library(posterior)

# Get banned pollsters
source("banned_pollsters.R")

setwd("../")

ratings <- read_csv("ratings/pollster_ratings_silver.csv") %>% janitor::clean_names()

ratings_24 <- read_csv("ratings/pollster_ratings_silver_2024.csv") %>% janitor::clean_names()

setwd(paste0(getwd(), "/R/"))

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSj1f5S4BKqd_0T83atnX5WIXMhwd326O-jg5vXNjvylt8DIWNW07rXmMUy9wGEz5ZVDlfLbea_hrd9/pub?output=csv"

polls <- read_csv(url)

polls <- polls %>% filter(!(pollster %in% banned_pollsters) & !(is.na(approve)) &
                            !(is.na(disapprove)) & !(is.na(sample_size)) &
                            !(population == 'A'))

# write_csv(polls, "president_approval_polls.csv")

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
  df <- df %>% mutate(sample_size_weight = sqrt(pmin(sample_size, size_cap)) / sqrt(median(pmin(sample_size, size_cap))))

  # Quick wrangling
  df <- df %>% mutate(
    pollster_ratname = recode(pollster,
                              "Quantus Insights" = "Quantus Polls and News",
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
  
  ### Multiple polls in short window weights
  df <- df %>% mutate(
    poll_spon_id = group_indices(., pollster))
  df <- df %>% rowwise() %>% mutate(zone_flood_weight = 1 / sqrt(pid_in_window(end_date, poll_spon_id))) %>%
    ungroup()
  
  ### Recency weight
  window <- 21
  df <- df %>% mutate(recency_weight = 0.1^(as.numeric(date - end_date, units = "days")/window))
  
  ## Partisan downweight
  partisan_dw <- 0.7
  df <- df %>% mutate(
    partisan_downweight = if_else(partisan == "NA", 1, partisan_dw)
  )
  
  ### Bring it all together
  df <- df %>% mutate(total_weight = sample_size_weight * quality_weight * zone_flood_weight * recency_weight * partisan_downweight)
  df$total_weight <- df$total_weight / sum(df$total_weight)
  
  # Drop columns from ratings data frame
  df <- df %>% select(-predictive_plus_minus, -mean_reverted_bias, -number_of_polls, -cat, -grade)
  
  return(df)
}

avg_over_time <- function(data_frame) {
  df <- data_frame # Copy data frame
  
  ## Coalesce for alternate answers
  df <- df %>% mutate(alternate_answers = coalesce(alternate_answers, 0))
  
  
  # date_interv <- ymd("2025-01-23") %--% today()
  
  yes_curve <- numeric(0)
  no_curve <- numeric(0)
  other_curve <- numeric(0)
  net_curve <- numeric(0)
  
  yes_std <- numeric(0)
  no_std <- numeric(0)
  other_std <- numeric(0)
  net_std <- numeric(0)
  
  date_interv <- seq(ymd("2025-01-21"), today(), by = "day")
  
  print(interactive())
  
  df_avg <- tibble(
    end_date = date_interv
  )
  
  avg_oneday <- function(date) {
    df_weights <- poll_avg(data_frame, date)
    appr_avg <- sum(df_weights$total_weight * df_weights$approve)
    disappr_avg <- sum(df_weights$total_weight * df_weights$disapprove)
    net_avg <- sum(df_weights$total_weight * df_weights$net)
    
    appr_sd <- sqrt(sum(df_weights$total_weight * (df_weights$approve - appr_avg)^2))
    disappr_sd <- sqrt(sum(df_weights$total_weight * (df_weights$disapprove - disappr_avg)^2))
    net_sd <- sqrt(sum(df_weights$total_weight * (df_weights$net - net_avg)^2))
    
    return(list(approve = appr_avg, 
                disapprove = disappr_avg, 
                net = net_avg,
                approve_std = appr_sd,
                disapprove_std = disappr_sd,
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
    approve_lower_ci = approve - 1.96*approve_std,
    approve_upper_ci = approve + 1.96*approve_std,
    disapprove_lower_ci = disapprove - 1.96*disapprove_std,
    disapprove_upper_ci = disapprove + 1.96*disapprove_std,
    net_lower_ci = net - 1.96*net_std,
    net_upper_ci = net + 1.96*net_std
  )
  
  return(df_avg)
}

polls <- polls %>% mutate(end_date = ymd(end_date), 
                          start_date = ymd(start_date),
                          poll_id = group_indices(., pollster, sponsors, start_date, end_date))

polls_tracking <- tracking_polls_pipeline(polls) %>% select(-interval) # Drop interval column

polls <- polls %>% filter(tracking == "FALSE")

polls <- bind_rows(polls, polls_tracking)

polls <- polls %>% arrange(pollster) %>%
  mutate(mode = replace_na(mode, "Unknown"))

polls <- polls %>% 
  mutate(population = recode(population, "RV" = "a", "LV" = "b")) %>% 
  arrange(population) %>% 
  distinct(poll_id, .keep_all = TRUE) %>% 
  mutate(population = recode(population, "a" = "RV", "b" = "LV"))

polls <- polls %>% mutate(net = approve - disapprove, partisan = replace_na(partisan, "NA")) 

# polls <- poll_avg(polls, today())

df_avg <- avg_over_time(polls)

df_avg <- df_avg %>% mutate(lagged_net = lag(net, 1))

polls <- polls %>% left_join(df_avg %>% select(end_date, net, approve, disapprove) %>% rename(appr_avg = approve, disappr_avg = disapprove), 
                             join_by(end_date)) %>%
  rename(net = net.x, net_avg = net.y)

## Approval adjustments
fit <- stan_glmer(approve ~ 0 + (1 | pollster) + (1 | partisan) + (1 | population) + 
                    (1 | mode) + appr_avg,
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

polls <- polls %>% select(-net_avg) # Drop net avg

polls <- polls %>%
  left_join( (rownames_to_column(ranef(fit)$pollster) %>% rename(pollster = rowname, house_effect = "(Intercept)") %>% mutate(house_effect = -1 * house_effect)), join_by(pollster)) %>%  
  left_join( (rownames_to_column(ranef(fit)$mode) %>% rename(mode = rowname, mode_adj = "(Intercept)") %>% mutate(mode_adj = -1 * mode_adj)), join_by(mode)) %>%
  left_join((rownames_to_column(ranef(fit)$population) %>% rename(population = rowname, pop_adj = "(Intercept)") %>% mutate(population = as.character(population), pop_adj = pop_a - pop_adj)), join_by(population))%>%
  left_join( (rownames_to_column(ranef(fit)$partisan) %>% rename(partisan = rowname, partisan_adj = "(Intercept)") %>% mutate(partisan_adj = np_a - partisan_adj)), join_by(partisan))

polls_og <- polls %>% arrange(end_date)

polls <- polls %>% mutate(
  approve = approve + house_effect + mode_adj + pop_adj + partisan_adj
) %>% arrange(end_date)

polls <- polls %>% select(-house_effect, -mode_adj, -pop_adj, -partisan_adj)

## Disapproval adjustments
fit <- stan_glmer(disapprove ~ 0 + (1 | pollster) + (1 | partisan) + (1 | population) + 
                    (1 | mode) + disappr_avg,
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
  disapprove = disapprove + house_effect + mode_adj + pop_adj + partisan_adj
) %>% arrange(end_date)

polls <- polls %>% mutate(
  net = approve - disapprove
)
# today_avg = poll_avg(polls, today())
approval_stats <- avg_over_time(polls)

ggplot(
  approval_stats, aes(x = end_date)
) + geom_line(size = 1, mapping = aes(y = approve, color = "Approve")) +
  geom_line(size = 1, mapping = aes(y = disapprove, color = "Disapprove")) +
  scale_color_manual(
    name = "Legend",
    values = c("Approve" = "#228833", "Disapprove" = "#aa3377")
  ) +
  geom_ribbon(aes(ymin = approve_lower_ci, ymax = approve_upper_ci), fill = "#60be65",
              alpha = 0.4) +
  geom_ribbon(aes(ymin = disapprove_lower_ci, ymax = disapprove_upper_ci), fill = "#e86eaf",
              alpha = 0.4) +
  geom_point(data = polls, mapping = aes(y = approve), color = "#228833", alpha = 0.2) +
  geom_point(data = polls, mapping = aes(y = disapprove), color = "#aa3377", alpha = 0.2) +
  labs(
    x = "Date",
    y = "%",
    title = "Presidential Approval (voters)"
  )

ggplot(
  approval_stats, mapping = aes(x = end_date, y = net)
) + geom_line(color = "red", size = 1) + 
  geom_ribbon(aes(ymin = net_lower_ci, ymax = net_upper_ci), fill = "#dc9a88", alpha = 0.4) +
  geom_point(data = polls, mapping = aes(y = net), color = "red", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  labs(
  x = "Date",
  y = "Net Approval %",
  title = "Presidential Net Approval (voters)"
)

setwd("../averages/")

write_csv(approval_stats, 'presidential_RV_approval.csv')

setwd("../R/")