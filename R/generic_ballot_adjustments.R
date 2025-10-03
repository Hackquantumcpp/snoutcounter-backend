library(tidyverse)
library(rstan)
library(rstanarm)
library(janitor)
library(rsample) # rsample in tidymodels
library(DescTools)

setwd("../")

ratings <- read_csv("pollster_ratings_silver.csv") %>% janitor::clean_names()

setwd(paste0(getwd(), "/R/"))

banned_pollsters <- c("ActiVote",
                      "Trafalgar Group", "Trafalgar Group/InsiderAdvantage",
                      "TIPP", "Big Data Poll")

url <- "https://docs.google.com/spreadsheets/d/1_y0_LJmSY6sNx8qd51T70n0oa_ugN50AVFKuJmXO1-s/export?format=csv&gid=2042605142"
  
polls <- read_csv(url)

polls <- polls %>% filter(!(pollster %in% banned_pollsters))

polls <- polls %>% filter(
  is.na(sample_size) == FALSE, # For now, we can try imputing sample sizes later
  is.na(state) == TRUE
)

# write_csv(polls, "generic_ballot_polls.csv")

tracking_polls_pipeline <- function(data_frame) {
  df <- data_frame %>% filter(tracking == TRUE)
  pollsters <- as.vector(df %>% distinct(pollster_id))$pollster_id
  
  df_tracking <- tibble()
  
  for (p in pollsters) {
    df_pollst <- df %>% filter(pollster_id == p) %>%
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
  df <- df %>% mutate(sample_size = Winsorize(sample_size, val = quantile(sample_size, probs = c(0.025, 0.975), na.rm = FALSE)))
  df <- df %>% mutate(sample_size_weight = sqrt(pmin(sample_size, size_cap)) / sqrt(median(pmin(sample_size, size_cap))))

  # Quick wrangling
  df$display_name[df$display_name == "Noble Predictive Insights"] <- "OH Predictive Insights"
  for (har in c("HarrisX", "HarrisX/Harris Poll")) {
    df$display_name[df$display_name == har] <- "Harris Insights & Analytics"
  }
  df$sponsors[df$display_name == "CNN/SSRS"] <- "CNN"
  df$display_name[df$display_name == "CNN/SSRS"] <- "SSRS"
  df$display_name[df$display_name == "University of Massachusetts Department of Political Science/YouGov"] <- "University of Massachusetts (Amherst)"
  
  
  ### Quality weights
  df <- df %>% left_join(ratings %>% rename(display_name = pollster), join_by(display_name)) %>%
    filter(
      !(display_name %in% (ratings %>% filter(grade == "F@@16") %>% select(pollster)))
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
    sponsor_ids = coalesce(sponsor_ids, "NA"),
    poll_spon_id = str_c(as.character(pollster_id), sponsor_ids)
    )
  df <- df %>% rowwise() %>% mutate(zone_flood_weight = 1 / sqrt(pid_in_window(end_date, pollster_id))) %>%
    ungroup()
  
  ### Recency weight
  window <- 30
  df <- df %>% mutate(recency_weight = 0.1^(as.numeric(date - end_date, units = "days")/window))
  
  ### Bring it all together
  df <- df %>% mutate(total_weight = sample_size_weight * quality_weight * zone_flood_weight * recency_weight)
  df$total_weight <- df$total_weight / sum(df$total_weight)
  
  # Drop columns from ratings data frame
  df <- df %>% select(-predictive_plus_minus, -mean_reverted_bias, -number_of_polls, -cat, -grade)
  
  return(df)
}

avg_over_time <- function(data_frame) {
  df <- data_frame # Copy data frame
  
  # date_interv <- ymd("2025-01-23") %--% today()
  
  rep_curve <- numeric(0)
  dem_curve <- numeric(0)
  net_curve <- numeric(0)
  
  rep_std <- numeric(0)
  dem_std <- numeric(0)
  net_std <- numeric(0)
  
  date_interv <- seq(ymd("2025-01-21"), today(), by = "day")
  
  for (i in 1:length(date_interv)) {
    date = date_interv[i]  # Debug
    df_weights <- poll_avg(data_frame, date)
    rep_avg <- sum(df_weights$total_weight * df_weights$rep)
    dem_avg <- sum(df_weights$total_weight * df_weights$dem)
    net_avg <- sum(df_weights$total_weight * df_weights$net)
    
    rep_sd <- sqrt(sum(df_weights$total_weight * (df_weights$rep - rep_avg)^2))
    dem_sd <- sqrt(sum(df_weights$total_weight * (df_weights$dem - dem_avg)^2))
    net_sd <- sqrt(sum(df_weights$total_weight * (df_weights$net - net_avg)^2))
    
    rep_curve <- append(rep_curve, rep_avg)
    dem_curve <- append(dem_curve, dem_avg)
    net_curve <- append(net_curve, net_avg)
    
    rep_std <- append(rep_std, rep_sd)
    dem_std <- append(dem_std, dem_sd)
    net_std <- append(net_std, net_sd)
  }
  
  df_avg <- tibble(
    end_date = date_interv,
    rep = rep_curve,
    dem = dem_curve,
    net = net_curve,
    rep_std = rep_std,
    dem_std = dem_std,
    net_std = net_std,
    rep_lower_ci = rep_curve - 1.96*rep_std,
    rep_upper_ci = rep_curve + 1.96*rep_std,
    dem_lower_ci = dem_curve - 1.96*dem_std,
    dem_upper_ci = dem_curve + 1.96*dem_std,
    net_lower_ci = net_curve - 1.96*net_std,
    net_upper_ci = net_curve + 1.96*net_std
  )
  
  return(df_avg)
}

polls <- polls %>% mutate(end_date = ymd(end_date), start_date = ymd(start_date))

polls_tracking <- tracking_polls_pipeline(polls) %>% select(-interval) # Drop interval column

polls <- polls %>% filter(tracking == "FALSE")

polls <- bind_rows(polls, polls_tracking)

polls <- polls %>% arrange(pollster_id) %>%
  mutate(mode = replace_na(mode, "Unknown"))

polls <- polls %>% 
  mutate(population = recode(population, "lv" = "a", "rv" = "b", "a" = "c")) %>% 
  arrange(population) %>% 
  distinct(poll_id, .keep_all = TRUE) %>% 
  mutate(population = recode(population, "a" = "lv", "b" = "rv", "c" = "a"))

polls <- polls %>% mutate(net = rep - dem, partisan = replace_na(partisan, "NA")) 

polls$pollster_id <- factor(polls$pollster_id)

# polls <- poll_avg(polls, today())

df_avg <- avg_over_time(polls)

# df_avg <- df_avg %>% mutate(lagged_net = lag(net, 1))

polls <- polls %>% left_join(df_avg %>% select(end_date, net), join_by(end_date)) %>%
  rename(net = net.x, net_avg = net.y)

fit <- stan_glmer(net ~ (1 | pollster_id) + (1 | partisan) + 
                    (1 | mode) + net_avg, # Population adjustment disabled due to current lack of variance,
                  # Will be turned out later
                  family = gaussian(),
                  data = polls,
                  prior = normal(0, 1, autoscale = TRUE),
                  prior_covariance = decov(scale = 0.50),
                  adapt_delta = 0.95,
                  refresh = 0,
                  seed = 1010)

print(fit)
print(summary(fit))
print(fixef(fit))
print(ranef(fit))

# pop_a <- ranef(fit)$population[2, 1]
np_a <- ranef(fit)$partisan[1, 1]

polls <- polls %>% select(-net_avg) # Drop net avg

polls <- polls %>%
  left_join( (rownames_to_column(ranef(fit)$pollster_id) %>% rename(pollster_id = rowname, house_effect = "(Intercept)") %>% mutate(pollster_id = factor(pollster_id), house_effect = -1 * house_effect)), join_by(pollster_id)) %>%  
  left_join( (rownames_to_column(ranef(fit)$mode) %>% rename(mode = rowname, mode_adj = "(Intercept)") %>% mutate(mode_adj = -1 * mode_adj)), join_by(mode)) %>%
  # left_join((rownames_to_column(ranef(fit)$population) %>% rename(population = rowname, pop_adj = "(Intercept)") %>% mutate(population = as.character(population), pop_adj = pop_a - pop_adj)), join_by(population))%>%
  left_join( (rownames_to_column(ranef(fit)$partisan) %>% rename(partisan = rowname, partisan_adj = "(Intercept)") %>% mutate(partisan_adj = np_a - partisan_adj)), join_by(partisan))

polls <- polls %>% mutate(
  rep = rep + (house_effect + mode_adj + partisan_adj) / 2,
  dem = dem - (house_effect + mode_adj + partisan_adj) / 2,
  net = net + house_effect + mode_adj + partisan_adj
) %>% arrange(end_date)

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
    title = "Generic Ballot"
  ) + xlim(ymd('2025-01-21'), today())

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
) + xlim(ymd('2025-01-21'), today())