library(tidyverse)
library(rstan)
library(rstanarm)
library(janitor)

setwd("../")

ratings <- read_csv("pollster_ratings_silver.csv")

setwd(paste0(getwd(), "/R/"))

banned_pollsters <- c("ActiVote", "Big Data Poll", "Trafalgar Group", "Trafalgar Group/InsiderAdvantage",
                      "TIPP")

url <- "https://docs.google.com/spreadsheets/d/1_y0_LJmSY6sNx8qd51T70n0oa_ugN50AVFKuJmXO1-s/export?format=csv&gid=747663134#gid=747663134"

polls <- read_csv(url)

polls <- polls %>% filter(!(pollster %in% banned_pollsters))

polls_in_window <- function(data_frame, date, pid) {
  df <- data_frame # Copy data frame
  
  thres = date - 14
  df <- df %>% filter(pollster_id == pid & end_date >= thres)
  return(max(dim(df)[1], 1)) ## REMEMBER, IMPORTANT, NOT ZERO INDEXED IN R!!!
}

poll_avg <- function(data_frame, date) {
  # Copy data frame, filter for all those less than given date
  df <- data_frame %>% filter(end_date <= date)
  
  ### Sample size weights
  df <- df %>% mutate(sample_size_weight = sqrt(pmin(sample_size, 3000)) / sqrt(median(pmin(sample_size, 3000))))

  # Quick wrangling
  df$display_name[df$display_name == "Noble Predictive Insights"] <- "OH Predictive Insights"
  for (har in c("HarrisX", "HarrisX/Harris Poll")) {
    df$display_name[df$display_name == har] <- "Harris Insights & Analytics"
  }
  
  ### Quality weights 
  df <- df %>% left_join(ratings %>% janitor::clean_names() %>% rename(display_name = pollster), join_by(display_name)) %>%
    mutate(
      predictive_plus_minus = coalesce(predictive_plus_minus, 5),
      quality_weight = if_else(predictive_plus_minus < 0.5, exp(-predictive_plus_minus/1.3), 0.2)
    )
  
  pid_in_window <- function(end_date, pid) {
    return(polls_in_window(df, end_date, pid))
  }
  
  ### Multiple polls in short window weights
  df <- df %>% rowwise() %>% mutate(zone_flood_weight = 1 / sqrt(pid_in_window(end_date, pollster_id))) %>%
    ungroup()
  
  ### Recency weight
  df <- df %>% mutate(recency_weight = 0.1^(as.numeric(date - end_date, units = "days")/30))
  
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
  
  yes_curve <- numeric(0)
  no_curve <- numeric(0)
  net_curve <- numeric(0)
  
  yes_std <- numeric(0)
  no_std <- numeric(0)
  net_std <- numeric(0)
  
  date_interv <- seq(ymd("2025-01-23"), today(), by = "day")
  
  for (i in 1:length(date_interv)) {
    date = date_interv[i]  # Debug
    df_weights <- poll_avg(data_frame, date)
    appr_avg <- sum(df_weights$total_weight * df_weights$yes)
    disappr_avg <- sum(df_weights$total_weight * df_weights$no)
    net_avg <- sum(df_weights$total_weight * df_weights$net)
    
    appr_sd <- sqrt(sum(df_weights$total_weight * (df_weights$yes - appr_avg)^2))
    disappr_sd <- sqrt(sum(df_weights$total_weight * (df_weights$no - disappr_avg)^2))
    net_sd <- sqrt(sum(df_weights$total_weight * (df_weights$net - net_avg)^2))
    
    yes_curve <- append(yes_curve, appr_avg)
    no_curve <- append(no_curve, disappr_avg)
    net_curve <- append(net_curve, net_avg)
    
    yes_std <- append(yes_std, appr_sd)
    no_std <- append(no_std, disappr_sd)
    net_std <- append(net_std, net_sd)
  }
  
  df_avg <- tibble(
    end_date = date_interv,
    approve = yes_curve,
    disapprove = no_curve,
    net = net_curve,
    approve_std = yes_std,
    disapprove_std = no_std,
    net_std = net_std,
    approve_lower_ci = yes_curve - 1.96*yes_std,
    approve_upper_ci = yes_curve + 1.96*yes_std,
    disapprove_lower_ci = no_curve - 1.96*no_std,
    disapprove_upper_ci = no_curve + 1.96*no_std,
    net_lower_ci = net_curve - 1.96*net_std,
    net_upper_ci = net_curve + 1.96*net_std
  )
  
  return(df_avg)
}

polls <- polls %>% mutate(end_date = ymd(end_date), start_date = ymd(start_date)) %>% 
  mutate(population = recode(population, "a" = "a", "rv" = "b", "lv" = "c")) %>% 
  arrange(population) %>% 
  distinct(poll_id, .keep_all = TRUE) %>% 
  mutate(population = recode(population, "a" = "a", "b" = "rv", "c" = "lv"))

polls <- polls %>% mutate(net = yes - no, partisan = replace_na(partisan, "NA")) 

polls <- polls %>% filter(tracking == "FALSE") %>% arrange(pollster_id) %>%
  mutate(mode = replace_na(mode, "Unknown"))

polls$pollster_id <- factor(polls$pollster_id)

# polls <- poll_avg(polls, today())

df_avg <- avg_over_time(polls)

polls <- polls %>% left_join(df_avg %>% select(end_date, net), join_by(end_date)) %>%
  rename(net = net.x, net_avg = net.y)

fit <- stan_glmer(net ~ (1| pollster_id) + (1 | partisan) + (1 | population) + 
                    (1 | mode) + net_avg,
                  family = gaussian(),
                  data = polls,
                  prior = normal(0, 1, autoscale = TRUE),
                  prior_covariance = decov(scale = 0.50),
                  adapt_delta = 0.99,
                  refresh = 0,
                  seed = 1010)

print(fit)
print(fixef(fit))
print(ranef(fit))

pop_a <- ranef(fit)$population[1, 1]

polls <- polls %>% select(-net_avg) # Drop net avg

polls <- polls %>%
  left_join( (rownames_to_column(ranef(fit)$pollster_id) %>% rename(pollster_id = rowname, house_effect = "(Intercept)") %>% mutate(pollster_id = factor(pollster_id), house_effect = -1 * house_effect)), join_by(pollster_id)) %>%  
  left_join( (rownames_to_column(ranef(fit)$mode) %>% rename(mode = rowname, mode_adj = "(Intercept)") %>% mutate(mode_adj = -1 * mode_adj)), join_by(mode)) %>%
  left_join((rownames_to_column(ranef(fit)$population) %>% rename(population = rowname, pop_adj = "(Intercept)") %>% mutate(population = as.character(population), pop_adj = pop_a - pop_adj)), join_by(population))%>%
  left_join( (rownames_to_column(ranef(fit)$partisan) %>% rename(partisan = rowname, partisan_adj = "(Intercept)") %>% mutate(partisan_adj = -1 * partisan_adj)), join_by(partisan))

polls <- polls %>% mutate(
  yes = yes + (house_effect + mode_adj + pop_adj + partisan_adj) / 2,
  no = no - (house_effect + mode_adj + pop_adj + partisan_adj) / 2,
  net = net + house_effect + mode_adj + pop_adj + partisan_adj
)

today_avg = poll_avg(polls, today())
approval_stats <- avg_over_time(polls)

ggplot(
  approval_stats, aes(x = end_date)
) + geom_line(color = "#228833", size = 1, mapping = aes(y = approve)) +
  geom_line(color = "#aa3377", size = 1, mapping = aes(y = disapprove)) +
  geom_ribbon(aes(ymin = approve_lower_ci, ymax = approve_upper_ci), fill = "#60be65",
              alpha = 0.4) +
  geom_ribbon(aes(ymin = disapprove_lower_ci, ymax = disapprove_upper_ci), fill = "#e86eaf",
              alpha = 0.4) +
  geom_point(data = polls, mapping = aes(y = yes), color = "#228833", alpha = 0.2) +
  geom_point(data = polls, mapping = aes(y = no), color = "#aa3377", alpha = 0.2) +
  labs(
    x = "Date",
    y = "%",
    title = "Presidential Approval"
  )

ggplot(
  approval_stats, mapping = aes(x = end_date, y = net)
) + geom_line(color = "red", size = 1) + 
  geom_ribbon(aes(ymin = net_lower_ci, ymax = net_upper_ci), fill = "#dc9a88", alpha = 0.4) +
  geom_point(data = polls, mapping = aes(y = net), color = "red", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  labs(
  x = "Date",
  y = "Net Approval %",
  title = "Presidential Net Approval"
)
