


skewed_df <- function(n = 100, shape1 = 2.3, shape2 = 6.8, seed = 1234) {
  skewed <- function(n, skew = 'right', shape1, shape2, seed) {
    set.seed(seed)
    if(skew == 'right') {
      skewed <- rbeta(n, shape1, shape2)
    } else if (skew == 'left') {
      skewed <- rbeta(n, shape2, shape1)
    }
    skewed_scaled <- 1 + 4 * skewed
    return(round(skewed_scaled))
  }
  
  set.seed(seed)
  seeds <- seed:(seed + 9)
  
  df <- data.frame(matrix(ncol = length(seeds), nrow = n))
  
  for (i in 1:length(seeds)) {
    skew_direction <- ifelse(i %% 2 == 0, 'right', 'left')
    if(i == 4 || i == 10) {
      shape1_adj <- shape1 * 2  # Increase shape1 to make values larger
      shape2_adj <- shape2 * 2  # Increase shape2 to make values larger
    } else {
      shape1_adj <- shape1
      shape2_adj <- shape2
    }
    df[,i] <- skewed(n, skew_direction, shape1_adj, shape2_adj, seeds[i])
    names(df)[i] <- paste('q', i, sep = '_')
  }
  return(df)
}

calc_sus <- function(data) {
  data |> 
    mutate(total_odd = rowSums(select(data, ends_with(c("1", "3", "5", "7", "9"))), na.rm = TRUE)) |> 
    mutate(total_even = rowSums(select(data, ends_with(c("0", "2", "4", "6", "8"))), na.rm = TRUE)) |> 
    mutate(total_odd = total_odd - 5) |> 
    mutate(total_even = 25 - total_even) |> 
    mutate(total = total_odd + total_even) |> 
    mutate(sus = total * 2.5) |> select(-total_odd, -total_even, -total)
}


sim_data <- skewed_df(100, 1, 2, 1234) |> calc_sus()

sim_data <- sim_data %>% 
  mutate(across(starts_with("q_"), 
                ~ case_when(
                  . == 1 ~ "Strongly disagree",
                  . == 2 ~ "Somewhat disagree",
                  . == 3 ~ "Neither agree nor disagree",
                  . == 4 ~ "Somewhat agree",
                  . == 5 ~ "Strongly agree",
                  TRUE ~ "NA" 
                )))

write.csv(sim_data, "sim_data.csv")

