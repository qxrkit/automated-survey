

generate_data <- function(n, col_names) {
  data <- tibble(participant = c(1:n))
  
  for (col_name in col_names) {
    data[[col_name]] <- sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7))
  }
  
  return(data)
}

convert_data_from_wide_to_long <- function(data) {
  data_long <- data |> pivot_longer(-participant, names_to = "action", values_to = "value")
  
  data_long <- data_long |>
    mutate(action = str_replace_all(action, "_", " ")) |>
    mutate(action = str_to_sentence(action)) |>
    mutate(action = fct_inorder(action)) |>
    mutate(action = fct_rev(action))
  
  return(data_long)
  
}



plot_task_success <- function(data) {
  
  
  ggplot(data, aes(x = factor(participant), y = action, fill = as.factor(value))) +
  geom_tile(color = "grey") +
  scale_fill_manual(values = c("#fffbf0", "#2ca786")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_x_discrete(expand = expansion(mult = c(0, .1)), position = "top") +
  coord_fixed(ratio = 1) +
  labs(
    x = NULL, y = NULL, fill = "Value",
    title = "<b>Participant Task <span style = 'color: #2ca786;'>Success</span> Heatmap</b>",
    subtitle = "(Each column represents a participant)"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 28, lineheight = 1,
      padding = margin(0),
      margin = margin(0, 0, 4, 0)
    )
  ) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 22)) +
  theme(text = element_text(family = "Open Sans"))
  
}


plot_task_failure <- function(data) {
  
  ggplot(data, aes(x = factor(participant), y = action, fill = as.factor(value))) +
    geom_tile(color = "grey") +
    scale_fill_manual(values = c("#BC412B", "#fffbf0")) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_x_discrete(expand = expansion(mult = c(0, .1)), position = "top") +
    coord_fixed(ratio = 1) +
    labs(
      x = NULL, y = NULL, fill = "Value",
      title = "<b>Participant Task <span style = 'color: #BC412B;'>Error</span> Heatmap</b>",
      subtitle = "(Each column represents a participant)"
    ) +
    theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        size = 26, lineheight = 1,
        padding = margin(0),
        margin = margin(0, 0, 4, 0)
      )
    ) +
    theme(legend.position = "none") +
    theme(axis.text.y = element_text(size = 20)) +
    theme(text = element_text(family = "Open Sans"))
}



get_benchmark_result <- function(data, num_tasks, benchmark) {
  result_df <- data |>
    mutate(action = fct_rev(action)) |>
    group_by(action) |>
    summarise(total = sum(value)) |>
    mutate(tasks = num_tasks) |>
    # mutate(percent = round(total/15, 2)) |>
    pmap_dfr(function(total, action, tasks) {
      result_table <- benchmark_event(benchmark = benchmark, count = total, total = tasks, input = "values", output = "tibble")
      result_table <- mutate(result_table, action = action) |> select(-output_text, -result, -benchmark)
      result_table
    }) |>
    relocate(action, probability) |>
    mutate(probability = round(probability, 2))
  
  
  result_df <- result_df |>
    mutate(percent = count / total) |>
    mutate(percent = round(percent, 2)) |>
    select(-total) |>
    relocate(probability, .after = last_col()) |>
    rename(success_rate = percent)
  
  
  result_df <- result_df |>
    mutate(action = fct_rev(action)) |>
    mutate(probability = probability * 100)
}



plot_benchmark_results <- function(data) {
  
  ggplot(data, aes(x = probability, y = action)) +
    geom_bar(stat = "identity") +
    scale_x_continuous(
      position = "top",
      expand = expansion(mult = c(0, .1)), limits = c(0, 100)
    ) +
    theme(
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = "grey")
    ) +
    labs(
      title = "Probability (%) that the task success rate will exceed the benchmark",
      subtitle = "Benchmark set at 75%",
      x = NULL,
      y = NULL
    ) +
    geom_text(
      data = result_df,
      aes(
        label = ifelse(probability == 0, "0", probability),
        hjust = -0.5
      ),
      size = 5
    ) +
    theme(text = element_text(family = "Open Sans")) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank()
    ) +
    theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        size = 26, lineheight = 1,
        padding = margin(0),
        margin = margin(0, 0, 4, 0)
      )
    ) +
    theme(legend.position = "none") +
    theme(axis.text.y = element_text(size = 20))
}









### Anova functions


create_data <- function(mean, sd, seed) {
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Task A
  data_A <- data.frame(
    participant = rep(1:24, each = 1),
    group = c(rep("Group 1", 12), rep("Group 2", 12)),
    task = "A",
    order = c(rep(c(1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3), 2)),
    performance = c(rnorm(12, mean = mean[1], sd = sd[1]), 
                    rnorm(12, mean = mean[2], sd = sd[2]))
  )
  
  # Task B
  data_B <- data.frame(
    participant = rep(1:24, each = 1),
    group = c(rep("Group 1", 12), rep("Group 2", 12)),
    task = "B",
    order = c(rep(c(2, 3, 1, 3, 1, 2, 2, 3, 1, 3, 1, 2), 2)),
    performance = c(rnorm(12, mean = mean[3], sd = sd[3]), 
                    rnorm(12, mean = mean[4], sd = sd[4]))
  )
  
  # Task C
  data_C <- data.frame(
    participant = rep(1:24, each = 1),
    group = c(rep("Group 1", 12), rep("Group 2", 12)),
    task = "C",
    order = c(rep(c(3, 2, 3, 1, 2, 1, 3, 2, 3, 1, 2, 1), 2)),
    performance = c(rnorm(12, mean = mean[5], sd = sd[5]), 
                    rnorm(12, mean = mean[6], sd = sd[6]))
  )
  
  data <- bind_rows(data_A, data_B, data_C)
  data <- data |> 
    mutate(participant = factor(participant),
           group = factor(group),
           task = factor(task),
           order = factor(order))
  
  data
}


create_plot <- function(data) {
  data %>%
    ggplot(aes(x = performance, y = group, fill = group)) +
    geom_jitter(width = 0.3, alpha = 0.4, size = 1) +
    geom_boxplot(alpha = 0.5) +  # Adjust alpha for boxplot transparency
    scale_x_continuous(limits = c(0, 28), position = "top") +
    facet_wrap(~task, ncol = 1) +
    scale_fill_manual(values = c("#DBBF8B", "#3D618F")) +
    theme(legend.position = "none") +
    stat_summary(geom = "crossbar", width = 1, fatten = 2, fill = "white", colour = "white", fun.data = function(x) {
      return(c(y = median(x), ymin = median(x), ymax = median(x)))
    }) +
    stat_summary(geom = "label", fun = median, vjust = 0.5, hjust = 0.5, fill = rgb(1, 1, 1, 0.8), color = "grey", aes(label = round(after_stat(x)))) +
    stat_summary(geom = "text", fun = median, vjust = 0.5, hjust = 0.5, fill = rgb(1, 1, 1, 0.5), color = "black", aes(label = round(after_stat(x)))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_text(size = 14, hjust = 0),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      axis.ticks.x = element_line(colour = "grey")
    ) +
    labs(
      title = "Performance Varied Based on Task and Group",
      subtitle = "Distribution of performance scores (higher is better)",
      x = NULL,
      caption = "Data: Project Skywalker (Device Testing Study)"
    ) +
    theme(text = element_text(family = "Open Sans"))
  
}

between_group_differences <- function(data) {
  data_wide <- data %>% 
    pivot_wider(!c(order),names_from = task, values_from = performance)
  
  a <- data_wide %>% 
    infer::t_test(A ~ group, order = c("Group 2", "Group 1")) %>% 
    mutate(contrast = "Group 2 - Group 1",
           task = "A") %>% relocate(task, contrast) 
  
  
  b <- data_wide %>% 
    infer::t_test(B ~ group, order = c("Group 2", "Group 1")) %>% 
    mutate(contrast = "Group 2 - Group 1",
           task = "B") %>% relocate(task, contrast) 
  
  c <- data_wide %>% 
    infer::t_test(C ~ group, order = c("Group 2", "Group 1")) %>% 
    mutate(contrast = "Group 2 - Group 1",
           task = "C") %>% relocate(task, contrast) 
  
  bind_rows(a, b, c) %>% 
    mutate(p = scales::pvalue(p_value)) %>% 
    select(-alternative, -p_value) %>% 
    mutate(across(where(is.numeric), round,3)) 
}

effect_of_order <- function(data) {
  
  m = ezANOVA(dv=performance, between=group, within=order, wid=participant, type=3, data=data)
  
  mauchly_test <- m$Mauchly %>% 
    data.frame() %>% 
    clean_names() %>% 
    rename(p.value = p) %>% 
    mutate(p.value = scales::pvalue(p.value)) %>% 
    mutate(across(where(is.numeric), round, 2)) %>% 
    select(-4) 
  
  anova_result <- m$ANOVA %>% 
    mutate(p = scales::pvalue(p)) %>% 
    mutate(across(where(is.numeric), round,3)) %>% 
    select(-`p<.05`) %>% 
    clean_names() %>% 
    rename(dfn = d_fn, dfd = d_fd)
  
  return(list(mauchly_test, anova_result))
}


effect_of_group <- function(data) {
  
  m = ezANOVA(dv=performance, between=group, within=task, wid=participant, type=3, data=data)
  
  mauchly_test <- m$Mauchly %>% 
    data.frame() %>% 
    mutate(across(where(is.numeric), round, 2)) %>% 
    clean_names() %>% 
    select(-4) 
  
  anova_result <- m$ANOVA %>%
    mutate(p = scales::pvalue(p)) %>% 
    mutate(across(where(is.numeric), round,3)) %>% 
    select(-`p<.05`) %>% 
    clean_names() %>% 
    rename(dfn = d_fn, dfd = d_fd) 
  
  return(list(mauchly_test, anova_result))
}


within_group_differences <- function(data) {
  
  Group_1 <- data %>% filter(group == "Group 1")
  
  Group_2 <- data %>% filter(group == "Group 2")
  
  ## Within-Group Differences
  
  ## Pairwise differences - Group 1
  
  model <- lm(performance ~ task, data=Group_1)
  model.em <- emmeans(model, "task")
  
  pairs(model.em, adjust = "holm")  %>% data.frame() %>%  
    mutate(p.value = scales::pvalue(p.value)) %>% 
    mutate(across(where(is.numeric), round,3)) 
  
  ## Pairwise differences - Group 2
  
  
  model <- lm(performance ~ task, data=Group_2)
  model.em <- emmeans(model, "task")
  
  pairs(model.em, adjust = "holm") %>% data.frame() %>%  
    mutate(p.value = scales::pvalue(p.value)) %>% 
    mutate(across(where(is.numeric), round,3)) 
}
