library(shiny)
library(tidyverse)
library(uxr)
library(showtext)
library(ggtext)
library(gt)
library(gtExtras)
library(bslib)



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
      data = data,
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


generate_data <- function(n, col_names, failure, success) {
  data <- tibble(participant = c(1:n))
  
  for (col_name in col_names) {
    data[[col_name]] <- sample(c(0, 1), n, replace = TRUE, prob = c(failure, success))
  }
  
  return(data)
}



ui <- page_sidebar(
  
  
  title = "Benchmark",
  sidebar = sidebar(
    h3("Simulation Settings"),
    
    numericInput("n", "Number of Samples:", min = 1, max = 100, value = 15),
    textInput("tasks", "Tasks (comma-separated):", value = "sign_in,navigate,search,find_category,find_item,find_review,add_to_cart,edit_address,check_out,track_status"),
    numericInput("failure", "Failure Probability:", min = 0, max = 1, value = 0.3, step = 0.1),
    numericInput("success", "Success Probability:", min = 0, max = 1, value = 0.7, step = 0.1),
    numericInput("seed", "Seed:", min = 1, value = 123),
    numericInput("benchmark", "Benchmark (%):", min = 0, max = 100, value = 75),
    
    hr(),
    h3("Data Upload"),
    
    fileInput("file", "Upload CSV File:", accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv")
    ),
    downloadButton("downloadCSV", "Download CSV Template"),
    helpText("Or paste your data into the text area below. It should be a table in CSV format, where each column represents a task and each row represents a participant. For example:"),
    textAreaInput("data", "", rows = 3, placeholder = "sign_in,navigate,search\n1,0,1\n0,1,0\n1,1,1")
    
  ),
  fillable = FALSE,
    
!!!list(  card(
  full_screen = TRUE,
  card_header("Task Success Plot"),
  plotOutput("taskSuccessPlot")
),
card(
  full_screen = TRUE,
  card_header("Task Error Plot"),
  plotOutput("taskErrorPlot")
),
card(
  full_screen = TRUE,
  card_header("Benchmark Plot"),
  plotOutput("benchmarkPlot")
),
card(
  full_screen = TRUE,
  card_header("Benchmark Table"),
  tableOutput("benchmarkTable")
))
  
)




# Define server logic 
server <- function(input, output) {

  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      "template.csv"
    },
    content = function(file) {
      write.csv(generate_data(15, str_split(input$tasks, ',')[[1]], input$probabilities[1], input$probabilities[2]), file, row.names = FALSE)
      
    }
  )
  
  # Reactive function that generates or reads the data
  reactive_data <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath)
    } else if (input$data != "") {
      read.table(text = input$data, sep = ",", header = TRUE, stringsAsFactors = FALSE)
    } else {
      set.seed(input$seed)
      generate_data(input$n, str_split(input$tasks, ',')[[1]], input$failure, input$success)
    }
  })
  
  # Reactive function that transforms the data to long format
  reactive_data_long <- reactive({
    convert_data_from_wide_to_long(reactive_data())
  })
  
  # Reactive function that calculates the benchmark results
  reactive_result_df <- reactive({
    get_benchmark_result(reactive_data_long(), input$n, input$benchmark / 100)
  })
  
  # Outputs for the plots
  output$taskSuccessPlot <- renderPlot({
    plot_task_success(reactive_data_long())
  })
  output$taskErrorPlot <- renderPlot({
    plot_task_failure(reactive_data_long())
  })
  output$benchmarkPlot <- renderPlot({
    plot_benchmark_results(reactive_result_df())
  })
  
  # Output for the table
  output$benchmarkTable <- renderTable({
    reactive_result_df()
  })
  
  # Download handlers for the plots
  output$downloadTaskSuccessPlot <- downloadHandler(
    filename = function() {
      "task_success_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plot_task_success(reactive_data_long()), device = "png")
    }
  )
  output$downloadTaskErrorPlot <- downloadHandler(
    filename = function() {
      "task_error_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plot_task_failure(reactive_data_long()), device = "png")
    }
  )
  output$downloadBenchmarkPlot <- downloadHandler(
    filename = function() {
      "benchmark_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plot_benchmark_results(reactive_result_df()), device = "png")
    }
  )
  
  # Download handler for the table
  output$downloadBenchmarkTable <- downloadHandler(
    filename = function() {
      "benchmark_table.csv"
    },
    content = function(file) {
      write.csv(reactive_result_df(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)




