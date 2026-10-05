library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(plotly)
library(htmltools)
library(magrittr)
df <- readxl::read_excel("Dashboard.xlsx")
df <- dplyr::select(df, -dplyr::any_of(c("Limitations...32", "Link")))
df <- dplyr::mutate(df, dplyr::across(everything(), ~ ifelse(is.na(.), "NA", as.character(.))))


rating_colors <- c(
  "Strong Positive" = "#2E7D32",
  "Moderate Positive" = "#F9A825",
  "No Significant Effect" = "#C62828",
  "Not Measured" = "#9E9E9E",
  "NA" = "#9E9E9E"
)

rating_icons <- c(
  "Strong Positive" = "●",
  "Moderate Positive" = "●",
  "No Significant Effect" = "●",
  "Not Measured" = "●",
  "NA" = "●"
)

badge <- function(x) {
  color <- rating_colors[[x]]
  if (is.null(color)) color <- "#9E9E9E"
  div(
    style = paste0(
      "background:", color, ";
      color:white;
      padding:6px 12px;
      border-radius:999px;
      font-weight:700;
      display:inline-block;
      min-width:130px;
      text-align:center;"
    ),
    x
  )
}

ui <- page_navbar(
  title = "Bicycle Access & Educational Outcomes",
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#1B4332",
    secondary = "#52796F",
    success = "#2E7D32",
    warning = "#F9A825",
    danger = "#C62828",
    base_font = font_google("Inter"),
    heading_font = font_google("Playfair Display")
  ),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #F7F8F3; }
      .card {
        border-radius: 18px;
        box-shadow: 0 8px 24px rgba(0,0,0,0.08);
        border: none;
      }
      .hero {
        background: linear-gradient(135deg, #1B4332, #52796F);
        color: white;
        padding: 38px;
        border-radius: 24px;
        margin-bottom: 24px;
      }
      .hero h1 {
        font-size: 42px;
        font-weight: 800;
      }
      .small-muted {
        color: #6c757d;
        font-size: 14px;
      }
    "))
  ),
  
  nav_panel(
    "Home",
    div(
      class = "hero",
      h1("Interactive Literature Review Dashboard"),
      h4("Bicycle access, transportation barriers, and educational outcomes"),
      p("Evidence comparison across selected studies")
    ),
    
    layout_column_wrap(
      width = 1/4,
      value_box("Studies", nrow(df), theme = "primary"),
      value_box("Countries", n_distinct(df$Country), theme = "secondary"),
      value_box("Outcomes", "5", theme = "success"),
      value_box("Evidence Ratings", "Green / Yellow / Red", theme = "warning")
    ),
    
    card(
      card_header("Study Overview"),
      DTOutput("home_table")
    )
  ),
  
  nav_panel(
    "Evidence Matrix",
    card(
      card_header("Traffic-Light Evidence Matrix"),
      DTOutput("matrix")
    ),
    card(
      card_header("Evidence Rating Distribution"),
      plotlyOutput("rating_plot", height = "430px")
    )
  ),
  
  nav_panel(
    "Outcome Tabs",
    layout_column_wrap(
      width = 1/2,
      card(card_header("Attendance"), DTOutput("attendance")),
      card(card_header("Transportation / Commute"), DTOutput("transportation")),
      card(card_header("Enrollment"), DTOutput("enrollment")),
      card(card_header("Dropout"), DTOutput("dropout")),
      card(card_header("Test Scores"), DTOutput("testscores"))
    )
  ),
  
  nav_panel(
    "Study Explorer",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("study", "Select Study", choices = df$Study_Name)
      ),
      uiOutput("study_profile")
    )
  ),
  
  nav_panel(
    "Literature Summary",
    layout_column_wrap(
      width = 1/2,
      card(card_header("Key Findings"), DTOutput("key_findings")),
      card(card_header("Policy Recommendations"), DTOutput("policy")),
      card(card_header("Limitations"), DTOutput("limits"))
    )
  )
)

server <- function(input, output, session) {
  
  output$home_table <- renderDT({
    df %>%
      select(
        Study_Name, Year, Country, Publication, Research_Design,
        Sample_Size, Population, Intervention, Follow_Up_Months
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 4, scrollX = TRUE)
      )
  })
  
  output$matrix <- renderDT({
    matrix_df <- df %>%
      select(
        Study_Name,
        Attendance_Rating,
        Transportation_Rating,
        Enrollment_Rating,
        Dropout_Rating,
        TestScore_Rating
      )
    
    datatable(
      matrix_df,
      rownames = FALSE,
      escape = FALSE,
      options = list(pageLength = 4, scrollX = TRUE, dom = "t")
    ) %>%
      formatStyle(
        columns = names(matrix_df)[-1],
        backgroundColor = styleEqual(names(rating_colors), rating_colors),
        color = "white",
        fontWeight = "bold",
        textAlign = "center"
      )
  })
  
  output$rating_plot <- renderPlotly({
    plot_df <- df %>%
      select(
        Attendance_Rating,
        Transportation_Rating,
        Enrollment_Rating,
        Dropout_Rating,
        TestScore_Rating
      ) %>%
      pivot_longer(
        everything(),
        names_to = "Outcome",
        values_to = "Rating"
      ) %>%
      mutate(
        Outcome = str_replace(Outcome, "_Rating", ""),
        Rating = ifelse(Rating == "NA", "Not Measured", Rating)
      ) %>%
      count(Outcome, Rating)
    
    plot_ly(
      plot_df,
      x = ~Outcome,
      y = ~n,
      color = ~Rating,
      colors = rating_colors,
      type = "bar"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = "Number of Studies"),
        legend = list(title = list(text = "Rating"))
      )
  })
  
  make_outcome_table <- function(metric, effect, rating) {
    df %>%
      select(Study_Name, all_of(metric), all_of(effect), all_of(rating)) %>%
      rename(
        Metric = all_of(metric),
        Effect = all_of(effect),
        Rating = all_of(rating)
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 4, scrollX = TRUE, dom = "t")
      ) %>%
      formatStyle(
        "Rating",
        backgroundColor = styleEqual(names(rating_colors), rating_colors),
        color = "white",
        fontWeight = "bold",
        textAlign = "center"
      )
  }
  
  output$attendance <- renderDT({
    make_outcome_table("Attendance_Metric", "Attendance_Effect", "Attendance_Rating")
  })
  
  output$transportation <- renderDT({
    make_outcome_table("Transportation_Metric", "Transportation_Effect", "Transportation_Rating")
  })
  
  output$enrollment <- renderDT({
    make_outcome_table("Enrollment_Metric", "Enrollment_Effect", "Enrollment_Rating")
  })
  
  output$dropout <- renderDT({
    make_outcome_table("Dropout_Metric", "Dropout_Effect", "Dropout_Rating")
  })
  
  output$testscores <- renderDT({
    make_outcome_table("TestScore_Metric", "TestScore_Effect", "TestScore_Rating")
  })
  
  output$study_profile <- renderUI({
    s <- df %>% filter(Study_Name == input$study)
    
    tagList(
      layout_column_wrap(
        width = 1/3,
        value_box("Country", s$Country, theme = "primary"),
        value_box("Year", s$Year, theme = "secondary"),
        value_box("Evidence Quality", s$Evidence_Quality, theme = "success")
      ),
      
      card(
        card_header(h3(s$Study_Name)),
        p(strong("Publication: "), s$Publication),
        p(strong("Research Design: "), s$Research_Design),
        p(strong("Sample Size: "), s$Sample_Size),
        p(strong("Population: "), s$Population),
        p(strong("Intervention: "), s$Intervention),
        p(strong("Follow-Up Months: "), s$Follow_Up_Months),
        hr(),
        
        h4("Outcome Ratings"),
        tags$table(
          class = "table table-bordered",
          tags$tr(tags$th("Outcome"), tags$th("Rating")),
          tags$tr(tags$td("Attendance"), tags$td(badge(s$Attendance_Rating))),
          tags$tr(tags$td("Transportation"), tags$td(badge(s$Transportation_Rating))),
          tags$tr(tags$td("Enrollment"), tags$td(badge(s$Enrollment_Rating))),
          tags$tr(tags$td("Dropout"), tags$td(badge(s$Dropout_Rating))),
          tags$tr(tags$td("Test Scores"), tags$td(badge(s$TestScore_Rating)))
        ),
        
        hr(),
        p(strong("Key Finding: "), s$Key_Finding),
        p(strong("Policy Recommendation: "), s$Policy_Recommendation),
        p(strong("Limitations: "), s$Limitations),
        tags$a("Open Study Link", href = s$Google_Scholar_Link, target = "_blank")
      )
    )
  })
  
  output$key_findings <- renderDT({
    df %>%
      select(Study_Name, Key_Finding) %>%
      datatable(rownames = FALSE, options = list(pageLength = 4, scrollX = TRUE))
  })
  
  output$policy <- renderDT({
    df %>%
      select(Study_Name, Policy_Recommendation) %>%
      datatable(rownames = FALSE, options = list(pageLength = 4, scrollX = TRUE))
  })
  
  output$limits <- renderDT({
    df %>%
      select(Study_Name, Limitations) %>%
      datatable(rownames = FALSE, options = list(pageLength = 4, scrollX = TRUE))
  })
}

shinyApp(ui, server)
