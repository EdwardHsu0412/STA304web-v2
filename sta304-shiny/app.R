library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(colourpicker)

source(file.path("R", "analysis.R"), local = TRUE)
survey <- load_survey()
study_limits <- range(survey$study_hours)
select_control <- function(...) shiny::selectInput(..., selectize = FALSE)
plot_label <- function(text, width = 36) paste(strwrap(text, width = width), collapse = "\n")

metric <- function(label, value, detail = NULL) {
  tags$div(class = "metric", tags$div(class = "metric-label", label),
           tags$div(class = "metric-value", value),
           tags$div(class = "metric-detail", detail))
}
section_heading <- function(kicker, title, description = NULL) {
  tags$div(class = "section-heading", tags$p(class = "eyebrow", kicker),
           tags$h2(title), tags$p(class = "section-description", description))
}
reading_box <- function(title, ...) {
  tags$div(class = "reading-box", tags$h3(title), ...)
}
chart_theme <- function() {
  theme_minimal(base_size = 13, base_family = "sans") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#E7ECF0", linewidth = .35),
          plot.title = element_text(face = "bold", size = 15, colour = "#263D4F", margin = margin(b = 14)),
          axis.title = element_text(colour = "#354B5D"),
          axis.text = element_text(colour = "#465B6B"),
          legend.position = "top", legend.title = element_blank(),
          plot.margin = margin(15, 20, 12, 12))
}

ui <- page_navbar(
  title = tags$span(class = "brand", "Social Media & cGPA"),
  id = "main_nav", selected = "Explore",
  window_title = "STA304 Group 18 | Social Media & cGPA",
  lang = "en", fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#087E8B",
                   base_font = "Segoe UI, Arial, sans-serif", border_radius = ".6rem"),
  navbar_options = navbar_options(bg = "#2C3E50", theme = "dark"),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "navigation.js"),
    tags$meta(name = "description", content = "Explore Group 18's survey of social media use, study habits, and self-reported cGPA in STA304H5.")
  ),
  sidebar = sidebar(
    title = "Explore the survey", width = 310, open = "desktop",
    tags$p(class = "sidebar-intro", "Choose a view, then explore a group of students."),
    conditionalPanel("input.main_nav == 'Explore'",
      select_control("view", "What would you like to see?", choices = c(
        "Social media & cGPA" = "relationship",
        "Compare student groups" = "groups",
        "Distribution of a variable" = "distribution",
        "Survey responses & summaries" = "responses")),
      conditionalPanel("input.view == 'relationship'",
        select_control("x_variable", "Compare cGPA with", choices = predictor_choices,
                    selected = "total_social_hours"),
        checkboxInput("show_fit", "Show fitted line and 95% band", TRUE)),
      conditionalPanel("input.view == 'groups'",
        select_control("group_variable", "Which variable?", choices = variable_choices, selected = "cgpa"),
        radioButtons("group_style", "Plot style", choices = c("Boxplot" = "box", "Violin" = "violin"), inline = TRUE),
        checkboxInput("show_points", "Show individual responses", TRUE)),
      conditionalPanel("input.view == 'distribution'",
        select_control("dist_variable", "Which variable?", choices = variable_choices, selected = "total_social_hours"),
        sliderInput("bins", "Number of histogram bins", min = 4, max = 25, value = 10, step = 1))
    ),
    conditionalPanel("input.main_nav == 'Analysis'",
      select_control("model_type", "Regression model", choices = c(
        "EDA: platform use, study, bedtime & status" = "eda",
        "Status adjusted for total use & study" = "adjusted")),
      tags$p(class = "control-note", "Both models predict cGPA. All estimates use the current filters.")),
    tags$hr(),
    tags$h3(class = "sidebar-section", "Filter the responses"),
    checkboxGroupInput("status_filter", "Student registration status",
                       choices = c("Domestic", "International"), selected = c("Domestic", "International")),
    sliderInput("study_filter", "Daily study time (hours)", min = study_limits[1], max = study_limits[2],
                value = study_limits, step = .5),
    tags$p(class = "control-note", "Filters update every plot, statistic, model, and response table."),
    actionButton("reset_filters", "Reset filters", class = "btn-outline-secondary w-100"),
    tags$hr(),
    tags$details(class = "appearance", open = NA,
      tags$summary("Plot appearance"),
      tags$p(class = "control-note", "Click a colour field to choose a shade, or type a colour name or hex code."),
      conditionalPanel("input.main_nav == 'Explore' && input.view == 'relationship'",
        checkboxInput("scatter_multicolour", "Use a different colour for each student status?", FALSE),
        conditionalPanel("!input.scatter_multicolour",
          colourInput("scatter_colour", "Input the point colour.", value = "#5B10A7", width = "100%")),
        conditionalPanel("input.scatter_multicolour",
          select_control("scatter_palette", "Colour palette (one colour per group):", choices = palette_choices))),
      conditionalPanel("input.main_nav == 'Explore' && input.view == 'groups'",
        checkboxInput("group_multicolour", "Use a different colour for each group?", FALSE),
        conditionalPanel("!input.group_multicolour",
          colourInput("group_colour", "Input the box colour.", value = "#FF6666", width = "100%")),
        conditionalPanel("input.group_multicolour",
          select_control("group_palette", "Colour palette (one colour per group):", choices = palette_choices))),
      conditionalPanel("input.main_nav == 'Explore' && input.view == 'distribution'",
        checkboxInput("dist_multicolour", "Use a different colour for each bar?", FALSE),
        conditionalPanel("!input.dist_multicolour",
          colourInput("dist_colour", "Input the bar colour.", value = "#6699FF", width = "100%")),
        conditionalPanel("input.dist_multicolour",
          select_control("dist_palette", "Colour palette (one colour per bar):", choices = palette_choices))),
      conditionalPanel("input.main_nav == 'Analysis'",
        colourInput("analysis_colour", "Input the analysis plot colour.", value = "#5B10A7", width = "100%")),
      sliderInput("point_size", "Point size", min = 1.5, max = 4.5, value = 2.8, step = .1)),
    tags$div(class = "sidebar-foot", tags$strong("STA304H5 · Fall 2026"),
             tags$p("Group 18 · University of Toronto Mississauga"))
  ),
  nav_panel("Explore", value = "Explore",
    tags$div(class = "page-content",
      tags$div(class = "page-heading",
        tags$p(class = "eyebrow", "STA304 STUDENT SURVEY · GROUP 18"),
        tags$h1("Social media and academic performance"),
        tags$p("Explore how online habits, study time, and student status relate to self-reported cGPA.")),
      uiOutput("selection_summary"),
      uiOutput("headline_metrics"),
      conditionalPanel("input.view == 'relationship'",
        section_heading("RESEARCH QUESTION 1", "Is daily social media use associated with cGPA?",
                        "Select a measure in the sidebar. Each point represents one survey response."),
        tags$div(class = "plot-panel", plotOutput("relationship_plot", height = "390px")),
        tags$details(class = "axis-controls", tags$summary("Adjust axis limits"),
          fluidRow(column(6, uiOutput("x_zoom_ui")),
                   column(6, sliderInput("cgpa_zoom", "cGPA axis limits", min = 0, max = 4, value = c(0, 4), step = .1))),
          tags$p(class = "control-note", "Axis limits zoom the plot only. Statistics still use all filtered responses.")),
        uiOutput("correlation_reading"),
        tags$p(class = "plot-note", "The fitted line and shaded 95% band describe the average linear trend. Identical answers may overlap.")),
      conditionalPanel("input.view == 'groups'",
        section_heading("RESEARCH QUESTION 2", "How do domestic and international students compare?",
                        "Explore the distribution within each group. Use Analysis to adjust for social media use and study time."),
        tags$div(class = "plot-panel", plotOutput("groups_plot", height = "390px")),
        tags$details(class = "axis-controls", tags$summary("Adjust the vertical axis"), uiOutput("group_zoom_ui"),
          tags$p(class = "control-note", "Zooming does not remove responses from the summary statistics.")),
        uiOutput("groups_reading"),
        tags$div(class = "table-panel", tags$h3("Summary by student status"), tableOutput("group_table")),
        tags$p(class = "plot-note", "A box spans the middle 50% of responses; its line marks the median. A violin shows the estimated distribution. Points are slightly offset horizontally for visibility.")),
      conditionalPanel("input.view == 'distribution'",
        section_heading("DAILY HABITS & ACADEMIC EXPERIENCE", "What does a typical response look like?",
                        "Choose any key variable and adjust the histogram bins."),
        tags$div(class = "plot-panel", plotOutput("distribution_plot", height = "390px")),
        tags$details(class = "axis-controls", tags$summary("Adjust the horizontal axis"), uiOutput("dist_zoom_ui"),
          tags$p(class = "control-note", "Zooming changes the view only. The interpretation uses the full filtered selection.")),
        uiOutput("distribution_reading")),
      conditionalPanel("input.view == 'responses'",
        section_heading("THE RESPONSES", "Explore the data behind the plots",
                        "Search and sort the response table. Original survey aliases have been replaced with response numbers."),
        downloadButton("download_data", "Download filtered responses", class = "btn-outline-primary"),
        tags$p(class = "plot-note", "The download contains all responses selected by the sidebar filters. Table search narrows this table only."),
        tags$div(class = "table-panel", DTOutput("response_table")),
        tags$div(class = "table-panel", tags$h3("Summary statistics for the current selection"),
                 tableOutput("summary_table")),
        tags$p(class = "plot-note", "SD measures spread around the mean. IQR is the width of the middle 50% of values (R quantile type 6, as in the EDA).")),
      tags$footer("Voluntary survey · Self-reported data · Associations do not establish cause and effect")
    )
  ),
  nav_panel("Analysis", value = "Analysis",
    tags$div(class = "page-content",
      section_heading("REGRESSION & DIAGNOSTICS", "Does the association remain after adjustment?",
                      "Compare the original EDA model with a model focused on total social media use, study time, and student status."),
      uiOutput("analysis_selection"),
      uiOutput("model_metrics"),
      uiOutput("model_reading"),
      tags$div(class = "table-panel", tags$h3("Linear regression results"),
        tableOutput("regression_table"),
        tags$p(class = "plot-note", "Coefficients are cGPA differences associated with one unit of a predictor, holding the other included variables fixed. Domestic is the reference status when both groups are selected. Confidence intervals and p-values use conventional OLS t inference."),
        tags$p(class = "plot-note", "The intercept is the fitted cGPA when all included numeric predictors equal zero. When both statuses are selected, domestic is the reference group. Zero study time lies outside the observed range.")),
      tags$div(class = "plot-panel", plotOutput("coefficient_plot", height = "380px")),
      tags$h3(class = "assumptions-title", "Interpretation of model assumptions"),
      tags$p(class = "section-description", "The EDA raised concerns about linearity, equal residual variance, and mild departures from normality. Inspect the current filtered model below. These plots suggest concerns; they do not prove that an assumption is violated."),
      select_control("diagnostic", "Diagnostic plot", choices = c(
        "Linearity: residuals vs fitted" = "residuals",
        "Constant variance: spread vs fitted" = "spread",
        "Normality: normal Q-Q plot" = "qq"), width = "100%"),
      tags$div(class = "plot-panel", plotOutput("diagnostic_plot", height = "340px")),
      uiOutput("diagnostic_reading"),
      tags$div(class = "assumption-notes",
        tags$h4("Independence and sampling"),
        tags$p("Independence cannot be established from a diagnostic plot. Participation was voluntary and included mutual survey completion with other groups. These responses may not represent all STA304H5 students, and the results should not be generalized to all university students.")),
      tags$footer("Exploratory analysis · Filtering creates smaller, less precise estimates · Repeated exploration is not a confirmatory test")
    )
  ),
  nav_panel("About", value = "About",
    tags$div(class = "page-content about-content",
      section_heading("ABOUT THE STUDY", "Online habits in STA304H5",
                      "Group 18: Edward, Angela, Sasha, Ali, and Prativa"),
      tags$div(class = "about-grid",
        tags$div(class = "about-block", tags$h3("Our questions"),
          tags$ol(tags$li("Is average daily social media use associated with self-reported cumulative GPA?"),
                  tags$li("Do domestic and international students differ in cGPA after accounting for social media use and daily study time?"))),
        tags$div(class = "about-block", tags$h3("Who answered?"),
          tags$p("The target population was students enrolled in STA304H5 at the University of Toronto Mississauga in Fall 2026. A voluntary Google Forms questionnaire was shared on the course discussion board. The group also engaged in mutual survey completion with peer groups."))),
      tags$div(class = "about-block", tags$h3("From 72 responses to 64"),
        tags$p("This app uses the same eight record exclusions as the submitted EDA, as confirmed by the group. The original workbook has six yellow-marked IDs; two additional, unmarked records were excluded in the EDA. No extra records are removed by the app."),
        tags$p("The EDA described the exclusions as seven implausible daily-use records and one zero-cGPA record. A cGPA of zero is within the questionnaire range; its exclusion follows the group's EDA decision and should not be treated as a universal cleaning rule."),
        tags$p("The cleaned sample has 42 domestic and 22 international students. Survey aliases are replaced with sequential response numbers. The original Excel file is unchanged.")),
      tags$div(class = "about-block", tags$h3("What the measures mean"),
        tags$dl(class = "definitions",
          tags$dt("cGPA"), tags$dd("Self-reported cumulative GPA on a 0-4 scale. Answers are used as supplied; they are not rounded again."),
          tags$dt("Total social media use"), tags$dd("The sum of hours reported for short-form video, text-based platforms, and calls/messaging. Activities can overlap, so this is a sum of reported category hours rather than verified distinct screen time."),
          tags$dt("Time off before bed"), tags$dd("Minutes between stopping social media use and sleeping, as reported by the respondent."),
          tags$dt("Satisfaction"), tags$dd("Satisfaction with cGPA, from 1 (worst) to 10 (best)."))),
      tags$div(class = "about-block", tags$h3("What can we conclude?"),
        tags$p("In the full cleaned sample, the EDA found almost no linear correlation between total reported social media use and cGPA. Its regression also provided insufficient evidence for the proposed associations. This does not establish that social media has no relationship with academic performance."),
        tags$p("The sample is small and voluntary. GPA, time use, and satisfaction are self-reported, with possible rounding and recall error. Group sizes differ, other relevant factors were not measured, and the study is observational. Filters describe the selected responses and can make estimates less stable.")),
      tags$div(class = "about-block", tags$h3("How to use this app"),
        tags$p("Choose a view under Explore, filter by student status or study time, and change variables, colours, or axis limits. Analysis refits the selected regression using the same filtered responses. An empty selection or a model that cannot be estimated produces an explanatory message."),
        tags$p("The sidebar filters affect all data outputs. Table search affects only the response table. Axis limits affect only what is visible in a plot.")),
      tags$div(class = "about-block source-note", tags$h3("Sources and design"),
        tags$p("Data: the group's supplied survey workbook. Study context and initial analysis: STA304 Exploratory Data Analysis, Group 18, October 25, 2026. App requirements: STA304 Shiny Website Instructions, 2026."),
        tags$p("The sidebar-and-tabs navigation is inspired by the course example. The research questions, data preparation, plots, regression summaries, diagnostic explanations, filters, and styling are specific to this project.")),
      tags$footer("STA304H5 · University of Toronto Mississauga · Fall 2026")
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    filter_survey(survey, input$status_filter %||% character(), input$study_filter %||% study_limits)
  })
  scatter_colours <- reactive({
    values <- if (isTRUE(input$scatter_multicolour)) palette_colours(input$scatter_palette %||% "rainbow", 2) else
      rep(plot_colour(input$scatter_colour, "#5B10A7"), 2)
    stats::setNames(values, c("Domestic", "International"))
  })
  comparison_colours <- reactive({
    values <- if (isTRUE(input$group_multicolour)) palette_colours(input$group_palette %||% "rainbow", 2) else
      rep(plot_colour(input$group_colour, "#FF6666"), 2)
    stats::setNames(values, c("Domestic", "International"))
  })
  analysis_colour <- reactive(plot_colour(input$analysis_colour, "#5B10A7"))
  correlation <- reactive(pearson_result(filtered(), input$x_variable %||% "total_social_hours"))
  model_result <- reactive(fit_survey_model(filtered(), input$model_type %||% "eda"))
  model_coefficients <- reactive(coefficient_table(model_result()))

  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "status_filter", selected = c("Domestic", "International"))
    updateSliderInput(session, "study_filter", value = study_limits)
  })

  selection_ui <- function() {
    d <- filtered()
    groups <- levels(droplevels(d$status))
    name <- if (length(groups) == 2) "All student statuses" else if (length(groups)) groups else "No students selected"
    tags$div(class = paste("selection-summary", if (!nrow(d)) "empty-selection"),
      tags$span(class = "selection-dot", ""),
      tags$span(tags$strong(paste(nrow(d), "of", nrow(survey), "responses")), " · ", name),
      if (nrow(d) < 10) tags$span(class = "selection-caution", "Small selection: estimates may be unavailable or imprecise."))
  }
  output$selection_summary <- renderUI(selection_ui())
  output$analysis_selection <- renderUI(selection_ui())
  output$headline_metrics <- renderUI({
    d <- filtered()
    n <- nrow(d)
    tags$div(class = "metrics-grid",
      metric("Responses selected", as.character(n), sprintf("%d domestic · %d international", sum(d$status == "Domestic"), sum(d$status == "International"))),
      metric("Mean cGPA", if (n) number_text(mean(d$cgpa)) else "—", "On a 0-4 scale"),
      metric("Median social media use", if (n) paste0(number_text(median(d$total_social_hours), 1), " h") else "—", "Sum of reported hours per day"),
      metric("Mean daily study time", if (n) paste0(number_text(mean(d$study_hours), 1), " h") else "—", "Self-reported hours per day"))
  })

  output$x_zoom_ui <- renderUI({
    v <- input$x_variable %||% "total_social_hours"
    limits <- axis_range(survey, v)
    sliderInput("x_zoom", "Horizontal axis limits", min = limits[1], max = limits[2], value = limits,
                step = if (v == "cooldown_minutes") 1 else .1)
  })
  output$group_zoom_ui <- renderUI({
    v <- input$group_variable %||% "cgpa"
    limits <- axis_range(survey, v)
    sliderInput("group_zoom", "Vertical axis limits", min = limits[1], max = limits[2], value = limits,
                step = if (v == "cooldown_minutes") 1 else .1)
  })
  output$dist_zoom_ui <- renderUI({
    v <- input$dist_variable %||% "total_social_hours"
    limits <- axis_range(survey, v)
    sliderInput("dist_zoom", "Horizontal axis limits", min = limits[1], max = limits[2], value = limits,
                step = if (v == "cooldown_minutes") 1 else .1)
  })

  relationship_chart <- reactive({
    d <- filtered()
    req(nrow(d) > 0)
    v <- input$x_variable %||% "total_social_hours"
    p <- ggplot(d, aes(x = .data[[v]], y = cgpa)) +
      geom_point(aes(colour = status, shape = status), size = input$point_size %||% 2.8, alpha = .8) +
      scale_colour_manual(values = scatter_colours(), drop = FALSE) +
      scale_shape_manual(values = c(Domestic = 16, International = 17), drop = FALSE) +
      labs(title = plot_label(paste("cGPA and", short_labels[v])), x = plot_label(unname(variable_labels[v])), y = "Self-reported cGPA (0-4)") +
      chart_theme() +
      coord_cartesian(xlim = safe_zoom(input$x_zoom, axis_range(survey, v)),
                      ylim = safe_zoom(input$cgpa_zoom, c(0, 4)))
    if (isTRUE(input$show_fit) && nrow(d) >= 3 && length(unique(d[[v]])) > 1 && length(unique(d$cgpa)) > 1) {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, colour = "#2C3E50", fill = "#A6BAC7", linewidth = .8, alpha = .22)
    }
    p
  })
  output$relationship_plot <- renderPlot({
    validate(need(nrow(filtered()) > 0, "No responses match the filters. Select a student status or reset the filters."))
    relationship_chart()
  }, res = 110)
  output$correlation_reading <- renderUI({
    r <- correlation()
    if (!r$available) return(reading_box("Reading the relationship", tags$p(r$message)))
    v <- input$x_variable %||% "total_social_hours"
    trend <- if (abs(r$r) < .1) "almost no straight-line relationship" else paste(
      if (abs(r$r) < .3) "a weak" else if (abs(r$r) < .5) "a moderate" else "a strong",
      if (r$r > 0) "positive linear relationship" else "negative linear relationship")
    reading_box("Reading the relationship",
      tags$p(sprintf("Among these %d responses, %s and cGPA show %s (Pearson r = %.3f).", r$n, short_labels[v], trend, r$r)),
      tags$p(class = "stat-line", sprintf("95%% confidence interval for r: [%.3f, %.3f] · p-value: %s", r$ci[1], r$ci[2], p_text(r$p))),
      tags$p(if (r$p >= .05) "These data provide insufficient evidence of a linear association at the 5% level. This does not establish that no relationship exists." else
        "This exploratory selection shows evidence of a linear association at the 5% level. The association does not establish cause and effect."))
  })

  groups_chart <- reactive({
    d <- filtered()
    req(nrow(d) > 0)
    v <- input$group_variable %||% "cgpa"
    p <- ggplot(d, aes(x = status, y = .data[[v]], colour = status, fill = status))
    if (identical(input$group_style, "violin") && all(table(droplevels(d$status)) >= 3)) {
      p <- p + geom_violin(alpha = .14, trim = TRUE, linewidth = .7) +
        geom_boxplot(width = .14, outlier.shape = NA, alpha = .45, linewidth = .7)
    } else {
      p <- p + geom_boxplot(width = .45, outlier.shape = if (isTRUE(input$show_points)) NA else 16, alpha = .18, linewidth = .8)
    }
    if (isTRUE(input$show_points)) p <- p + geom_point(aes(shape = status),
      position = position_jitter(width = .11, height = 0, seed = 304), alpha = .65, size = input$point_size %||% 2.8)
    counts <- table(d$status)
    p + scale_colour_manual(values = comparison_colours()) + scale_fill_manual(values = comparison_colours()) +
      scale_shape_manual(values = c(Domestic = 16, International = 17)) +
      scale_x_discrete(labels = function(x) paste0(x, "\n(n = ", counts[x], ")"), drop = TRUE) +
      labs(title = plot_label(paste(unname(variable_labels[v]), "by student status")), x = NULL, y = plot_label(unname(variable_labels[v]))) +
      coord_cartesian(ylim = safe_zoom(input$group_zoom, axis_range(survey, v))) +
      chart_theme() + theme(legend.position = "none")
  })
  output$groups_plot <- renderPlot({
    validate(need(nrow(filtered()) > 0, "No responses match the filters. Select a student status or reset the filters."))
    groups_chart()
  }, res = 110)
  output$group_table <- renderTable({
    validate(need(nrow(filtered()) > 0, "No responses selected."))
    group_summary(filtered(), input$group_variable %||% "cgpa")
  }, digits = 2, striped = TRUE, bordered = FALSE, na = "Unavailable", width = "100%")
  output$groups_reading <- renderUI({
    d <- filtered()
    if (!nrow(d)) return(reading_box("Reading the comparison", tags$p("Reset the filters to see a group comparison.")))
    v <- input$group_variable %||% "cgpa"
    s <- group_summary(d, v)
    if (nrow(s) < 2) return(reading_box("Reading the comparison", tags$p(sprintf("Only %s students are selected (n = %d). Select both statuses to compare their distributions.", tolower(s$Status[1]), nrow(d)))))
    gap <- s$Mean[s$Status == "International"] - s$Mean[s$Status == "Domestic"]
    reading_box("Reading the comparison",
      tags$p(sprintf("For %s, the mean is %.2f among domestic students and %.2f among international students. The international-minus-domestic difference is %.2f in the displayed units.", short_labels[v], s$Mean[s$Status == "Domestic"], s$Mean[s$Status == "International"], gap)),
      tags$p("This is a descriptive comparison. It does not adjust for study time or social media use; the regression on the Analysis tab provides that adjustment."))
  })

  distribution_chart <- reactive({
    d <- filtered()
    req(nrow(d) > 0)
    v <- input$dist_variable %||% "total_social_hours"
    r <- range(survey[[v]])
    breaks <- if (v == "satisfaction") seq(.5, 10.5, 1) else seq(r[1] - .001, r[2] + .001, length.out = (input$bins %||% 10) + 1)
    p <- ggplot(d, aes(x = .data[[v]]))
    if (isTRUE(input$dist_multicolour)) {
      p <- p + geom_histogram(aes(fill = after_stat(factor(x))), breaks = breaks,
                              colour = "white", linewidth = .35) +
        scale_fill_manual(values = palette_colours(input$dist_palette %||% "rainbow", length(breaks) - 1), guide = "none")
    } else {
      p <- p + geom_histogram(breaks = breaks, fill = plot_colour(input$dist_colour, "#6699FF"),
                              colour = "white", linewidth = .35)
    }
    p +
      scale_y_continuous(breaks = function(x) unique(round(pretty(x, n = 5))), expand = expansion(mult = c(0, .08))) +
      labs(title = plot_label(paste("Distribution of", short_labels[v])), x = plot_label(unname(variable_labels[v])), y = "Number of responses") +
      coord_cartesian(xlim = safe_zoom(input$dist_zoom, axis_range(survey, v))) + chart_theme()
  })
  output$distribution_plot <- renderPlot({
    validate(need(nrow(filtered()) > 0, "No responses match the filters. Select a student status or reset the filters."))
    distribution_chart()
  }, res = 110)
  output$distribution_reading <- renderUI({
    d <- filtered()
    if (!nrow(d)) return(reading_box("Reading the distribution", tags$p("No responses selected.")))
    v <- input$dist_variable %||% "total_social_hours"
    x <- d[[v]]
    reading_box("Reading the distribution",
      tags$p(sprintf("Across %d responses, %s has a median of %.2f and a mean of %.2f. The middle 50%% spans %.2f to %.2f; observed values range from %.2f to %.2f, in the units on the horizontal axis.",
        length(x), short_labels[v], median(x), mean(x), quantile(x, .25, type = 6), quantile(x, .75, type = 6), min(x), max(x))),
      tags$p(if (v == "satisfaction") "Each bar is one satisfaction rating. Changing the bin control applies to the continuous variables; satisfaction always uses its ten response levels." else "Each bar counts responses in an interval. Changing the bins changes the shape of the display, while the summary statistics stay the same."))
  })

  output$response_table <- renderDT({
    datatable(readable_responses(filtered()), rownames = FALSE, escape = TRUE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 25, 64), scrollX = TRUE,
                     language = list(emptyTable = "No responses match the current filters.")),
      class = "compact stripe hover") |>
      formatRound(columns = 3:10, digits = 2)
  }, server = FALSE)
  output$summary_table <- renderTable({
    validate(need(nrow(filtered()) > 0, "No responses selected."))
    quantitative_summary(filtered())
  }, digits = 2, striped = TRUE, width = "100%", na = "Unavailable")
  output$download_data <- downloadHandler(
    filename = function() "sta304_filtered_responses.csv",
    content = function(file) write.csv(readable_responses(filtered()), file, row.names = FALSE, na = "")
  )

  output$model_metrics <- renderUI({
    m <- model_result()
    if (!m$available) return(NULL)
    tags$div(class = "metrics-grid",
      metric("Model responses", m$n, "Current sidebar selection"),
      metric("R-squared", number_text(m$summary$r.squared, 3), "Proportion of cGPA variation explained"),
      metric("Adjusted R-squared", number_text(m$summary$adj.r.squared, 3), "Accounts for the number of predictors"),
      metric("Overall model p-value", p_text(m$p), "F-test against an intercept-only model"))
  })
  output$model_reading <- renderUI({
    m <- model_result()
    if (!m$available) return(reading_box("Model unavailable for this selection", tags$p(m$message)))
    rows <- m$coefficients[m$coefficients$term != "(Intercept)", , drop = FALSE]
    status <- rows[rows$term == "statusInternational", , drop = FALSE]
    reading_box("Reading the adjusted results",
      tags$p(sprintf("This model explains %.1f%% of cGPA variation in the selected responses (overall p-value %s). %s", 100 * m$summary$r.squared, p_text(m$p),
        if (all(rows$p >= .05)) "None of the included predictors has a p-value below 0.05. The data provide insufficient evidence for these adjusted associations." else
          "At least one predictor has a p-value below 0.05 in this exploratory selection. Interpret its size and confidence interval together with the model assumptions.")),
      if (nrow(status)) tags$p(sprintf("Holding the other included variables fixed, international students have an estimated cGPA difference of %.3f relative to domestic students (95%% CI [%.3f, %.3f]; p-value %s).", status$estimate, status$lower, status$upper, p_text(status$p))),
      lapply(m$notes, tags$p),
      tags$p(class = "control-note", "These are observational, exploratory estimates. Conventional confidence intervals may be unreliable when model assumptions are questionable."))
  })
  output$regression_table <- renderTable({
    m <- model_result()
    validate(need(m$available, m$message %||% "Model unavailable."))
    model_coefficients()
  }, striped = TRUE, bordered = FALSE, width = "100%", sanitize.text.function = identity)
  coefficient_chart <- reactive({
    m <- model_result()
    req(m$available)
    d <- m$coefficients[m$coefficients$term != "(Intercept)", , drop = FALSE]
    d$label <- factor(d$label, levels = rev(d$label))
    ggplot(d, aes(x = estimate, y = label)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#7D8A95") +
      geom_segment(aes(x = lower, xend = upper, yend = label), colour = analysis_colour(), linewidth = 1) +
      geom_point(colour = analysis_colour(), size = input$point_size %||% 2.8) +
      labs(title = "Adjusted associations\nand 95% confidence intervals", x = "Estimated difference in cGPA", y = NULL) + chart_theme()
  })
  output$coefficient_plot <- renderPlot({
    m <- model_result()
    validate(need(m$available, m$message %||% "Model unavailable."))
    coefficient_chart()
  }, res = 110)

  diagnostic_chart <- reactive({
    m <- model_result()
    req(m$available)
    d <- data.frame(fitted = fitted(m$fit), residual = residuals(m$fit),
                    standardized = rstandard(m$fit))
    colour <- analysis_colour()
    diagnostic <- input$diagnostic %||% "residuals"
    if (diagnostic == "qq") {
      return(ggplot(d, aes(sample = standardized)) + stat_qq(colour = colour, size = 2.1) +
        stat_qq_line(colour = "#7D8A95", linewidth = .8) +
        labs(title = "Normal Q-Q plot\nof standardized residuals", x = "Theoretical normal quantiles", y = "Standardized residuals") + chart_theme())
    }
    if (diagnostic == "spread") {
      d$spread <- sqrt(abs(d$standardized))
      return(ggplot(d, aes(x = fitted, y = spread)) + geom_point(colour = colour, alpha = .75, size = 2.1) +
        geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#D55E00", linewidth = .8, span = .9) +
        labs(title = "Residual spread across fitted cGPA", x = "Fitted cGPA", y = "Square root of |standardized residual|") + chart_theme())
    }
    ggplot(d, aes(x = fitted, y = residual)) + geom_hline(yintercept = 0, linetype = "dashed", colour = "#7D8A95") +
      geom_point(colour = colour, alpha = .75, size = 2.1) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#D55E00", linewidth = .8, span = .9) +
      labs(title = "Residuals versus fitted cGPA", x = "Fitted cGPA", y = "Residual (reported minus fitted cGPA)") + chart_theme()
  })
  output$diagnostic_plot <- renderPlot({
    m <- model_result()
    validate(need(m$available, m$message %||% "Model unavailable."))
    diagnostic_chart()
  }, res = 110)
  output$diagnostic_reading <- renderUI({
    diagnostic <- input$diagnostic %||% "residuals"
    explanation <- switch(diagnostic,
      residuals = c("Linearity", "A roughly flat smooth line near zero supports a linear mean relationship. A curved pattern suggests that a straight-line model may miss structure."),
      spread = c("Constant residual variance", "The smooth line should be roughly horizontal and the vertical spread should be similar across fitted values. A trend in spread raises concern about unequal residual variance."),
      qq = c("Residual normality", "Points close to the reference line support approximately normal residuals. Systematic tail departures suggest caution with the model's conventional confidence intervals and p-values."))
    reading_box(explanation[1], tags$p(explanation[2]),
      tags$p(class = "control-note", "Diagnostics refresh with the selected model and filters. Small selections can produce unstable smooth curves."))
  })
}

shinyApp(ui, server)
