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
default_colours <- list(points = "#14735D", groups = "#4C638F", bars = "#AA5039", analysis = "#14735D")

metric <- function(label, value, detail = NULL) {
  tags$div(class = "metric", tags$div(class = "metric-label", label),
           tags$div(class = "metric-value", value),
           tags$div(class = "metric-detail", detail))
}
chart_header <- function(title, description, ...) {
  tags$header(class = "chart-header",
    tags$div(tags$h2(title), tags$p(class = "section-description", description)),
    tags$div(class = "chart-inputs", ...))
}
reading_box <- function(title, ...) {
  tags$div(class = "reading-box", tags$h3(title), ...)
}
# Plain-language summary shown first in every interpretation; statistical detail follows it.
plain_lead <- function(...) tags$p(class = "plain-lead", tags$span(class = "plain-tag", "In plain terms"), ...)
detail_label <- function(text = "Statistical details") tags$p(class = "detail-label", text)
plain_note <- function(...) tags$p(class = "plot-note plain-note", tags$strong("In plain terms: "), ...)
unit_labels <- c(cgpa = "grade points", total_social_hours = "hours a day", study_hours = "hours a day",
                 short_video_hours = "hours a day", text_hours = "hours a day", communication_hours = "hours a day",
                 cooldown_minutes = "minutes", satisfaction = "points on the 1-10 scale")
percent_text <- function(x) if (length(x) && is.finite(x)) sprintf("%.0f%%", 100 * x) else "an unknown share"
word_list <- function(x) if (length(x) <= 2) paste(x, collapse = " and ") else
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
plain_term <- function(label) {
  label <- sub(" \\(.*\\)$", "", label)
  ifelse(label == "International vs domestic", "student status", tolower(label))
}
# Describes what the current diagnostic plot shows, in plain words plus the supporting number.
diagnostic_finding <- function(fit, diagnostic) {
  fitted_values <- stats::fitted(fit)
  res <- stats::residuals(fit)
  std <- stats::rstandard(fit)
  if (diagnostic == "residuals") {
    smooth <- tryCatch(suppressWarnings(stats::predict(stats::loess(res ~ fitted_values, span = .9))), error = function(e) NULL)
    if (is.null(smooth) || !all(is.finite(smooth)) || stats::sd(res) == 0)
      return(list(plain = "There are too few responses here to draw a reliable trend line, so this check can't tell us much.", detail = "Smooth curve unavailable for this selection."))
    bend <- max(abs(smooth)) / stats::sd(res)
    list(plain = if (bend < .25) "The orange line stays close to zero all the way across, so a straight-line model looks reasonable for these students."
                 else "The orange line bends away from zero, so a straight-line model may be missing a curved pattern in these students' cGPAs.",
         detail = sprintf("Largest distance of the smooth curve from zero: %.2f residual standard deviations (under 0.25 treated as close to flat).", bend))
  } else if (diagnostic == "spread") {
    rho <- suppressWarnings(stats::cor(fitted_values, sqrt(abs(std)), method = "spearman"))
    if (!is.finite(rho)) return(list(plain = "There isn't enough variation here to judge whether the spread changes.", detail = "Spearman correlation unavailable."))
    list(plain = if (abs(rho) < .2) "The points are spread about evenly from left to right, so the model's errors look similar for students with low and high predicted cGPAs."
                 else if (rho > 0) "The points spread out more toward the right, so the model's predictions are less precise for students with higher predicted cGPAs."
                 else "The points spread out more toward the left, so the model's predictions are less precise for students with lower predicted cGPAs.",
         detail = sprintf("Spearman correlation between fitted cGPA and residual spread: %.2f (under 0.2 in size treated as roughly even).", rho))
  } else {
    sw <- if (length(res) >= 3 && length(res) <= 5000 && stats::sd(res) > 0) stats::shapiro.test(res)$p.value else NA_real_
    far <- sum(abs(std) > 2)
    plain <- if (is.finite(sw) && sw >= .05) "The points mostly follow the reference line, so the model's errors look roughly bell-shaped and its p-values are reasonable to read."
             else "The points drift away from the line at the ends, meaning some students' cGPAs are much further from the model's prediction than expected. Treat the p-values and confidence intervals with some caution."
    if (far) plain <- paste(plain, sprintf("%d of %d students %s unusually far from their predicted cGPA.", far, length(res), if (far == 1) "sits" else "sit"))
    list(plain = plain,
         detail = sprintf("Shapiro-Wilk normality test p-value: %s · %d %s with |standardized residual| > 2.", p_text(sw), far, if (far == 1) "response" else "responses"))
  }
}
chart_theme <- function() {
  theme_minimal(base_size = 13, base_family = "sans") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#E2E8E1", linewidth = .35),
          plot.title = element_text(face = "bold", size = 15, colour = "#18382E", margin = margin(b = 12)),
          axis.title = element_text(colour = "#2D463D"),
          axis.text = element_text(colour = "#4D6158"),
          legend.position = "top", legend.title = element_blank(),
          plot.margin = margin(12, 16, 12, 12))
}

ui <- page_navbar(
  title = tags$span(class = "brand", "Study & Scroll"),
  id = "main_nav", selected = "Explore",
  window_title = "STA304 Group 18 | Study & Scroll",
  lang = "en", fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#205A47", bg = "#F7F7F0", fg = "#18382E",
                   base_font = "system-ui, sans-serif", border_radius = ".5rem"),
  navbar_options = navbar_options(bg = "#FCFDF8", theme = "light"),
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src = "navigation.js"),
      tags$meta(name = "description", content = "Explore Group 18's survey of social media use, study habits, and self-reported cGPA in STA304H5.")
    ),
    conditionalPanel("input.main_nav == 'Explore'",
      tags$section(class = "site-shell survey-hero",
        tags$div(class = "hero-copy",
          tags$p(class = "byline", "STA304H5 · Group 18 · Fall 2026"),
          tags$h1("Study time, screen time and cGPA"),
          tags$p("Explore how online habits and study time relate to self-reported grades in our class survey.")),
        tags$section(class = "snapshot", `aria-label` = "Sample snapshot",
          tags$h2("Sample snapshot"), uiOutput("headline_metrics")))),
    conditionalPanel("input.main_nav == 'Analysis'",
      tags$header(class = "site-shell section-intro",
        tags$h1("Regression results"),
        tags$p("See how cGPA varies with social media use, study time, and student status after adjustment."))),
    conditionalPanel("input.main_nav == 'About'",
      tags$header(class = "site-shell section-intro",
        tags$p(class = "byline", "STA304H5 · University of Toronto Mississauga"),
        tags$h1("Study notes"),
        tags$p("Group 18: Edward, Angela, Sasha, Ali, and Prativa"))),
    conditionalPanel("input.main_nav == 'Explore' || input.main_nav == 'Analysis'",
      tags$div(class = "site-shell",
        tags$section(class = "sample-filters", `aria-label` = "Sample filters",
          tags$div(class = "filter-heading", tags$h2("Sample filters"),
            uiOutput("selection_summary", `aria-live` = "polite")),
          checkboxGroupInput("status_filter", "Student status", inline = TRUE,
                             choices = c("Domestic", "International"), selected = c("Domestic", "International")),
          sliderInput("study_filter", "Daily study time (hours)", min = study_limits[1], max = study_limits[2],
                      value = study_limits, step = .5, width = "100%"),
          actionButton("reset_filters", "Reset filters", class = "btn-outline-primary")),
        tags$div(class = "tool-dock",
          conditionalPanel("input.main_nav == 'Explore'", class = "view-switch",
            radioButtons("view", "Chart or data view", inline = TRUE, width = "auto", choices = c(
              "Association" = "relationship",
              "Group comparison" = "groups",
              "Distributions" = "distribution",
              "Responses" = "responses"))),
          conditionalPanel("input.main_nav == 'Analysis'", class = "model-switch",
            select_control("model_type", "Regression model", width = "100%", choices = c(
              "EDA model: platform use, study, bedtime and status" = "eda",
              "Status adjusted for total use and study" = "adjusted"))),
          conditionalPanel("input.main_nav == 'Analysis' || input.view != 'responses'", class = "style-slot",
            tags$details(class = "plot-styling",
              tags$summary("Plot styling"),
              tags$div(class = "plot-style-menu",
                tags$p(class = "control-note", "Pick a shade or enter a colour name or hex code."),
                conditionalPanel("input.main_nav == 'Explore' && input.view == 'relationship'",
                  checkboxInput("scatter_multicolour", "Colour by student status", FALSE),
                  conditionalPanel("!input.scatter_multicolour",
                    colourInput("scatter_colour", "Point colour", value = default_colours$points, width = "100%")),
                  conditionalPanel("input.scatter_multicolour",
                    select_control("scatter_palette", "Palette", choices = palette_choices, width = "100%"))),
                conditionalPanel("input.main_nav == 'Explore' && input.view == 'groups'",
                  checkboxInput("group_multicolour", "Colour by student group", FALSE),
                  conditionalPanel("!input.group_multicolour",
                    colourInput("group_colour", "Group colour", value = default_colours$groups, width = "100%")),
                  conditionalPanel("input.group_multicolour",
                    select_control("group_palette", "Palette", choices = palette_choices, width = "100%"))),
                conditionalPanel("input.main_nav == 'Explore' && input.view == 'distribution'",
                  checkboxInput("dist_multicolour", "Give each bar its own colour", FALSE),
                  conditionalPanel("!input.dist_multicolour",
                    colourInput("dist_colour", "Bar colour", value = default_colours$bars, width = "100%")),
                  conditionalPanel("input.dist_multicolour",
                    select_control("dist_palette", "Palette", choices = palette_choices, width = "100%"))),
                conditionalPanel("input.main_nav == 'Analysis'",
                  colourInput("analysis_colour", "Plot colour", value = default_colours$analysis, width = "100%")),
                conditionalPanel("input.main_nav == 'Analysis' || input.view == 'relationship' || input.view == 'groups'",
                  sliderInput("point_size", "Point size", min = 1.5, max = 4.5, value = 2.8, step = .1, width = "100%"))
              )
            )
          )
        )
      )
    )
  ),
  nav_panel("Explore", value = "Explore",
    tags$main(class = "site-shell page-content",
      conditionalPanel("input.view == 'relationship'",
        tags$section(class = "chart-card",
          chart_header("How do daily habits relate to cGPA?",
                       "Each point represents one survey response. Identical answers may overlap.",
            select_control("x_variable", "Compare cGPA with", choices = predictor_choices,
                           selected = "total_social_hours", width = "100%"),
            checkboxInput("show_fit", "Show trend line and 95% band", TRUE)),
          tags$div(class = "plot-panel", plotOutput("relationship_plot", height = "370px")),
          tags$details(class = "axis-controls", tags$summary("Axis limits"),
            tags$div(class = "axis-grid",
              uiOutput("x_zoom_ui"),
              sliderInput("cgpa_zoom", "cGPA axis limits", min = 0, max = 4, value = c(0, 4), step = .1)),
            tags$p(class = "control-note", "Axis limits zoom the plot. Statistics use all filtered responses."))),
        uiOutput("correlation_reading"),
        plain_note("If the trend line is nearly flat, this habit doesn't tell us much about a student's cGPA. The wider the shaded band, the less sure we are about where the line really sits."),
        tags$p(class = "plot-note", "The trend line and shaded 95% band describe the average linear relationship.")),
      conditionalPanel("input.view == 'groups'",
        tags$section(class = "chart-card",
          chart_header("How do the student groups compare?",
                       "Compare domestic and international students on a measure of your choice.",
            select_control("group_variable", "Measure", choices = variable_choices, selected = "cgpa", width = "100%"),
            tags$div(class = "inline-controls",
              radioButtons("group_style", "Plot style", choices = c("Boxplot" = "box", "Violin" = "violin"), inline = TRUE),
              checkboxInput("show_points", "Show responses", TRUE))),
          tags$div(class = "plot-panel", plotOutput("groups_plot", height = "370px")),
          tags$details(class = "axis-controls", tags$summary("Axis limits"), uiOutput("group_zoom_ui"),
            tags$p(class = "control-note", "Summary statistics use all filtered responses."))),
        uiOutput("groups_reading"),
        tags$div(class = "table-panel", tags$h3("Summary by student status"), tableOutput("group_table")),
        plain_note("The thick line inside each box is the middle student in that group. A taller box or wider violin means students in that group gave more varied answers."),
        tags$p(class = "plot-note", "A box spans the middle 50% of responses; its line marks the median. A violin shows the estimated distribution. Points are slightly offset horizontally for visibility.")),
      conditionalPanel("input.view == 'distribution'",
        tags$section(class = "chart-card",
          chart_header("What does a typical response look like?",
                       "The histogram shows where responses cluster and how widely they vary.",
            select_control("dist_variable", "Measure", choices = variable_choices, selected = "total_social_hours", width = "100%"),
            conditionalPanel("input.dist_variable != 'satisfaction'",
              sliderInput("bins", "Histogram bins", min = 4, max = 25, value = 10, step = 1, width = "100%"))),
          tags$div(class = "plot-panel", plotOutput("distribution_plot", height = "370px")),
          tags$details(class = "axis-controls", tags$summary("Axis limits"), uiOutput("dist_zoom_ui"),
            tags$p(class = "control-note", "The interpretation uses all filtered responses."))),
        uiOutput("distribution_reading")),
      conditionalPanel("input.view == 'responses'",
        tags$div(class = "table-panel response-panel",
          chart_header("Survey responses",
                       "Search or sort the table. The download includes every response in the current sample filters.",
            downloadButton("download_data", "Download filtered responses", class = "btn-outline-primary")),
          DTOutput("response_table")),
        tags$div(class = "table-panel", tags$h3("Summary statistics for the current selection"),
                 tableOutput("summary_table")),
        plain_note("The mean is the average answer and the median is the middle answer. SD and IQR show how spread out the answers are: bigger numbers mean students answered less alike."),
        tags$p(class = "plot-note", "SD measures spread around the mean. IQR is the width of the middle 50% of values (R quantile type 6, as in the EDA)."))
    )
  ),
  nav_panel("Regression", value = "Analysis",
    tags$main(class = "site-shell page-content",
      uiOutput("model_reading"),
      uiOutput("model_metrics"),
      tags$section(class = "table-panel",
        tags$h2("Model coefficients"),
        tags$div(class = "result-grid",
          tags$div(class = "table-scroll", tableOutput("regression_table")),
          tags$figure(class = "coefficient-figure",
            tags$figcaption(
              tags$h3("Adjusted cGPA estimates"),
              tags$p(class = "section-description", "Dots mark estimates. Lines show 95% confidence intervals.")),
            plotOutput("coefficient_plot", height = "365px"))),
        plain_note("Each row asks: if two students were alike in everything else in the model, how different would we expect their cGPAs to be if one had a bit more of this habit? A dot near the dashed zero line, with a line crossing it, means no clear difference."),
        tags$p(class = "plot-note", "Each coefficient estimates a cGPA difference while holding the other predictors fixed. Domestic is the reference group when both statuses are selected. Confidence intervals and p-values use conventional OLS t inference."),
        tags$p(class = "plot-note", "The intercept is the fitted cGPA at the reference status and zero for every numeric predictor. Zero study time lies outside the observed range.")),
      tags$section(class = "chart-card",
        chart_header("Interpretation of model assumptions",
                     "The EDA raised concerns about linearity, equal residual variance, and mild departures from normality. Use the plots to examine these patterns in the selected data.",
          select_control("diagnostic", "Diagnostic plot", choices = c(
            "Linearity: residuals vs fitted" = "residuals",
            "Constant variance: spread vs fitted" = "spread",
            "Normality: normal Q-Q plot" = "qq"), width = "100%")),
        tags$div(class = "plot-panel", plotOutput("diagnostic_plot", height = "340px"))),
      uiOutput("diagnostic_reading"),
      tags$div(class = "assumption-notes",
        tags$h4("Independence and sampling"),
        plain_note("These are the students who chose to answer our survey, not a random sample of the class, so the results describe them and shouldn't be stretched to all students."),
        tags$p("Independence cannot be established from a diagnostic plot. Participation was voluntary and included mutual survey completion with other groups. These responses may not represent all STA304H5 students, and the results should not be generalized to all university students."))
    )
  ),
  nav_panel("Study notes", value = "About",
    tags$main(class = "site-shell page-content about-grid",
      tags$section(class = "about-block", tags$h2("Our questions"),
        tags$p("We went into this survey wanting to answer two things:"),
        tags$ol(tags$li("Is average daily social media use associated with self-reported cumulative GPA?"),
                tags$li("Do domestic and international students differ in cGPA after accounting for social media use and daily study time?"))),
      tags$section(class = "about-block", tags$h2("Who answered?"),
        tags$p("Our target population was students enrolled in STA304H5 at the University of Toronto Mississauga in Fall 2026. We shared a voluntary Google Forms questionnaire on the course discussion board, and we also traded survey completions with other groups (we filled out theirs, they filled out ours)."),
        tags$p("One note: because students chose whether to answer, the people in our sample aren't a random draw from the class.")),
      tags$section(class = "about-block", tags$h2("From 72 responses to 64"),
        tags$p("We received 72 responses and removed 8 of them before any analysis. These are the exact same 8 records we excluded in our EDA, so the app and the EDA work from identical data. The app doesn't remove anything extra."),
        tags$p("We flagged 6 of those records when we first went through the raw responses, then caught 2 more while writing the EDA. Seven reported daily social media use that we judged wasn't realistic, and one reported a cGPA of 0."),
        tags$p("One note on that last one: a cGPA of 0 is technically allowed on our questionnaire's 0-4 scale. Removing it was a judgment call we made for our data, not a cleaning rule we'd apply everywhere."),
        tags$p("That leaves 64 students: 42 domestic and 22 international. We swapped each survey alias for a plain response number to keep answers anonymous, and we kept our original raw data file untouched.")),
      tags$section(class = "about-block", tags$h2("What the measures mean"),
        tags$dl(class = "definitions",
          tags$dt("cGPA"), tags$dd("Self-reported cumulative GPA on a 0-4 scale. We used answers exactly as students gave them and didn't round them again."),
          tags$dt("Total social media use"), tags$dd("The hours a student reported for short-form video, text-based platforms and calls/messaging, added together. These activities can overlap (someone might message while watching videos), so this is a sum of reported hours rather than verified screen time."),
          tags$dt("Time off before bed"), tags$dd("How many minutes a student says they stop using social media before going to sleep."),
          tags$dt("Satisfaction"), tags$dd("How satisfied a student is with their cGPA, from 1 (worst) to 10 (best)."))),
      tags$section(class = "about-block", tags$h2("What can we conclude?"),
        tags$p(class = "plain-lead", tags$span(class = "plain-tag", "In plain terms"),
          "In our survey, students who used more social media didn't have noticeably different GPAs than students who used less. That doesn't prove social media has no effect on grades. Our sample may just be too small to pick one up."),
        tags$p("In the full cleaned sample, our EDA found almost no linear correlation between total reported social media use and cGPA. Our regression also gave insufficient evidence for the associations we proposed. This doesn't establish that social media has no relationship with academic performance."),
        tags$p("Some limits we want to be upfront about: the sample is small and voluntary. GPA, time use and satisfaction are all self-reported, so there's room for rounding and memory error. Our two groups are different sizes, we didn't measure every factor that could matter, and this is an observational study, so it can show patterns but not causes. Filtering down to a smaller group describes just those students and can make the estimates less stable.")),
      tags$section(class = "about-block", tags$h2("How to use this app"),
        tags$p("Every results box starts with a short plain-language summary. If you want the statistics behind it (correlations, confidence intervals, p-values), they're right underneath."),
        tags$p("Use the sample filters above the charts to pick student status and study time. In Explore, choose Association, Group comparison, Distributions or Responses. Open Plot styling to change colours, and use Axis limits to zoom in."),
        tags$p("Regression uses the same sample filters. Searching the table only affects the table, and axis limits only change what's visible in the plot."))
    )
  ),
  footer = tags$footer(class = "site-shell site-footer",
    tags$span("Group 18 · University of Toronto Mississauga · Fall 2026"),
    tags$span("Voluntary survey · Self-reported data"))
)

server <- function(input, output, session) {
  filtered <- reactive({
    filter_survey(survey, input$status_filter %||% character(), input$study_filter %||% study_limits)
  })
  scatter_colours <- reactive({
    values <- if (isTRUE(input$scatter_multicolour)) palette_colours(input$scatter_palette %||% "rainbow", 2) else
      rep(plot_colour(input$scatter_colour, default_colours$points), 2)
    stats::setNames(values, c("Domestic", "International"))
  })
  comparison_colours <- reactive({
    values <- if (isTRUE(input$group_multicolour)) palette_colours(input$group_palette %||% "rainbow", 2) else
      rep(plot_colour(input$group_colour, default_colours$groups), 2)
    stats::setNames(values, c("Domestic", "International"))
  })
  analysis_colour <- reactive(plot_colour(input$analysis_colour, default_colours$analysis))
  correlation <- reactive(pearson_result(filtered(), input$x_variable %||% "total_social_hours"))
  model_result <- reactive(fit_survey_model(filtered(), input$model_type %||% "eda"))
  model_coefficients <- reactive(coefficient_table(model_result()))

  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "status_filter", selected = c("Domestic", "International"))
    updateSliderInput(session, "study_filter", value = study_limits)
  })

  output$selection_summary <- renderUI({
    d <- filtered()
    groups <- levels(droplevels(d$status))
    name <- if (length(groups) == 2) "All student statuses" else if (length(groups)) groups else "No students selected"
    tags$div(class = "selection-summary",
      tags$span(tags$strong(paste(nrow(d), "of", nrow(survey), "responses")), " · ", name),
      if (nrow(d) < 10) tags$span(class = "selection-caution", "Small selection: estimates may be unavailable or imprecise."))
  })
  output$headline_metrics <- renderUI({
    d <- filtered()
    n <- nrow(d)
    tags$div(class = "metrics-grid",
      metric("Responses selected", as.character(n), sprintf("%d domestic · %d international", sum(d$status == "Domestic"), sum(d$status == "International"))),
      metric("Mean cGPA", if (n) number_text(mean(d$cgpa)) else "—", "On a 0–4 scale"),
      metric("Median social media use", if (n) paste0(number_text(median(d$total_social_hours), 1), " h") else "—", "Per day"),
      metric("Mean study time", if (n) paste0(number_text(mean(d$study_hours), 1), " h") else "—", "Per day"))
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
      p <- p + geom_smooth(method = "lm", formula = y ~ x, colour = "#284A3D", fill = "#9BB5A5", linewidth = .8, alpha = .23)
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
    more <- if (v == "satisfaction") "higher" else "more"
    pattern <- if (abs(r$r) < .1) "didn't have noticeably different cGPAs from students who reported less" else paste(
      "tended to have", if (abs(r$r) < .3) "slightly" else if (abs(r$r) < .5) "somewhat" else "clearly",
      if (r$r > 0) "higher" else "lower", "cGPAs")
    chance <- if (r$p >= .05) "A pattern this small could easily show up by chance in a sample this size, so we can't call it a real link." else
      "A pattern this strong is unlikely to be just chance, but it doesn't mean one causes the other."
    reading_box("Reading the relationship",
      plain_lead(sprintf("Students who reported %s %s %s. %s", more, short_labels[v], pattern, chance)),
      detail_label(),
      tags$p(sprintf("Among these %d responses, %s and cGPA show %s (Pearson r = %.3f).", r$n, short_labels[v], trend, r$r)),
      tags$p(class = "stat-line", sprintf("95%% confidence interval for r: [%.3f, %.3f] · p-value: %s", r$ci[1], r$ci[2], p_text(r$p))),
      tags$p(class = "stat-line", "Pearson r runs from -1 to 1. Values near 0 mean no straight-line pattern; values near -1 or 1 mean the points fall close to a line."),
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
    dom <- s$Mean[s$Status == "Domestic"]
    intl <- s$Mean[s$Status == "International"]
    gap <- intl - dom
    unit <- unit_labels[[v]]
    spread <- stats::sd(d[[v]])
    size <- if (is.finite(spread) && spread > 0) abs(gap) / spread else 0
    lead <- if (size < .2) {
      sprintf("Domestic and international students reported about the same %s on average (%.2f vs %.2f %s, a gap of only %.2f).",
              short_labels[v], dom, intl, unit, abs(gap))
    } else {
      sprintf("International students reported %s %s %s than domestic students on average (%.2f vs %.2f %s, a gap of %.2f).",
              if (size < .5) "somewhat" else "noticeably", if (gap > 0) "higher" else "lower", short_labels[v], intl, dom, unit, abs(gap))
    }
    reading_box("Reading the comparison",
      plain_lead(lead, " With groups this small, a gap like this could shift if a few different students had answered."),
      detail_label(),
      tags$p(sprintf("For %s, the mean is %.2f among domestic students and %.2f among international students. The international-minus-domestic difference is %.2f %s.", short_labels[v], dom, intl, gap, unit)),
      tags$p(class = "stat-line", sprintf("Standardized difference: %.2f standard deviations (under 0.2 is usually treated as negligible, 0.5 or more as moderate).", if (spread > 0) gap / spread else 0)),
      tags$p("This comparison describes the groups' responses. Use Regression to compare cGPA while accounting for study time and social media use."))
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
      p <- p + geom_histogram(breaks = breaks, fill = plot_colour(input$dist_colour, default_colours$bars),
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
    digits <- if (v == "cgpa") 2 else if (v == "cooldown_minutes") 0 else 1
    fmt <- function(z) formatC(z, format = "f", digits = digits)
    skew <- if (length(x) > 1 && stats::sd(x) > 0) (mean(x) - median(x)) / stats::sd(x) else 0
    skew_text <- if (skew > .15) " A few unusually high answers pull the average up above that typical value." else
      if (skew < -.15) " A few unusually low answers pull the average down below that typical value." else ""
    reading_box("Reading the distribution",
      plain_lead(sprintf("The typical student here reported %s of about %s %s, and half of the students fell between %s and %s.%s",
        short_labels[v], fmt(median(x)), unit_labels[[v]], fmt(quantile(x, .25, type = 6)), fmt(quantile(x, .75, type = 6)), skew_text)),
      detail_label(),
      tags$p(sprintf("Across %d responses, %s has a median of %.2f and a mean of %.2f. The middle 50%% spans %.2f to %.2f; observed values range from %.2f to %.2f, in the units on the horizontal axis.",
        length(x), short_labels[v], median(x), mean(x), quantile(x, .25, type = 6), quantile(x, .75, type = 6), min(x), max(x))),
      tags$p(if (v == "satisfaction") "Each bar counts responses at one satisfaction rating." else "Each bar counts responses in an interval. Changing the bins changes the shape of the display, while the summary statistics stay the same."))
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
      metric("Model responses", m$n, "Current sample selection"),
      metric("R-squared", number_text(m$summary$r.squared, 3), "Proportion of cGPA variation explained"),
      metric("Adjusted R-squared", number_text(m$summary$adj.r.squared, 3),
             if (is.finite(m$summary$adj.r.squared) && m$summary$adj.r.squared < 0)
               "Accounts for the number of predictors. Below zero means the model does no better than just guessing the average cGPA"
             else "Accounts for the number of predictors"),
      metric("Overall model p-value", p_text(m$p),
             paste("F-test against an intercept-only model.",
                   if (is.finite(m$p) && m$p < .05) "Below 0.05: the model predicts better than the average alone"
                   else "0.05 or above: no clear sign the model beats the average alone")))
  })
  output$model_reading <- renderUI({
    m <- model_result()
    if (!m$available) return(reading_box("Model unavailable for this selection", tags$p(m$message)))
    rows <- m$coefficients[m$coefficients$term != "(Intercept)", , drop = FALSE]
    status <- rows[rows$term == "statusInternational", , drop = FALSE]
    factors <- plain_term(rows$label)
    clear <- factors[rows$p < .05]
    r2 <- m$summary$r.squared
    link_text <- if (!length(clear)) {
      sprintf("Once everything else in the model is taken into account, none of %s shows a clear link with cGPA.", word_list(factors))
    } else {
      sprintf("Once everything else in the model is taken into account, %s %s a clear link with cGPA, while the rest don't.",
              word_list(clear), if (length(clear) == 1) "shows" else "show")
    }
    fit_text <- sprintf("Together, these factors account for %sabout %s of the differences in students' cGPAs.",
                        if (r2 < .2) "only " else "", percent_text(r2))
    status_text <- if (nrow(status)) {
      crosses <- status$lower < 0 && status$upper > 0
      sprintf(" Comparing students with the same habits, international students' predicted cGPA is %.2f grade points %s than domestic students'%s",
              abs(status$estimate), if (status$estimate < 0) "lower" else "higher",
              if (crosses) sprintf(", but the plausible range (%.2f to %.2f) includes zero, so we can't tell which group actually does better.", status$lower, status$upper)
              else sprintf(", and the plausible range (%.2f to %.2f) doesn't include zero.", status$lower, status$upper))
    } else ""
    reading_box("Reading the adjusted results",
      plain_lead(link_text, " ", fit_text, status_text),
      detail_label(),
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
      scale_y_discrete(labels = function(labels) vapply(labels, plot_label, character(1), width = 18)) +
      scale_x_continuous(n.breaks = 3) +
      labs(x = "cGPA difference", y = NULL) +
      chart_theme() +
      theme(axis.text.y = element_text(size = 10, lineheight = .95),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_text(size = 11),
            plot.margin = margin(8, 10, 8, 6))
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
    m <- model_result()
    finding <- if (isTRUE(m$available)) diagnostic_finding(m$fit, diagnostic) else NULL
    reading_box(explanation[1],
      if (!is.null(finding)) plain_lead(finding$plain),
      if (!is.null(finding)) detail_label("How to read this plot"),
      tags$p(explanation[2]),
      if (!is.null(finding)) tags$p(class = "stat-line", finding$detail),
      tags$p(class = "control-note", "Diagnostics refresh with the selected model and filters. Small selections can produce unstable smooth curves."))
  })
}

shinyApp(ui, server)
