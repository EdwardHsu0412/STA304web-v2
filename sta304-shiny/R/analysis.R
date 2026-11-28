# Shared data preparation, summaries, and statistical calculations.
variable_labels <- c(
  cgpa = "cGPA (0-4)",
  total_social_hours = "Total social media use (hours/day)",
  study_hours = "Study time (hours/day)",
  short_video_hours = "Short-form video (hours/day)",
  text_hours = "Text-based platforms (hours/day)",
  communication_hours = "Calls and messaging (hours/day)",
  cooldown_minutes = "Time off social media before bed (minutes)",
  satisfaction = "Satisfaction with cGPA (1-10)"
)
short_labels <- c(
  cgpa = "cGPA", total_social_hours = "total daily social media use",
  study_hours = "daily study time", short_video_hours = "short-form video use",
  text_hours = "text-based platform use", communication_hours = "calls and messaging",
  cooldown_minutes = "time off social media before bed", satisfaction = "cGPA satisfaction"
)
variable_choices <- stats::setNames(names(variable_labels), variable_labels)
predictor_choices <- variable_choices[variable_choices != "cgpa"]
palette_choices <- c("Rainbow" = "rainbow", "Heat" = "heat", "Terrain" = "terrain", "Topo" = "topo")
palette_colours <- function(palette = "rainbow", n = 2L) {
  switch(palette, heat = grDevices::heat.colors(n), terrain = grDevices::terrain.colors(n),
         topo = grDevices::topo.colors(n), grDevices::rainbow(n))
}
plot_colour <- function(value, fallback) {
  if (length(value) != 1 || is.na(value) || !nzchar(value)) return(fallback)
  tryCatch({ grDevices::col2rgb(value); value }, error = function(e) fallback)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

load_survey <- function(path = file.path("data", "survey_clean.csv")) {
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("response_id", "status", "satisfaction", "cgpa", "study_hours",
                "short_video_hours", "text_hours", "communication_hours", "cooldown_minutes")
  stopifnot(all(required %in% names(d)), !anyNA(d), !anyDuplicated(d$response_id))
  numeric_columns <- setdiff(required, "status")
  stopifnot(all(vapply(d[numeric_columns], is.numeric, logical(1))))
  stopifnot(all(d$status %in% c("Domestic", "International")),
            all(d$cgpa >= 0 & d$cgpa <= 4),
            all(d$satisfaction >= 1 & d$satisfaction <= 10))
  d$status <- factor(d$status, levels = c("Domestic", "International"))
  d$total_social_hours <- d$short_video_hours + d$text_hours + d$communication_hours
  d
}

filter_survey <- function(d, statuses, study_range) {
  d[d$status %in% statuses & d$study_hours >= study_range[1] &
      d$study_hours <= study_range[2], , drop = FALSE]
}

number_text <- function(x, digits = 2) {
  if (!length(x) || !is.finite(x)) return("Unavailable")
  formatC(x, format = "f", digits = digits)
}
p_text <- function(p) {
  if (!length(p) || !is.finite(p)) return("Unavailable")
  if (p < .001) "< 0.001" else sprintf("%.3f", p)
}

pearson_result <- function(d, variable = "total_social_hours") {
  n <- nrow(d)
  if (n < 4 || length(unique(d[[variable]])) < 2 || length(unique(d$cgpa)) < 2) {
    return(list(available = FALSE, n = n,
                message = "A correlation needs at least four responses and variation in both variables."))
  }
  test <- stats::cor.test(d[[variable]], d$cgpa, method = "pearson", conf.level = .95)
  list(available = TRUE, n = n, r = unname(test$estimate), p = test$p.value,
       ci = unname(test$conf.int), test = test)
}

group_summary <- function(d, variable) {
  groups <- levels(droplevels(d$status))
  if (!length(groups)) return(data.frame())
  do.call(rbind, lapply(groups, function(g) {
    x <- d[d$status == g, variable]
    data.frame(Status = g, Responses = length(x), Mean = mean(x), Median = median(x),
               SD = if (length(x) > 1) sd(x) else NA_real_,
               IQR = stats::IQR(x, type = 6), check.names = FALSE)
  }))
}

quantitative_summary <- function(d) {
  if (!nrow(d)) return(data.frame())
  do.call(rbind, lapply(names(variable_labels), function(v) {
    x <- d[[v]]
    data.frame(Variable = unname(variable_labels[v]), Responses = length(x),
               Mean = mean(x), Median = median(x),
               SD = if (length(x) > 1) sd(x) else NA_real_,
               IQR = stats::IQR(x, type = 6), Minimum = min(x), Maximum = max(x),
               check.names = FALSE)
  }))
}

fit_survey_model <- function(d, specification = "eda") {
  predictors <- if (specification == "adjusted") {
    c("study_hours", "total_social_hours")
  } else {
    c("study_hours", "short_video_hours", "text_hours", "communication_hours", "cooldown_minutes")
  }
  notes <- character()
  if (nrow(d) < 8) {
    return(list(available = FALSE, message = "Select at least eight responses to fit a model."))
  }
  constant <- predictors[vapply(d[predictors], function(x) length(unique(x)) < 2, logical(1))]
  if (length(constant)) {
    predictors <- setdiff(predictors, constant)
    notes <- c(notes, paste("Constant predictors omitted:", paste(variable_labels[constant], collapse = ", ")))
  }
  d$status <- droplevels(d$status)
  if (nlevels(d$status) > 1) {
    predictors <- c(predictors, "status")
  } else {
    notes <- c(notes, "Only one student status is selected, so an international-versus-domestic difference cannot be estimated.")
  }
  if (length(unique(d$cgpa)) < 2 || nrow(d) <= length(predictors) + 2) {
    return(list(available = FALSE, message = "This selection has too little information to estimate the model reliably."))
  }
  fit <- stats::lm(stats::reformulate(predictors, response = "cgpa"), data = d)
  if (fit$rank < length(stats::coef(fit))) {
    return(list(available = FALSE, message = "Some predictors are redundant in this selection. Broaden the filters or choose the simpler model."))
  }
  s <- summary(fit)
  ci <- stats::confint(fit)
  coefficients <- data.frame(term = rownames(s$coefficients),
                             estimate = unname(s$coefficients[, 1]),
                             lower = unname(ci[, 1]), upper = unname(ci[, 2]),
                             p = unname(s$coefficients[, 4]), row.names = NULL)
  coefficient_labels <- c(
    "(Intercept)" = "Intercept",
    study_hours = "Study time (+1 hour/day)",
    total_social_hours = "Total social media (+1 hour/day)",
    short_video_hours = "Short-form video (+1 hour/day)",
    text_hours = "Text-based platforms (+1 hour/day)",
    communication_hours = "Calls and messaging (+1 hour/day)",
    cooldown_minutes = "Time off before bed (+1 minute)",
    statusInternational = "International vs domestic"
  )
  coefficients$label <- unname(coefficient_labels[coefficients$term])
  f <- s$fstatistic
  model_p <- if (is.null(f)) NA_real_ else stats::pf(f[1], f[2], f[3], lower.tail = FALSE)
  list(available = TRUE, fit = fit, summary = s, coefficients = coefficients,
       n = nrow(d), p = unname(model_p), notes = notes, specification = specification)
}

coefficient_table <- function(result) {
  if (!result$available) return(data.frame())
  c <- result$coefficients
  data.frame(Predictor = c$label,
             `Coefficient (cGPA)` = vapply(c$estimate, number_text, character(1), digits = 4),
             `95% confidence interval` = sprintf("[%.4f, %.4f]", c$lower, c$upper),
             `p-value` = vapply(c$p, p_text, character(1)), check.names = FALSE)
}

readable_responses <- function(d) {
  out <- d[c("response_id", "status", names(variable_labels))]
  names(out) <- c("Response", "Student status", unname(variable_labels))
  out
}

axis_range <- function(d, variable) {
  if (variable == "cgpa") return(c(0, 4))
  if (variable == "satisfaction") return(c(1, 10))
  c(0, max(1, ceiling(max(d[[variable]], na.rm = TRUE))))
}

safe_zoom <- function(value, fallback) {
  if (length(value) != 2 || any(!is.finite(value)) || value[1] >= value[2]) return(fallback)
  value
}
