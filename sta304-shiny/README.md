# Social Media & cGPA

An R Shiny app for STA304H5, Group 18: Edward, Angela, Sasha, Ali, and Prativa.

The app explores daily social media use, study habits, registration status, and self-reported cumulative GPA among students in STA304H5 at the University of Toronto Mississauga, Fall 2026.

## Local preview

Open the [local preview](http://127.0.0.1:3847/) while it is running.

To reopen it on this computer, double-click **Start Local Preview.cmd**. Keep its window open while using the app. The launcher uses the R packages prepared for this project when that local library is present.

In RStudio, open **STA304.Rproj**, then **app.R**, and click **Run App**. The app requires `shiny`, `bslib`, `ggplot2`, and `DT` in the R library used by RStudio. Package installation is separate from the app; the app never installs packages at startup.

## What visitors can explore

- **Explore:** a scatterplot of cGPA versus a selected measure, a student-status comparison with boxplots or violins, a histogram, and a searchable and sortable response table.
- **Analysis:** the EDA regression or a model adjusting student status for total social media use and study time. Results appear in a compact coefficient/95% CI/p-value table, an interval plot, and selectable diagnostic plots.
- **About:** research questions, sampling design, cleaning decisions, measure definitions, and limitations.

Student-status and study-time filters update all data outputs. Visitors can change variables, colours, point size, histogram bins, and axis limits. Axis limits change only the visible plot area; statistics use all responses selected by the filters. Searching the response table affects that table only, while the download contains the full sidebar-filtered selection.

## Data and exclusions

The supplied `dataset_forweb.xlsx` has 72 survey responses. At the user's explicit direction, this app carries forward the EDA's eight record exclusions, leaving **64 responses: 42 domestic and 22 international**. Six of those records had yellow ID cells in the workbook, and two were unmarked. No new exclusion rule was applied.

The original workbook is unchanged and is not bundled with the app. `data/survey_clean.csv` contains the retained observations with original survey aliases replaced by sequential response numbers. Only the first nine populated source columns were used; the empty tenth column was ignored. Supplied numeric values are preserved without further rounding. `total_social_hours` is calculated in R as short-form video hours + text-based platform hours + calls/messaging hours. These categories may overlap in real time.

The EDA describes its exclusions as seven implausible daily-time reports and one zero-cGPA response. The app follows that confirmed record list, rather than presenting a zero GPA as automatically invalid or attempting to reconstruct a new cleaning rule from the values.

### Recalculated full-sample results

All calculations use the supplied workbook after the agreed exclusions. Some values printed in the older EDA differ from this workbook, so those printed results are not hard-coded into the app.

| Measure | Recalculated value |
| --- | ---: |
| Responses | 64 |
| Mean cGPA | 3.0420 |
| Median total social media hours/day | 5.00 |
| Mean total social media hours/day | 5.6175 |
| Pearson r: total social media and cGPA | 0.004151 |
| Pearson 95% CI | [-0.241905, 0.249706] |
| Pearson p-value | 0.974029 |
| Original EDA specification, R-squared | 0.040686 |
| Original EDA specification, overall model p-value | 0.874085 |

There is insufficient evidence of a linear association in this sample. This is not evidence that no relationship exists. The small voluntary sample, self-reporting, possible model-assumption concerns, and observational design limit interpretation.

The EDA model is:

```r
cgpa ~ study_hours + short_video_hours + text_hours +
  communication_hours + cooldown_minutes + status
```

The research-question-focused model is:

```r
cgpa ~ study_hours + total_social_hours + status
```

Domestic is the reference status when both groups are selected. A single-status filter omits the status term and explains why the group difference is unavailable. Constant predictors are omitted with a note. Empty, very small, or rank-deficient selections receive explanatory messages. Confidence intervals and p-values are conventional OLS estimates. IQR uses R quantile type 6, matching the EDA's explicit group-summary convention.

## TA feedback incorporated

Regression coefficients are presented in a table with confidence intervals and p-values. Assumption explanations use proper headings and short labelled sections. Interpretations avoid claiming that the plots prove assumption violations. There is no package-installation paragraph or raw `report::report()` output in the site.

## Publishing after local review

This version is local only, as requested. A public URL has not been created or submitted.

The course requires Posit Connect Cloud and a public GitHub repository. After local review:

1. Put this app folder's contents in the intended GitHub repository (keep `app.R`, `R/`, `data/`, and `www/` together).
2. In the app directory, generate the deployment dependency file with `rsconnect::writeManifest(appFiles = c("app.R", "R/analysis.R", "data/survey_clean.csv", "www/style.css", "www/navigation.js"))` and commit it. Use an R version supported by Connect Cloud. The local preview was tested with R 4.6.1; at the time of preparation, the platform documentation lists support through R 4.6.0.
3. In Connect Cloud, choose **Publish → Shiny**, select the repository and branch, and choose **app.R** as the primary file.
4. Confirm that the deployed link opens in a private browser window, test all controls, and add the real public URL to this README before submission.

See [Posit's Shiny deployment guide](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html) and [supported R versions](https://docs.posit.co/connect-cloud/user/platform/r.html).

## Sources and design

- Group 18's supplied survey workbook.
- *STA304 Exploratory Data Analysis*, Group 18, October 25, 2026.
- *STA304 Shiny Website Instructions*, 2026, and the supplied TA feedback.
- The [course example](https://annahuynhly-sta304example.share.connect.posit.cloud/) informed the dark navigation bar, sidebar, and Explore/Analysis/About organization. All survey content, code, calculations, plots, interpretations, and controls here were written for this project.

Extra project-specific controls include study-time filtering, boxplot/violin selection, searchable responses and filtered downloads, two regression specifications, and selectable diagnostics.
