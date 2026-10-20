# ACSF key steps - Quarto report template

Stand-alone HTML report collating the key ACSF processing steps, reusing plots from the
ACSF Shiny apps (App_ts_weekly, Coding management, ACSF sales vis).

```
ACSF_report/
├── ACSF_steps.qmd            # the report
├── R/acsf_plot_helpers.R     # plot builders shared with the Shiny apps + demo data
└── data/                     # pipeline outputs (.rds) when demo: false
```

## Render

```bash
quarto render ACSF_steps.qmd                       # demo data
quarto render ACSF_steps.qmd -P demo:false         # real pipeline outputs in data/
quarto render ACSF_steps.qmd -P ref_period:2024-25 -P demo:false
```

Output is a single self-contained `ACSF_steps.html` (`embed-resources: true`).

## Wiring in the apps

Move each app's chart code out of `renderHighchart({ ... })` into a function in
`R/acsf_plot_helpers.R`, then have the app call it:

```r
# app server
output$ts_plot <- renderHighchart(plot_ts_weekly(ts_filtered(), series_sel = input$series))
```

The report calls the same function on the same data, so the two never drift apart.
Have the pipeline save the data each app reads as `.rds` in `data/`.
