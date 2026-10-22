#===================================================================================================
# acsf_app_loader.R
# Get the data each ACSF Shiny app builds at startup, WITHOUT launching the app.
#
# The idea: an app file is just a list of top-level expressions - library() calls, source()
# calls, data loading, ui <- ..., server <- function(...), and finally shinyApp(ui, server).
# Only that last one starts the app. So we parse the file, drop the launch call, and run
# everything else. What's left behind is exactly the data the app would have had.
#
# This means the report reads the SAME files, through the SAME code, as the app - no
# duplicated loading logic and no .rds caches to go stale.
#
# What it can't get you: anything computed inside server() (reactives, renderHighchart).
# Those only exist while the app runs. That's what dump_render_code() below is for.
#===================================================================================================

# Calls that start an app. Anything else at top level gets run.
LAUNCH_FNS <- c("shinyApp", "runApp", "shinyAppDir", "shinyAppFile")

is_launch_call <- function(e) {
  if (!is.call(e)) return(FALSE)
  fn <- sub("^shiny:::?", "", deparse(e[[1]])[1])
  if (fn %in% LAUNCH_FNS) return(TRUE)
  # app <- shinyApp(...)
  if (fn %in% c("<-", "=") && length(e) == 3) return(is_launch_call(e[[3]]))
  FALSE
}

# Run an app file minus its launch call. Returns an environment holding everything it made.
#
# Runs with the working directory set to the app's folder, because the apps use relative
# paths (e.g. source("ACSF_source.R")). Note some apps source() helpers with local = FALSE,
# which puts objects in the global environment instead; the environment returned here has
# the global environment as its parent, so get()/exists() on it still finds them.
load_app_env <- function(app_file) {
  stopifnot(file.exists(app_file))
  exprs <- parse(app_file, keep.source = FALSE)
  keep  <- !vapply(exprs, is_launch_call, logical(1))

  env <- new.env(parent = globalenv())
  old <- setwd(dirname(app_file))
  on.exit(setwd(old), add = TRUE)

  global_before <- ls(globalenv())
  for (e in exprs[keep]) eval(e, env)

  # Remember what landed in global too, so app_inventory() can list it
  attr(env, "global_new") <- setdiff(ls(globalenv()), global_before)
  attr(env, "app_file")   <- app_file
  env
}

# Same, but one broken app doesn't take the whole report down with it.
safe_load_app <- function(app_file) {
  tryCatch(load_app_env(app_file),
           error = function(e) structure(list(), class = "app_load_error",
                                         app_file = app_file, msg = conditionMessage(e)))
}

# What data did the app leave behind? One row per data frame / list.
app_inventory <- function(env) {
  if (inherits(env, "app_load_error"))
    return(tibble(object = "LOAD FAILED", class = attr(env, "msg"),
                  rows = NA_integer_, cols = NA_integer_))

  nms <- unique(c(ls(env), attr(env, "global_new")))
  out <- map(nms, \(n) {
    x <- get(n, envir = env)
    if (!is.data.frame(x) && !is.list(x)) return(NULL)
    tibble(object = n,
           class  = class(x)[1],
           rows   = if (is.data.frame(x)) nrow(x) else length(x),
           cols   = if (is.data.frame(x)) ncol(x) else NA_integer_)
  }) %>%
    list_rbind()

  if (is.null(out)) return(tibble(object = "(no data objects found)", class = NA_character_,
                                  rows = NA_integer_, cols = NA_integer_))

  # shiny's ui object is a list too - not data
  out %>% filter(!class %in% c("shiny.tag", "shiny.tag.list", "bslib_page"))
}

#----------------------------------------------------------------------
# Pull every renderHighchart / renderDT / renderPlot block out of an app, as written
# (comments included), so the chart code can be moved into plot functions.
# Writes to a text file because the ACSF app alone is 3,000+ lines.
#----------------------------------------------------------------------
RENDER_FNS <- c("renderHighchart", "renderHighchart2", "renderDT", "renderDataTable",
                "renderPlot", "renderPlotly")

dump_render_code <- function(app_file, out_file = NULL) {
  pd   <- getParseData(parse(app_file, keep.source = TRUE), includeText = TRUE)
  hits <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% RENDER_FNS, ]

  # SYMBOL_FUNCTION_CALL -> its expr -> the call -> (usually) output$x <- call
  up      <- function(ids) pd$parent[match(ids, pd$id)]
  call_id <- up(up(hits$id))
  asg_id  <- up(call_id)
  asg_txt <- pd$text[match(asg_id, pd$id)]
  use_id  <- ifelse(grepl("^output\\$", asg_txt), asg_id, call_id)

  blocks <- vapply(use_id, \(i) getParseText(pd, i), character(1))
  lines  <- pd$line1[match(use_id, pd$id)]
  txt    <- paste0("# ---- ", basename(app_file), " line ", lines, " ----\n", blocks,
                   collapse = "\n\n")

  if (is.null(out_file)) cat(txt, "\n") else writeLines(txt, out_file)
  invisible(length(blocks))
}
