options(statcram.plot = FALSE)

# Run code with scripted answers to the menu / wizard prompts; output is captured.
scripted <- function(inputs, code) {
  old <- options(statcram.input = inputs)
  on.exit(options(old))
  out <- utils::capture.output(res <- force(code))
  list(out = out, res = res, left = getOption("statcram.input"))
}

# Put objects in the global environment (where the menu looks) for one test.
# Objects created during the test (tables, models, vectors) are removed afterwards.
in_global <- function(objs, code) {
  before <- ls(.GlobalEnv)
  for (n in names(objs)) assign(n, objs[[n]], envir = .GlobalEnv)
  on.exit(rm(list = union(names(objs), setdiff(ls(.GlobalEnv), before)), envir = .GlobalEnv))
  force(code)
}

q <- function(expr) { utils::capture.output(r <- expr); r }
