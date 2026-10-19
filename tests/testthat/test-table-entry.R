test_that("count table: placeholders, multi-value fill, fix a cell, long version", {
  in_global(list(), {
    s <- scripted(c("2", "3", "2", "region", "north south east", "loyalty", "low high", "1",
                    "20", "30", "25 25 30 20", "4=27", "", "regions", ""),
                  sc_table())
    expect_length(s$left, 0)
    expect_true(any(grepl("\\[6\\]", s$out)))
    tab <- get("regions", envir = .GlobalEnv)
    expect_equal(tab$high, c(30, 27, 20))
    expect_equal(attr(tab, "statcram_kind"), "counts")
    long <- get("regions_long", envir = .GlobalEnv)
    expect_equal(nrow(long), sum(c(20, 30, 25, 27, 30, 20)))
    m <- as.matrix(tab[-1])
    expect_equal(q(chisq_indep(tab))$statistic, unname(chisq.test(m, correct = FALSE)$statistic))
    expect_true(any(grepl("regions <- data.frame", s$out)))
  })
})

test_that("class table from boundaries, with 'b' (back) and decimal commas", {
  in_global(list(), {
    s <- scripted(c("3", "6", "10 25 30 35 40 45 60", "2", "1", "6 12", "b", "12 24 28 18 12", "", "cls"), sc_table())
    cls <- get("cls", envir = .GlobalEnv)
    expect_equal(cls$class[1], "10-25")
    expect_equal(q(desc_classes(cls))$mean, 36.6)
    s2 <- scripted(c("1", "3", "2", "before after", "2", "5,5 6 7", "8 9 10", "", "d1"), sc_table())
    d1 <- get("d1", envir = .GlobalEnv)
    expect_equal(d1$before, c(5.5, 6, 7))
    expect_equal(q(test_paired(d1$after, d1$before))$estimate, mean(c(8, 9, 10) - c(5.5, 6, 7)))
  })
})

test_that("frequency list, cancel, and edit of an existing table", {
  in_global(list(), {
    s0 <- scripted(c("4", "3", "q"), sc_table())
    expect_null(s0$res)
    expect_true(any(grepl("cancelled", s0$out)))
    s <- scripted(c("4", "3", "channel", "web shop phone", "1", "151 117 140", "", "ch", "n"), sc_table())
    ch <- get("ch", envir = .GlobalEnv)
    expect_equal(q(chisq_gof(ch))$statistic, unname(chisq.test(c(151, 117, 140))$statistic))
    expect_equal(q(desc_prop(ch, event = "web"))$estimate, 151 / 408)
    s3 <- scripted(c("2=120", "", "", "n"), sc_table(edit = ch))
    expect_equal(get("ch", envir = .GlobalEnv)$count, c(151, 120, 140))
  })
})

test_that("frequency list of a count variable: proportions + n, variable name, numeric long table", {
  in_global(list(), {
    # 220 customers, relative frequencies of the number of products (exam-style table)
    scripted(c("4", "5", "products", "1 2 4 5 8", "2", "0.3 0.25 0.25 0.15 0.05", "", "pp"), sc_table())
    pp <- get("pp", envir = .GlobalEnv)
    out <- capture.output(desc_freq(pp, n = 220))
    expect_true(any(grepl("Variable: products   n = 220", out, fixed = TRUE)))
    expect_true(any(grepl("quantitative discrete", out, fixed = TRUE)))
    expect_true(any(grepl("with n = 220 the counts are f_k = p_k x n", out, fixed = TRUE)))
    expect_true(any(grepl("= 2.95", out, fixed = TRUE)))
    expect_true(any(grepl("n = unknown", capture.output(desc_freq(pp)), fixed = TRUE)))
    expect_true(any(grepl("not a whole number", capture.output(desc_freq(pp, n = 221)), fixed = TRUE)))
    expect_error(desc_freq(pp, n = 2.5), "positive whole number")
    # counts and the one-row-per-unit table: numeric labels stay numeric
    s <- scripted(c("4", "5", "products", "1 2 4 5 8", "1", "66 55 55 33 11", "", "pc", "y"), sc_table())
    pl <- get("pc_long", envir = .GlobalEnv)
    expect_true(is.numeric(pl$products))
    expect_equal(as.vector(table(pl$products)), c(66, 55, 55, 33, 11))
    expect_true(any(grepl("quantitative discrete", capture.output(desc_freq(pl$products)), fixed = TRUE)))
    expect_true(any(grepl("Variable: products   n = 220", capture.output(desc_freq(get("pc", envir = .GlobalEnv))), fixed = TRUE)))
    # text labels still become a factor in the order typed
    scripted(c("4", "3", "size", "small medium large", "1", "5 6 7", "", "sz", "y"), sc_table())
    expect_equal(levels(get("sz_long", envir = .GlobalEnv)$size), c("small", "medium", "large"))
  })
})
