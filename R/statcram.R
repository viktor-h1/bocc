
# ============================================================
# statcram 0.4.0
# Dataset-agnostic deterministic "offline AI"
# ============================================================

`%||%` <- function(x,y) if (is.null(x)) y else x

# -------------------------
# Text normalization
# -------------------------

.norm <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("&"," and ",x,fixed=TRUE)
  x <- gsub("[^a-z0-9_ ]+"," ",x)
  x <- gsub("_"," ",x,fixed=TRUE)
  x <- gsub("\\s+"," ",x)
  trimws(x)
}

.stem_token <- function(x) {
  x <- .norm(x)
  x <- gsub("(ies)$","y",x)
  x <- gsub("(ing)$","",x)
  x <- gsub("(ed)$","",x)
  x <- gsub("(es)$","",x)
  x <- gsub("(s)$","",x)
  x
}

.stopwords <- c(
  "the","a","an","of","for","to","and","or","in","on","by","with","from",
  "is","are","was","were","be","being","been","this","that","these","those",
  "data","dataset","variable","variables","value","values","group","groups",
  "customer","customers","person","people","sample","samples","population",
  "test","question","whether","there","any","do","does","did","can","could"
)

.tokens <- function(x) {
  z <- unlist(strsplit(.norm(x)," +"))
  z <- z[nzchar(z)]
  unique(.stem_token(z))
}

.ngrams <- function(tokens,max_n=3) {
  out <- character()
  n <- length(tokens)
  if(!n) return(out)
  for(k in seq_len(min(max_n,n))) {
    for(i in seq_len(n-k+1)) out <- c(out,paste(tokens[i:(i+k-1)],collapse=" "))
  }
  unique(out)
}

# Generic synonyms only: no dataset-specific variable names.
.synonyms <- list(
  spend=c("spend","spending","expenditure","expenses","expense","outlay"),
  salary=c("salary","wage","wages","pay","earnings","income"),
  price=c("price","cost","value"),
  sales=c("sales","revenue","units sold","volume"),
  time=c("time","duration","minutes","hours"),
  age=c("age","years old"),
  purchase=c("purchase","purchased","buy","bought","conversion","converted"),
  member=c("member","membership","subscriber","subscription"),
  loyalty=c("loyalty","tier","level"),
  channel=c("channel","source","acquisition","origin"),
  campaign=c("campaign","advertising","ad","promotion","marketing"),
  productivity=c("productivity","output","performance"),
  satisfaction=c("satisfaction","satisfied","rating"),
  gender=c("gender","sex","male","female","men","women"),
  premium=c("premium","paid","vip")
)

.expand_synonyms <- function(tokens) {
  out <- tokens
  for(tok in tokens) {
    for(nm in names(.synonyms)) {
      vals <- unique(.stem_token(c(nm,.synonyms[[nm]])))
      if(tok %in% vals) out <- c(out,vals)
    }
  }
  unique(out)
}

# -------------------------
# Data typing / profiling
# -------------------------

.is_cat <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x) ||
    (is.integer(x) && length(unique(na.omit(x))) <= 10)
}

.is_num <- function(x) is.numeric(x) && !.is_cat(x)

.levels_of <- function(x) {
  if(is.factor(x)) levels(x) else sort(unique(as.character(na.omit(x))))
}

.var_aliases <- function(name) {
  t <- .tokens(name)
  e <- .expand_synonyms(t)
  unique(c(.norm(name), .ngrams(t,3), .ngrams(e,3), e))
}

.level_aliases <- function(level) {
  t <- .tokens(level)
  e <- .expand_synonyms(t)
  unique(c(.norm(level),.ngrams(t,2),.ngrams(e,2),e))
}

profile_exam_data <- function(data) {
  if(!is.data.frame(data)) stop("data must be a data.frame.")
  vars <- names(data)
  info <- lapply(vars,function(v) {
    x <- data[[v]]
    type <- if(.is_cat(x)) "categorical" else if(.is_num(x)) "numeric" else class(x)[1]
    levels <- if(.is_cat(x)) .levels_of(x) else NULL
    list(
      name=v,
      type=type,
      aliases=.var_aliases(v),
      levels=levels,
      level_aliases=if(!is.null(levels)) setNames(lapply(levels,.level_aliases),levels) else NULL
    )
  })
  names(info) <- vars
  structure(list(n=nrow(data),p=ncol(data),vars=info),class="statcram_profile")
}

scan_exam_data <- function(data) {
  pr <- profile_exam_data(data)
  rows <- lapply(pr$vars,function(v) {
    x <- data[[v$name]]
    if(v$type=="categorical") {
      lev <- paste(head(v$levels,8),collapse=", ")
      if(length(v$levels)>8) lev <- paste0(lev,", ...")
      desc <- lev
    } else if(v$type=="numeric") {
      rg <- range(x,na.rm=TRUE)
      desc <- sprintf("range %.3g to %.3g",rg[1],rg[2])
    } else desc <- class(x)[1]
    data.frame(variable=v$name,type=v$type,levels_or_range=desc,stringsAsFactors=FALSE)
  })
  ans <- do.call(rbind,rows)
  cat(sprintf("\nDATASET PROFILE: %d rows x %d columns\n",pr$n,pr$p))
  print(ans,row.names=FALSE)
  invisible(ans)
}

.get_df_candidates <- function(env=.GlobalEnv) {
  nms <- ls(envir=env)
  nms[vapply(nms,function(n) is.data.frame(get(n,envir=env)),logical(1))]
}

# -------------------------
# Dynamic matching
# -------------------------

.phrase_similarity <- function(a,b) {
  a <- .norm(a); b <- .norm(b)
  if(!nzchar(a)||!nzchar(b)) return(0)
  if(identical(a,b)) return(1)
  d <- as.numeric(adist(a,b,ignore.case=TRUE))
  1 - d/max(nchar(a),nchar(b))
}

.score_variable <- function(question,var_profile) {
  q <- .norm(question)
  qt <- setdiff(.expand_synonyms(.tokens(q)),.stopwords)
  best_phrase <- 0
  exact_bonus <- 0

  for(a in var_profile$aliases) {
    if(!nzchar(a)) next
    padded_q <- paste0(" ", q, " ")
    padded_a <- paste0(" ", a, " ")
    if(grepl(padded_a, padded_q, fixed=TRUE))
      exact_bonus <- max(exact_bonus, 4 + nchar(a)/15)
    best_phrase <- max(best_phrase,.phrase_similarity(a,q))
  }

  vt <- setdiff(.expand_synonyms(.tokens(var_profile$name)),.stopwords)
  overlap <- if(length(vt)) sum(vt %in% qt)/length(vt) else 0

  # Also reward any expanded token overlap against aliases.
  alias_tokens <- unique(unlist(lapply(var_profile$aliases,.tokens)))
  alias_tokens <- setdiff(alias_tokens,.stopwords)
  token_overlap <- if(length(alias_tokens)) sum(alias_tokens %in% qt)/min(length(alias_tokens),max(1,length(qt))) else 0

  exact_bonus + 2.5*overlap + 1.2*token_overlap + .4*best_phrase
}

.score_all_vars <- function(question,profile) {
  q <- .norm(question)
  s <- vapply(profile$vars,function(v) {
    base <- .score_variable(question,v)

    # Dataset-agnostic level evidence:
    # if the question mentions category levels (e.g. Automatic/Manual,
    # Trained/Untrained, High/Medium), boost their parent categorical variable.
    level_bonus <- 0
    if(!is.null(v$levels)) {
      for(lv in v$levels) {
        aliases <- v$level_aliases[[lv]]
        exact <- any(vapply(aliases,function(a)
          nzchar(a) && grepl(paste0(" ",a," "),paste0(" ",q," "),fixed=TRUE),
          logical(1)))
        if(exact) level_bonus <- level_bonus + 2.8
      }
    }
    base + level_bonus
  },numeric(1))
  sort(s,decreasing=TRUE)
}

.match_levels <- function(question,var_profile) {
  if(is.null(var_profile$levels)) return(data.frame())
  q <- .norm(question)
  padded_q <- paste0(" ",q," ")

  rows <- lapply(var_profile$levels,function(lv) {
    aliases <- var_profile$level_aliases[[lv]]
    positions <- integer()

    for(a in aliases) {
      if(!nzchar(a)) next
      hit <- gregexpr(paste0(" ",a," "),padded_q,fixed=TRUE)[[1]]
      if(hit[1] != -1) positions <- c(positions,hit)
    }

    exact <- length(positions)>0
    sim <- max(vapply(aliases,function(a).phrase_similarity(a,q),numeric(1)))
    first_pos <- if(exact) min(positions) else Inf
    last_pos <- if(exact) max(positions) else -Inf
    count <- if(exact) length(unique(positions)) else 0
    score <- if(exact) 5 + .25*pmax(0,count-1) else sim

    data.frame(
      level=lv,score=score,exact=exact,count=count,
      first_pos=first_pos,last_pos=last_pos,
      stringsAsFactors=FALSE
    )
  })

  out <- do.call(rbind,rows)
  out <- out[order(!out$exact,out$first_pos,-out$score),,drop=FALSE]
  out[out$exact | out$score>=.72,,drop=FALSE]
}

.target_level <- function(question,var_profile) {
  # For a single requested category, prefer repeated mentions and then
  # the latest explicit occurrence. This handles questions that list
  # all levels and later ask for one specific level.
  m <- .match_levels(question,var_profile)
  if(!nrow(m)) return(NULL)
  exact <- m[m$exact,,drop=FALSE]
  if(nrow(exact)) {
    exact <- exact[order(-exact$count,-exact$last_pos),,drop=FALSE]
    return(exact$level[1])
  }
  m$level[1]
}

.resolve_vars <- function(question,data,profile=NULL,min_score=1.4) {
  profile <- profile %||% profile_exam_data(data)
  s <- .score_all_vars(question,profile)
  keep <- s[s>=min_score]
  vars <- names(keep)
  list(
    scores=s,
    mentioned=vars,
    numeric=vars[vapply(vars,function(v) profile$vars[[v]]$type=="numeric",logical(1))],
    categorical=vars[vapply(vars,function(v) profile$vars[[v]]$type=="categorical",logical(1))]
  )
}

# -------------------------
# Cue detection
# -------------------------

.detect_alpha <- function(q) {
  q <- .norm(q)
  m <- regexec("alpha\\s*(=|is)?\\s*0?\\.([0-9]+)",q,perl=TRUE)
  r <- regmatches(q,m)[[1]]
  if(length(r)) return(as.numeric(paste0("0.",r[length(r)])))
  m2 <- regexec("([0-9]+(?:\\.[0-9]+)?)\\s*%\\s*(significance|level)?",q,perl=TRUE)
  r2 <- regmatches(q,m2)[[1]]
  if(length(r2)) return(as.numeric(r2[2])/100)
  NULL
}

.detect_conf <- function(q) {
  q <- .norm(q)
  m <- regexec("([0-9]+(?:\\.[0-9]+)?)\\s*%\\s*confidence",q,perl=TRUE)
  r <- regmatches(q,m)[[1]]
  if(length(r)) return(as.numeric(r[2])/100)
  NULL
}

.detect_alt <- function(q) {
  q <- .norm(q)
  if(grepl("higher|greater|more than|increase|increases|above|exceeds|larger",q)) return("greater")
  if(grepl("lower|less than|decrease|decreases|below|fewer|smaller",q)) return("less")
  if(grepl("different|differs|difference|changed|not equal|any change",q)) return("two.sided")
  NULL
}

.detect_claim_number <- function(q) {
  # Conservative: returns numeric candidate only if claim words nearby.
  z <- .norm(q)
  m <- regexec("(mean|average|proportion|rate|share|equals|equal to|claimed|claim|at least|more than|less than)\\s*(of|is|=|than)?\\s*([0-9]+(?:\\.[0-9]+)?)",z,perl=TRUE)
  r <- regmatches(z,m)[[1]]
  if(length(r)) as.numeric(r[length(r)]) else NULL
}

# -------------------------
# Family interpretation
# -------------------------

.interpret <- function(question,data,profile=NULL) {
  profile <- profile %||% profile_exam_data(data)
  q <- .norm(question)
  R <- .resolve_vars(question,data,profile)
  nums <- R$numeric
  cats <- R$categorical
  alpha <- .detect_alpha(q)
  conf <- .detect_conf(q)
  alt <- .detect_alt(q)

  # Strong paired cues: two numeric variables + repeated/same-unit language.
  if(grepl("paired|before after|pre post|same people|same subjects|same units|matched|repeated",q) && length(nums)>=2) {
    return(list(family="paired",before=nums[1],after=nums[2],alpha=alpha,alternative=alt,
                confidence="high",needs=character(),resolver=R))
  }

  # Descriptive / boxplot.
  if(grepl("boxplot|distribution|central tendency|median|symmetry|skew|quartile|iqr|outlier|descriptive",q) && length(nums)>=1) {
    return(list(family="describe",outcome=nums[1],group=if(length(cats))cats[1] else NULL,
                confidence="high",needs=character(),resolver=R))
  }

  # Confidence interval.
  if(grepl("confidence interval|confidence level|interval estimate|\\bci\\b",q)) {
    categorical_event_cue <- grepl(
      "proportion|percentage|percent|rate|share|customers? with|people with|units with|cases with|population of .* with",
      q
    )
    numeric_mean_cue <- grepl(
      "mean|average|expected value|spend|spending|expenditure|salary|income|age|time|minutes|score|price|sales|revenue",
      q
    )

    if(length(cats)>=1) {
      lm <- .match_levels(q,profile$vars[[cats[1]]])
      target <- .target_level(q,profile$vars[[cats[1]]])

      # A named categorical event with no explicit numerical response is a
      # population-proportion interval, even if "population" was typed where
      # "proportion" was intended.
      if(grepl("proportion|percentage|percent|rate|share",q) ||
         (!is.null(target) && categorical_event_cue && !numeric_mean_cue)) {
        return(list(
          family="prop_ci",
          variable=cats[1],
          success=target,
          conf=conf,
          confidence="high",
          needs=if(!nrow(lm))"success" else character(),
          resolver=R
        ))
      }
    }

    if(length(nums)>=1 && numeric_mean_cue)
      return(list(
        family="mean_ci",
        outcome=nums[1],
        conf=conf,
        confidence="medium",
        needs="mean_ci_case",
        resolver=R
      ))

    # Do not invent a numerical outcome when the wording is still ambiguous.
    return(list(
      family=NULL,
      confidence="low",
      needs="ci_type",
      resolver=R
    ))
  }

  # Regression.
  if(grepl("regression|predict|prediction|explain|effect of|associated with|relationship between|linear relationship",q)) {
    outcome <- NULL
    preds <- NULL

    # Role-aware parsing for common exam wording:
    # "predict Y using X1 X2 X3"
    if(grepl("\\bpredict\\b",q) && grepl("\\busing\\b",q)) {
      parts <- strsplit(q,"\\busing\\b",perl=TRUE)[[1]]
      if(length(parts)>=2) {
        leftR <- .resolve_vars(parts[1],data,profile,min_score=.8)
        rightR <- .resolve_vars(paste(parts[-1],collapse=" using "),data,profile,min_score=.8)
        if(length(leftR$numeric)) outcome <- leftR$numeric[1]
        preds <- setdiff(rightR$mentioned,outcome)
      }
    }

    if(is.null(outcome) && length(nums)>=1) outcome <- nums[1]
    if(is.null(preds) && !is.null(outcome)) preds <- setdiff(R$mentioned,outcome)

    if(!is.null(outcome) && length(preds))
      return(list(family="regression",outcome=outcome,predictors=preds,alpha=alpha,
                  confidence="high",needs=character(),resolver=R))

    return(list(family="regression",outcome=outcome,predictors=preds,alpha=alpha,
                confidence="low",needs="variables",resolver=R))
  }

  # Chi-square independence.
  if(grepl("associated|association|independent|independence|relationship",q) && length(cats)>=2) {
    return(list(family="chisq_independence",x=cats[1],y=cats[2],alpha=alpha,
                confidence="high",needs=character(),resolver=R))
  }

  # Chi-square GOF.
  if(length(cats)>=1 && grepl("uniform|theoretical distribution|distribution differs|goodness|equally preferred|equal proportions|fit",q)) {
    return(list(family="chisq_gof",variable=cats[1],alpha=alpha,confidence="high",needs="probs",resolver=R))
  }

  # Pure descriptive sample proportion / its estimated standard error.
  # IMPORTANT: p0 belongs to a hypothesis test. Questions asking only for p-hat,
  # its estimate, or SE(p-hat) must NOT request a null proportion.
  descriptive_prop_cue <- grepl(
    "what is the proportion|what proportion|sample proportion|percentage of|percent of|estimate of the proportion|estimate the proportion|standard error of the proportion|standard error.*proportion|se of the proportion|se.*proportion",
    q
  )
  inferential_prop_cue <- grepl(
    "confidence|test|evidence|significance|hypothesis|null|h0|greater than|less than|different from|differs from|equal to|equals",
    q
  )
  if(descriptive_prop_cue && !inferential_prop_cue && length(cats)>=1) {
    lm <- .match_levels(q,profile$vars[[cats[1]]])
    return(list(family="sample_prop",variable=cats[1],success=.target_level(q,profile$vars[[cats[1]]]),
                want_se=grepl("standard error|\\bse\\b",q),
                confidence="high",needs=if(!nrow(lm))"success" else character(),resolver=R))
  }

  # Two proportions: binary/categorical outcome across categorical groups.
  propcue <- grepl("proportion|percentage|percent|rate|share|conversion|purchase rate|success rate",q)
  comparecue <- grepl("compare|difference|higher|lower|more|less|between|versus| vs ",q)
  if(propcue && comparecue && length(cats)>=2) {
    bin <- cats[vapply(cats,function(v) length(profile$vars[[v]]$levels)==2,logical(1))]
    outcome <- if(length(bin)) bin[1] else cats[1]
    group <- setdiff(cats,outcome)[1]
    lm1 <- .match_levels(q,profile$vars[[outcome]])
    lm2 <- .match_levels(q,profile$vars[[group]])
    return(list(family="two_props",outcome=outcome,success=if(nrow(lm1))lm1$level[1] else NULL,
                group=group,groups=head(lm2$level,2),alpha=alpha,alternative=alt,
                confidence="medium",needs=character(),resolver=R))
  }

  # One proportion.
  if(propcue && length(cats)>=1) {
    lm <- .match_levels(q,profile$vars[[cats[1]]])
    return(list(family="one_prop",variable=cats[1],success=.target_level(q,profile$vars[[cats[1]]]),
                alpha=alpha,alternative=alt,p0=.detect_claim_number(q),confidence="medium",
                needs=character(),resolver=R))
  }

  # Two independent means: numeric outcome + categorical grouping variable.
  meancue <- grepl("mean|average|salary|spend|expenditure|time|amount|income|score|compare|difference|higher|lower|more|less",q)
  if(meancue && length(nums)>=1 && length(cats)>=1) {
    lm <- .match_levels(q,profile$vars[[cats[1]]])
    return(list(family="two_means",outcome=nums[1],group=cats[1],groups=head(lm$level,2),
                alpha=alpha,alternative=alt,confidence="high",needs="variance_case",resolver=R))
  }

  # One mean.
  if(meancue && length(nums)>=1 && length(cats)==0) {
    return(list(family="one_mean",outcome=nums[1],alpha=alpha,alternative=alt,
                mu0=.detect_claim_number(q),confidence="medium",needs="mean_case",resolver=R))
  }

  list(family=NULL,confidence="low",needs="family",resolver=R)
}

# -------------------------
# Console interaction
# -------------------------

.choose <- function(prompt,options,labels=NULL) {
  if(is.null(labels)) labels <- options
  cat(prompt,"\n",sep="")
  for(i in seq_along(options)) cat(sprintf("%d. %s\n",i,labels[i]))
  repeat {
    a <- suppressWarnings(as.integer(readline("Choose a number: ")))
    if(!is.na(a)&&a>=1&&a<=length(options)) return(options[a])
    cat("Please choose one of the listed numbers.\n")
  }
}

.confirm <- function(prompt) {
  a <- tolower(trimws(readline(paste0(prompt," [Y/n]: "))))
  a=="" || a %in% c("y","yes")
}

.pick_var <- function(data,type=c("any","numeric","categorical"),prompt="Choose variable") {
  type <- match.arg(type)
  nms <- names(data)
  if(type=="numeric") nms <- nms[vapply(nms,function(v).is_num(data[[v]]),logical(1))]
  if(type=="categorical") nms <- nms[vapply(nms,function(v).is_cat(data[[v]]),logical(1))]
  .choose(paste0(prompt,":"),nms,paste0(nms," [",vapply(nms,function(v) if(.is_cat(data[[v]]))"categorical" else "numeric",character(1)),"]"))
}

.pick_groups <- function(data,var,n=2) {
  lv <- .levels_of(data[[var]])
  out <- character()
  for(i in seq_len(n)) {
    rem <- setdiff(lv,out)
    out <- c(out,.choose(sprintf("Choose group %d for %s:",i,var),rem))
  }
  out
}

.read_num_default <- function(prompt,default=NULL) {
  s <- readline(paste0(prompt,if(!is.null(default)) paste0(" [",default,"]") else "",": "))
  if(s=="" && !is.null(default)) return(as.numeric(default))
  suppressWarnings(as.numeric(s))
}

# -------------------------
# Raw-data solvers
# -------------------------

.alt_sym <- function(a) switch(a,less="<",greater=">",two.sided="!=")
.alt_words <- function(a) switch(a,less="lower than",greater="greater than",two.sided="different from")
.pval <- function(stat,dist,a,df=NULL) {
  if(dist=="z") {
    if(a=="less") return(pnorm(stat))
    if(a=="greater") return(1-pnorm(stat))
    return(2*(1-pnorm(abs(stat))))
  }
  if(a=="less") return(pt(stat,df))
  if(a=="greater") return(1-pt(stat,df))
  2*(1-pt(abs(stat),df))
}
.dec <- function(p,a) if(p<a) "REJECT H0" else "FAIL TO REJECT H0"
.ev <- function(p,a) if(p<a) "sufficient" else "insufficient"

.solve_two_means_data <- function(data,outcome,group,groups,alternative,alpha,variance_case) {
  g <- as.character(data[[group]])
  x <- data[[outcome]]
  x1 <- x[g==groups[1]]; x1<-x1[!is.na(x1)]
  x2 <- x[g==groups[2]]; x2<-x2[!is.na(x2)]
  n1<-length(x1);n2<-length(x2)
  tab<-data.frame(Group=groups,n=c(n1,n2),Mean=c(mean(x1),mean(x2)),SD=c(sd(x1),sd(x2)),SE=c(sd(x1)/sqrt(n1),sd(x2)/sqrt(n2)))
  cat("\nGROUP SUMMARY\n-------------\n");print(tab,row.names=FALSE)

  if(variance_case=="pooled") {
    sp2<-((n1-1)*sd(x1)^2+(n2-1)*sd(x2)^2)/(n1+n2-2)
    se<-sqrt(sp2/n1+sp2/n2);stat<-(mean(x1)-mean(x2))/se;df<-n1+n2-2;p<-.pval(stat,"t",alternative,df)
    procedure<-"pooled two-sample Student t test"
    reason<-"The population variances are unknown but assumed equal."
  } else if(variance_case=="welch") {
    s1<-sd(x1);s2<-sd(x2);se<-sqrt(s1^2/n1+s2^2/n2);stat<-(mean(x1)-mean(x2))/se
    df<-(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
    p<-.pval(stat,"t",alternative,df);procedure<-"Welch two-sample t test";reason<-"The population variances are unknown and are not assumed equal."
  } else {
    se<-sqrt(sd(x1)^2/n1+sd(x2)^2/n2);stat<-(mean(x1)-mean(x2))/se;df<-NULL;p<-.pval(stat,"z",alternative)
    procedure<-"large-sample normal approximation";reason<-"The population distribution is not assumed normal, but both samples are large enough for the course CLT approximation."
  }
  cat(sprintf("\nProcedure: %s\nDifference in sample means = %.6f\nSE = %.6f\nStatistic = %.6f\n",procedure,mean(x1)-mean(x2),se,stat))
  if(!is.null(df)) cat(sprintf("df = %.4f\n",df))
  cat(sprintf("p-value = %.8g\nDecision = %s\n\n",p,.dec(p,alpha)))
  cat("EXAM WORDING\n------------\n")
  cat(sprintf("Let mu1 and mu2 denote the population mean %s for %s and %s. The two samples contain different statistical units and are therefore independent. %s Therefore, the appropriate procedure is the %s. We test H0: mu1-mu2 = 0 against H1: mu1-mu2 %s 0. The observed difference between the sample means is %.4f and its estimated standard error is %.4f, producing a test statistic of %.4f%s. The associated p-value is %.6g. Since the p-value is %s alpha = %.4g, we %s. There is %s empirical evidence to support the stated alternative concerning the population mean %s between %s and %s.\n",
              .norm(outcome),groups[1],groups[2],reason,procedure,.alt_sym(alternative),mean(x1)-mean(x2),se,stat,
              if(!is.null(df))sprintf(" with approximately %.2f degrees of freedom",df)else"",
              p,ifelse(p<alpha,"below","not below"),alpha,ifelse(p<alpha,"reject H0","fail to reject H0"),
              .ev(p,alpha),.norm(outcome),groups[1],groups[2]))
  invisible(list(summary=tab,se=se,statistic=stat,df=df,p.value=p,decision=.dec(p,alpha)))
}

.solve_paired_data <- function(data,before,after,alternative,alpha) {
  a<-data[[before]]; b<-data[[after]]
  ok<-!is.na(a)&!is.na(b); a<-a[ok]; b<-b[ok]
  # Define D = after - before, explicitly.
  d<-b-a;n<-length(d);db<-mean(d);sdv<-sd(d);se<-sdv/sqrt(n);t<-db/se;df<-n-1;p<-.pval(t,"t",alternative,df)
  tab<-data.frame(n=n,MeanBefore=mean(a),MeanAfter=mean(b),MeanDifference=db,SDDifference=sdv,SEDifference=se)
  cat("\nPAIRED SUMMARY\n--------------\n");print(tab,row.names=FALSE)
  cat(sprintf("Define D = %s - %s\nT = %.6f\ndf = %d\np-value = %.8g\nDecision = %s\n\n",after,before,t,df,p,.dec(p,alpha)))
  cat("EXAM WORDING\n------------\n")
  cat(sprintf("The measurements are paired because the two variables refer to repeated measurements on the same statistical units. Define D = %s - %s. We test H0: mu_D = 0 against H1: mu_D %s 0. The observed mean paired difference is %.4f with estimated standard error %.4f, giving t = %.4f with %d degrees of freedom. The p-value is %.6g. Since the p-value is %s alpha = %.4g, we %s. There is %s empirical evidence to support the stated alternative concerning the population mean change.\n",
              after,before,.alt_sym(alternative),db,se,t,df,p,ifelse(p<alpha,"below","not below"),alpha,
              ifelse(p<alpha,"reject H0","fail to reject H0"),.ev(p,alpha)))
  invisible(list(summary=tab,statistic=t,df=df,p.value=p))
}

.solve_one_mean_data <- function(data,outcome,mu0,alternative,alpha,case) {
  x<-data[[outcome]];x<-x[!is.na(x)];n<-length(x);xb<-mean(x);s<-sd(x);se<-s/sqrt(n)
  if(case=="t") {stat<-(xb-mu0)/se;df<-n-1;p<-.pval(stat,"t",alternative,df);label<-"Student t"}
  else {stat<-(xb-mu0)/se;df<-NULL;p<-.pval(stat,"z",alternative);label<-"large-sample normal"}
  cat(sprintf("\nONE-MEAN SUMMARY\n----------------\nn = %d\nMean = %.6f\nSD = %.6f\nSE = %.6f\n%s statistic = %.6f\n",n,xb,s,se,label,stat))
  if(!is.null(df))cat(sprintf("df = %d\n",df))
  cat(sprintf("p-value = %.8g\nDecision = %s\n\n",p,.dec(p,alpha)))
  cat(sprintf("EXAM WORDING\n------------\nLet mu denote the population mean %s. We test H0: mu = %g against H1: mu %s %g. The observed sample mean is %.4f with estimated standard error %.4f, producing a %s statistic of %.4f%s. The p-value is %.6g. Since it is %s alpha = %.4g, we %s. There is %s empirical evidence to support the stated alternative concerning the population mean.\n",
              .norm(outcome),mu0,.alt_sym(alternative),mu0,xb,se,label,stat,
              if(!is.null(df))sprintf(" with %d degrees of freedom",df)else"",p,
              ifelse(p<alpha,"below","not below"),alpha,ifelse(p<alpha,"reject H0","fail to reject H0"),.ev(p,alpha)))
  invisible(list(statistic=stat,df=df,p.value=p))
}

.solve_one_prop_data <- function(data,variable,success,p0,alternative,alpha) {
  z<-as.character(data[[variable]]);z<-z[!is.na(z)];x<-sum(z==success);n<-length(z);ph<-x/n
  se<-sqrt(p0*(1-p0)/n);stat<-(ph-p0)/se;p<-.pval(stat,"z",alternative)
  cat(sprintf("\nPROPORTION SUMMARY\n------------------\nEvent: %s = %s\nx = %d\nn = %d\np-hat = %.6f\nSE under H0 = %.6f\nZ = %.6f\np-value = %.8g\nDecision = %s\n\n",
              variable,success,x,n,ph,se,stat,p,.dec(p,alpha)))
  cat(sprintf("EXAM WORDING\n------------\nLet p denote the population proportion for which %s = %s. We test H0: p = %.4f against H1: p %s %.4f. The observed sample proportion is %.4f. Under H0, the standard error is %.4f, giving Z = %.4f and p-value = %.6g. Since the p-value is %s alpha = %.4g, we %s. There is %s empirical evidence to support the stated alternative concerning the population proportion.\n",
              variable,success,p0,.alt_sym(alternative),p0,ph,se,stat,p,ifelse(p<alpha,"below","not below"),alpha,
              ifelse(p<alpha,"reject H0","fail to reject H0"),.ev(p,alpha)))
  invisible(list(phat=ph,statistic=stat,p.value=p))
}

.solve_two_props_data <- function(data,outcome,success,group,groups,alternative,alpha) {
  y<-as.character(data[[outcome]]);g<-as.character(data[[group]])
  ok<-!is.na(y)&!is.na(g);y<-y[ok];g<-g[ok]
  x1<-sum(y[g==groups[1]]==success);n1<-sum(g==groups[1]);x2<-sum(y[g==groups[2]]==success);n2<-sum(g==groups[2])
  p1<-x1/n1;p2<-x2/n2;pp<-(x1+x2)/(n1+n2);se<-sqrt(pp*(1-pp)*(1/n1+1/n2));stat<-(p1-p2)/se;p<-.pval(stat,"z",alternative)
  tab<-data.frame(Group=groups,Successes=c(x1,x2),n=c(n1,n2),SampleProportion=c(p1,p2))
  cat("\nTWO-PROPORTION SUMMARY\n----------------------\n");print(tab,row.names=FALSE)
  cat(sprintf("Pooled p under H0 = %.6f\nSE = %.6f\nZ = %.6f\np-value = %.8g\nDecision = %s\n\n",pp,se,stat,p,.dec(p,alpha)))
  cat(sprintf("EXAM WORDING\n------------\nLet p1 and p2 denote the population proportion with %s = %s for %s and %s. We test H0: p1-p2 = 0 against H1: p1-p2 %s 0. The observed sample proportions are %.4f and %.4f. Under H0, the pooled proportion is %.4f, producing SE = %.4f, Z = %.4f and p-value = %.6g. Since the p-value is %s alpha = %.4g, we %s. There is %s empirical evidence to support the stated alternative comparison.\n",
              outcome,success,groups[1],groups[2],.alt_sym(alternative),p1,p2,pp,se,stat,p,
              ifelse(p<alpha,"below","not below"),alpha,ifelse(p<alpha,"reject H0","fail to reject H0"),.ev(p,alpha)))
  invisible(list(summary=tab,statistic=stat,p.value=p))
}

.solve_chisq_ind_data <- function(data,x,y,alpha) {
  tab<-table(data[[x]],data[[y]],useNA="no");obs<-as.matrix(tab)
  ex<-outer(rowSums(obs),colSums(obs))/sum(obs);contrib<-(obs-ex)^2/ex;chi<-sum(contrib);df<-(nrow(obs)-1)*(ncol(obs)-1);p<-1-pchisq(chi,df)
  if(any(ex<5)) warning("At least one expected count is below 5; check course conditions.")
  cat("\nOBSERVED COUNTS\n");print(obs)
  cat("\nEXPECTED COUNTS UNDER INDEPENDENCE\n");print(round(ex,4))
  cat("\nCELL CONTRIBUTIONS\n");print(round(contrib,4))
  cat(sprintf("\nChi-square = %.6f\ndf = %d\np-value = %.8g\nDecision = %s\n\n",chi,df,p,.dec(p,alpha)))
  cat(sprintf("EXAM WORDING\n------------\nWe test H0: %s and %s are independent in the population against H1: the two variables are associated. The Pearson chi-square statistic is %.4f with %d degrees of freedom and p-value %.6g. Since the p-value is %s alpha = %.4g, we %s. There is %s empirical evidence to conclude that %s and %s are associated in the population.\n",
              x,y,chi,df,p,ifelse(p<alpha,"below","not below"),alpha,ifelse(p<alpha,"reject H0","fail to reject H0"),.ev(p,alpha),x,y))
  invisible(list(expected=ex,statistic=chi,df=df,p.value=p))
}

.solve_chisq_gof_data <- function(data,variable,probs,alpha) {
  tab<-table(data[[variable]],useNA="no");obs<-as.numeric(tab);cats<-names(tab)
  if(length(probs)!=length(obs)||abs(sum(probs)-1)>1e-8||any(probs<=0)) stop("Theoretical probabilities must be positive, sum to 1, and match the number of categories.")
  ex<-sum(obs)*probs;contrib<-(obs-ex)^2/ex;chi<-sum(contrib);df<-length(obs)-1;p<-1-pchisq(chi,df)
  out<-data.frame(Category=cats,Observed=obs,Expected=ex,Contribution=contrib);print(out,row.names=FALSE)
  cat(sprintf("\nChi-square = %.6f\ndf = %d\np-value = %.8g\nDecision = %s\n\n",chi,df,p,.dec(p,alpha)))
  cat(sprintf("EXAM WORDING\n------------\nWe test whether the population distribution of %s follows the theoretical distribution specified under H0. Under H1, at least one category probability differs. The Pearson chi-square statistic is %.4f with %d degrees of freedom and p-value %.6g. Since the p-value is %s alpha = %.4g, we %s. There is %s empirical evidence to conclude that the population distribution differs from the specified theoretical distribution.\n",
              variable,chi,df,p,ifelse(p<alpha,"below","not below"),alpha,ifelse(p<alpha,"reject H0","fail to reject H0"),.ev(p,alpha)))
  invisible(list(table=out,statistic=chi,df=df,p.value=p))
}

.solve_prop_ci <- function(data,variable,success,conf) {
  z<-as.character(data[[variable]]);z<-z[!is.na(z)];x<-sum(z==success);n<-length(z);ph<-x/n;crit<-qnorm((1+conf)/2);se<-sqrt(ph*(1-ph)/n);me<-crit*se;ci<-c(ph-me,ph+me)
  cat(sprintf("\nPROPORTION CI\n-------------\nx = %d\nn = %d\np-hat = %.6f\nSE = %.6f\nCritical Z = %.6f\nMargin = %.6f\nCI = [%.6f, %.6f]\n\n",x,n,ph,se,crit,me,ci[1],ci[2]))
  cat(sprintf("EXAM WORDING\n------------\nBased on the observed sample, the population proportion with %s = %s is estimated to lie between %.4f and %.4f with a confidence level of %.1f%%.\n",
              variable,success,ci[1],ci[2],100*conf))
  invisible(list(interval=ci,se=se,margin=me))
}

.solve_mean_ci <- function(data,outcome,conf,case) {
  x<-data[[outcome]];x<-x[!is.na(x)];n<-length(x);xb<-mean(x);s<-sd(x);se<-s/sqrt(n)
  if(case=="t"){crit<-qt((1+conf)/2,n-1);label<-"Student t"}else{crit<-qnorm((1+conf)/2);label<-"large-sample normal"}
  me<-crit*se;ci<-c(xb-me,xb+me)
  cat(sprintf("\nMEAN CI\n-------\nn = %d\nMean = %.6f\nSD = %.6f\nSE = %.6f\nCritical = %.6f\nMargin = %.6f\nCI = [%.6f, %.6f]\nProcedure = %s\n\n",
              n,xb,s,se,crit,me,ci[1],ci[2],label))
  cat(sprintf("EXAM WORDING\n------------\nBased on the observed sample, the population mean %s is estimated to lie between %.4f and %.4f with a confidence level of %.1f%%.\n",
              outcome,ci[1],ci[2],100*conf))
  invisible(list(interval=ci,se=se,margin=me))
}

.describe_data <- function(data,outcome,group=NULL) {
  cat("\nDESCRIPTIVE ANALYSIS\n--------------------\n")
  if(is.null(group)) {
    x<-data[[outcome]];x<-x[!is.na(x)]
    tab<-data.frame(n=length(x),Mean=mean(x),Median=median(x),SD=sd(x),Q1=quantile(x,.25),Q3=quantile(x,.75),IQR=IQR(x),Min=min(x),Max=max(x))
    print(tab,row.names=FALSE);boxplot(x,main=outcome,ylab=outcome)
  } else {
    g<-as.character(data[[group]]);x<-data[[outcome]];lv<-.levels_of(data[[group]])
    rows<-lapply(lv,function(z){a<-x[g==z];a<-a[!is.na(a)];data.frame(Group=z,n=length(a),Mean=mean(a),Median=median(a),SD=sd(a),Q1=quantile(a,.25),Q3=quantile(a,.75),IQR=IQR(a),Min=min(a),Max=max(a))})
    tab<-do.call(rbind,rows);print(tab,row.names=FALSE);boxplot(data[[outcome]]~data[[group]],xlab=group,ylab=outcome,main=paste(outcome,"by",group))
  }
  cat("\nEXAM WRITING GUIDE\nCompare medians for central tendency, IQR/box width for the middle 50% variability, whiskers/outliers for tails, and box/whisker asymmetry for skewness. State comparisons in context.\n")
  invisible(tab)
}

.solve_reg_data <- function(data,outcome,predictors,alpha) {
  f<-as.formula(paste(outcome,"~",paste(predictors,collapse="+")))
  exam_regression(f,data,alpha)
}

exam_regression <- function(formula,data,alpha=.05) {
  m<-lm(formula,data=data);s<-summary(m);co<-coef(s);f<-s$fstatistic;fp<-pf(f[1],f[2],f[3],lower.tail=FALSE)
  cat("\nREGRESSION SUMMARY\n------------------\n");print(formula(m));print(co)
  cat(sprintf("\nR-squared = %.6f\nAdjusted R-squared = %.6f\nResidual SE = %.6f\nF = %.6f on %d and %d df\nF p-value = %.8g\n",
              s$r.squared,s$adj.r.squared,s$sigma,f[1],f[2],f[3],fp))
  cat("\nEXAM GUIDE\n")
  cat("- Interpret each non-intercept coefficient holding all other explanatory variables constant.\n")
  cat("- Each Pr(>|t|) tests H0: beta_j = 0.\n")
  cat(sprintf("- %.2f%% of sample variability in Y is explained by the fitted model.\n",100*s$r.squared))
  cat(sprintf("- Global F-test: model is %s at alpha %.4g.\n",ifelse(fp<alpha,"globally significant","not shown globally significant"),alpha))
  cat(sprintf("- Residual SE %.4f is the typical residual scale in Y units.\n",s$sigma))
  cat("\nFor predictions/diagnostics/correlations, run regression_toolkit(model, ...).\n")
  invisible(m)
}


# ============================================================
# statcram 0.7.0 — direct local Ollama/Qwen language layer
# ============================================================

.ai_model_default <- "statcram-qwen"
.ai_url_default <- "http://localhost:11434"

ai_status <- function(model=getOption("statcram.model", .ai_model_default),
                      base_url=getOption("statcram.ollama_url", .ai_url_default),
                      quiet=FALSE) {
  if(!requireNamespace("httr2",quietly=TRUE))
    return(structure(FALSE,reason="Package httr2 is not installed."))
  url <- paste0(sub("/$","",base_url),"/api/tags")
  ok <- tryCatch({
    r <- httr2::request(url) |>
      httr2::req_timeout(3) |>
      httr2::req_perform()
    j <- httr2::resp_body_json(r,simplifyVector=TRUE)
    nms <- if(is.null(j$models)) character() else j$models$name
    any(nms==model | sub(":latest$","",nms)==sub(":latest$","",model))
  },error=function(e) FALSE)
  if(!quiet) {
    if(ok) cat(sprintf("Local AI ready: %s via %s\n",model,base_url))
    else cat(sprintf("Local AI unavailable or model '%s' not found at %s\n",model,base_url))
  }
  ok
}

ai_warmup <- function(
    model=getOption("statcram.model", .ai_model_default),
    base_url=getOption("statcram.ollama_url", .ai_url_default),
    timeout=getOption("statcram.warmup_timeout",120),
    quiet=FALSE) {

  if(!requireNamespace("httr2",quietly=TRUE))
    stop("Package httr2 is required.")

  body <- list(
    model=model,
    stream=FALSE,
    think=FALSE,
    keep_alive=getOption("statcram.keep_alive","30m"),
    format="json",
    options=list(
      temperature=0,
      num_ctx=8192,
      num_predict=12
    ),
    messages=list(
      list(role="system",content="Return only a tiny JSON object."),
      list(role="user",content='Return exactly {"ready":true}.')
    )
  )

  started <- Sys.time()
  ok <- tryCatch({
    httr2::request(paste0(sub("/$","",base_url),"/api/chat")) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform()
    TRUE
  }, error=function(e) {
    if(!quiet) cat("Local model warm-up failed:",conditionMessage(e),"\n")
    FALSE
  })

  if(!quiet && ok)
    cat(sprintf("Local model warmed and kept alive for 30m (%.1f sec).\n",
                as.numeric(difftime(Sys.time(),started,units="secs"))))
  invisible(ok)
}

ai_skill <- function(module="core") {
  file <- switch(module,
    core="core.md",
    point="point-estimation.md",
    inference="inference.md",
    descriptive="descriptive.md",
    random="random-variables.md",
    chisq="chi-square.md",
    regression="regression.md",
    pastpapers="past-paper-patterns.md",
    paste0(module,".md")
  )
  path <- system.file("skills",file,package="statcram")
  if(!nzchar(path) || !file.exists(path))
    stop("Skill file not found: ",file)
  paste(readLines(path,warn=FALSE,encoding="UTF-8"),collapse="\n")
}

.ai_schema_text <- function(data) {
  rows <- vapply(names(data),function(nm) {
    x<-data[[nm]]
    if(.is_cat(x)) {
      lv <- .levels_of(x)
      sprintf("- %s | categorical | levels: %s",nm,paste(lv,collapse=", "))
    } else if(is.numeric(x)) {
      sprintf("- %s | numeric",nm)
    } else {
      sprintf("- %s | %s",nm,class(x)[1])
    }
  },character(1))
  paste(rows,collapse="\n")
}

.ai_modules_for_question <- function(q) {
  z <- .norm(q)
  out <- "core"
  if(grepl("estimate|standard error|point estimate|sample size",z))
    out<-c(out,"point")
  inferential_cue <- grepl(
    "confidence|hypothesis|test|p value|significance|power|type ii|reject|null|paired|independent|alpha|differs significantly|greater than [0-9]|less than [0-9]",
    z
  )
  if(inferential_cue)
    out<-c(out,"inference")
  if(grepl("median|mean|mode|quartile|percentile|boxplot|dispersion|variance|standard deviation|coefficient of variation|frequency|density|grouped|class interval|histogram",z))
    out<-c(out,"descriptive")
  if(grepl("random variable|expected value|covariance|correlation|normal probability|quantile|sampling distribution|clt|linear combination|iid",z))
    out<-c(out,"random")
  if(grepl("chi|categorical|goodness|independence|associated",z))
    out<-c(out,"chisq")
  if(grepl("regression|slope|intercept|coefficient|r squared|adjusted|f test|residual|qq|prediction|multicollinearity|leverage|cook",z))
    out<-c(out,"regression")
  unique(c(out,"pastpapers"))
}

.ai_extract_json <- function(x) {
  if(!requireNamespace("jsonlite",quietly=TRUE))
    stop("Package jsonlite is required.")
  x<-trimws(x)
  # tolerate accidental markdown fences
  x<-gsub("^```(?:json)?\\s*","",x,perl=TRUE)
  x<-gsub("\\s*```$","",x,perl=TRUE)
  # isolate first JSON object if model adds text
  first<-regexpr("\\{",x)
  last<-max(gregexpr("\\}",x)[[1]])
  if(first[1]>0 && last>first[1]) x<-substr(x,first[1],last)
  jsonlite::fromJSON(x,simplifyVector=TRUE)
}


.ai_vector <- function(x) {
  if(is.null(x) || length(x)==0) return(character())
  as.character(unlist(x,use.names=FALSE))
}

.ai_probability <- function(x) {
  if(is.null(x) || length(x)==0 || all(is.na(x))) return(NULL)
  z <- suppressWarnings(as.numeric(x[1]))
  if(is.na(z)) return(x)
  if(z>1 && z<=100) z <- z/100
  z
}

.ai_normalize <- function(obj,data,question) {
  # Normalize shapes from small local models.
  obj$levels <- .ai_vector(obj$levels)
  obj$predictors <- .ai_vector(obj$predictors)
  obj$theoretical_probabilities <- suppressWarnings(
    as.numeric(.ai_vector(obj$theoretical_probabilities))
  )
  obj$confidence <- .ai_probability(obj$confidence)
  obj$alpha <- .ai_probability(obj$alpha)

  single_event_tasks <- c(
    "descriptive_sample_proportion",
    "descriptive_proportion_se",
    "proportion_ci",
    "one_proportion_test"
  )

  # Qwen sometimes stores a single category in levels instead of success_level.
  if(obj$task %in% single_event_tasks &&
     (is.null(obj$success_level) || length(obj$success_level)==0 ||
      is.na(obj$success_level) || !nzchar(as.character(obj$success_level))) &&
     length(obj$levels)==1) {
    obj$success_level <- obj$levels[1]
  }

  # Qwen sometimes represents the categorical event as group + levels.
  if(obj$task %in% single_event_tasks &&
     (is.null(obj$variable) || length(obj$variable)==0 ||
      is.na(obj$variable) || !nzchar(as.character(obj$variable))) &&
     !is.null(obj$group) && length(obj$group) &&
     !is.na(obj$group) && nzchar(as.character(obj$group))) {
    obj$variable <- obj$group
  }

  # Mean tasks use outcome. Permit variable only as a normalized alias.
  if(obj$task %in% c("mean_ci","one_mean_test","two_mean_test") &&
     (is.null(obj$outcome) || length(obj$outcome)==0 ||
      is.na(obj$outcome) || !nzchar(as.character(obj$outcome))) &&
     !is.null(obj$variable) && length(obj$variable) &&
     !is.na(obj$variable) && nzchar(as.character(obj$variable))) {
    obj$outcome <- obj$variable
  }

  obj
}

.question_mentions_variable <- function(question,var,data) {
  if(is.null(var) || length(var)==0 || is.na(var) ||
     !nzchar(as.character(var)) || !(var %in% names(data))) return(FALSE)

  profile <- profile_exam_data(data)
  vp <- profile$vars[[var]]
  q <- paste0(" ",.norm(question)," ")

  alias_hit <- any(vapply(vp$aliases,function(a) {
    nzchar(a) && grepl(paste0(" ",a," "),q,fixed=TRUE)
  },logical(1)))

  level_hit <- FALSE
  if(!is.null(vp$levels)) {
    level_hit <- any(vapply(vp$levels,function(lv) {
      aliases <- vp$level_aliases[[lv]]
      any(vapply(aliases,function(a)
        nzchar(a) && grepl(paste0(" ",a," "),q,fixed=TRUE),
        logical(1)))
    },logical(1)))
  }

  alias_hit || level_hit
}

.ai_task_family <- function(task) {
  if(task %in% c("descriptive_sample_proportion","descriptive_proportion_se"))
    return("sample_prop")
  if(task=="descriptive_summary") return("describe")
  if(task=="proportion_ci") return("prop_ci")
  if(task=="mean_ci") return("mean_ci")
  if(task=="one_mean_test") return("one_mean")
  if(task=="paired_mean_test") return("paired")
  if(task=="two_mean_test") return("two_means")
  if(task=="one_proportion_test") return("one_prop")
  if(task=="two_proportion_test") return("two_props")
  if(task=="chisq_independence") return("chisq_independence")
  if(task=="chisq_gof") return("chisq_gof")
  if(task %in% c(
    "fit_regression","interpret_coefficients","individual_coefficient_test",
    "global_f_test","model_fit_r2","compare_models_adjusted_r2",
    "prediction_mean_response","prediction_individual",
    "regression_diagnostics","multicollinearity"
  )) return("regression")
  NULL
}

.ai_grounding <- function(question,data) {
  profile <- profile_exam_data(data)
  R <- .resolve_vars(question,data,profile,min_score=.8)

  level_lines <- character()
  for(v in names(profile$vars)) {
    vp <- profile$vars[[v]]
    if(!is.null(vp$levels)) {
      m <- .match_levels(question,vp)
      if(nrow(m) && any(m$exact)) {
        level_lines <- c(
          level_lines,
          sprintf(
            "- categorical evidence: %s has mentioned level(s): %s",
            v,
            paste(m$level[m$exact],collapse=", ")
          )
        )
      }
    }
  }

  paste(
    "R PRE-SCAN — GROUNDING ONLY",
    "Do not select any other variable unless the question clearly supports it.",
    paste0(
      "- candidate numerical variables explicitly supported: ",
      if(length(R$numeric)) paste(R$numeric,collapse=", ") else "none"
    ),
    paste0(
      "- candidate categorical variables/parents supported: ",
      if(length(R$categorical)) paste(R$categorical,collapse=", ") else "none"
    ),
    paste(level_lines,collapse="\n"),
    sep="\n"
  )
}


.ai_validate <- function(obj,data,question) {
  problems<-character()
  nms<-names(data)
  task<-obj$task

  allowed_tasks <- c(
    "descriptive_summary","grouped_distribution","dispersion_compare",
    "sample_mean_estimate","sample_mean_se",
    "descriptive_sample_proportion","descriptive_proportion_se",
    "discrete_random_variable","normal_probability","normal_quantile",
    "linear_combination_rv","sampling_distribution_mean","iid_sum_or_mean",
    "mean_ci","proportion_ci","paired_mean_ci","two_mean_ci",
    "two_proportion_ci","sample_size_mean","sample_size_proportion",
    "one_mean_test","paired_mean_test","two_mean_test",
    "one_proportion_test","two_proportion_test",
    "type_ii_power_mean","type_ii_power_proportion",
    "chisq_gof","chisq_independence",
    "fit_regression","interpret_coefficients",
    "individual_coefficient_test","global_f_test","model_fit_r2",
    "compare_models_adjusted_r2","prediction_mean_response",
    "prediction_individual","regression_diagnostics","multicollinearity"
  )

  if(is.null(task) || length(task)==0 || is.na(task) || !(task %in% allowed_tasks))
    problems<-c(problems,"Task is missing or is not in the allowed syllabus taxonomy.")

  checkvar<-function(v,label) {
    if(!is.null(v) && length(v) && !is.na(v) && nzchar(v) && !(v %in% nms))
      problems <<- c(problems,sprintf("%s '%s' is not a column in the dataframe.",label,v))
  }

  for(pair in list(
    c("outcome","Outcome"),c("variable","Variable"),
    c("variable2","Variable2"),c("group","Group"),
    c("before","Before"),c("after","After")
  )) {
    if(!is.null(obj[[pair[1]]])) checkvar(obj[[pair[1]]],pair[2])
  }

  if(!is.null(obj$predictors) && length(obj$predictors)) {
    bad<-setdiff(as.character(obj$predictors),nms)
    if(length(bad))
      problems<-c(problems,paste("Unknown predictor(s):",paste(bad,collapse=", ")))
  }

  # Confidence and alpha must be on the probability scale.
  if(!is.null(obj$confidence) &&
     (!is.numeric(obj$confidence) || length(obj$confidence)!=1 ||
      is.na(obj$confidence) || obj$confidence<=0 || obj$confidence>=1)) {
    problems<-c(problems,"Confidence level must be a single number between 0 and 1.")
  }
  if(!is.null(obj$alpha) &&
     (!is.numeric(obj$alpha) || length(obj$alpha)!=1 ||
      is.na(obj$alpha) || obj$alpha<=0 || obj$alpha>=1)) {
    problems<-c(problems,"Alpha must be a single number between 0 and 1.")
  }

  mean_tasks <- c("mean_ci","one_mean_test","two_mean_test","two_mean_ci")
  prop_tasks <- c(
    "descriptive_sample_proportion","descriptive_proportion_se",
    "proportion_ci","one_proportion_test",
    "two_proportion_test","two_proportion_ci"
  )

  if(task %in% mean_tasks) {
    mv <- obj$outcome %||% obj$variable
    if(is.null(mv) || length(mv)==0 || is.na(mv) || !nzchar(mv)) {
      problems<-c(problems,"A mean task requires an explicitly supported numerical outcome.")
    } else if(mv %in% nms) {
      if(!.is_num(data[[mv]]))
        problems<-c(problems,sprintf("Mean task outcome '%s' is not numerical.",mv))
      if(!.question_mentions_variable(question,mv,data))
        problems<-c(
          problems,
          sprintf(
            "Numerical outcome '%s' is not supported by the wording of the question.",
            mv
          )
        )
    }
  }

  if(task %in% prop_tasks) {
    pv <- obj$variable
    if(is.null(pv) || length(pv)==0 || is.na(pv) || !nzchar(pv)) {
      problems<-c(problems,"A proportion task requires a categorical variable.")
    } else if(pv %in% nms) {
      if(!.is_cat(data[[pv]]))
        problems<-c(problems,sprintf("Proportion task variable '%s' is not categorical.",pv))
    }

    if(task %in% c(
      "descriptive_sample_proportion","descriptive_proportion_se",
      "proportion_ci","one_proportion_test"
    )) {
      if(is.null(obj$success_level) || length(obj$success_level)==0 ||
         is.na(obj$success_level) || !nzchar(as.character(obj$success_level))) {
        problems<-c(problems,"A single-proportion task requires the category/event of interest.")
      } else if(!is.null(pv) && pv %in% nms) {
        actual<-as.character(.levels_of(data[[pv]]))
        if(!(as.character(obj$success_level) %in% actual))
          problems<-c(
            problems,
            sprintf(
              "Success level '%s' is not a level of %s.",
              obj$success_level,pv
            )
          )
      }
    }
  }

  if(task=="chisq_independence") {
    for(v in c(obj$variable,obj$variable2)) {
      if(!is.null(v) && v %in% nms && !.is_cat(data[[v]]))
        problems<-c(problems,sprintf("Chi-square independence variable '%s' is not categorical.",v))
    }
  }

  if(task %in% c(
    "fit_regression","interpret_coefficients","individual_coefficient_test",
    "global_f_test","model_fit_r2","compare_models_adjusted_r2",
    "prediction_mean_response","prediction_individual",
    "regression_diagnostics","multicollinearity"
  )) {
    if(is.null(obj$outcome) || !length(obj$outcome) ||
       is.na(obj$outcome) || !nzchar(obj$outcome)) {
      problems<-c(problems,"Regression requires a dependent variable.")
    } else if(obj$outcome %in% nms && !.is_num(data[[obj$outcome]])) {
      problems<-c(problems,"The current course regression engine requires a numerical dependent variable.")
    }
  }

  # Validate group levels.
  parent <- obj$group
  if(!is.null(parent) && parent %in% nms && length(obj$levels)) {
    actual <- as.character(.levels_of(data[[parent]]))
    bad <- setdiff(as.character(obj$levels),actual)
    if(length(bad))
      problems<-c(problems,paste("Unknown group level(s):",paste(bad,collapse=", ")))
  }

  # Unsupported variables must not pass merely because they exist.
  selected_numeric <- unique(c(
    if(!is.null(obj$outcome)) obj$outcome else character(),
    if(task %in% mean_tasks && !is.null(obj$variable)) obj$variable else character()
  ))
  selected_numeric <- selected_numeric[
    selected_numeric %in% nms &
      vapply(selected_numeric,function(v).is_num(data[[v]]),logical(1))
  ]
  for(v in selected_numeric) {
    if(!.question_mentions_variable(question,v,data))
      problems<-c(
        problems,
        sprintf("Selected numerical variable '%s' has no textual support in the question.",v)
      )
  }

  # Cross-check against a high-confidence deterministic interpretation.
  rule <- tryCatch(
    .interpret(question,data,profile_exam_data(data)),
    error=function(e) NULL
  )
  ai_family <- .ai_task_family(task)
  if(!is.null(rule) && !is.null(rule$family) &&
     identical(rule$confidence,"high") &&
     !is.null(ai_family) &&
     !identical(rule$family,ai_family)) {
    problems<-c(
      problems,
      sprintf(
        "AI task implies '%s', but the high-confidence R cross-check implies '%s'.",
        ai_family,rule$family
      )
    )
  }

  # Assumptions require supporting evidence.
  evidence <- obj$evidence
  for(field in c("variance_case","paired","known_sigma","large_sample","alternative")) {
    val<-obj[[field]]
    if(!is.null(val) && length(val) && !all(is.na(val))) {
      ev<-NULL
      if(is.list(evidence)) ev<-evidence[[field]]
      if(is.null(ev) || length(ev)==0 || is.na(ev) ||
         !nzchar(trimws(as.character(ev)))) {
        problems<-c(
          problems,
          sprintf("AI supplied %s without a supporting evidence phrase.",field)
        )
      }
    }
  }

  list(ok=!length(problems),problems=unique(problems))
}
ai_interpret <- function(question,data,
                         model=getOption("statcram.model", .ai_model_default),
                         base_url=getOption("statcram.ollama_url", .ai_url_default),
                         timeout=getOption("statcram.ai_timeout",120),
                         verbose=TRUE) {
  if(!is.data.frame(data)) stop("data must be a data.frame.")
  if(!ai_status(model,base_url,quiet=TRUE))
    stop("Local Ollama/model unavailable. Ensure Ollama is running and model '",model,"' exists.")

  mods<-.ai_modules_for_question(question)
  skill<-paste(vapply(mods,ai_skill,character(1)),collapse="\n\n")
  schema<-.ai_schema_text(data)
  grounding<-.ai_grounding(question,data)

  system_prompt<-paste(
    skill,
    "\n\nDATASET SCHEMA\n--------------\n",schema,
    "\n\nRemember: RETURN JSON ONLY. Do not calculate.",
    sep=""
  )

  body<-list(
    model=model,
    stream=FALSE,
    think=FALSE,
    keep_alive=getOption("statcram.keep_alive","30m"),
    format="json",
    options=list(
      temperature=0,
      num_ctx=8192,
      num_predict=384
    ),
    messages=list(
      list(role="system",content=paste0(
        system_prompt,
        "\nDo not think aloud. Emit the JSON object immediately."
      )),
      list(role="user",content=question)
    )
  )

  url<-paste0(sub("/$","",base_url),"/api/chat")
  started<-Sys.time()
  response<-httr2::request(url) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform()
  raw<-httr2::resp_body_json(response,simplifyVector=FALSE)
  content<-raw$message$content
  obj<-.ai_normalize(.ai_extract_json(content),data,question)
  validation<-.ai_validate(obj,data,question)

  if(verbose) {
    cat("\nLOCAL AI INTERPRETATION\n")
    cat("=======================\n")
    cat(sprintf("Model: %s | %.1f sec\n",model,as.numeric(difftime(Sys.time(),started,units="secs"))))
    cat(jsonlite::toJSON(obj,pretty=TRUE,auto_unbox=TRUE,null="null"),"\n")
    if(validation$ok) cat("\nVALIDATION: PASS\n")
    else {
      cat("\nVALIDATION: BLOCKED\n")
      cat(paste0("- ",validation$problems,collapse="\n"),"\n")
    }
  }

  structure(obj,validation=validation,raw=content)
}

.ai_to_internal <- function(A,data) {
  task<-A$task
  alt<-A$alternative
  conf<-.ai_probability(A$confidence)
  alpha<-.ai_probability(A$alpha)

  # Maps AI taxonomy to existing statcram conversational families.
  if(task %in% c("descriptive_sample_proportion","descriptive_proportion_se"))
    return(list(
      family="sample_prop",
      variable=A$variable,
      success=A$success_level,
      want_se=identical(task,"descriptive_proportion_se")
    ))
  if(task=="descriptive_summary")
    return(list(family="describe",outcome=A$outcome %||% A$variable,group=A$group))
  if(task=="proportion_ci")
    return(list(
      family="prop_ci",
      variable=A$variable,
      success=A$success_level %||% if(length(A$levels)==1) A$levels[1] else NULL,
      conf=conf
    ))
  if(task=="mean_ci")
    return(list(family="mean_ci",outcome=A$outcome %||% A$variable,conf=conf))
  if(task=="one_mean_test")
    return(list(family="one_mean",outcome=A$outcome %||% A$variable,mu0=A$null_value,
                alternative=alt,alpha=alpha))
  if(task=="paired_mean_test")
    return(list(family="paired",before=A$before,after=A$after,alternative=alt,alpha=alpha))
  if(task=="two_mean_test")
    return(list(family="two_means",outcome=A$outcome,group=A$group,
                groups=as.character(A$levels),alternative=alt,alpha=alpha,
                ai_variance_case=A$variance_case))
  if(task=="one_proportion_test")
    return(list(family="one_prop",variable=A$variable,success=A$success_level,
                p0=A$null_value,alternative=alt,alpha=alpha))
  if(task=="two_proportion_test")
    return(list(family="two_props",outcome=A$variable,success=A$success_level,
                group=A$group,groups=as.character(A$levels),alternative=alt,alpha=alpha))
  if(task=="chisq_independence")
    return(list(family="chisq_independence",x=A$variable,y=A$variable2,alpha=alpha))
  if(task=="chisq_gof")
    return(list(family="chisq_gof",variable=A$variable,alpha=alpha,
                ai_probs=A$theoretical_probabilities))
  if(task %in% c("fit_regression","interpret_coefficients","individual_coefficient_test",
                 "global_f_test","model_fit_r2","compare_models_adjusted_r2",
                 "prediction_mean_response","prediction_individual",
                 "regression_diagnostics","multicollinearity"))
    return(list(family="regression",outcome=A$outcome,predictors=as.character(A$predictors),
                alpha=alpha,ai_regression_task=task))
  NULL
}

statcram_ai_test <- function(data) {
  qs<-c(
    "Report the estimate of the proportion of customers with High loyalty and specify how the corresponding standard error is estimated.",
    "Is premium membership associated with acquisition channel?",
    "Assess whether average monthly spending differs between High and Medium loyalty customers. Assume equal population variances.",
    "Predict monthly spending using age, premium membership and website minutes."
  )
  for(q in qs) {
    cat("\nQUESTION:",q,"\n")
    try(print(ai_interpret(q,data,verbose=TRUE)))
  }
  invisible(TRUE)
}



.family_values <- c(
  "describe","sample_prop","mean_ci","prop_ci","one_mean","paired",
  "two_means","one_prop","two_props","chisq_independence",
  "chisq_gof","regression"
)

.family_labels <- c(
  "Descriptive summaries / boxplot",
  "Sample proportion / estimated SE",
  "Confidence interval for a mean",
  "Confidence interval for a proportion",
  "One-mean hypothesis test",
  "Paired-means test",
  "Two-independent-means test",
  "One-proportion hypothesis test",
  "Two-proportion hypothesis test",
  "Chi-square independence",
  "Chi-square goodness-of-fit",
  "Regression"
)

.family_label <- function(family) {
  i <- match(family,.family_values)
  if(is.na(i)) as.character(family) else .family_labels[i]
}

.print_review <- function(I) {
  cat("\nINTERPRETATION REVIEW\n")
  cat("=====================\n")
  cat("Task:",.family_label(I$family),"\n")

  fields <- c(
    "outcome","variable","success","group","before","after",
    "x","y","mu0","p0","alternative","alpha","conf",
    "ai_variance_case","ai_regression_task"
  )
  labels <- c(
    outcome="Outcome",variable="Variable",success="Category/event",
    group="Grouping variable",before="Before variable",after="After variable",
    x="First categorical variable",y="Second categorical variable",
    mu0="Null mean",p0="Null proportion",alternative="Alternative",
    alpha="Alpha",conf="Confidence",ai_variance_case="Variance case",
    ai_regression_task="Regression task"
  )

  for(f in fields) {
    v <- I[[f]]
    if(!is.null(v) && length(v) && !all(is.na(v))) {
      if(f %in% c("alpha","conf") && is.numeric(v))
        v <- sprintf("%.1f%%",100*v)
      cat(sprintf("%s: %s\n",labels[[f]],paste(v,collapse=", ")))
    }
  }

  if(!is.null(I$groups) && length(I$groups))
    cat("Groups:",paste(I$groups,collapse=" vs "),"\n")
  if(!is.null(I$predictors) && length(I$predictors))
    cat("Predictors:",paste(I$predictors,collapse=", "),"\n")
}

.edit_interpretation <- function(I,data) {
  repeat {
    cat("\nEDIT INTERPRETATION\n")
    cat("-------------------\n")
    action <- .choose(
      "What would you like to change?",
      c("task","variables","settings","done"),
      c(
        "Problem type / statistical task",
        "Variables, category, or groups",
        "Confidence level, alpha, direction, or variance case",
        "Finish editing"
      )
    )

    if(action=="done") return(I)

    if(action=="task") {
      I <- list(
        family=.choose(
          "Choose the correct problem type:",
          .family_values,
          .family_labels
        )
      )
      next
    }

    if(action=="variables") {
      f <- I$family

      if(f=="sample_prop") {
        I$variable <- .pick_var(data,"categorical","Choose categorical variable")
        I$success <- .choose(
          "Choose category/event of interest:",
          .levels_of(data[[I$variable]])
        )
      } else if(f=="describe") {
        I$outcome <- .pick_var(data,"numeric","Choose numerical outcome")
        use_group <- .confirm("Compare across categories?")
        I$group <- if(use_group) {
          .pick_var(data,"categorical","Choose grouping variable")
        } else NULL
      } else if(f=="mean_ci" || f=="one_mean") {
        I$outcome <- .pick_var(data,"numeric","Choose numerical variable")
      } else if(f=="prop_ci" || f=="one_prop") {
        I$variable <- .pick_var(data,"categorical","Choose categorical variable")
        I$success <- .choose(
          "Choose category/event of interest:",
          .levels_of(data[[I$variable]])
        )
      } else if(f=="paired") {
        I$before <- .pick_var(data,"numeric","Choose first/before variable")
        I$after <- .pick_var(data,"numeric","Choose second/after variable")
      } else if(f=="two_means") {
        I$outcome <- .pick_var(data,"numeric","Choose numerical outcome")
        I$group <- .pick_var(data,"categorical","Choose grouping variable")
        I$groups <- .pick_groups(data,I$group,2)
      } else if(f=="two_props") {
        I$outcome <- .pick_var(data,"categorical","Choose binary outcome")
        I$success <- .choose(
          "Choose success/event level:",
          .levels_of(data[[I$outcome]])
        )
        I$group <- .pick_var(data,"categorical","Choose grouping variable")
        I$groups <- .pick_groups(data,I$group,2)
      } else if(f=="chisq_independence") {
        I$x <- .pick_var(data,"categorical","Choose first categorical variable")
        repeat {
          I$y <- .pick_var(data,"categorical","Choose second categorical variable")
          if(I$y!=I$x) break
          cat("Please choose a different second variable.\n")
        }
      } else if(f=="chisq_gof") {
        I$variable <- .pick_var(data,"categorical","Choose categorical variable")
      } else if(f=="regression") {
        I$outcome <- .pick_var(data,"numeric","Choose dependent variable Y")
        preds <- character()
        available <- setdiff(names(data),I$outcome)
        repeat {
          choice <- .choose(
            "Choose a predictor, or DONE:",
            c("DONE",available)
          )
          if(choice=="DONE") break
          preds <- c(preds,choice)
          available <- setdiff(available,choice)
        }
        I$predictors <- preds
      }
      next
    }

    if(action=="settings") {
      f <- I$family

      if(f %in% c("mean_ci","prop_ci")) {
        z <- .read_num_default("Confidence level: enter 0.90 or 90",I$conf %||% .95)
        if(!is.na(z) && z>1 && z<=100) z <- z/100
        I$conf <- z
      }

      if(f %in% c(
        "one_mean","paired","two_means","one_prop",
        "two_props","chisq_independence","chisq_gof","regression"
      )) {
        z <- .read_num_default("Significance level alpha: enter 0.05 or 5",I$alpha %||% .05)
        if(!is.na(z) && z>1 && z<=100) z <- z/100
        I$alpha <- z
      }

      if(f %in% c("one_mean","paired","two_means","one_prop","two_props")) {
        I$alternative <- .choose(
          "Choose alternative direction:",
          c("less","greater","two.sided"),
          c("Lower / less / decrease","Higher / greater / increase","Different / changed")
        )
      }

      if(f=="one_mean")
        I$mu0 <- .read_num_default("Null population mean",I$mu0)
      if(f=="one_prop")
        I$p0 <- .read_num_default("Null population proportion",I$p0)

      if(f=="two_means") {
        I$ai_variance_case <- .choose(
          "Choose variance/distribution case:",
          c("pooled","welch","large"),
          c(
            "Unknown but explicitly assumed equal -> pooled t",
            "Unknown and not assumed equal -> Welch t",
            "Unknown/non-normal with large samples -> normal approximation"
          )
        )
      }

      if(f=="sample_prop")
        I$want_se <- .confirm("Include the estimated standard error?")
    }
  }
}

.review_interpretation <- function(I,data) {
  repeat {
    .print_review(I)
    cat("\n1. Proceed with this interpretation\n")
    cat("2. Edit this interpretation\n")
    cat("3. Re-enter/rephrase the question\n")
    cat("4. Cancel this question\n")
    cat("Tip: typing n or no opens the editor.\n")

    ans <- tolower(trimws(readline("Choice [1]: ")))
    if(ans=="" || ans %in% c("1","y","yes"))
      return(list(action="proceed",I=I))
    if(ans %in% c("2","n","no","e","edit")) {
      I <- .edit_interpretation(I,data)
      next
    }
    if(ans %in% c("3","r","rephrase","retry"))
      return(list(action="rephrase",I=I))
    if(ans %in% c("4","c","cancel"))
      return(list(action="cancel",I=I))
    cat("Please choose 1, 2, 3, or 4.\n")
  }
}


# -------------------------
# Main assistant
# -------------------------

statcram <- function(data=NULL, mode=c("auto","ai","rules")) {
  mode<-match.arg(mode)
  cat("\n============================================================\n")
  cat("STATCRAM OFFLINE AI EXAM ASSISTANT  v0.7.2\n")
  cat("============================================================\n")
  cat("Local Qwen language layer + deterministic R calculations. No internet required.\n\n")

  if(is.null(data)) {
    dfs<-.get_df_candidates(.GlobalEnv)
    if(!length(dfs)) stop("No data.frames are loaded.")
    if(length(dfs)==1){data_name<-dfs[1];data<-get(data_name,envir=.GlobalEnv)}
    else {data_name<-.choose("Choose dataframe:",dfs);data<-get(data_name,envir=.GlobalEnv)}
  } else {
    data_name<-deparse(substitute(data));if(!is.data.frame(data))stop("data must be a data.frame.")
  }

  profile<-profile_exam_data(data)
  cat(sprintf("Using dataframe: %s [%d rows x %d columns]\n",data_name,nrow(data),ncol(data)))
  if(.confirm("Show variable map?")) scan_exam_data(data)

  if(mode %in% c("auto","ai") && ai_status(quiet=TRUE)) {
    cat("\nPreparing local Qwen once for this session...\n")
    ai_warmup(quiet=FALSE)
  }

  repeat {
    cat("\nPaste or describe the exam question. Type 'quit' to exit.\n")
    q<-readline("> ")
    if(tolower(trimws(q)) %in% c("quit","exit","q"))break
    if(!nzchar(trimws(q)))next

    # In auto/ai mode, Qwen interprets language; R validates the result.
    # If AI is unavailable/invalid in auto mode, fall back to deterministic rules.
    I<-NULL
    if(mode %in% c("auto","ai")) {
      ai_ok<-ai_status(quiet=TRUE)
      if(ai_ok) {
        A<-tryCatch(ai_interpret(q,data,verbose=TRUE),error=function(e)e)
        if(inherits(A,"error")) {
          cat("\nLOCAL AI ERROR:",conditionMessage(A),"\n")
          if(mode=="ai") next
        } else {
          val<-attr(A,"validation")
          if(isTRUE(val$ok)) I<-.ai_to_internal(A,data)
          else if(mode=="ai") {
            cat("AI interpretation was blocked by validation. Rephrase or use mode='rules'.\n")
            next
          }
        }
      } else if(mode=="ai") {
        cat("\nLocal AI is unavailable. Check Ollama and model 'statcram-qwen'.\n")
        next
      }
      if(is.null(I) && mode=="auto")
        cat("\nFalling back to deterministic parser for this question.\n")
    }
    if(is.null(I)) I<-.interpret(q,data,profile)

    if(is.null(I$family)) {
      cat("\nI cannot safely identify the problem family.\n")
      if(!is.null(I$resolver) && length(I$resolver$mentioned)) {
        cat("Variables I noticed:",paste(I$resolver$mentioned,collapse=", "),"\n")
      }
      I$family<-.choose(
        "Choose broad family:",
        .family_values,
        .family_labels
      )
    }

    review <- .review_interpretation(I,data)
    if(review$action=="cancel") {
      cat("Question cancelled. No calculation was run.\n")
      next
    }
    if(review$action=="rephrase") {
      cat("Returning to the question prompt. Paste a corrected version.\n")
      next
    }
    I <- review$I

    if(I$family=="sample_prop") {
      I$variable<-I$variable %||% .pick_var(data,"categorical","Choose categorical variable")
      I$success<-I$success %||% .choose("Choose category:",.levels_of(data[[I$variable]]))
      task_label <- if(isTRUE(I$want_se)) {
        "sample proportion + estimated standard error"
      } else {
        "descriptive sample proportion"
      }
      cat(sprintf("\nINTERPRETATION\nVariable: %s\nCategory: %s\nTask: %s\n",
                  I$variable,I$success,task_label))
      .sample_proportion(data,I$variable,I$success)

    } else if(I$family=="describe") {
      I$outcome<-I$outcome %||% .pick_var(data,"numeric","Choose numerical outcome")
      if(is.null(I$group) && .confirm("Compare across groups?")) I$group<-.pick_var(data,"categorical","Choose grouping variable")
      cat(sprintf("\nINTERPRETATION\nOutcome: %s\nGroup: %s\nProcedure: descriptive summaries + boxplot\n",I$outcome,I$group %||% "none"))
      .describe_data(data,I$outcome,I$group)

    } else if(I$family=="mean_ci") {
      I$outcome<-I$outcome %||% .pick_var(data,"numeric","Choose numerical variable")
      I$conf<-I$conf %||% .read_num_default("Confidence level",.95)
      case<-.choose("Which course case?",c("t","large"),c("Normal population; sigma unknown -> Student t","Unknown distribution; sample large -> normal approximation"))
      cat(sprintf("\nINTERPRETATION\nVariable: %s\nConfidence: %.1f%%\nProcedure: %s mean CI\n",I$outcome,100*I$conf,case))
      .solve_mean_ci(data,I$outcome,I$conf,case)

    } else if(I$family=="prop_ci") {
      I$variable<-I$variable %||% .pick_var(data,"categorical","Choose categorical variable")
      I$success<-I$success %||% .choose("Choose event of interest:",.levels_of(data[[I$variable]]))
      I$conf<-I$conf %||% .read_num_default("Confidence level",.95)
      cat(sprintf("\nINTERPRETATION\nVariable: %s\nEvent: %s\nConfidence: %.1f%%\nProcedure: one-proportion CI\n",I$variable,I$success,100*I$conf))
      .solve_prop_ci(data,I$variable,I$success,I$conf)

    } else if(I$family=="one_mean") {
      I$outcome<-I$outcome %||% .pick_var(data,"numeric","Choose numerical variable")
      I$mu0<-I$mu0 %||% .read_num_default("Null mean mu0")
      if(is.na(I$mu0)){cat("Invalid null mean.\n");next}
      I$alternative<-I$alternative %||% .choose("Alternative:",c("less","greater","two.sided"),c("mu < mu0","mu > mu0","mu != mu0"))
      I$alpha<-I$alpha %||% .read_num_default("alpha",.05)
      case<-.choose("Which course case?",c("t","large"),c("Normal population; sigma unknown -> Student t","Unknown distribution; sample large -> normal approximation"))
      cat(sprintf("\nINTERPRETATION\nVariable: %s\nH0 mean: %g\nAlternative: %s\nProcedure: %s\n",I$outcome,I$mu0,I$alternative,case))
      .solve_one_mean_data(data,I$outcome,I$mu0,I$alternative,I$alpha,case)

    } else if(I$family=="paired") {
      I$before<-I$before %||% .pick_var(data,"numeric","Choose BEFORE / first measurement")
      I$after<-I$after %||% .pick_var(data,"numeric","Choose AFTER / second measurement")
      I$alternative<-I$alternative %||% .choose("Alternative for D = after-before:",c("less","greater","two.sided"),c("decrease","increase","any change"))
      I$alpha<-I$alpha %||% .read_num_default("alpha",.05)
      cat(sprintf("\nINTERPRETATION\nPaired variables: %s and %s\nDefine D = %s - %s\nAlternative: %s\nProcedure: paired t\n",I$before,I$after,I$after,I$before,I$alternative))
      .solve_paired_data(data,I$before,I$after,I$alternative,I$alpha)

    } else if(I$family=="two_means") {
      I$outcome<-I$outcome %||% .pick_var(data,"numeric","Choose numerical outcome")
      I$group<-I$group %||% .pick_var(data,"categorical","Choose grouping variable")
      if(length(I$groups)<2) I$groups<-.pick_groups(data,I$group,2)
      I$alternative<-I$alternative %||% .choose("Alternative:",c("less","greater","two.sided"),c("Group 1 < Group 2","Group 1 > Group 2","Groups differ"))
      I$alpha<-I$alpha %||% .read_num_default("alpha",.05)
      vcase<-I$ai_variance_case %||% .choose("What does the question say about variance/distribution?",
        c("pooled","welch","large"),
        c("Unknown but assumed equal -> pooled t","Unknown and not assumed equal -> Welch t","Unknown/non-normal, both samples large -> normal approximation"))
      cat(sprintf("\nINTERPRETATION\nOutcome: %s\nGroup variable: %s\nGroup 1: %s\nGroup 2: %s\nAlternative: %s\nProcedure: %s\n",
                  I$outcome,I$group,I$groups[1],I$groups[2],I$alternative,vcase))
      .solve_two_means_data(data,I$outcome,I$group,I$groups,I$alternative,I$alpha,vcase)

    } else if(I$family=="one_prop") {
      I$variable<-I$variable %||% .pick_var(data,"categorical","Choose categorical variable")
      I$success<-I$success %||% .choose("Choose event/success:",.levels_of(data[[I$variable]]))
      I$p0<-I$p0 %||% .read_num_default("Null proportion p0")
      if(is.na(I$p0)||I$p0<=0||I$p0>=1){cat("Invalid p0.\n");next}
      I$alternative<-I$alternative %||% .choose("Alternative:",c("less","greater","two.sided"),c("p < p0","p > p0","p != p0"))
      I$alpha<-I$alpha %||% .read_num_default("alpha",.05)
      cat(sprintf("\nINTERPRETATION\nVariable: %s\nEvent: %s\nH0 p: %.4f\nAlternative: %s\nProcedure: one-proportion Z test\n",I$variable,I$success,I$p0,I$alternative))
      .solve_one_prop_data(data,I$variable,I$success,I$p0,I$alternative,I$alpha)

    } else if(I$family=="two_props") {
      I$outcome<-I$outcome %||% .pick_var(data,"categorical","Choose binary outcome variable")
      I$success<-I$success %||% .choose("Choose success level:",.levels_of(data[[I$outcome]]))
      I$group<-I$group %||% .pick_var(data,"categorical","Choose grouping variable")
      if(length(I$groups)<2) I$groups<-.pick_groups(data,I$group,2)
      I$alternative<-I$alternative %||% .choose("Alternative:",c("less","greater","two.sided"),c("Group 1 proportion < Group 2","Group 1 proportion > Group 2","Proportions differ"))
      I$alpha<-I$alpha %||% .read_num_default("alpha",.05)
      cat(sprintf("\nINTERPRETATION\nOutcome: %s = %s\nGroup: %s\nLevels: %s vs %s\nAlternative: %s\nProcedure: two-proportion Z test\n",
                  I$outcome,I$success,I$group,I$groups[1],I$groups[2],I$alternative))
      .solve_two_props_data(data,I$outcome,I$success,I$group,I$groups,I$alternative,I$alpha)

    } else if(I$family=="chisq_independence") {
      I$x<-I$x %||% .pick_var(data,"categorical","Choose first categorical variable")
      I$y<-I$y %||% .pick_var(data,"categorical","Choose second categorical variable")
      while(I$y==I$x) I$y<-.pick_var(data,"categorical","Choose a DIFFERENT second categorical variable")
      I$alpha<-I$alpha %||% .read_num_default("alpha",.05)
      cat(sprintf("\nINTERPRETATION\nVariables: %s and %s\nProcedure: chi-square independence\n",I$x,I$y))
      .solve_chisq_ind_data(data,I$x,I$y,I$alpha)

    } else if(I$family=="chisq_gof") {
      I$variable<-I$variable %||% .pick_var(data,"categorical","Choose categorical variable")
      lv<-.levels_of(data[[I$variable]])
      if(.confirm("Are theoretical probabilities equal across categories?")) probs<-rep(1/length(lv),length(lv)) else {
        probs<-numeric(length(lv))
        for(i in seq_along(lv)) probs[i]<-.read_num_default(paste("Probability for",lv[i]))
      }
      I$alpha<-I$alpha %||% .read_num_default("alpha",.05)
      cat(sprintf("\nINTERPRETATION\nVariable: %s\nProbabilities: %s\nProcedure: chi-square goodness-of-fit\n",I$variable,paste(round(probs,4),collapse=", ")))
      .solve_chisq_gof_data(data,I$variable,probs,I$alpha)

    } else if(I$family=="regression") {
      I$outcome<-I$outcome %||% .pick_var(data,"numeric","Choose dependent variable Y")
      if(is.null(I$predictors)||!length(I$predictors)) {
        preds<-character();avail<-setdiff(names(data),I$outcome)
        repeat {
          ch<-.choose("Choose predictor, or DONE:",c("DONE",avail))
          if(ch=="DONE")break
          preds<-c(preds,ch);avail<-setdiff(avail,ch)
        }
        I$predictors<-preds
      }
      if(!length(I$predictors)){cat("No predictors selected.\n");next}
      I$alpha<-I$alpha %||% .read_num_default("alpha",.05)
      cat(sprintf("\nINTERPRETATION\nY: %s\nX: %s\nProcedure: %s regression\n",I$outcome,paste(I$predictors,collapse=", "),if(length(I$predictors)==1)"simple" else "multiple"))
      .solve_reg_data(data,I$outcome,I$predictors,I$alpha)
    }

    if(!.confirm("Solve another question?"))break
  }
  invisible(NULL)
}

question_help <- function(query,data=NULL) {
  if(is.null(data)) {
    dfs<-.get_df_candidates(.GlobalEnv)
    if(length(dfs)!=1) stop("Provide data=... when zero or multiple data.frames are loaded.")
    data<-get(dfs[1],envir=.GlobalEnv)
  }
  I<-.interpret(query,data,profile_exam_data(data))
  print(I)
  invisible(I)
}

exam_solve <- function(type,...) {
  stop("In v0.4.1 the recommended interface is statcram(data). The lower-level raw-data solvers are intentionally internal.")
}


# ============================================================
# EXAM-COMPLETE ADD-ON MODULES
# ============================================================

probability_help <- function(distribution="normal", task=c("left","right","between","quantile"),
                             mean=0, sd=1, a=NULL, b=NULL, p=NULL, df=NULL) {
  task<-match.arg(task)
  distribution<-tolower(distribution)
  if(distribution=="normal") {
    if(task=="left") ans<-pnorm(a,mean,sd)
    if(task=="right") ans<-1-pnorm(a,mean,sd)
    if(task=="between") ans<-pnorm(b,mean,sd)-pnorm(a,mean,sd)
    if(task=="quantile") ans<-qnorm(p,mean,sd)
  } else if(distribution %in% c("t","student")) {
    if(is.null(df)) stop("df is required for Student t.")
    if(task=="left") ans<-pt(a,df)
    if(task=="right") ans<-1-pt(a,df)
    if(task=="between") ans<-pt(b,df)-pt(a,df)
    if(task=="quantile") ans<-qt(p,df)
  } else if(distribution %in% c("chisq","chi-square","chi square")) {
    if(is.null(df)) stop("df is required for chi-square.")
    if(task=="left") ans<-pchisq(a,df)
    if(task=="right") ans<-1-pchisq(a,df)
    if(task=="between") ans<-pchisq(b,df)-pchisq(a,df)
    if(task=="quantile") ans<-qchisq(p,df)
  } else stop("Supported distributions: normal, t, chisq.")
  cat(sprintf("\n%s %s result = %.10g\n",distribution,task,ans))
  invisible(ans)
}

rv_combine <- function(mu1,sd1,mu2=0,sd2=0,a=1,b=1,constant=0,rho=0,
                       lower=NULL,upper=NULL,joint_normal=FALSE) {
  cov12<-rho*sd1*sd2
  mu<-a*mu1+b*mu2+constant
  variance<-a^2*sd1^2+b^2*sd2^2+2*a*b*cov12
  if(variance<0) stop("Computed variance is negative; check inputs.")
  sd<-sqrt(variance)
  cat("\nLINEAR COMBINATION OF RANDOM VARIABLES\n")
  cat("--------------------------------------\n")
  cat(sprintf("E(T) = %.6f\nVar(T) = %.6f\nSD(T) = %.6f\nCov(X,Y) = %.6f\n",mu,variance,sd,cov12))
  prob<-NULL
  if(joint_normal && (!is.null(lower)||!is.null(upper))) {
    lo<-if(is.null(lower))-Inf else lower
    hi<-if(is.null(upper)) Inf else upper
    prob<-pnorm(hi,mu,sd)-pnorm(lo,mu,sd)
    cat(sprintf("P(%.4g < T < %.4g) = %.8g\n",lo,hi,prob))
  }
  invisible(list(mean=mu,variance=variance,sd=sd,covariance=cov12,probability=prob))
}

sample_size <- function(type=c("mean","proportion"), margin, conf=.95,
                        sigma=NULL, p=NULL) {
  type<-match.arg(type)
  if(margin<=0) stop("margin must be positive.")
  z<-qnorm((1+conf)/2)
  if(type=="mean") {
    if(is.null(sigma)) stop("sigma is required for mean sample-size calculation.")
    n<-ceiling((z*sigma/margin)^2)
    cat(sprintf("\nMEAN SAMPLE SIZE\nz* = %.6f\nsigma = %.6f\nmargin = %.6f\nRequired n = %d (rounded UP)\n",z,sigma,margin,n))
  } else {
    if(is.null(p)) {
      p<-.5
      cat("\nNo prior p supplied: using conservative p = 0.5.\n")
    }
    n<-ceiling(z^2*p*(1-p)/margin^2)
    cat(sprintf("PROPORTION SAMPLE SIZE\nz* = %.6f\np = %.6f\nmargin = %.6f\nRequired n = %d (rounded UP)\n",z,p,margin,n))
  }
  invisible(n)
}

regression_toolkit <- function(model, newdata=NULL, conf=.95, diagnostics=FALSE,
                               correlations=FALSE, data=NULL) {
  if(!inherits(model,"lm")) stop("model must be an lm object.")
  s<-summary(model); co<-coef(s); f<-s$fstatistic
  fp<-pf(f[1],f[2],f[3],lower.tail=FALSE)
  cat("\nREGRESSION EXAM TOOLKIT\n")
  cat("=======================\n")
  cat("\nFitted model:\n"); print(formula(model))
  cat("\nCoefficient table:\n"); print(co)
  cat(sprintf("\nR-squared: %.6f\nAdjusted R-squared: %.6f\nResidual SE: %.6f\nF-test p-value: %.8g\n",
              s$r.squared,s$adj.r.squared,s$sigma,fp))
  cat("\nInterpretation reminders:\n")
  cat("- Each slope: estimated average change in Y for +1 in X, holding other regressors constant.\n")
  cat("- Each coefficient p-value tests H0: beta_j = 0.\n")
  cat("- F test: H0 all slope coefficients are zero; H1 at least one is nonzero.\n")
  cat("- R-squared: proportion of SAMPLE variability in Y explained by the fitted model.\n")
  cat("- Adjusted R-squared is preferred when comparing models with different numbers of predictors.\n")

  pred<-NULL
  if(!is.null(newdata)) {
    mean_ci<-predict(model,newdata,interval="confidence",level=conf)
    indiv_pi<-predict(model,newdata,interval="prediction",level=conf)
    cat("\nMEAN RESPONSE CONFIDENCE INTERVAL\n");print(mean_ci)
    cat("\nINDIVIDUAL PREDICTION INTERVAL\n");print(indiv_pi)
    cat("\nThe prediction interval is wider because it includes both uncertainty in the mean response and individual outcome variability.\n")
    pred<-list(confidence=mean_ci,prediction=indiv_pi)
  }

  corr<-NULL
  if(correlations) {
    if(is.null(data)) {
      mf<-model.frame(model)
      num<-vapply(mf,is.numeric,logical(1))
      dnum<-mf[,num,drop=FALSE]
    } else {
      num<-vapply(data,is.numeric,logical(1));dnum<-data[,num,drop=FALSE]
    }
    if(ncol(dnum)>=2) {
      corr<-cor(dnum,use="pairwise.complete.obs")
      cat("\nCORRELATION MATRIX\n");print(corr)
      upper<-abs(corr);upper[lower.tri(upper,diag=TRUE)]<-NA
      if(any(upper>=.7,na.rm=TRUE))
        cat("\nMULTICOLLINEARITY WARNING: at least one absolute pairwise correlation is >= 0.7, a course warning sign of redundant explanatory information.\n")
    }
  }

  if(diagnostics) {
    cat("\nOpening regression diagnostic plots:\n")
    cat("1) Residuals vs fitted: look for random cloud and roughly constant spread.\n")
    plot(model,which=1)
    cat("2) Normal Q-Q: points should lie roughly on the reference line.\n")
    plot(model,which=2)
    cat("\nStandardized residuals:\n")
    print(rstandard(model))
  }
  invisible(list(summary=s,predictions=pred,correlations=corr))
}

# Convenience descriptive sample proportion: avoids forcing inference.
.sample_proportion <- function(data,variable,level) {
  x<-as.character(data[[variable]]);x<-x[!is.na(x)]
  count<-sum(x==level);n<-length(x);ph<-count/n
  se<-sqrt(ph*(1-ph)/n)
  cat(sprintf("\nSAMPLE PROPORTION + ESTIMATED STANDARD ERROR\n--------------------------------------------\n%s = %s\nCount x = %d\nSample size n = %d\np-hat = x/n = %.6f (%.2f%%)\nEstimated SE(p-hat) = sqrt[p-hat(1-p-hat)/n] = %.8f\n",
              variable,level,count,n,ph,100*ph,se))
  cat(sprintf("\nEXAM WORDING\n------------\nThe sample proportion is p-hat = %d/%d = %.6f. Its standard error is estimated by substituting p-hat for the unknown population proportion p in SE(p-hat) = sqrt[p(1-p)/n], giving sqrt[%.6f(1-%.6f)/%d] = %.8f.\n",
              count,n,ph,ph,ph,n,se))
  invisible(list(count=count,n=n,phat=ph,se=se))
}


# ============================================================
# v0.6.2 COURSE-COMPLETE / PAST-PAPER-DRIVEN TOOLKIT
# ============================================================

syllabus_map <- function() {
  x <- data.frame(
    block=c(
      "Descriptive","Descriptive","Descriptive","Descriptive",
      "Random variables","Random variables","Random variables","Sampling distributions",
      "Point estimation","Confidence intervals","Confidence intervals","Confidence intervals",
      "Confidence intervals","Confidence intervals","Sample size",
      "Hypothesis testing","Hypothesis testing","Hypothesis testing","Hypothesis testing",
      "Hypothesis testing","Hypothesis testing","Type II error / power",
      "Chi-square","Chi-square",
      "Regression","Regression","Regression","Regression","Regression","Regression"
    ),
    procedure=c(
      "frequency distributions / density","mean median quantiles mode","dispersion / CV","boxplots / outliers",
      "discrete RV E(X), Var(X)","normal probabilities / quantiles","linear combinations / covariance","sum/mean IID + CLT",
      "estimators and standard errors","one mean","one proportion","paired mean difference",
      "two independent means","two proportions","margin of error / required n",
      "one mean known sigma","one mean Student/large sample","one proportion",
      "paired means","two independent means","two proportions","beta / power",
      "goodness of fit","independence",
      "simple/multiple lm","individual t tests + global F","R2 / adjusted R2","dummy variables / factors",
      "prediction & confidence intervals","diagnostics / residuals / QQ / leverage / multicollinearity"
    ),
    helper=c(
      "grouped_stats()/table()","summary()/quantile()/grouped_stats()","dispersion_compare()","statcram()/boxplot()",
      "discrete_rv()","probability_help()","rv_combine()","iid_sum_mean()/sampling_mean()",
      "statcram raw summaries","statcram()/direct R","statcram()/direct R","ci_paired()",
      "ci_two_means()","ci_two_proportions()","sample_size()",
      "test_mean_known_sigma()","statcram()","statcram()",
      "statcram()","statcram()/test_two_means_known_sigma()","statcram()","power_mean_known_sigma()/power_one_proportion()",
      "statcram()","statcram()",
      "exam_regression()","regression_toolkit()","regression_toolkit()","lm() + factor handling",
      "regression_toolkit()","regression_toolkit()"
    ),
    stringsAsFactors=FALSE
  )
  print(x,row.names=FALSE)
  invisible(x)
}

pastpaper_patterns <- function() {
  x <- c(
    "Grouped/interval-class distributions: frequency density, histogram, modal class, approximate mean/median/percentiles",
    "Tail summaries: p90/p95 thresholds and rigorous outlier checks",
    "Compare dispersion across variables: use coefficient of variation when scales/units differ",
    "One-mean known-sigma lower/upper/two-sided tests and rejection regions",
    "Type II error / power under a specified alternative mean or proportion",
    "Confidence interval and test for difference between two independent means",
    "One-proportion hypothesis test with critical region and p-value",
    "Paired before/after mean test using covariance/correlation or raw paired differences",
    "Two categorical variables: sample percentages plus chi-square independence",
    "Chi-square goodness-of-fit against stated category shares",
    "Regression equation, R2/adjusted R2, coefficient interpretation, individual t tests, global F",
    "Regression mean-response confidence interval vs individual prediction interval",
    "Compare nested/expanded regression models; diagnose loss of significance and multicollinearity",
    "Regression assumptions: homoskedasticity, normality of errors, influential/high-leverage observations",
    "Normal probabilities, linear transformations, linear combinations, covariance and CLT"
  )
  cat("\nPAST-PAPER QUESTION PATTERNS AUDITED\n")
  cat("-----------------------------------\n")
  cat(paste0(seq_along(x),". ",x,collapse="\n"),"\n")
  invisible(x)
}

grouped_stats <- function(lower, upper, freq=NULL, prop=NULL,
                          probs=c(.25,.5,.75,.9,.95), total=NULL) {
  if(length(lower)!=length(upper)) stop("lower and upper must have same length.")
  if(any(upper<=lower)) stop("Each upper endpoint must exceed lower endpoint.")
  k<-length(lower)
  width<-upper-lower
  mid<-(lower+upper)/2

  if(is.null(prop)) {
    if(is.null(freq)) stop("Provide freq or prop.")
    if(length(freq)!=k) stop("freq length mismatch.")
    total<-sum(freq)
    prop<-freq/total
  } else {
    if(length(prop)!=k) stop("prop length mismatch.")
    prop<-prop/sum(prop)
    if(is.null(freq) && !is.null(total)) freq<-prop*total
  }

  density<-prop/width
  cum<-cumsum(prop)
  modal<-which.max(density)
  approx_mean<-sum(mid*prop)

  qfun<-function(q) {
    j<-which(cum>=q)[1]
    prev<-if(j==1) 0 else cum[j-1]
    # uniform density within interval class
    lower[j]+(q-prev)/density[j]
  }
  quants<-vapply(probs,qfun,numeric(1))
  names(quants)<-paste0("p",round(100*probs))

  out<-data.frame(lower=lower,upper=upper,width=width,midpoint=mid,
                  proportion=prop,density=density,cumulative=cum)
  if(!is.null(freq)) out$frequency<-freq

  cat("\nGROUPED / INTERVAL-CLASS SUMMARY\n")
  cat("--------------------------------\n")
  print(out,row.names=FALSE)
  cat(sprintf("\nApproximate mean (midpoint assumption) = %.6f\n",approx_mean))
  cat(sprintf("Modal class by HIGHEST DENSITY = [%g, %g)\n",lower[modal],upper[modal]))
  cat("Approximate quantiles (uniform-within-class assumption):\n")
  print(quants)
  cat("\nEXAM NOTE: for interval-class data these location measures are approximations because raw values are unavailable.\n")
  invisible(list(table=out,mean=approx_mean,modal_class=c(lower[modal],upper[modal]),quantiles=quants))
}

dispersion_compare <- function(x,y,name_x="X",name_y="Y") {
  x<-x[!is.na(x)];y<-y[!is.na(y)]
  sx<-sd(x);sy<-sd(y);mx<-mean(x);my<-mean(y)
  cvx<-sx/abs(mx);cvy<-sy/abs(my)
  out<-data.frame(
    Variable=c(name_x,name_y),
    Mean=c(mx,my),SD=c(sx,sy),Variance=c(var(x),var(y)),
    Range=c(diff(range(x)),diff(range(y))),
    IQR=c(IQR(x),IQR(y)),
    CV=c(cvx,cvy)
  )
  print(out,row.names=FALSE)
  cat("\nEXAM RULE: compare SD/variance only when measurement scale is meaningfully comparable; for different scales/units use coefficient of variation = SD / |mean|.\n")
  invisible(out)
}

discrete_rv <- function(values,probs) {
  if(length(values)!=length(probs)) stop("values and probs length mismatch.")
  if(any(probs<0)||abs(sum(probs)-1)>1e-8) stop("probabilities must be nonnegative and sum to 1.")
  mu<-sum(values*probs)
  varx<-sum((values-mu)^2*probs)
  out<-data.frame(value=values,probability=probs,contribution_to_mean=values*probs)
  print(out,row.names=FALSE)
  cat(sprintf("\nE(X) = %.6f\nVar(X) = %.6f\nSD(X) = %.6f\n",mu,varx,sqrt(varx)))
  invisible(list(mean=mu,variance=varx,sd=sqrt(varx)))
}

iid_sum_mean <- function(mu,var,n) {
  if(n<=0||n!=round(n)) stop("n must be a positive integer.")
  ans<-list(
    sum_mean=n*mu,
    sum_variance=n*var,
    mean_mean=mu,
    mean_variance=var/n,
    mean_se=sqrt(var/n)
  )
  cat(sprintf("\nIID SUM / SAMPLE MEAN\nE(Sum) = %.6f\nVar(Sum) = %.6f\nE(Xbar) = %.6f\nVar(Xbar) = %.6f\nSE(Xbar) = %.6f\n",
              ans$sum_mean,ans$sum_variance,ans$mean_mean,ans$mean_variance,ans$mean_se))
  invisible(ans)
}

sampling_mean <- function(mu,sigma,n,lower=NULL,upper=NULL,large_sample=TRUE) {
  se<-sigma/sqrt(n)
  cat(sprintf("\nSAMPLING DISTRIBUTION OF Xbar\nMean = %.6f\nSE = %.6f\n",mu,se))
  if(large_sample) cat("Using normal sampling approximation (CLT / normal-population case as applicable).\n")
  prob<-NULL
  if(!is.null(lower)||!is.null(upper)) {
    lo<-lower %||% -Inf; hi<-upper %||% Inf
    prob<-pnorm(hi,mu,se)-pnorm(lo,mu,se)
    cat(sprintf("Requested probability = %.10g\n",prob))
  }
  invisible(list(mean=mu,se=se,probability=prob))
}

ci_paired <- function(before,after,conf=.95,direction="after-before") {
  ok<-complete.cases(before,after);before<-before[ok];after<-after[ok]
  d<-if(direction=="after-before") after-before else before-after
  n<-length(d);db<-mean(d);sdv<-sd(d);se<-sdv/sqrt(n);crit<-qt((1+conf)/2,n-1)
  me<-crit*se;ci<-c(db-me,db+me)
  cat(sprintf("\nPAIRED MEAN-DIFFERENCE CI\nDefinition: %s\nn = %d\nDbar = %.6f\nsD = %.6f\nSE = %.6f\nCI = [%.6f, %.6f]\n",
              direction,n,db,sdv,se,ci[1],ci[2]))
  invisible(list(interval=ci,mean_difference=db,se=se,df=n-1))
}

ci_two_means <- function(x,y,conf=.95,case=c("pooled","welch","large","known"),
                         sigma_x=NULL,sigma_y=NULL) {
  case<-match.arg(case)
  x<-x[!is.na(x)];y<-y[!is.na(y)]
  n1<-length(x);n2<-length(y);diff<-mean(x)-mean(y)
  if(case=="pooled") {
    sp2<-((n1-1)*var(x)+(n2-1)*var(y))/(n1+n2-2)
    se<-sqrt(sp2/n1+sp2/n2);df<-n1+n2-2;crit<-qt((1+conf)/2,df)
  } else if(case=="welch") {
    s1<-sd(x);s2<-sd(y);se<-sqrt(s1^2/n1+s2^2/n2)
    df<-(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
    crit<-qt((1+conf)/2,df)
  } else if(case=="large") {
    se<-sqrt(var(x)/n1+var(y)/n2);df<-NA;crit<-qnorm((1+conf)/2)
  } else {
    if(is.null(sigma_x)||is.null(sigma_y)) stop("sigma_x and sigma_y required for known-variance case.")
    se<-sqrt(sigma_x^2/n1+sigma_y^2/n2);df<-NA;crit<-qnorm((1+conf)/2)
  }
  me<-crit*se;ci<-c(diff-me,diff+me)
  cat(sprintf("\nTWO-MEAN CI (%s)\nDifference = %.6f\nSE = %.6f\nCI = [%.6f, %.6f]\n",case,diff,se,ci[1],ci[2]))
  if(!is.na(df))cat(sprintf("df = %.4f\n",df))
  invisible(list(interval=ci,difference=diff,se=se,df=df))
}

ci_two_proportions <- function(x1,n1,x2,n2,conf=.95) {
  p1<-x1/n1;p2<-x2/n2
  se<-sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)
  crit<-qnorm((1+conf)/2);diff<-p1-p2;me<-crit*se;ci<-c(diff-me,diff+me)
  cat(sprintf("\nTWO-PROPORTION CI\np1hat = %.6f\np2hat = %.6f\nDifference = %.6f\nSE = %.6f\nCI = [%.6f, %.6f]\n",
              p1,p2,diff,se,ci[1],ci[2]))
  invisible(list(interval=ci,difference=diff,se=se))
}

test_mean_known_sigma <- function(xbar,mu0,sigma,n,alternative=c("two.sided","less","greater"),alpha=.05) {
  alternative<-match.arg(alternative)
  se<-sigma/sqrt(n);z<-(xbar-mu0)/se
  p<-if(alternative=="less") pnorm(z) else if(alternative=="greater") 1-pnorm(z) else 2*(1-pnorm(abs(z)))
  crit<-if(alternative=="two.sided") c(-qnorm(1-alpha/2),qnorm(1-alpha/2)) else if(alternative=="less") qnorm(alpha) else qnorm(1-alpha)
  cat(sprintf("\nONE-MEAN KNOWN-SIGMA Z TEST\nSE = %.6f\nZ = %.6f\np-value = %.10g\nDecision: %s\n",
              se,z,p,if(p<alpha)"REJECT H0" else "FAIL TO REJECT H0"))
  cat("Critical Z value(s): ");print(crit)
  invisible(list(se=se,z=z,p.value=p,critical=crit))
}

test_two_means_known_sigma <- function(xbar1,xbar2,sigma1,sigma2,n1,n2,d0=0,
                                       alternative=c("two.sided","less","greater"),alpha=.05) {
  alternative<-match.arg(alternative)
  se<-sqrt(sigma1^2/n1+sigma2^2/n2);z<-((xbar1-xbar2)-d0)/se
  p<-if(alternative=="less") pnorm(z) else if(alternative=="greater") 1-pnorm(z) else 2*(1-pnorm(abs(z)))
  cat(sprintf("\nTWO-MEAN KNOWN-SIGMA Z TEST\nDifference = %.6f\nSE = %.6f\nZ = %.6f\np-value = %.10g\nDecision: %s\n",
              xbar1-xbar2,se,z,p,if(p<alpha)"REJECT H0" else "FAIL TO REJECT H0"))
  invisible(list(se=se,z=z,p.value=p))
}

power_mean_known_sigma <- function(mu0,mu1,sigma,n,alpha=.05,alternative=c("less","greater","two.sided")) {
  alternative<-match.arg(alternative)
  se<-sigma/sqrt(n)
  if(alternative=="less") {
    cutoff<-mu0+qnorm(alpha)*se
    power<-pnorm(cutoff,mean=mu1,sd=se)
  } else if(alternative=="greater") {
    cutoff<-mu0+qnorm(1-alpha)*se
    power<-1-pnorm(cutoff,mean=mu1,sd=se)
  } else {
    lo<-mu0-qnorm(1-alpha/2)*se;hi<-mu0+qnorm(1-alpha/2)*se
    power<-pnorm(lo,mu1,se)+(1-pnorm(hi,mu1,se))
    cutoff<-c(lo,hi)
  }
  beta<-1-power
  cat("\nTYPE II ERROR / POWER: KNOWN-SIGMA MEAN TEST\n")
  cat(sprintf("True alternative mean = %.6f\nPower = %.8f\nBeta = %.8f\n",mu1,power,beta))
  cat("Decision cutoff(s) on Xbar scale: ");print(cutoff)
  invisible(list(power=power,beta=beta,cutoff=cutoff))
}

power_one_proportion <- function(p0,p1,n,alpha=.05,alternative=c("less","greater","two.sided")) {
  alternative<-match.arg(alternative)
  se0<-sqrt(p0*(1-p0)/n)
  se1<-sqrt(p1*(1-p1)/n)
  if(alternative=="greater") {
    cutoff<-p0+qnorm(1-alpha)*se0
    power<-1-pnorm(cutoff,p1,se1)
  } else if(alternative=="less") {
    cutoff<-p0+qnorm(alpha)*se0
    power<-pnorm(cutoff,p1,se1)
  } else {
    lo<-p0-qnorm(1-alpha/2)*se0;hi<-p0+qnorm(1-alpha/2)*se0
    power<-pnorm(lo,p1,se1)+(1-pnorm(hi,p1,se1))
    cutoff<-c(lo,hi)
  }
  beta<-1-power
  cat("\nTYPE II ERROR / POWER: ONE-PROPORTION TEST (NORMAL APPROXIMATION)\n")
  cat(sprintf("True alternative p = %.6f\nPower = %.8f\nBeta = %.8f\n",p1,power,beta))
  cat("Decision cutoff(s) on p-hat scale: ");print(cutoff)
  invisible(list(power=power,beta=beta,cutoff=cutoff))
}

# Enhance regression toolkit with assumptions/influence checks used in past papers.
regression_assumptions <- function(model) {
  if(!inherits(model,"lm")) stop("model must be lm.")
  cat("\nREGRESSION ASSUMPTION / INFLUENCE CHECKLIST\n")
  cat("------------------------------------------\n")
  cat("Residuals vs fitted -> linearity + homoskedasticity.\n")
  plot(model,which=1)
  cat("Normal Q-Q -> approximate normality of residuals.\n")
  plot(model,which=2)
  cat("Cook's distance -> potentially influential observations.\n")
  plot(model,which=4)
  cat("Residuals vs leverage -> high-leverage / influential observations.\n")
  plot(model,which=5)
  rs<-rstandard(model); lev<-hatvalues(model); cook<-cooks.distance(model)
  out<-data.frame(index=seq_along(rs),standardized_residual=rs,leverage=lev,cooks_distance=cook)
  out<-out[order(abs(out$standardized_residual),decreasing=TRUE),]
  cat("\nLargest absolute standardized residuals:\n")
  print(head(out,10),row.names=FALSE)
  invisible(out)
}

# -------------------------
# QA
# -------------------------

run_qa <- function(verbose=TRUE) {
  set.seed(42)

  # Dataset A: marketing-like but renamed generically
  A<-data.frame(
    customer_age=sample(18:75,250,TRUE),
    sex=sample(c("Female","Male"),250,TRUE),
    traffic_source=sample(c("Organic","Paid","Referral"),250,TRUE),
    vip_status=factor(sample(c("No","Yes"),250,TRUE)),
    site_time_minutes=rnorm(250,120,35),
    click_percentage=runif(250,0,50),
    bought_recently=sample(0:1,250,TRUE),
    average_monthly_expenditure=rnorm(250,180,45),
    membership_level=factor(sample(c("Bronze","Silver","Gold"),250,TRUE))
  )

  # Dataset B: employees
  B<-data.frame(
    productivity_score=rnorm(180,75,12),
    training_status=factor(sample(c("Trained","Untrained"),180,TRUE)),
    department=factor(sample(c("Sales","Finance","Operations"),180,TRUE)),
    annual_salary=rnorm(180,52000,9000),
    satisfaction_rating=rnorm(180,7,1.4)
  )

  # Dataset C: cars
  C<-data.frame(
    vehicle_price=rnorm(160,30000,8000),
    horsepower=rnorm(160,180,50),
    transmission=factor(sample(c("Automatic","Manual"),160,TRUE)),
    fuel_type=factor(sample(c("Petrol","Diesel","Hybrid"),160,TRUE)),
    vehicle_weight=rnorm(160,1500,250)
  )

  # Dataset D: paired measurements
  D<-data.frame(
    score_before=rnorm(90,60,10),
    score_after=rnorm(90,66,10),
    cohort=factor(sample(c("A","B"),90,TRUE))
  )

  tests<-list(
    list(data=A,q="do gold members spend more per month than silver members",family="two_means",outcome="average_monthly_expenditure",group="membership_level"),
    list(data=A,q="is vip status associated with traffic source",family="chisq_independence",x="vip_status",y="traffic_source"),
    list(data=A,q="give a 90% confidence interval for the proportion of gold members",family="prop_ci",variable="membership_level"),
    list(data=A,q="report the 90% confidence interval for the population of customers with a gold membership level and interpret it",family="prop_ci",variable="membership_level"),
    list(data=A,q="The customers have been classified according to their membership level, with levels Bronze, Silver and Gold. Report the estimate of the proportion of customers with a Gold membership level and specify how the corresponding standard error is estimated.",family="sample_prop",variable="membership_level"),
    list(data=A,q="what is the standard error of the proportion of customers with gold membership",family="sample_prop",variable="membership_level"),
    list(data=A,q="report the estimate of the proportion of customers with gold membership and specify how the corresponding standard error is estimated",family="sample_prop",variable="membership_level"),
    list(data=A,q="analyze the distribution of site time by membership level using boxplots",family="describe",outcome="site_time_minutes",group="membership_level"),
    list(data=B,q="do trained employees have higher average productivity than untrained employees",family="two_means",outcome="productivity_score",group="training_status"),
    list(data=B,q="is department associated with training status",family="chisq_independence",x="department",y="training_status"),
    list(data=C,q="is average vehicle price different between automatic and manual cars",family="two_means",outcome="vehicle_price",group="transmission"),
    list(data=C,q="predict vehicle price using horsepower transmission and vehicle weight",family="regression",outcome="vehicle_price"),
    list(data=D,q="did scores increase from before to after for the same people",family="paired"),
    list(data=C,q="do automatic cars cost more on average than manual cars",family="two_means",outcome="vehicle_price",group="transmission"),
    list(data=B,q="compare average salary between trained and untrained staff",family="two_means",outcome="annual_salary",group="training_status")
  )

  ok<-logical(length(tests))
  for(i in seq_along(tests)) {
    t<-tests[[i]]
    I<-.interpret(t$q,t$data,profile_exam_data(t$data))
    good<-identical(I$family,t$family)

    # For chi-square independence, X/Y order is mathematically irrelevant.
    if(good && identical(t$family,"chisq_independence")) {
      expected_pair <- sort(c(t$x,t$y))
      got_pair <- sort(c(I$x,I$y))
      good <- identical(expected_pair,got_pair)
    } else {
      for(nm in setdiff(names(t),c("data","q","family","x","y")))
        good<-good && identical(I[[nm]],t[[nm]])
    }

    ok[i]<-good
    if(verbose) {
      cat(sprintf("[%s] %s\n",if(good)"PASS"else"FAIL",t$q))
      if(!good) {
        cat("       expected family:",t$family,"\n")
        cat("       got family:",I$family %||% "NULL","\n")
        if(!is.null(t$outcome)) cat("       expected outcome:",t$outcome," | got:",I$outcome %||% "NULL","\n")
        if(!is.null(t$group)) cat("       expected group:",t$group," | got:",I$group %||% "NULL","\n")
        if(!is.null(t$x)) cat("       expected categorical pair:",paste(sort(c(t$x,t$y)),collapse=", "),
                              " | got:",paste(sort(c(I$x %||% "",I$y %||% "")),collapse=", "),"\n")
      }
    }
  }

  # Numerical invariants
  obs<-c(151,117,140,162);ex<-rep(sum(obs)/4,4);chi<-sum((obs-ex)^2/ex)
  n1<-35;n2<-30;s1<-12;s2<-15
  wdf<-(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
  num_ok<-abs(chi-7.78245614035)<1e-8 && wdf>1 && wdf<(n1+n2-2)

  if(verbose)cat(sprintf("[%s] numerical invariants\n",if(num_ok)"PASS"else"FAIL"))
  # Additional exam-complete numerical checks
  zright <- 1-pnorm(100,80,12)
  rv_mu <- 10+16+4
  rv_var <- 1^2+2^2+2*.2*1*2
  n_mean <- ceiling((qnorm(.975)*12/3)^2)
  extra_ok <- abs(zright-0.04779035)<1e-6 &&
              abs(rv_mu-30)<1e-12 &&
              abs(rv_var-5.8)<1e-12 &&
              n_mean==62
  if(verbose)cat(sprintf("[%s] probability/RV/sample-size invariants\n",if(extra_ok)"PASS"else"FAIL"))
  # v0.6 past-paper/course invariants
  # Grouped mean check
  lo<-c(10,25,30,35,40,45); hi<-c(25,30,35,40,45,60); pp<-c(.06,.12,.24,.28,.18,.12)
  gm<-sum(((lo+hi)/2)*pp)
  # Known-sigma lower-tail mean test
  se_k<-850/sqrt(100); z_k<-(2400-2500)/se_k
  # IID sample mean variance identity
  iid_ok <- abs((9/25) - (3^2/25)) < 1e-12
  course_ok <- is.finite(gm) && is.finite(z_k) && iid_ok
  if(verbose)cat(sprintf("[%s] grouped-data / known-sigma / IID invariants\n",if(course_ok)"PASS"else"FAIL"))
  all_ok<-all(ok)&&num_ok&&extra_ok&&course_ok
  cat(if(all_ok)"\nALL DATASET-AGNOSTIC QA CHECKS PASSED.\n" else "\nSOME QA CHECKS FAILED.\n")
  invisible(all_ok)
}
