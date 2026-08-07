get_sdt <- function(df) {
  n_signal <- sum(df$x_stim)
  n_noise <- sum(!df$x_stim)
  p_hit <- sum(df$y_behav[df$x_stim==1])/n_signal
  p_far <- sum(df$y_behav[df$x_stim==0])/n_noise
  return(sdt(pHit = p_hit, pFA = p_far, nSignal = n_signal, nNoise = n_noise)$ideal_obs)
}

sdt <- function(pHit, pFA, nHits, nFA, cormethod = 'hautus', nSignal = NULL, nNoise = NULL) {
  
  # Check for 'hautus' or 'macmillan' corrections without necessary arguments
  if (!is.null(cormethod) && (cormethod == 'hautus' || cormethod == 'macmillan') && (is.null(nSignal) || is.null(nNoise))) {
    stop('Need to pass nSignal and nNoise trials to function to apply Hautus or MacMillan correction')
  }
  
  # Apply correction methods
  if (cormethod == 'arbitrary') {
    pHit[pHit == 1] <- 0.99999
    pFA[pFA == 1] <- 0.99999
    pHit[pHit == 0] <- 0.00001
    pFA[pFA == 0] <- 0.00001
  } else if (cormethod == 'hautus') {
    nHits <- pHit * nSignal + 0.5
    nFA <- pFA * nNoise + 0.5
    pHit <- nHits / (nSignal + 1)
    pFA <- nFA / (nNoise + 1)
  } else if (cormethod == 'macmillan') {
    # Adjust pHit and pFA according to MacMillan method
    adjust_values <- function(p, n) {
      p[p == 1] <- (n[p == 1] - 0.5) / n[p == 1]
      p[p == 0] <- 0.5 / n[p == 0]
      return(p)
    }
    pHit <- adjust_values(pHit, nSignal)
    pFA <- adjust_values(pFA, nNoise)
  }
  
  # Convert to Z scores
  zHit <- qnorm(pHit)
  zFA <- qnorm(pFA)
  
  # Calculate d-prime
  d <- zHit - zFA
  
  # Calculate BETA if needed
  yHit <- dnorm(zHit)
  yFA <- dnorm(zFA)
  beta <- yHit / yFA
  
  # Calculate criterion c
  criterion_c <- -(zHit + zFA) / 2
  
  # Calculate the ideal criterion and ideal observer score
  propS <- nSignal / (nSignal + nNoise)
  propN <- 1 - propS
  ideal_c <- log(propN / propS)
  if (d == 0) {
    ideal_l = 0
  } else {
    ideal_l <- d / 2 + ideal_c / d
  }
  ideal_obs <- propS * (1 - pnorm(ideal_l - d)) + propN * pnorm(ideal_l)
  
  # Return a list of results
  return(tibble(d = d, beta = beta, criterion_c = criterion_c, ideal_c = ideal_c, ideal_obs = ideal_obs))
}

pretty_num <- function(x, digits = 2) {
  as.character(round(x, digits))
}

pretty_bf <- function(bf) {
  if (!is.numeric(bf)) {
    stop(sprintf("`bf` should be numeric, not '%s'", class(bf)))
  }
  
  if (is.infinite(bf)) {
    return("BF~10~ > 10^40^")
  } else if (bf > 100) {
    exponent <- round(log10(bf))
    return(paste0("BF~10~ = 10^", exponent, "^"))
  } else if (bf > 10) {
    return(paste0("BF~10~ = ", pretty_num(bf, 0)))
  } else if (bf > 1) {
    return(paste0("BF~10~ = ", pretty_num(bf, 2)))
  } else {
    return(paste0("BF~10~ = ", pretty_num(bf, 3)))
  }
}

pretty_d <- function(d) {
  pretty_num(d, 2)
}

soa_ms <- function(soa) {
  soa * 1000
}

pretty_bf_number <- function(bf, output = c("markdown", "ggplot")) {
  # Ensure 'output' is either "markdown" or "ggplot"
  output <- match.arg(output)
  
  # Check that bf is numeric
  if (!is.numeric(bf)) {
    stop(sprintf("`bf` should be numeric, not '%s'", class(bf)))
  }
  
  # Handle infinite Bayes factors as > 10^40
  if (is.infinite(bf)) {
    if (output == "ggplot") {
      # Expression-style label for ggplot
      return('paste(">", 10^40)')
    } else {
      # Markdown-style label
      return(">10^40^")
    }
  }
  
  # For very large finite BFs, report as 10^k
  if (bf > 100) {
    # Order of magnitude
    exponent <- round(log10(bf))
    
    if (output == "ggplot") {
      # Expression-style label for ggplot
      return(paste0("10^", exponent))
    } else {
      # Markdown-style label with superscript
      return(paste0("10^", exponent, "^"))
    }
  }
  
  # For BFs between 10 and 100, no decimals
  if (bf > 10) {
    return(pretty_num(bf, 0))
  }
  
  # For BFs between 1 and 10, two decimals
  if (bf > 1) {
    return(pretty_num(bf, 2))
  }
  
  # For BFs <= 1, three decimals
  pretty_num(bf, 3)
}

get_label_bf <- function(bf) {
  bf <- round(bf, 2)
  if (bf >= 10) {
    return('Effect')
  } else if (bf <= .1) {
    return("No-Effect")
  } else (
    return('Indecisive')
  )
}

make_soa_factor <- 
  function(df) {
    # get values, sort and make text
    soa_values <- df %>% mutate(soa = soa * 1000) %>% pull(soa) %>% unique() %>% sort() %>% as.character()
    # convert soa to factor
    return(df %>% 
      mutate(soa = soa * 1000) %>% 
      mutate(soa = factor(soa, levels=soa_values)))
  }

format_participant_session <- function(df) {
  df %>% 
    mutate(participant = sprintf('%03d', as.integer(participant)),
           session = sprintf('%02d', as.integer(session)))
}

# function to check that all r-hat values are lower than 1.01
check_all_rhat <- function(fit) {
  rhat(fit) %>% 
    enframe %>% 
    filter(str_detect(name, 'L_1', negate = TRUE)) %>% 
    mutate(rhat = value <= 1.01) %>% 
    reframe(rhat = all(rhat))  %>% 
    pull(rhat)
}

make_trim_table_group <- function(comp) {
  
  format_d <- function(x) {
    as.character(round(x, 2))
  }
  
  table_comp <-
    comp %>%
    mutate(
      SOA = paste0(soa * 1000, "&nbsp;ms"),
      trim = recode(
        trim,
        "2sd"   = "2%",
        "2.3sd" = "1%",
        "2.5sd" = "0.5%"
      ),
      stat = recode(
        stat,
        "bf"      = "BF~10~",
        "cohensd" = "*d*"
      ),
      value_fmt = case_when(
        stat == "BF~10~" ~ purrr::map_chr(value, ~ pretty_bf_number(.x, output = "markdown")),
        stat == "*d*"    ~ format_d(value),
        TRUE ~ as.character(value)
      )
    ) %>%
    select(SOA, stat, trim, value_fmt) %>%
    pivot_wider(
      names_from = trim,
      values_from = value_fmt
    ) %>%
    arrange(
      as.numeric(gsub("&nbsp;ms", "", SOA)),
      factor(stat, levels = c("BF~10~", "*d*"))
    ) %>%
    select(SOA, stat, `0.5%`, `1%`, `2%`) %>%
    mutate(
      SOA = if_else(stat == "*d*", "", SOA)
    )
  
  knitr::kable(
    table_comp,
    format = "pipe",
    align = c("r", "l", "c", "c", "c")
  )
}

make_trim_table_subject <- function(comp, participants_per_block = 2) {
  
  format_d <- function(x) {
    as.character(round(x, 1))
  }
  
  table_comp <-
    comp %>%
    mutate(
      Pp = as.integer(participant),
      SOA = as.character(soa * 1000),
      trim = recode(
        trim,
        "2sd"   = "2%",
        "2.3sd" = "1%",
        "2.5sd" = "0.5%"
      ),
      stat = recode(
        stat,
        "bf"      = "BF~10~",
        "cohensd" = "*d*"
      ),
      value_fmt = case_when(
        stat == "BF~10~"  ~ purrr::map_chr(value, ~ pretty_bf_number(.x, output = "markdown")),
        stat == "*d*" ~ format_d(value),
        TRUE ~ as.character(value)
      )
    ) %>%
    select(Pp, SOA, stat, trim, value_fmt) %>%
    pivot_wider(
      names_from = trim,
      values_from = value_fmt
    ) %>%
    arrange(
      Pp,
      as.numeric(SOA),
      factor(stat, levels = c("BF~10~", "*d*"))
    ) %>%
    select(Pp, SOA, stat, `0.5%`, `1%`, `2%`) %>%
    mutate(
      SOA = if_else(stat == "*d*", "", SOA)
    )
  
  table_comp <-
    table_comp %>%
    group_by(Pp) %>%
    mutate(
      Pp_display = if_else(row_number() == 1, as.character(Pp), "")
    ) %>%
    ungroup()
  
  participants <- unique(table_comp$Pp)
  
  table_comp <-
    table_comp %>%
    mutate(
      participant_num = match(Pp, participants),
      block = ceiling(participant_num / participants_per_block)
    ) %>%
    group_by(block) %>%
    mutate(
      row_in_block = row_number()
    ) %>%
    ungroup() %>%
    select(
      block,
      row_in_block,
      Pp = Pp_display,
      SOA,
      stat,
      `0.5%`,
      `1%`,
      `2%`
    )
  
  blocks <- sort(unique(table_comp$block))
  max_rows <- max(table_comp$row_in_block)
  
  table_wide <-
    purrr::map(blocks, function(b) {
      
      x <-
        table_comp %>%
        filter(block == b) %>%
        select(-block)
      
      x <-
        tibble(row_in_block = seq_len(max_rows)) %>%
        left_join(x, by = "row_in_block") %>%
        select(-row_in_block)
      
      x <-
        x %>%
        mutate(
          across(everything(), ~ replace_na(as.character(.x), ""))
        )
      
      names(x) <- paste0(names(x), "_", b)
      
      x
    }) %>%
    purrr::reduce(dplyr::bind_cols)
  
  display_names <- rep(
    c("Pp", "SOA", "stat", "0.5%", "1%", "2%"),
    length(blocks)
  )
  
  knitr::kable(
    table_wide,
    format = "pipe",
    col.names = display_names,
    align = rep(c("r", "r", "l", "c", "c", "c"), length(blocks))
  )
}

# get bf10 for a given predictor (inverts the null BF to get the alternative BF)
get_bf_alt <- function(f, predictor) {
  # get hyp object
  hyp_0 <- hypothesis(f, paste0(predictor, '=0'))
  # get bf
  bf_null <- hyp_0$hypothesis$Evid.Ratio
  # get bf alt
  bf_alt <- exp(-log(abs(bf_null)))
  
  return(bf_alt)
}

# get bf10 for the mask congruency predictor
get_bf_alt_mask <- function(f) {
  # get hyp object
  hyp_0 <- hypothesis(f, 'x_congruent=0')
  # get bf
  bf_null <- hyp_0$hypothesis$Evid.Ratio
  # get bf alt
  bf_alt <- exp(-log(abs(bf_null)))
  
  return(bf_alt)
}

# format BF into "10^x^" style for markdown tables
format_bf <- function(bf) {
  if (is.na(bf)) return(NA)
  sci <- scales::scientific(bf, digits = 0)
  if (grepl("e", sci)) {
    parts <- strsplit(sci, "e")[[1]]
    exp <- as.numeric(parts[2])
    paste0("10^", exp, "^")
  } else {
    sprintf("%.2f", as.numeric(bf))
  }
}

# build a named vector mapping participant IDs to "Prefix N (k sessions)" labels
make_participant_label <- function(df, prefix = "Participant") {
  df %>%
    select(participant, session) %>%
    distinct() %>%
    group_by(participant) %>%
    count() %>%
    mutate(label = sprintf('%s %s (%s sessions)', prefix, as.integer(participant), n)) %>%
    select(-n) %>%
    deframe()
}

# build a wide-format BF / Cohen's d markdown table per participant x SOA
build_participant_bf_table <- function(data, convert_soa_to_ms = TRUE) {
  table_long <-
    data %>%
    select(participant, soa, bf, cohensd) %>%
    arrange(participant, soa) %>%
    mutate(
      participant = as.integer(participant),
      soa_ms = paste0(if (convert_soa_to_ms) soa * 1000 else soa, " ms"),
      bf_fmt = vapply(bf, pretty_bf_number, character(1)),
      d_fmt  = sprintf("%.1f", cohensd)
    ) %>%
    mutate(bf_fmt = str_remove(str_remove(bf_fmt, 'paste\\('), '\\)')) %>%
    select(participant, soa_ms, bf_fmt, d_fmt) %>%
    pivot_wider(
      names_from = soa_ms,
      values_from = c(bf_fmt, d_fmt)
    ) %>%
    arrange(participant)

  table_bf <- table_long %>%
    select(participant, starts_with("bf_fmt")) %>%
    rename_with(~gsub("bf_fmt_", "", .x))

  table_d <- table_long %>%
    select(participant, starts_with("d_fmt")) %>%
    rename_with(~gsub("d_fmt_", "", .x))

  table_final <- bind_rows(
    table_bf %>% mutate(measure = "BF~10~"),
    table_d %>% mutate(measure = "*d*")
  ) %>%
    relocate(measure, .after = participant)

  table_final <- table_final %>%
    arrange(participant, desc(measure)) %>%
    rename(P = participant, stat = measure)

  names(table_final) <- gsub(" ms", "&nbsp;ms", names(table_final))

  kable(table_final, format = "markdown")
}

get_sdt_by_soa <- function(dat) {
  
  dat %>%
    summarise(
      n_hit  = sum(x_stim == 1 & y_behav == 1, na.rm = TRUE),
      n_miss = sum(x_stim == 1 & y_behav == 0, na.rm = TRUE),
      n_fa   = sum(x_stim == 0 & y_behav == 1, na.rm = TRUE),
      n_cr   = sum(x_stim == 0 & y_behav == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      sdt = list(psycho::dprime(
        n_hit  = n_hit,
        n_fa   = n_fa,
        n_miss = n_miss,
        n_cr   = n_cr,
        adjusted = TRUE
      ))
    ) %>%
    unnest_wider(sdt) %>%
    ungroup() %>%
    select(
      n_hit, n_miss, n_fa, n_cr,
      dprime,
      criterion = c,
      beta
    )
}