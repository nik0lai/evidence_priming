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

# pretty_bf <- function(bf) {
#   if (bf > 100) {
#     return('>100')
#   } else if (bf > 3) {
#     return(round(bf))
#   } else if (bf > 0) {
#     return(round(bf, 2))
#   } else {
#     return(round(bf, 3))
#   }
# }

pretty_bf <- function(bf) {
  if (!is.numeric(bf)) {
    stop(sprintf("`bf` should be numeric, not '%s'", class(bf)))
  }
  
  if (bf > 100) {
    exponent <- round(log10(bf))
    # return(paste0("paste('>', 10^", exponent, ')'))
    return(paste0("paste(10^", exponent, ')'))
  } else if (bf > 10) {
    return(as.character(round(bf, 0)))
  } else {
    return(as.character(round(bf, 2)))
  }
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
    mutate(rhat = value <= 1.01) %>% 
    reframe(rhat = all(rhat))  %>% 
    pull(rhat)
}