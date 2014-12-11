library('dplyr')

load_and_cleanup_outcomes <- function(path) {
  if (!exists("cached_outcomes")) {
    if(!file.exists(path))
      stop(paste("file:'", path,"' doesn't exist !"))

    outcomes <- read.csv(path,colClasses = "character")
    names(outcomes) <- sapply(names(outcomes),tolower)
    #message(outcome) <- sapply()
    for (i in c(11,13:17,19:23,25:29,31:35,37:41,43:46)) {
      outcomes[,i] <- as.numeric(outcomes[,i])
    }
    cached_outcomes <<- outcomes
    outcomes
  } else {
    message("Using cache !")
    cached_outcomes
  }
}


rankhospital <- function(state, outcome, num = "best") {
  outcome <- tolower(outcome)
  ## Read outcome data
  outcomes <- load_and_cleanup_outcomes('data/outcome-of-care-measures.csv')
  ## Check that state and outcome are valid
  if(!any(outcomes$state == state))
    stop("invalid state")

  state_outcomes <- outcomes[outcomes$state == state,]

  # replace space with \\. in the outcome name, so matching is easier
  pat_outcome <- gsub(' ','\\.',outcome)

  # search for matching columns in the outcome names
  matching_cols <- grep(pat_outcome,names(outcomes),ignore.case=T)
  if(length(matching_cols) == 0)
    stop("invalid outcome")

  param_name <- paste0('hospital.30.day.death..mortality..rates.from.',gsub(' ','.',outcome))
  message(paste("parameter:", param_name))
  # state-specific outcomes
  state_outcomes <- outcomes[outcomes$state == state,]
  # filter bad outcomes
  bad <- is.na(state_outcomes[param_name])
  state_outcomes <- state_outcomes[!bad,]
  
  ## Return hospital name in that state with the given rank
  sorted_outcomes <- arrange(state_outcomes,state_outcomes$param_name,hospital.name)
  sorted_outcomes$rank <- rank(sorted_outcomes[param_name],ties.method="first")
  
  ## 30-day death rate
  if (!is.numeric(num)) {
    if (num == "best")
      num <- 1
    else if (num == "worst")
      num <- max(sorted_outcomes$rank)
  }
  if(num < 1 || num > max(sorted_outcomes$rank))
    return(NA)
  sorted_outcomes[sorted_outcomes$rank==num,]$hospital.name
}
