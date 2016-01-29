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

rankstate <- function(outcomes,state,param,num) {
  state_outcomes <- outcomes[outcomes$state == state, c('state','hospital.name',param)]
  state_outcomes <- state_outcomes[!is.na(state_outcomes[[param]]),]
  state_outcomes <- arrange(state_outcomes,state_outcomes[[param]],hospital.name)
  state_outcomes$rank <- rank(state_outcomes[[param]],ties.method='first')
  if(!is.numeric(num)) {
    if(num == "best")
      num = 1
    else if (num == "worst")
      num = length(state_outcomes$rank)
    else
      error(paste0("invalid num specified !"))
  }
  state_outcomes[num,'hospital.name']
}

rankall <- function(outcome, num = "best") {
  outcome <- tolower(outcome)
  ## Read outcome data
  outcomes <- load_and_cleanup_outcomes('data/outcome-of-care-measures.csv')

  # replace space with \\. in the outcome name, so matching is easier
  pat_outcome <- gsub(' ','\\.',outcome)

  # search for matching columns in the outcome names
  matching_cols <- grep(pat_outcome,names(outcomes),ignore.case=T)
  if(length(matching_cols) == 0)
    stop("invalid outcome")

  param_name <- paste0('hospital.30.day.death..mortality..rates.from.',gsub(' ','.',outcome))
  message(paste("parameter:", param_name))
  
  #result <- NULL
  #for (st in sort(unique(outcomes$state))) {
  #  hospital <- rankstate(outcomes,st,param_name,num)
  #  result <- rbind(result,data.frame(state=st,hospital=hospital))
  #}
  #result
  
  ranked <- outcomes %>% 
    select(state,hospital.name,eval(param_name)) %>%
    group_by(state) %>%
    mutate(rank = row_number(eval(param_name)))
  
  if(!is.numeric(num)){
    if(num == "best")
      num = 1
    else if(num == "worst")
      # we'll use eval(later) on num 
      # because we are hackers and know about dynamic programming ;-)
      num = as.call(list(max,rank))
  }
  ranked %>% 
    select(state,hospital.name) %>% 
    filter(rank == eval(num)) %>%
    arrange(state)  
}
