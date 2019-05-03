# Function check_prob()
#Check if the parameter prob is valid
check_prob <- function(prob){
  if(prob>=0 & prob<=1){
    return(TRUE)
  }else{
    stop("prob has to be a number between 0 and 1")
  }
}


library(FRACTION)
# Function check_trials()
#Check if the parameter trials is valid
check_trials <- function(trials){
  if(is.integer(trials)==TRUE & trials>=0){
    return(TRUE)
  }else{
    stop("trials has to be an non-negative integer")
  }
}

#Function check_success()
# Test if an input success is a valid value for number of successes

check_success <- function(success,trials){
  if(is.integer(success)==TRUE & all(success>=0) & max(success)<=trials){
        return(TRUE)
      }else if(max(success)>trials){
        stop("success cannot be greater than trials")
      }else{
        stop("success has to be an non-negative integer")
      }
}



#Private Auxiliary Functions
#aux_mean()
#Culculate the mean of a binomial distribution
aux_mean <- function(trials, prob){
  aux_mean <- trials*prob
  return(aux_mean)
}
#aux_variance()
#Culculate the variance of the binomial distribution
aux_variance <- function(trials,prob){
  aux_variance <- trials*prob*(1-prob)
  return(aux_variance)
}

#aux_mode()

#calculate the mode of the binomial distribution
aux_mode <- function(trials,prob){
  library(FRACTION)
  if(is.wholenumber(trials*prob+prob)==TRUE){
    aux_mode <- c(floor(trials*prob+prob),floor(trials*prob+prob)-1)
  }else{
    aux_mode <- floor(trials*prob+prob)
  }
  return(aux_mode)
}
#aux_skewness()
#calculate the skewness of the binomial distribution
aux_skewness <- function(trials,prob){
  aux_skewness <- (1-2*prob)/sqrt(aux_variance(trials,prob))
  return(aux_skewness)
}



#aux_kurtosis()
#calculate the kurtosis of the binomial distribution
aux_kurtosis <- function(trials,prob){
  aux_kurtosis <- (1-6*prob*(1-prob))/sqrt(aux_variance(trials,prob))
  return(aux_kurtosis)
}







# Function bin_choose()
#' @title binomial choosing function
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n the number of trial (integer)
#' @param k the number of success (integer)
#' @return the number of comnination
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)

bin_choose <- function(n,k){

  if(max(k)>n){
    stop("k cannot be greater than n")
  }else{
    n=rep(n,length(k))
    bin_choose <- factorial(n)/(factorial(k)*factorial(n-k))
  }
  return(bin_choose)

}

# Function bin_probability()
#' @title binomial probability function
#' @description calculates the probabilities of the specific times of successes can occur in trials
#' @param success the number of success (integer)
#' @param trials the number of totoal trials (integer)
#' #' @param prob the possibility of each success (numeric)
#' @return the probability
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)



bin_probability <- function(success,trials,prob){
  #check if the success,trials and prob are valid
  check_prob(prob)
  check_success(success,trials)
  check_trials(trials)
  # if all parameters are valid then do the next job
  bin_probability <-bin_choose(trials,success)*(prob^success)*((1-prob)^(trials-success))
  return(bin_probability)
}

# Function bin_distribution()
#' @title binomial distribution function
#' @description show the distribution of probabilities of each success in n trials
#' @param trials the number of totoal trials (integer)
#' @param prob the possibility of each success (numeric)
#' @return a dataframe probabilities distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials,prob){
  a=0:trials
  bin_distribution <- bin_probability(a,trials,prob)
  tbl <- data.frame(success=a,probability=bin_distribution)
  class(tbl) <- c("bindis","data.frame")
  return(tbl)
}

# plot.bindis()
#' @export
plot.bindis <- function(x){
  barplot(x$probability,col='#888831',main="Histogram of binomial distribution",
          xlab="successes",ylab="probability")

}

# Function bin_cumulative()
#' @title binomial distribution function
#' @description show the distribution of probabilities of each success in n trials
#' and the cumulative probabilities.
#' @param trials the number of totoal trials (integer)
#' @param prob the possibility of each success (numeric)
#' @return a dataframe of probabilities distribution
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials,prob){
  tbl<- bin_distribution(trials,prob)
  tbl <- cbind(tbl,cumulative=cumsum(tbl$probability))
  class(tbl) <- c("bincum","data.frame")
  return(tbl)
}

# plot.bincum()
#' @export
plot.bincum <- function(x){
  plot(x=x$success,y=x$cumulative,type="b",main="plot of binomial cumulative distribution",xlab="successes",ylab="probability")
}

#Function bin_variable()
#' @title binomial distribution function
#' @description summary methods of binomial distribution
#' @param trials the number of totoal trials (integer)
#' @param prob the possibility of each success (numeric)
#' @return an object of class "binvar", that is, a binomial random variable object.
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials,prob){
  #Check if the parameter trials and prob are valid
  check_prob(prob)
  check_trials(trials)

  object <- list(trials=trials,prob=prob)
  class(object) <- "binvar"
  object
}

#method print.binvar()
#' @export
print.binvar <- function(x,...){

  cat("\n'Binomial variable'\n")
  cat('\nParameters')
  cat('\n-number of trials: ', x$trials[1])
  cat('\n-prob of success: ', x$prob[1], "\n")
  invisible(x)
}

#Methods summary.binvar()

#' @export


summary.binvar <- function(x,...){
  mean <- aux_mean(x$trials[[1]],x$prob[[1]])
  variance <- aux_variance(x$trials[[1]],x$prob[[1]])
  mode <- aux_mode(x$trials[[1]],x$prob[[1]])
  skewness <- aux_skewness(x$trials[[1]],x$prob[[1]])
  kurtosis <- aux_kurtosis(x$trials[[1]],x$prob[[1]])

  Parameters <-list(trials=x$trials[[1]],prob=x$prob[[1]])
  Measures <- list(mean=mean,variance=variance,
                   mode=mode,skewness=skewness,
                   kurtosis=kurtosis)
  obj <- list(Parameters=Parameters,
              Measures=Measures)
  class(obj) <- "summary.binvar"
  obj

}



#' @export
print.summary.binvar <- function(x,...){
  cat("\n'Summary Binomial'")
  cat("\nParameters")
  cat('\n-number of trials: ', x$Parameters[[1]])
  cat('\n-prob of success: ', x$Parameters[[2]], "\n")
  cat('\nMeasures')
  cat('\n-mean    : ', x$Measures[[1]])
  cat('\n-variance: ', x$Measures[[2]])
  cat('\n-mode    : ', x$Measures[[3]])
  cat('\n-skewness: ', x$Measures[[4]])
  cat('\n-kurtosis: ', x$Measures[[5]])

  invisible(x)
}

#Main Functions
#bin_mean
#' @title binomial mean function
#' @description Culculate the mean of a binomial distribution
#' @param trials the number of trial (integer)
#' @param prob the probability of success (integer)
#' @return the mean
#' @export

bin_mean <- function(trials, prob){
  check_prob(prob)
  check_trials(trials)
  bin_mean <- aux_mean(trials,prob)
  return(bin_mean)
}

#Culculate the variance of the binomial distribution
#' @title binomial variance function
#' @description Culculate the vaiance of a binomial distribution
#' @param trials the number of trial (integer)
#' @param prob the probability of success (integer)
#' @return the vaiance
#' @export

bin_variance <- function(trials,prob){
  check_prob(prob)
  check_trials(trials)
  bin_variance <- aux_variance(trials,prob)
  return(bin_variance)

}

#bin_mode()
#calculate the mode of the binomial distribution
#' @title binomial mode function
#' @description Culculate the vaiance of a binomial distribution
#' @param trials the number of trial (integer)
#' @param prob the probability of success (integer)
#' @return the mode
#' @export

bin_mode <- function(trials,prob){
  check_prob(prob)
  check_trials(trials)
  bin_mode <- aux_mode(trials,prob)
  return(bin_mode)
}
#bin_skewness()
#' @title binomial skewness function
#' @description Culculate the skewness of a binomial distribution
#' @param trials the number of trial (integer)
#' @param prob the probability of success (integer)
#' @return the skewness
#' @export
bin_skewness <- function(trials,prob){
  check_prob(prob)
  check_trials(trials)
  bin_skewness <- aux_skewness(trials,prob)
  return(bin_skewness)
}



#bin_kurtosis()
#calculate the kurtosis of the binomial distribution#' @title binomial variance function
#' @title binomial kurtosis function
#' @description Culculate the kurtosis of a binomial distribution
#' @param trials the number of trial (integer)
#' @param prob the probability of success (integer)
#' @return the kurtosis
#' @export
bin_kurtosis <- function(trials,prob){
  check_prob(prob)
  check_trials(trials)
  bin_kurtosis <- aux_kurtosis(trials,prob)
  return(bin_kurtosis)
}



