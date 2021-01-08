diff2 = function(x) return(c(NA, diff(x)))
diff3 = function(x) return(c(diff(x), 0))

pad_fun = function(x) {
  # Take a ragged list and pad it to matrix with zeros
  # Find out which is the longest bit of X
  nrows = lapply(x, 'nrow') %>% unlist
  max_row = max(nrows)
  size_max = nrow(x[[which.max(nrows)]])
  
  x_padded = matrix(NA, ncol = length(x), nrow = size_max)
  for(i in 1:length(x)) {
    x_padded[,i] = c(diff3(x[[i]][,1]), rep(0, max_row - nrows[i]))
  }
  return(x_padded)
}

# Function extract ane element from a list and tidy it up
grab_all = function(lst, el, name, date) {
  vals = lapply(lst, "[", el)
  
  # Add 0s to each vector to make them the same length
  val_padded = pad_fun(vals)
  
  # Now calculate medians and 90% CI
  val_median = (apply(val_padded, 1, 'quantile', 0.5))
  #val_high = (apply(val_padded, 1, 'quantile', 0.95))
  #val_low = (apply(val_padded, 1, 'quantile', 0.05))
  
  # Final data frame for YR
  nrows = lapply(lst, 'nrow') %>% unlist
  time_max = lst[[which.max(nrows)]]$Time
  dates = as.Date(date) + time_max 
  val_final = tibble::tibble(Date = dates, 
                     `Median` = val_median)
  # `Low5` = val_low,
  # `High95` = val_high)
  names(val_final) = c('Date', name)
  # paste(name,'- Median'),
  # paste(name, '- Low'),
  # paste(name, 'High'))
  return(val_final)
}
get_assumptions_text <- function(){
  HTML(paste0("<div style = 'color: black;'><p> This visualisation shows the standard model of an epidemic ",
  "where an individuals at any point in time can be in one of four states: S, ",
  "susceptible to infection; E, exposed to the infection, and so infectious, but ",
  "asymptomatic; I, infectious and symptomatic; R, recovered from the infection ",
  "and immune to further infection. It is known as an SEIR model.<p> Exposed ",
  "and Infectious people are the main actors in the system. They interact a ",
  "random number of times each day with Susceptible, Exposed, Infectious, and ",
  "Recovered people. The probability that a given interaction is with a ",
  "Susceptible person is the fraction of people in the population that are ",
  "Susceptible at that time. When they interact with a Susceptible person, the ",
  "Susceptible person moves to being Exposed. An interaction with an Exposed, ",
  "Infectious or Recovered person leads to no change in the system. We have ",
  "extended this model to allow for two populations (here represented as under ",
  "65 or over 65) which can mix together at a set rate, which we call the 'Cross ",
  "R number'.<p> Exposed people stay in that state for a random amount of time, ",
  "with an average given by the model parameters, whereupon they become Infectious. ",
  "Infectious people stay in that state for a random amount of time, with an ",
  "average given by the model parameters, whereupon they become Recovered. Once ",
  "there are no Exposed or Infectious people left, the epidemic has ended.<p> As ",
  "the system is stochastic, significant variability occurs when the number of ",
  "Exposed and Infectious people is small. When started with a small number of ",
  "Exposed and Infectious people, there is a chance that the epidemic dies out ",
  "before it can get going, or that it expands into a full-blown epidemic. ",
  "Towards the end of a full blown epidemic, there is significant heterogeneity ",
  "in the time until it ends. The closer the effective replicative value is to ",
  "1, the greater this variability. We have suppressed this variability in these ",
  "plots, but they are available in some of other apps.<p> The vaccination ",
  "aspect of the model is induced by instantly moving a set number of people ",
  "from from state S to state R. This continues on a daily basis until there are ",
  "no more people in the S category.<p> The forecasts produced by this system ",
  "are inherently unrealistic. By creating such a prediction and presenting it ",
  "to you makes this forecast less likely to happen. The government are likely ",
  "to act, or people will react by themselves if there are large numbers of ",
  "deaths.<p> The code presented here has been written by academics and not by ",
  "professional coders. It may contain bugs or other mistakes which we have not ",
  "disovered yet. All the code for this app is available in our <a href = 'https:",
  "//github.com/hamilton-institute/covid19ireland'>GitHub</a> repository which we ",
  "encourage you to look at and improve.</p>"))
  
}

get_assumptions_text2 <- function() {
  HTML(paste0("<div style = 'color: black;'><p> This visualisation shows the standard model of an epidemic ",
  "where an individuals at any point in time can be in one of four states: S, ",
  "susceptible to infection; E, exposed to the infection, and so infectious, but ",
  "asymptomatic; I, infectious and symptomatic; R, recovered from the infection ",
  "and immune to further infection. It is known as an SEIR model.<p> Exposed ",
  "and Infectious people are the main actors in the system. They interact a ",
  "random number of times each day with Susceptible, Exposed, Infectious, and ",
  "Recovered people. The probability that a given interaction is with a Susceptible ",
  "person is the fraction of people in the population that are Susceptible at that ",
  "time. When they interact with a Susceptible person, the Susceptible person moves ",
  "to being Exposed. An interaction with an Exposed, Infectious or Recovered person ",
  "leads to no change in the system. We have extended this model to allow for two ",
  "populations (here represented as under 65 or over 65) which can mix together ",
  "at a set rate, which we call the 'Cross R number'.<p> Exposed people stay in ",
  "that state for a random amount of time, with an average given by the model ",
  "parameters, whereupon they become Infectious. Infectious people stay in that ",
  "state for a random amount of time, with an average given by the model parameters, ",
  "whereupon they become Recovered. Once there are no Exposed or Infectious people ",
  "left, the epidemic has ended.<p> As the system is stochastic, significant ",
  "variability occurs when the number of Exposed and Infectious people is small. ",
  "When started with a small number of Exposed and Infectious people, there is a ",
  "chance that the epidemic dies out before it can get going, or that it expands ",
  "into a full-blown epidemic. Towards the end of a full blown epidemic, there ",
  "is significant heterogeneity in the time until it ends. The closer the ",
  "effective replicative value is to 1, the greater this variability. We have ",
  "suppressed this variability in these plots, but they are available in some of ",
  "other apps.<p> The vaccination aspect of the model is induced by splitting up ",
  "the Susceptible compartments into those awaiting vaccine, those who have been ",
  "vaccinated but  are awaiting its effects, and those it has failed (either ",
  "through refusal or through ineffectiveness). Similarly the Removed compartment ",
  "is split up into those who have been removed due to having the disease, and ",
  "those who have been removed due to being vaccinated.<p> The forecasts ",
  "produced by this system are inherently unrealistic. By creating such a ",
  "prediction and presenting it to you makes this forecast less likely to happen. ",
  "The government are likely to act, or people will react by themselves if there ",
  "are large numbers of deaths.<p> The code presented here has been written by ",
  "academics and not by professional coders. It may contain bugs or other mistakes ",
  "which we have not disovered yet. All the code for this app is available in ",
  "our <a href = 'https://github.com/hamilton-institute/covid19ireland'>GitHub</a> ",
  "repository which we encourage you to look at and improve."))
}