# Britt Besch
# Student ID: 12400991
# Ludwig-Maxmilians-Universität
# P19: Practical Course in Empirical Research Methods II: “Formal Modeling of Psychological Theories”



#------------------------------------------------------------
# single path formulae
#------------------------------------------------------------

#'Compute updated pos_reinforcement after a session/no session of BA
#'
#' @param ba Behavioral Activation, binary (0 or 1)
#' @param pos_reinforcement, extent of a person's positive reinforcement
#' @return Updated value of pos_reinforcement after applying the BA logic.
predict_pos_reinforcement <- function(ba, pos_reinforcement) {
  if (ba == 1) {
    # If person receives BA, increase pos_reinforcement linearly up to 20 sessions
    pos_reinforcement = min(pos_reinforcement + (0.5/20), 1)
  } else {
    # If person does not receive BA, pos_reinforcement decreases
    pos_reinforcement = max(pos_reinforcement - (0.1/20), 0)
  }
  return(pos_reinforcement)
}


#'Compute (updated) depression score 
#'
#' @param pos_reinforcement, extent of a person's positive reinforcement
#' @return Computed depression score based on the pos_reinforcement.
predict_depression <- function(pos_reinforcement) {
  # If pos_reinforcement is 0, then depression is 1
  # If pos_reinforcement is 1, then depression is 0
  # If pos_reinforcement is 0.5, then depression is 0.5
  # assuming linear relationship where depression = 1 - pos_reinforcement
  depression = 1-pos_reinforcement
  return(depression)
}

#------------------------------------------------------------
# Updating a person's values
#------------------------------------------------------------

#' Update a person's values on pos_reinforcement and depression
#' 
#' @param ba Behavioral Activation, binary (0 or 1)
#' @param pos_reinforcement, extent of a person's positive reinforcement
#' @param depression, extent of a person's severity of depression
#' @return A numeric vector containing the updated pos_reinforcement and depression values.
update_person <- function(ba, pos_reinforcement, depression) {
  
  pos_reinforcement <-  predict_pos_reinforcement(ba, pos_reinforcement) # update pos_reinforcement
  depression <- predict_depression(pos_reinforcement)# update depression score
  
  return(c(pos_reinforcement, depression))
}


#------------------------------------------------------------
# generate group of people
#------------------------------------------------------------

#' Generate a group of people with or without BA and their initial pos_reinforcement and depression
#' 
#' @param n The number of people to generate.
#' @return A dataframe with columns ba, pos_reinforcement, and depression for each generated person.
generate_people <- function(n) {
  set.seed(123) # Set seed for reproducibility
  people <- data.frame(
    ba = as.integer(runif(n) > 0.5), # 1 for BA, 0 for no BA, randomized
    pos_reinforcement = 0.8 * rbeta(n, shape1 = 5, shape2 = 3))
    # Draw from a beta distribution and apply the linear transformation
  people$depression <- predict_depression(people$pos_reinforcement)
  return(people)
}


#------------------------------------------------------------
# simulation
#------------------------------------------------------------

#' Simulate the complete BA intervention over a specified number of sessions
#' 
#' @param people Dataframe with initial values for ba, pos_reinforcement, and depression.
#' @param sessions Number of BA sessions to simulate.
#' @return The input dataframe updated with pos_reinforcement and depression scores after simulating all sessions.
complete_BA <- function(people, sessions) {
  for (i in seq(nrow(people))) {
    for (j in seq(sessions)) {
      results <- update_person(people$ba[i], people$pos_reinforcement[i], people$depression[i])
      people$pos_reinforcement[i] <- results[1]
      people$depression[i] <- results[2]
    }
  }
  return(people)
}

#' Simulate a study of BA intervention
#' 
#' @param n Number of people to include in the simulation.
#' @param sessions Number of sessions of BA to apply.
#' @return A dataframe of simulated people with their final pos_reinforcement and depression scores after the specified number of BA sessions.
simulate_study <- function(n, sessions) {
  people <- generate_people(n)
  # Complete BA interventions
  people <- complete_BA(people, sessions)
  
  return(people)
}

#------------------------------------------------------------
# "Conduction" of the study
#------------------------------------------------------------
sessions = 10
n = 1000
people_treated <- simulate_study(n, sessions)
head(people_treated, 10)



#------------------------------------------------------------
# evaluation of the study
#------------------------------------------------------------
#plots for showing the functional relationship of BA, pos_reinforcement and depression
library(ggplot2)
people_start <- generate_people(100)
head(people_start)


## Plotting relationship between pos_reinforcement & depression
ggplot(people_start, aes(x = pos_reinforcement, y = depression))+
  geom_point()+ 
  labs(x = "Pos_reinforcement",
       y = "Depression",
       title = "Relationship Between Pos_reinforcement and Depression")


## Plotting relationship between BA/no BA and pos_reinforcement over time/sessions
# Create a sequence of session numbers
num_session <-seq(20) 
results <- data.frame()

# Iterate over a range of sessions
for (session in num_session) { 
  for (i in seq(nrow(people_start))) {
    # Update pos_reinforcement and depression for each person
    updated_values <- update_person(people_start$ba[i], people_start$pos_reinforcement[i], people_start$depression[i])
    people_start$pos_reinforcement[i] <- updated_values[1]
    people_start$depression[i] <- updated_values[2]
  }
  # Store the updated values along with the session number
  session_data <- cbind(people_start, session = rep(session, nrow(people_start)))
  results <- rbind(results, session_data)
}

# Plot the results
ggplot(results, aes(x = session, y = pos_reinforcement, color = as.factor(ba), group = ba)) +
  geom_point()+
  labs(title = "Change in Positive Reinforcement Over Sessions",
       x = "Session Number",
       y = "Positive Reinforcement", 
       color = "Condition") +
  scale_color_manual(values = c('red', 'blue'), labels = c('No BA', 'BA')) +
  theme_minimal()
  


# t-test to calculate the differences in mean depression after "delivering" the BA
t.test(depression ~ ba, data = people_treated)
t1 <-  t.test(depression ~ ba, data = people_treated)


#Calculate effect size
library(compute.es)
ES <- tes(t1$statistic, n/2, n/2, level = 95, verbose=FALSE)
ES$d

# install.packages('ggrain')
library(ggrain)
ggplot(people_treated, 
       aes(x = factor(as.character(ba), levels = c(0,1), labels = c("No BA", "BA")), 
           y = depression)) + 
  geom_rain() + 
  ggtitle(paste0("Effect size: d = ", round(ES$d, 2)))+
  labs(x = "Condition",
       y = "Depression")

