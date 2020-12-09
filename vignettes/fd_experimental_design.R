## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata)
)

## ----distrubance-assignment, echo = TRUE, results = 'hide'---------------
#set random seed
set.seed(9925)

#define parameters
w <- c("top", "bottom")
x <- c("E", "W")  
y <- c(1, 2, 3, 4)
z <- c(0, 45, 65, 85)

# example treatment using group A
# all other groups simulated in identical fashion
# then merged at the end of file and written to .csv
plot.disturbance.a <- data.frame(group = "A", plot = sample(y), disturbance = sample(z))
plot.disturbance.a <- plot.disturbance.a[order(plot.disturbance.a$plot),]

# randomization of how which side E = east or W = west is chosen for
# top = top-down or bottom - bottom-up treatment
plot.treatment <- data.frame(plot.side = sample(w), treatment = sample(x))

