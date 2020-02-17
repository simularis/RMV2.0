# Libraries
library(ggplot2)
library(dplyr)
#library(patchwork) # To display 2 charts together
#library(hrbrthemes)


## Read the "project.rds" file stored by RMV2.0
#rds_file <- "C:/RMV2.0 Workshop/uncertain_savings3/Project_02.13.rds"
#rds_file <- "C:/RMV2.0 Workshop/uncertain_savings4/Project_02.16.rds"
rds_file <- "C:/RMV2.0 Workshop/uncertain_screening1/Project_02.17.rds"
Project <- readRDS(rds_file)
cat("Your project has the following models.",fill=T)
print(names(Project$model_obj_list$models_list))

View(Project)

# Pick a model from the models_list by number, then load its variables into the workspace
i <- 1
attach(Project$model_obj_list$models_list[[i]]$towt_model,name="towt_model")

## Use built-in library for plotting all weighting functions together, very slowly
#plot(timeVec, WeightMatrix[1,], type='l', ylab='WeightMatrix[j,]')
#for (j in 2:length(WeightMatrix[,1])) {
#  lines(timeVec, WeightMatrix[j,],'l')
#}
#rm(j)

##
# Build dataframe from timeVec (shape [n]) and WeightMatrix (shape [26,n]), transposed
df <- data.frame(
  day = timeVec,
  weights=t(WeightMatrix)
)
# "Melt" from wide table to long table (day, variable, value)
df2 <- reshape2::melt(data=df,id.vars="day")

## Opens spreadsheet style viewer
#View(df)
#View(df2)

## Built-in way to plot all the curves at once, somewhat faster, but with garish colors
#matplot(df,type='l')


## Use modern library to plot weighting functions together
## https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r
ggplot(data = df2, aes(x = day, y = value, colour = variable)) + geom_line() +
  theme(aspect.ratio=0.3)
#ggsave("my_weights_plot.png")

## Proceed with this step only if you have modified the code to save extra variables
#Project$model_obj_list$models_list[[1]]$towt_model$regOutList
nModelRuns <- length(regOutList)
cat(paste("There are ",nModelRuns,"model runs."),fill=T)
for (irun in 1:nModelRuns) {
  cat(paste("Model run ", irun),fill=T)
  regOut <- regOutList[[irun]]

# Explain how coefficients are linked to input variables in model
#print(regOut$amod$terms)
cat("The formula for the regression:",fill=T)
print(deparse(regOut$amod$terms))
cat("The type of each variable:",fill=T)
print(attr(regOut$amod$terms,"dataClasses"))
cat("The temperature knots used to define temperature variables X1, X2, ...:",fill=T)
print(regOut$tempKnots)

cat(paste("Number of parameters in occupied period model (excluding tempKnots)  :",regOut$amod$rank),fill=T)
cat(paste("Number of parameters in unoccupied period model (excluding tempKnots):",regOut$bmod$rank),fill=T)
}

# Show some or all of the coefficients in a table
cat("Regression fit coefficients for occupied periods:")
#print(head(regOut$amod$coefficients))
#print(tail(regOut$amod$coefficients))
print(regOut$amod)
# Result: e.g. ftow33 ... ftow158, X1 ... X5
# Not all ftow factors appear

cat("Regression fit coefficients for unoccupied periods:")
#print(head(regOut$bmod$coefficients))
#print(tail(regOut$bmod$coefficients))
print(regOut$bmod)
# Result: e.g. ftow1 ... ftow168, X1 ... X5


# What information was missing in project files?
# * For towt_models: TempKnots and Model regression (regOut) for each model run (resolved).
# * Note that based on defaults for lm(model=TRUE), the input data frame is saved in regOut$model.
# What remains missing?
# * For towt_models: We are not storing the timestamps used for weights.
#   (pointlist, used for tcenter = dataTime[pointlist[irun]]).
# * The hyperparameter timescaleDays.
# * Also, model improvement suggestion: choose tempKnots via k-means cluster analysis.


## Clean up: remove global variables from workspace and detach scope
## Comment out these lines if you want to inspect further
rm(rds_file, i, df, df2)
detach("towt_model")
rm(Project)
