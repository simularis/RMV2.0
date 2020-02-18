# Libraries
library(ggplot2)
library(dplyr)
library(glue)
#library(patchwork) # To display 2 charts together
#library(hrbrthemes)


## Read the "project.rds" file stored by RMV2.0
#rds_file <- "C:/RMV2.0 Workshop/uncertain_savings3/Project_02.13.rds"
#rds_file <- "C:/RMV2.0 Workshop/uncertain_savings4/Project_02.16.rds"
#rds_file <- "C:/RMV2.0 Workshop/uncertain_screening1/Project_02.17.rds"
#rds_file <- "C:/RMV2.0 Workshop/uncertain_screening2/Project_02.18.rds"
rds_file <- "C:/RMV2.0 Workshop/uncertain_savings5/Project_02.18.rds"
Project <- readRDS(rds_file)
cat("Your project has the following models.",fill=T)
print(names(Project$model_obj_list$models_list))

# Uncomment to have a graphical View appear and show you everything from the project file.
#View(Project)

# These will just print text info summary of the project file structure.
cat("Project file had the following structure:",fill=T)
print(names(Project))
# Typical for screening analysis
# [1] "files_path_sc"      "fahrenheit"         "Data_pre_summary_0" "Data_pre_summary"   "files_names"        "model_obj_list"
# [7] "p_name_sc"          "Data_pre"           "pre_dir_sc"         "files_names_mod"    "Model"
# Typical for screening analysis
# [1] "Data_pre_summary_0"  "load"                "files_names"         "post_names_sav"      "pre_names_sav"
# [6] "Data_pre"            "files_names_mod"     "p_name_sav"          "post_path_sav"       "Data_pre_summary"
# [11] "fahrenheit"          "post_dir_sav"        "model_obj_list"      "pre_path_sav"        "Data_post"
# [16] "Data_post_summary"   "nre_done"            "Data_post_summary_0" "pre_dir_sav"         "Model"

cat("Project file has Data_post? (is savings analysis?)",fill=T)
print("Data_post" %in% names(Project))
isSavings = "Data_post" %in% names(Project)
projectType <- if (isSavings) {"savings!"} else {"screenings!"}

cat("Type of models used?",fill=T)
print(Project$Model)
if (Project$Model != "TOWT") {
  cat("Warning: this script only understands TOWT models.",fill=T)
}

cat("Attempting to understand the first TOWT model. Grokking ...",fill=T)
# Pick a model from the models_list by number, then load its variables into the workspace
i <- 1
attach(Project$model_obj_list$models_list[[i]]$towt_model,name="towt_model")

dataTime <- if (isSavings) {timeVec} else {trainTime}
npoints = length(dataTime)
t0 = min(dataTime,na.rm=T)
t1 = max(dataTime,na.rm=T)
deltaT = as.numeric(difftime(t1,t0,units="days"))
nsegments = max(1,ceiling(deltaT/timescaleDays))
segmentwidth = (npoints-1)/nsegments
segmentDeltaT = deltaT / nsegments

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
figtitle0 <- if (isSavings) {
  "Raw weights for weighted average of baseline models in post-period\n(Denominator is sum of weights)"
  } else {
    "Weights used to generate baseline models\n(Weighted least squares linear regression)"
  }
figtitle <- glue("{figtitle0}
                  deltaT = {format(deltaT,digits=4)} days, \\
                  timescaleDays = {timescaleDays}, \\
                  segmentDays = {format(segmentDeltaT, digits=4)}")
cat(figtitle)
fig <- ggplot(data = df2, aes(x = day, y = value, colour = variable)) + geom_line() +
  theme(aspect.ratio=0.3) +
  ggtitle(figtitle)
print(fig)
ggsave("my_weights_plot.png")
rm(figtitle,fig)

# Note that WeightMatrix and timeVec are for the prediction period.
# For the training data, we are saving only trainTime, not the weights.
# For a savings project, prediction and training time intervals may be different!


## Proceed with this step only if you have modified the code to save extra variables
#Project$model_obj_list$models_list[[1]]$towt_model$regOutList

cat("The hyperparameter timescaleDays",fill=T)
print(timescaleDays)
cat("the index of timestamps used for weights",fill=T)
print(pointlist)
cat("the re-computed timestamps used for weights",fill=T)
#for (point in pointlist) {
#  print(trainTime[point])
#}
tCenters = trainTime[pointlist]
print(tCenters)

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
cat(fill=T)


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
# * For towt_models: We are not storing the timestamps used for weights. (resolved)
#   (pointlist, used for tcenter = dataTime[pointlist[irun]]).
# * The hyperparameter timescaleDays. (resolved)

# What remains missing?
# * Also, model improvement suggestion: choose tempKnots via k-means cluster analysis.


## Clean up: remove global variables from workspace and detach scope
## Comment out these lines if you want to inspect further
rm(rds_file, isSavings, projectType, i, df, df2, tCenters, nModelRuns, irun, regOut)
detach("towt_model")
rm(Project)
