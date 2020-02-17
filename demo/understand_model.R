# Libraries
library(ggplot2)
library(dplyr)
#library(patchwork) # To display 2 charts together
#library(hrbrthemes)


## Read the "project.rds" file stored by RMV2.0
rds_file <- "C:/RMV2.0 Workshop/uncertain_savings3/Project_02.13.rds"
Project <- readRDS(rds_file)
cat("Your project has the following models.",fill=T)
print(names(Project$model_obj_list$models_list))

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

## Remove these variables from the workspace
## Comment out these lines if you want to inspect further
rm(rds_file, i, df, df2)
detach("towt_model")
rm(Project)
