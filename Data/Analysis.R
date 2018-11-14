library(reshape2)
library(ggplot2)
library(plotly)
library(sdtoolkit)
#
# functions ---------------------------------------------------------------

find_expenditure = function(value_string){
  as.numeric(unlist(strsplit(value_string,","))[seq(1,length(value_string) * 9, 9)])
}

find_final_gap = function(value_string){
  as.numeric(unlist(strsplit(value_string,","))[seq(2,length(value_string) * 9,9)])
}

find_importance_tendency = function(value_string){
  as.factor(unlist(strsplit(value_string,","))[seq(3,length(value_string) * 9,9)])
}

find_knowledge_tendency = function(value_string){
  as.factor(unlist(strsplit(value_string,","))[seq(4,length(value_string) * 9,9)])
}

find_target_tendency = function(value_string){
  as.factor(unlist(strsplit(value_string,","))[seq(5,length(value_string) * 9,9)])
}

find_price_tendency = function(value_string){
  as.factor(unlist(strsplit(value_string,","))[seq(6,length(value_string) * 9,9)])
}

find_sq_investment = function(value_string){
  as.logical(toupper(unlist(strsplit(value_string,","))[seq(7,length(value_string) * 9,9)]))
}

find_centralized = function(value_string){
  as.logical(toupper(unlist(strsplit(value_string,","))[seq(8,length(value_string) * 9,9)]))
}

find_failure_count = function(value_string){
  as.numeric(unlist(strsplit(value_string,","))[seq(9,length(value_string) * 9,9)])
}

# READING DATA ------------------------------------------------------------

result_df = read.csv("Project_structure Main experiment - just final result-table.csv", stringsAsFactors = F, skip = 6, header = T)
colnames(result_df)[colnames(result_df) == "X.run.number."] = "run.number"
colnames(result_df)[colnames(result_df) == "X.step."] = "step"

result_df = melt(result_df, id.vars = colnames(result_df)[1:(ncol(result_df)-10)])
result_df = subset(result_df, select = -c(run.number, step, variable))
result_df[,1:(ncol(result_df)-1)] = as.factor(result_df[,1:(ncol(result_df)-1)])

# SEPARATION OF VALUES ----------------------------------------------------

result_df$kpi_expenditure = find_expenditure(result_df$value)
result_df$kpi_final_gap = find_final_gap(result_df$value)
result_df$kpi_failure_count = find_failure_count(result_df$value)
result_df$behavior_importance_tendency = find_importance_tendency(result_df$value)
result_df$behavior_knowledge_tendency = find_knowledge_tendency(result_df$value)
result_df$behavior_target_tendency = find_target_tendency(result_df$value)
result_df$behavior_price_tendency = find_price_tendency(result_df$value)
result_df$behavior_sq_investment = find_sq_investment(result_df$value)
result_df$behavior_centralized = find_centralized(result_df$value)
result_df$kpi_failure_count = find_failure_count(result_df$value)
result_df = subset(result_df, select = -value)
# write.csv(result_df, "edited_results_final_only_20000.csv", row.names = F)

result_df$behavior_price_tendency = factor(result_df$behavior_price_tendency, levels(result_df$behavior_price_tendency)[c(4,5,6,1,2,3)])

# PLOTING -----------------------------------------------------------------

par(mfrow=c(1,2))
plot(temp$behavior_importance_tendency,temp$kpi_expenditure)

ggplot(result_df, aes(x=behavior_importance_tendency, y=kpi_expenditure, color = (technology.increase))) + geom_point() +
  geom_jitter(width = 0.3, height = 0)


ggplot(result_df, aes(x=(behavior_importance_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_violin()
ggplot(result_df, aes(x=(behavior_importance_tendency), y=kpi_expenditure, fill = (technology.increase))) +  geom_boxplot()


ggplot(result_df, aes(x=(behavior_target_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_violin()
ggplot(result_df, aes(x=(behavior_target_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_boxplot()


ggplot(result_df, aes(x=(behavior_price_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_violin()
ggplot(result_df, aes(x=(behavior_price_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_boxplot()

# SCENARIO DISCOVERY ------------------------------------------------------

xmatrix <- result_df[1:5000,(ncol(result_df)-5):(ncol(result_df))]
outputvar <- result_df[1:5000,(ncol(result_df)-8)]

for (i in 1:6) {
  xmatrix[,i] = as.numeric(xmatrix[,i])
}
outputvar = outputvar / max(outputvar)

# outputvar[,2] = (-min(outputvar[,2]) + outputvar[,2]) / (max(outputvar[,2])-min(outputvar[,2]))
# outputvar[,1] = outputvar[,1] / max(outputvar[,1])
# outputvar[,3] = outputvar[,3] / max(outputvar[,3])

myboxes <- sdprim(x=xmatrix, y=outputvar)





data(exboxes)
dimplot(exboxes)
scatterbox(exboxes)

# etc ---------------------------------------------------------------------













ggplot(temp, aes(x=(behavior_importance_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_violin()
ggplot(temp, aes(x=(behavior_importance_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_boxplot()


ggplot(temp, aes(x=(behavior_target_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_violin()
ggplot(temp, aes(x=(behavior_target_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_boxplot()


ggplot(temp, aes(x=(behavior_price_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_violin()
ggplot(temp, aes(x=as.factor(behavior_price_tendency), y=kpi_expenditure, fill = (technology.increase))) + geom_boxplot()


subplot(p1, p2) %>% layout(margin = list(l = 50))





result_df[result_df$run.number==7,]

unique(c(result_df$month.before.technology.increase, result_df$technology.increase))


length(result_df$value)


temp = result_df[1:100,]

temp = read.csv("edited_results_final_only_1000.csv")

temp = result_df

plot_ly(data = result_df, y = ~kpi_expenditure, x = ~behavior_importance_tendency, type = "scatter", mode = "markers")


plot_ly(data = result_df, y = ~kpi_expenditure, x = ~behavior_importance_tendency, type = "violin")


temp[,1] = as.factor(temp[,1])

