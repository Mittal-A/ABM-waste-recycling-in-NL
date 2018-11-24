library(reshape2)
library(ggplot2)
library(plotly)

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
# write.csv(result_df, "edited_results_final_1000.csv", row.names = F)

result_df$behavior_price_tendency = factor(result_df$behavior_price_tendency, levels(result_df$behavior_price_tendency)[c(4,5,6,1,2,3)])


# GENERAL EXPLORATION -----------------------------------------------------

exp_plot = result_df[seq(1,nrow(result_df),90),]
ggplot(exp_plot, aes(y=kpi_expenditure, x=kpi_failure_count)) + geom_point(color='steelblue2') + facet_grid(. ~ technology.increase) + 
  labs( x = "Failure count (KPI)", y = "Expenditure (KPI)")

ggplot(result_df, aes(x=(technology.increase), y=kpi_expenditure, fill = (technology.increase))) + geom_violin() + 
  labs(fill = "Technology Increase", x = "Technology increase", y = "Expenditure (KPI)")
ggplot(result_df, aes(x=(technology.increase), y=kpi_expenditure, fill = (technology.increase))) +  geom_boxplot() +
  labs(fill = "Technology Increase", x = "Technology increase", y = "Expenditure (KPI)")


ggplot(result_df, aes(x = technology.increase, y=kpi_failure_count, fill = (technology.increase))) + geom_violin() + 
  labs(fill = "Technology Increase", y = "Failure count (KPI)", x = "Technology increase")
ggplot(result_df, aes(x=(technology.increase), y=kpi_failure_count, fill = (technology.increase))) +  geom_boxplot() +
  labs(fill = "Technology Increase", y = "Failure count (KPI)", x = "Technology increase")

#

# SOLUTION ACCEPTANCE LIMIT -----------------------------------------------
gs_expenditure_fraction = 0.85
gs_failure_fraction = 0.85
#
# SUCCESS RATE OF DIFFERENT SOLUTIONS -------------------------------------

worst_case_df = result_df[result_df$technology.increase == 0.5,]

worst_case_df$s = "BS"
worst_case_df$s[worst_case_df$kpi_expenditure <= gs_expenditure_fraction * max(worst_case_df$kpi_expenditure) &
                  worst_case_df$kpi_failure_count < gs_failure_fraction * max(worst_case_df$kpi_failure_count)] = "GS"

length(worst_case_df$s[worst_case_df$s=="GS"])

output_space = data.frame(table(worst_case_df[5:11]))
output_space = subset(output_space, !(behavior_importance_tendency == 1 & behavior_knowledge_tendency == 1)&
                        !(behavior_importance_tendency == 0.5 & behavior_knowledge_tendency == 0.5) & 
                        !(behavior_importance_tendency == 0.75 & behavior_knowledge_tendency == 1) &
                        !(behavior_importance_tendency == 1 & behavior_knowledge_tendency == 0.75) &
                        !(behavior_importance_tendency == 0.5 & behavior_knowledge_tendency == 0.75) &
                        !(behavior_importance_tendency == 0.75 & behavior_knowledge_tendency == 0.5) &
                        Freq > 0)

length(output_space$s[output_space$s=="BS"])
length(output_space$s[output_space$s=="GS"])

output_space_gs = output_space[duplicated(output_space[1:6]),]
output_space_bs = output_space[!duplicated(output_space[1:6]),]

output_space = merge(output_space_bs, output_space_gs, all.x = T, all.y = T,by = colnames(output_space)[1:6])
output_space$robustness.ratio = output_space$Freq.y / (output_space$Freq.x + output_space$Freq.y)
output_space$robustness[output_space$robustness.ratio >= 0.5] = "Robust"
output_space$robustness[output_space$robustness.ratio < 0.5] = "Un-robust"
output_space$robustness[is.na(output_space$robustness.ratio)] = "Un-robust"


length(output_space$robustness[output_space$robustness=="Robust"])

analyzed_result_df = merge(result_df, output_space, all.x = T, all.y = F,by = colnames(output_space)[1:6])



# PLOTTING ----------------------------------------------------------------

ggplot(analyzed_result_df, aes(x=(robustness), y=kpi_expenditure, fill = (technology.increase))) + geom_violin() + 
  geom_hline(yintercept = gs_expenditure_fraction * max(result_df$kpi_expenditure)) + 
  labs(fill = "Technology Increase", x = "Robustness", y = "Expenditure (KPI)")
ggplot(analyzed_result_df, aes(x=(robustness), y=kpi_expenditure, fill = (technology.increase))) +  geom_boxplot() +
  geom_hline(yintercept = gs_expenditure_fraction * max(result_df$kpi_expenditure)) +
  labs(fill = "Technology Increase", x = "Robustness", y = "Expenditure (KPI)")


ggplot(analyzed_result_df, aes(x=(robustness), y=kpi_failure_count, fill = (technology.increase))) + geom_violin() +
  geom_hline(yintercept = gs_failure_fraction * 240) + labs(fill = "Technology Increase", x = "Robustness", y = "Failure count (KPI)")
ggplot(analyzed_result_df, aes(x=(robustness), y=kpi_failure_count, fill = (technology.increase))) +  geom_boxplot() +
  geom_hline(yintercept = gs_failure_fraction * 240) + labs(fill = "Technology Increase", x = "Robustness", y = "Failure count (KPI)")

#
# PLOTTING GOOD SOLUTION DISTRIBUTION -------------------------------------

plot_df = output_space[output_space$robustness == "Robust",]
plot_df[,6] = as.numeric(as.logical(plot_df[,6]))
plot_df[,5] = as.numeric(as.logical(plot_df[,5]))

for (i in 1:4) {
  plot_df[,i] = as.numeric(as.character(plot_df[,i]))
}
plot_df$colorval = 1:nrow(plot_df)

options(viewer=NULL)


plot_ly(data = plot_df, type = 'parcoords',  line = list(color = ~colorval,colorscale="Viridis"),
          dimensions = list(
            list(range = c(0.5,1), label = 'Importance', values = ~behavior_importance_tendency, tickvals = c(0.5,0.75,1), ticktext = c("0.5","0.75","1")),
            list(range = c(0.5,1), label = 'Knowledge', values = ~behavior_knowledge_tendency, tickvals = c(0.5,0.75,1), ticktext = c("0.5","0.75","1")),
            list(range = c(3,18), label = 'Price', values = ~behavior_price_tendency, tickvals = c(3,6,9,12,15,18)),
            list(range = c(0.25,1), label = 'Target', values = ~behavior_target_tendency, tickvals = c(0.25,0.5,0.75,1), 
                 ticktext = c("0.25", "0.5","0.75","1")),
            list(range = c(0,1), label = 'Status quo', values = ~behavior_sq_investment, ticktext = c("True", "False"), tickvals = c(1,0)),
            list(range = c(0,1), label = 'Centralized?', values = ~behavior_centralized, ticktext = c("True", "False"), tickvals = c(1,0))
          )
  )%>%
  layout(margin = list(r = 50), font = list(size = 18))






# EXPLORATION INCLUDING ROBUSTNESS ----------------------------------------

non_robust_result = analyzed_result_df[analyzed_result_df$robustness=="Un-robust",]
robust_results = analyzed_result_df[analyzed_result_df$robustness=="Robust",]
exp_plot = rbind(non_robust_result[seq(1,nrow(non_robust_result),90),], robust_results[seq(1,nrow(robust_results),5),])
ggplot(exp_plot, aes(y=kpi_expenditure, x=kpi_failure_count, color = robustness)) + geom_point() + geom_vline(xintercept = gs_failure_fraction * 240) + 
  geom_hline(yintercept = gs_expenditure_fraction * max(result_df$kpi_expenditure)) + facet_grid(. ~ technology.increase) + 
  labs(x = "Failure count (KPI)", y = "Expenditure (KPI)", color = "Robustness")
#
analyzed_result_df[seq(1,nrow(analyzed_result_df),90),]