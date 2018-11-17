library(reshape2)

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

# INITIALIZATION OF BASE VALUES ------------------------------------------------------------

recycling_target = 0.65
eta_base = 0.35
theta_old= 0.3
theta_single = 0.5
theta_family = 1
theta_couple = 0.8
investment_multiplier = 0.002
betas_decrease_multiplier = 0.0005
investment_cost = 20
recycling_target_increase = 15
num_municipalities = 10
num_RC = 5

Vectorplus10 = c(0.715,	0.385,	0.33,	0.55,	1.1,	0.88,	0.0022,	0.00055,	22,	16.5,	11,	6)
Vectorminus10 =  c(0.585,	0.315,	0.27,	0.45,	0.9,	0.72,	0.0018,	0.00045,	18,	13.5,	9,	4)
Vectorplus25 = c(0.8125,	0.4375,	0.375,	0.625,	1.25,	1,	0.0025,	0.000625,	25,	18.75,	13,	7)
Vectorminus25 = c(0.4875,	0.2625,	0.225,	0.375,	0.75,	0.6,	0.0015,	0.000375,	15,	11.25,	8,	3)

# READING DATA ------------------------------------------------------------
# TOWriteDf = 0

# extract_data = function(file_name, new_value_vector)
# {
  result_df = read.csv("Project_structure Sensitivity Analysis plus 10%-table.csv", stringsAsFactors = F, skip = 6, header = T)
  colnames(result_df)[colnames(result_df) == "X.run.number."] = "run.number"
  colnames(result_df)[colnames(result_df) == "X.step."] = "step"
  
  result_df = melt(result_df, id.vars = colnames(result_df)[1:(ncol(result_df)-10)])
  result_df = subset(result_df, select = -c(run.number, step, variable))
  result_df[,1:(ncol(result_df)-1)] = as.factor(result_df[,1:(ncol(result_df)-1)])
  
  print("Passed")
  
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
#  write.csv(result_df, "edited_results_senstivityminus25%.csv", row.names = F)
  
# SUBSETTING EXPERIMENTS WITH CHANGE IN ONLY ONE BASE VALUE ----------------------------------------------------
  recycling_target_subset = subset(result_df, recycling.target != recycling_target & eta == eta_base
                                            & theta.old == theta_old & theta.single == theta_single 
                                            & theta.family == theta_family & theta.couple == theta_couple 
                                            & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                                            & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                                            & num.municipalities == num_municipalities & num.RC == num_RC)
  
  eta_subset = subset(result_df, recycling.target == recycling_target & eta != eta_base
                      & theta.old == theta_old & theta.single == theta_single 
                      & theta.family == theta_family & theta.couple == theta_couple 
                      & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                      & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                      & num.municipalities == num_municipalities & num.RC == num_RC)
  
  theta_old_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                          & theta.old != theta_old & theta.single == theta_single 
                          & theta.family == theta_family & theta.couple == theta_couple 
                          & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                          & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                          & num.municipalities == num_municipalities & num.RC == num_RC)
  
  theta_single_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                               & theta.old == theta_old & theta.single != theta_single 
                               & theta.family == theta_family & theta.couple == theta_couple 
                               & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                               & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                               & num.municipalities == num_municipalities & num.RC == num_RC)
  
  theta_family_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                               & theta.old == theta_old & theta.single == theta_single 
                               & theta.family != theta_family & theta.couple == theta_couple 
                               & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                               & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                               & num.municipalities == num_municipalities & num.RC == num_RC)
  
  theta_couple_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                               & theta.old == theta_old & theta.single == theta_single 
                               & theta.family == theta_family & theta.couple != theta_couple 
                               & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                               & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                               & num.municipalities == num_municipalities & num.RC == num_RC)
  
  investment_multiplier_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                                        & theta.old == theta_old & theta.single == theta_single 
                                        & theta.family == theta_family & theta.couple == theta_couple 
                                        & investment.multiplier != investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                                        & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                                        & num.municipalities == num_municipalities & num.RC == num_RC)
  
  betas_decrease_multiplier_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                                            & theta.old == theta_old & theta.single == theta_single 
                                            & theta.family == theta_family & theta.couple == theta_couple 
                                            & investment.multiplier == investment_multiplier & betas.decrease.multiplier != betas_decrease_multiplier
                                            & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                                            & num.municipalities == num_municipalities & num.RC == num_RC)
  
  investment_cost_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                                  & theta.old == theta_old & theta.single == theta_single 
                                  & theta.family == theta_family & theta.couple == theta_couple 
                                  & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                                  & investment.cost != investment_cost & recycling.target.increase == recycling_target_increase 
                                  & num.municipalities == num_municipalities & num.RC == num_RC)
  
  recycling_target_increase_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                                            & theta.old == theta_old & theta.single == theta_single 
                                            & theta.family == theta_family & theta.couple == theta_couple 
                                            & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                                            & investment.cost == investment_cost & recycling.target.increase != recycling_target_increase 
                                            & num.municipalities == num_municipalities & num.RC == num_RC)
  
  num_municipalities_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                                     & theta.old == theta_old & theta.single == theta_single 
                                     & theta.family == theta_family & theta.couple == theta_couple 
                                     & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                                     & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                                     & num.municipalities != num_municipalities & num.RC == num_RC)
  
  num_RC_subset = subset(result_df, recycling.target == recycling_target & eta == eta_base
                         & theta.old == theta_old & theta.single == theta_single 
                         & theta.family == theta_family & theta.couple == theta_couple 
                         & investment.multiplier == investment_multiplier & betas.decrease.multiplier == betas_decrease_multiplier
                         & investment.cost == investment_cost & recycling.target.increase == recycling_target_increase 
                         & num.municipalities == num_municipalities & num.RC != num_RC)
  
# CREATION OF SENSITIVITY TABLE/DATA FRAME ----------------------------------------------------
  
  SensitivityDf = data.frame("Variable_Name" = c("recycling target", "eta", "theta old", "theta single",
                                             "theta family", "theta couple", "investment multiplier",
                                             "betas decrease multiplier", "investment cost",
                                             "recycling target increase", "num municipalities",
                                             "num RC"),
                             "Base_Value" = c(recycling_target, eta_base, theta_old, theta_single,
                                              theta_family, theta_couple, investment_multiplier,
                                              betas_decrease_multiplier, investment_cost,
                                              recycling_target_increase, num_municipalities,
                                              num_RC),
                             "Changed_Value" = Vectorplus10,
                             "Expenditure_per_capita" = c(mean(recycling_target_subset$kpi_expenditure),
                                                          mean(eta_subset$kpi_expenditure),
                                                          mean(theta_old_subset$kpi_expenditure),
                                                          mean(theta_single_subset$kpi_expenditure),
                                                          mean(theta_family_subset$kpi_expenditure),
                                                          mean(theta_couple_subset$kpi_expenditure),
                                                          mean(investment_multiplier_subset$kpi_expenditure),
                                                          mean(betas_decrease_multiplier_subset$kpi_expenditure),
                                                          mean(investment_cost_subset$kpi_expenditure),
                                                          mean(recycling_target_increase_subset$kpi_expenditure),
                                                          mean(num_municipalities_subset$kpi_expenditure),
                                                          mean(num_RC_subset$kpi_expenditure)),
                             "Final_Gap" = c(mean(recycling_target_subset$kpi_final_gap),
                                             mean(eta_subset$kpi_final_gap),
                                             mean(theta_old_subset$kpi_final_gap),
                                             mean(theta_single_subset$kpi_final_gap),
                                             mean(theta_family_subset$kpi_final_gap),
                                             mean(theta_couple_subset$kpi_final_gap),
                                             mean(investment_multiplier_subset$kpi_final_gap),
                                             mean(betas_decrease_multiplier_subset$kpi_final_gap),
                                             mean(investment_cost_subset$kpi_final_gap),
                                             mean(recycling_target_increase_subset$kpi_final_gap),
                                             mean(num_municipalities_subset$kpi_final_gap),
                                             mean(num_RC_subset$kpi_final_gap)),
                             "Failure_Count" = c(mean(recycling_target_subset$kpi_failure_count),
                                                 mean(eta_subset$kpi_failure_count),
                                                 mean(theta_old_subset$kpi_failure_count),
                                                 mean(theta_single_subset$kpi_failure_count),
                                                 mean(theta_family_subset$kpi_failure_count),
                                                 mean(theta_couple_subset$kpi_failure_count),
                                                 mean(investment_multiplier_subset$kpi_failure_count),
                                                 mean(betas_decrease_multiplier_subset$kpi_failure_count),
                                                 mean(investment_cost_subset$kpi_failure_count),
                                                 mean(recycling_target_increase_subset$kpi_failure_count),
                                                 mean(num_municipalities_subset$kpi_failure_count),
                                                 mean(num_RC_subset$kpi_failure_count)))
#   TOWriteDf = SensitivityDf
# }

# extract_data("Project_structure Sensitivity Analysis plus 10%-table.csv", Vectorplus10)
# write.csv(TOWriteDf, "Sensitivity Analysis Results plus 10%.csv", row.names = F)
# extract_data("Project_structure Sensitivity Analysis minus 10%-table.csv", Vectorminus10)
# write.csv(TOWriteDf, "Sensitivity Analysis Results minus 10%.csv", row.names = F)
# extract_data("Project_structure Sensitivity Analysis plus 25%-table.csv", Vectorplus25)
# write.csv(TOWriteDf, "Sensitivity Analysis Results plus 25%.csv", row.names = F)
# extract_data("Project_structure Sensitivity Analysis minus 25%-table.csv", Vectorminus25)
# write.csv(TOWriteDf, "Sensitivity Analysis Results minus 25%.csv", row.names = F)
