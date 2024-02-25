#Jan15, 2024
#Genus percentage for random and strati samples byRAxMLEPA
getwd()
Correct_position<- read.csv("Genus_percentage_Random_Strati_RAxML.csv")
dim(Correct_position)
#In this csv file, Random and stratified results by RAxMLEPA are included. As I need random first I subset another dataframe from Correct_position dataset
Random_Correct_position <- Correct_position[1:50,]
head(Random_Correct_position,11)
names(Random_Correct_position)
#As we need an id to perform ezANOVA I'm adding an id column to this dataframe. In RF random sample dataset (longdata) we have id so I combine longdata to Random_Correct_position
#make sure to run this code with RF longdata code.
#id <- c(rep(1:10,4))
Random_Correct_position<- cbind(longdata, Random_Correct_position)
class(Random_Correct_position)
names(Random_Correct_position)
head(Random_Correct_position)
ezANOVA(data =Random_Correct_position, dv = percentatge_of_correct_placement, wid = id, within = sample_completeness)

#check assumptions
#Outliers can be easily identified using box plot methods, implemented in the R function identify_outliers() [rstatix package]
Random_Correct_position %>%
  group_by(sample_completeness) %>%
  identify_outliers(percentatge_of_correct_placement)
#There were no extreme outliers

#visualization
#Create a violin plot
violin_plot <- ggplot(Random_Correct_position, aes(x=sample_completeness, y=percentatge_of_correct_placement,color=sample_completeness))+ geom_violin(trim = FALSE)+theme(legend.position="none")+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.01)+labs(title="The box plot of Correct position percentage for sampling completeness")
violin_plot

#Normality assumption
#The normality assumption can be checked by computing Shapiro-Wilk test for each time point. If the data is normally distributed, the p-value should be greater than 0.05.
Random_Correct_position %>%
  group_by(sample_completeness) %>%  
  shapiro_test(percentatge_of_correct_placement)
#The longdata Path_distance was normally distributed at each sample completeness, as assessed by Shapiro-Wilk’s test (p > 0.05).

#QQ plot draws the correlation between a given data and the normal distribution. Create QQ plots for each level backbone tree(sample completeness)
ggqqplot(Random_Correct_position, "percentatge_of_correct_placement", facet.by = "sample_completeness",color = "sample_completeness")

#From the resultant plots, as all the points fall approximately along the reference line, we can assume normality

#The assumption of sphericity will be automatically checked during the computation of the ANOVA test using the R function anova_test() [rstatix package]. The Mauchly’s test is internally used to assess the sphericity assumption
#By using the function get_anova_table() [rstatix] to extract the ANOVA table, the Greenhouse-Geisser sphericity correction is automatically applied to factors violating the sphericity assumption
# res.aov <- anova_test(data = longdata, dv = RF_distance, wid = id,within = sample_completeness)
# get_anova_table(res.aov)

#Post-hoc tests
#perform multiple pairwise paired t-tests between the levels of the within-subjects factor (Sample_completeness). P-values are adjusted using the Bonferroni multiple testing correction method.

# pairwise comparisons
pwc <- Random_Correct_position %>%
  pairwise_t_test(
    percentatge_of_correct_placement ~ sample_completeness, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

#All the pairwise differences are statistically significant

#Correlation tests need numerical variables, so convert more than one column in R data frame to numeric values
Random_Correct_position_new <- lapply(Random_Correct_position,as.numeric)

#Calculate the repeated measures correlation coefficient. id is variable giving the subject name/id for each observation. RF_distance is a numeric variable giving the observations for one measure, sample_completeness is a numeric variable giving the observations for the second measure, longdat is the data frame containing the variables.
rmcorr(id, percentatge_of_correct_placement , sample_completeness, Random_Correct_position_new )

#non-parametric for repeated messures anova is friedman test

res.fried <- Random_Correct_position %>% friedman_test(percentatge_of_correct_placement ~ sample_completeness |id)
res.fried
#The correct species percentage was statistically significantly different at the different time points during the diet, X2(2) = 30, p = 0.00000138

#effect size
#The Kendall’s W can be used as the measure of the Friedman test effect size. It is calculated as follow : W = X2/N(K-1); where W is the Kendall’s W value; X2 is the Friedman test statistic value; N is the sample size. k is the number of measurements per subject (M. T. Tomczak and Tomczak 2014).

#The Kendall’s W coefficient assumes the value from 0 (indicating no relationship) to 1 (indicating a perfect relationship).

#Kendall’s W uses the Cohen’s interpretation guidelines of 0.1 - < 0.3 (small effect), 0.3 - < 0.5 (moderate effect) and >= 0.5 (large effect). Confidence intervals are calculated by bootstap.
Random_Correct_position %>% friedman_effsize(percentatge_of_correct_placement ~ sample_completeness |id)
#A large size is detected W=1

#multiple pair-wise comparisons
# From the output of the Friedman test, we know that there is a significant difference between groups, but we don’t know which pairs of groups are different.
# 
# A significant Friedman test can be followed up by pairwise Wilcoxon signed-rank tests for identifying which groups are different.
# 
# Note that, the data must be correctly ordered by the blocking variable (id) so that the first observation for time t1 will be paired with the first observation for time t2, and so on.
# 
# Pairwise comparisons using paired Wilcoxon signed-rank test. P-values are adjusted using the Bonferroni multiple testing correction method.
pwc <- Random_Correct_position%>%
  wilcox_test(percentatge_of_correct_placement ~ sample_completeness, paired = TRUE, p.adjust.method = "bonferroni")
pwc

#-------------------------
Correct_position<- read.csv("Genus_percentage_Random_Strati_RAxML.csv")
dim(Correct_position)
#In this csv file, Random and stratified results by RAxMLEPA are included. As I need Stratified samples I subset another dataframe from Correct_position dataset
Correct_position <- Correct_position[51:100,]
head(Correct_position,11)
names(Correct_position)
#As we need an id to perform ezANOVA I'm adding an id column to this dataframe. In RF random sample dataset (longdata) we have id so I combine longdata to Random_Correct_position
#make sure to run this code with RF longdata code.
#id <- c(rep(1:10,4))
Correct_position<- cbind(longdata, Correct_position)
class(Correct_position)
names(Correct_position)
head(Correct_position)
ezANOVA(data =Correct_position, dv = percentatge_of_correct_placement, wid = id, within = sample_completeness)

#check assumptions
#Outliers can be easily identified using box plot methods, implemented in the R function identify_outliers() [rstatix package]
Correct_position %>%
  group_by(sample_completeness) %>%
  identify_outliers(percentatge_of_correct_placement)
#There were no extreme outliers

#visualization
#Create a violin plot
violin_plot <- ggplot(Correct_position, aes(x=sample_completeness, y=percentatge_of_correct_placement,color=sample_completeness))+ geom_violin(trim = FALSE)+theme(legend.position="none")+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.01)+labs(title="The box plot of Correct position percentage for sampling completeness")
violin_plot

#Normality assumption
#The normality assumption can be checked by computing Shapiro-Wilk test for each time point. If the data is normally distributed, the p-value should be greater than 0.05.
Correct_position %>%
  group_by(sample_completeness) %>%  
  shapiro_test(percentatge_of_correct_placement)
#The longdata Path_distance was normally distributed at each sample completeness, as assessed by Shapiro-Wilk’s test (p > 0.05).

#QQ plot draws the correlation between a given data and the normal distribution. Create QQ plots for each level backbone tree(sample completeness)
ggqqplot(Correct_position, "percentatge_of_correct_placement", facet.by = "sample_completeness",color = "sample_completeness")

#From the resultant plots, as all the points fall approximately along the reference line, we can assume normality

#The assumption of sphericity will be automatically checked during the computation of the ANOVA test using the R function anova_test() [rstatix package]. The Mauchly’s test is internally used to assess the sphericity assumption
#By using the function get_anova_table() [rstatix] to extract the ANOVA table, the Greenhouse-Geisser sphericity correction is automatically applied to factors violating the sphericity assumption
# res.aov <- anova_test(data = longdata, dv = RF_distance, wid = id,within = sample_completeness)
# get_anova_table(res.aov)

#Post-hoc tests
#perform multiple pairwise paired t-tests between the levels of the within-subjects factor (Sample_completeness). P-values are adjusted using the Bonferroni multiple testing correction method.

# pairwise comparisons
pwc <- Correct_position %>%
  pairwise_t_test(
    percentatge_of_correct_placement ~ sample_completeness, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

#All the pairwise differences are statistically significant

#Correlation tests need numerical variables, so convert more than one column in R data frame to numeric values
Correct_position_new <- lapply(Correct_position,as.numeric)

#Calculate the repeated measures correlation coefficient. id is variable giving the subject name/id for each observation. RF_distance is a numeric variable giving the observations for one measure, sample_completeness is a numeric variable giving the observations for the second measure, longdat is the data frame containing the variables.
rmcorr(id, percentatge_of_correct_placement , sample_completeness, Correct_position_new )

#non-parametric for repeated messures anova is friedman test

res.fried <- Correct_position %>% friedman_test(percentatge_of_correct_placement ~ sample_completeness |id)
res.fried
#The correct species percentage was statistically significantly different at the different time points during the diet, X2(2) = 30, p = 0.00000138

#effect size
#The Kendall’s W can be used as the measure of the Friedman test effect size. It is calculated as follow : W = X2/N(K-1); where W is the Kendall’s W value; X2 is the Friedman test statistic value; N is the sample size. k is the number of measurements per subject (M. T. Tomczak and Tomczak 2014).

#The Kendall’s W coefficient assumes the value from 0 (indicating no relationship) to 1 (indicating a perfect relationship).

#Kendall’s W uses the Cohen’s interpretation guidelines of 0.1 - < 0.3 (small effect), 0.3 - < 0.5 (moderate effect) and >= 0.5 (large effect). Confidence intervals are calculated by bootstap.
Correct_position %>% friedman_effsize(percentatge_of_correct_placement ~ sample_completeness |id)
#A large size is detected W=1

#multiple pair-wise comparisons
# From the output of the Friedman test, we know that there is a significant difference between groups, but we don’t know which pairs of groups are different.
# 
# A significant Friedman test can be followed up by pairwise Wilcoxon signed-rank tests for identifying which groups are different.
# 
# Note that, the data must be correctly ordered by the blocking variable (id) so that the first observation for time t1 will be paired with the first observation for time t2, and so on.
# 
# Pairwise comparisons using paired Wilcoxon signed-rank test. P-values are adjusted using the Bonferroni multiple testing correction method.
pwc <- Correct_position%>%
  wilcox_test(percentatge_of_correct_placement ~ sample_completeness, paired = TRUE, p.adjust.method = "bonferroni")
pwc
