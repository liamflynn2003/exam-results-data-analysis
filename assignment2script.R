############################################################
# Statistics Assignment (Stats and Probability)
#
# Liam Flynn / ID: 20098690 / Programme: Applied Computing / Game Dev.
#
# R commands to address eight questions pertaining to the
# exam results of leaving-cert students.
############################################################
# Load the data from the clipboard

setwd("C:/Users/Liam/OneDrive - Waterford Institute of Technology/Statistics & Probability")
students = read.csv("54 Liam Flynn 20098690.csv")
attach(students)
############################################################
#
# Q1 -Chi Squared Test#######################################################
#
# Create Chart
plot(table(school, group))
#calculate p value
q1_table = table(students$school, students$group)
q1_result = chisq.test(q1_table)
q1_result$p.value
#
# Q2 - Correlation Coefficent#######################################################
#
# Create a scatter plot of IQ vs. final exam mark
plot(iq, mark, xlab = "IQ", ylab = "Final Exam Mark", main = "Scatter Plot: IQ vs. Final Exam Mark")
#calculate
q2_cor_coef = cor(students$mark,students$iq,use = "complete.obs")
q2_cor_coef
############################################################
#
# Q3 - Calculate & Compare Ratios#######################################################
#
# Create subsets for each discipline
school1 = subset(students, school == "1")
school2 = subset(students, school == "2")
school3 = subset(students, school == "3")

#calculate ratios
ratio1 = table(school1$sex) / nrow(school1)
ratio2 = table(school2$sex) / nrow(school2)
ratio3 = table(school3$sex) / nrow(school3)
#display ratios
ratio1
ratio2
ratio3
#create barchart

# Create a vector of ratios
ratios <- c(ratio1["1"], ratio2["1"], ratio3["1"], ratio1["2"], ratio2["2"], ratio3["2"])

# Specify the labels for the bars
bar_labels <- c("School 1 (Male)", "School 2 (Male)", "School 3 (Male)",
                "School 1 (Female)", "School 2 (Female)", "School 3 (Female)")

# Create the bar plot
barplot(ratios, names.arg = bar_labels, xlab = "Schools", ylab = "Ratio",
        main = "Male-Female Ratio between Schools")
#############################################################################
# Q4 - paired ttest#######################################################
#
#Carry out paired t-test
q4 = t.test(mark,midterm,paired=TRUE)
q4
#make dot plot
change = mark - midterm
dotchart(change, labels = students$id, groups = students$discipline,
         xlab = "Change in Results",color="black",pch=16, ylab = "Student", main = "Change from Midterm to Final Exam")
##############################################
# Q5 - Correlation Coefficient#######################################################
#
# Create a scatter plot of IQ vs. final exam mark
plot(points, mark, xlab = "LC Points", ylab = "Final Exam Mark", main = "Scatter Plot: Leaving Certificate Points vs. Final Exam Mark")
#calculate
q5_cor_coef = cor(students$mark,students$points,use = "complete.obs")
q5_cor_coef
##############################################################
# Q6 - ANOVA#######################################################
#
#ANOVA test
q6 = aov(mark ~ group, data = students)
summary(q6)
#plot
boxplot(mark ~ group, data = students,
        xlab = "Lecture Group", ylab = "Final Exam Mark",
        main = "Boxplot: Final Exam Mark/Lecture Group Comparison")
####################################################
# Q7 - ANOVA#######################################################
#
# ANOVA test
q7 <- aov(mark ~ school, data = students)
summary(q7)
#plot
boxplot(mark ~ school, data = students,
        xlab = "School", ylab = "Final Exam Mark",
        main = "Boxplot: Final Exam Mark/Discipline Comparison")
########################################################################
# Q8 - t-test#######################################################
#
#Subsets for m and f
f = subset(students, sex == "2")
m = subset(students, sex == "1")
#t-test
q8 = t.test(f$mark, m$mark)

# Print the t-test result
print(q8)
boxplot(f$mark, m$mark,  xlab = "Sex", ylab = "Final Exam Mark",
        main = "Boxplot: Final Exam Mark by Sex", horizontal = TRUE)


