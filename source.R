## author: Raquel Ribeiro, ribeiro@minervaproject.com

## questions we want to address in this report:
## Q1: evolution of grades by week for each student
## Q2: grades by LO/HC for each student

## importing helpful packages
library(dplyr)
library(plyr)
library(tidyr)
library(highcharter)
library(viridis)
library(stringr)
library(xts)
library(anytime)
## setting the working directory
setwd("~/Documents/cs166/summer internship/grades_analysis/Grading Tracker test//")

## loading data and basic cleaning up

data <- read.csv("sample_data_for_reporting.csv")
names(data) <- gsub("\\.", "_", names(data))

# renaming  HC and LO tables
names(data)[names(data) == "HC_Name"] <- "LO"
names(data)[names(data) == "Learning_Outcome_Name"] <- "HC"
print(dim(data))


# treat N/A as actual NAs
is.na(data) <- (data == "NA") |(data == "N/A") | (data == "")

# removing incomplete instances with no Score data
data <- data[complete.cases(data$Score),]
print(dim(data))

# resetting the index
rownames(data) <- NULL

# convert to timestamp
# data$Updated_Date <- anytime::anytime(data$Updated_Date)

## generating week numbers

data$Week <- as.factor(round(as.numeric(as.character(data$Class_Code))))



## [RHR] this will need to be automated
data <- transform(data, 
                  Assignment= ifelse(
                    Assignment_Title=="Elevator simulation", 
                    "assignment_1", 
                    NA)
)



################################################################
########################TO DO###################################
# Add weight column                                            #
## assignments have weight 6, and anything else has weight 1 
data <- transform(data, 
                  weight= ifelse(
                    Assessment_Type=="assignment", 
                    6, 
                    1)
)
# Add weighted score column                                    #
data <- mutate(data,
       weighted_score = Score * weight)
################################################################
################################################################


### TEST

calculate_LO_avg <- function(my_data) {
  # given a data for a specific Student, get the average LO over time
  split_date_func <- function(df) {
    # truncate the Updated Date column so that it contains only the date information
    df <- mutate(df, Updated_Date=substring(Updated_Date, 1,10))
    df <- mutate(df, Updated_Date=as.POSIXct(Updated_Date, format = "%Y-%m-%d"))
    mcmodeling_data <- ddply(df %>% arrange(Updated_Date), .(Updated_Date), summarise, 
                             sum_weighted = sum(weighted_score), 
                             tot_weights = sum(weight))
    
    ###### By this point we have a dataframe with:
    # * date
    # * total weighted scores
    # * total weights
    # What we want to do is taking the running average up to a certain date
    # For date #2, it equals: 
    # (total weighted scores of date 1 + total weighted scores of date 2)/(total weights of date1 + total weights of date 2)
    
    ## Idea: we can create 
    # * a new column (column_a) that is the cumsum of the vector column total weights
    # * a new column (column_b) that is the cumsum of the total weighted scores
    # * the quantity of interest would be column_b/column_a
    column_a <- cumsum(mcmodeling_data$tot_weights)
    column_b <- cumsum(mcmodeling_data$sum_weighted)
    mcmodeling_data$running_avg <- column_b/column_a
    return(mcmodeling_data)
  }
  data_with_avg_LO <- ddply(my_data, .(LO), split_date_func)
  return(data_with_avg_LO)
}

plotting_LO_evolution <- function(my_data, student_name) {
  display_LOs <- c('networkanalysis', 'networkmodeling')
  visible_data <- filter(my_data, LO %in% display_LOs)
  invisible_data <- filter(my_data, ! LO %in% display_LOs)
 
  mcmodeling_LO_chart <- highchart() %>%
    hc_add_series(
      data = visible_data%>%
        arrange(Updated_Date),
      type = 'line',
      hcaes(
        x = datetime_to_timestamp(Updated_Date),
        y = running_avg,
        group = LO
      ),
      visible=TRUE,
      point_Width = 10
    ) %>%
    hc_add_series(
      data = invisible_data%>%
        arrange(Updated_Date),
      type = 'line',
      hcaes(
        x = datetime_to_timestamp(Updated_Date),
        y = running_avg,
        group = LO
      ),
      visible=FALSE,
      point_Width = 20
    ) %>%
    
    hc_xAxis(title = list(text = "Time"),
             type = 'datetime'
    ) %>%
    hc_yAxis( max = 5, min=1,
              allowDecimals = FALSE,
              visible = TRUE
    ) %>%
    hc_tooltip(
      # hc_tooltip: Options for the tooltip that appears when the user hovers over a series or point.
    ) %>%
    hc_title(
      text = 'Evolution of Average LO score'
    ) %>% hc_subtitle(
      text = paste0('for ', student_name)
    )
}

list_of_students <- unique(data$Student_Name)
num_students <- length(list_of_students)
list_LO_evolution_student = list()
# list_scores_data_chart_student_CO = list()
for (i in 1:num_students) {
  student_name = list_of_students[i]
  my_data <- data[data$Student_Name == student_name,]
  
  my_data <- my_data[!is.na(my_data$LO), ]
  my_data$Student_Name <- factor(my_data$Student_Name)
  my_data <- calculate_LO_avg(my_data)
  name_string <- paste('scores_data_chart_student', toString(i), sep="")
  list_LO_evolution_student[[name_string]] <- plotting_LO_evolution(my_data, student_name)
}

## FOR LO BAR CHART FOR EACH STUDENT
list_of_students <- unique(data$Student_Name)
num_students <- length(list_of_students)
list_LO_score_student = list()
# list_scores_data_chart_student_CO = list()
plotting_LO_avg <- function(data, student_name){
  
  # display_LOs <- c('networkanalysis', 'networkmodeling')
  # visible_data <- filter(my_data, LO %in% display_LOs)
  # invisible_data <- filter(my_data, ! LO %in% display_LOs)
  data <- data %>%
    arrange(LO)
  # chart <- data %>%
  #   hchart('column', hcaes(x='LO', y='running_avg'))
  chart <- highchart() %>%
  hc_add_series(
    data = data,
    type = 'column',
    hcaes(
      x = 'LO',
      y = 'running_avg'
    ),
    name = 'Average score',
    visible=TRUE
  ) %>%
    hc_xAxis(title = list(text = "LO"),
             type = 'category'
    ) %>%
    hc_legend(enabled = F)
    # hc_yAxis( max = 5, min=1,
    #           allowDecimals = FALSE,
    #           visible = TRUE
    # ) %>%
    # hc_tooltip(
    #   # hc_tooltip: Options for the tooltip that appears when the user hovers over a series or point.
    # ) %>%
    # hc_title(
    #   text = 'Average LO score'
    # ) %>% hc_subtitle(
    #   text = paste0('for ', student_name)
    # )
}
######
# student_name <- 'Student_1'
# my_data <- data[data$Student_Name == student_name,]
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# my_data <- calculate_LO_avg(my_data)
# my_data <- my_data %>%
#   group_by(LO) %>%
#   arrange(Updated_Date) %>%
#   filter(row_number()==n())
# rere <- highchart() %>%
#   hc_chart(type='column') %>%
#   hc_add_series(
#     data = my_data,
#     hcaes(
#       x = 'LO',
#       y = 'running_avg'
#     ))
#   
# my_data %>%
#   hchart('column', hcaes(x='LO', y='running_avg'))
######
for (i in 1:num_students) {
  student_name = list_of_students[i]
  my_data <- data[data$Student_Name == student_name,]
  
  my_data <- my_data[!is.na(my_data$LO), ]
  my_data$Student_Name <- factor(my_data$Student_Name)
  my_data <- calculate_LO_avg(my_data)
  my_data <- my_data %>%
                  group_by(LO) %>%
                  arrange(Updated_Date) %>%
                  filter(row_number()==n())

  name_string <- paste('scores_data_chart_student', toString(i), sep="")
  list_LO_score_student[[name_string]] <- plotting_LO_avg(my_data, student_name)
}

## quantitative - by week
### filtering the data this way, only in-class grades are included
all_scores_data <- data[complete.cases(data$Week),]

### in the block below: split the all_scores_data by week (.(Week)), apply summarise to
### each week. avg_os and median_os are functions passed on to summarise. 
### The result is a dataframe containing columns Week, avg_os, median_os
all_scores_data <- ddply(all_scores_data %>% arrange(Week), .(Week), summarise, 
                 avg_os = mean(Score), 
                 median_os = median(Score))

n_weeks <- length(unique(all_scores_data$Week))

scores_data_chart <- highchart() %>%
  hc_add_series(
    data = all_scores_data%>%
      arrange(Week),
    type = 'line',
    hcaes(
      x = Week,
      y = avg_os
    ),
    name = "Average score", 
    # name is what will show up when hovering the mouse over the plot ("Average score: 3.86", for example)
    # color = viridis_pal(alpha = 0.5)(8)[1:n_weeks], 
    point_Width = 20
  ) %>%
  hc_xAxis(title = list(text = "Week"), 
           allowDecimals = FALSE,
    # visible = TRUE
    type = 'line'
    # opposite = TRUE
  ) %>%
  hc_yAxis( max = 4, min=2, 
    allowDecimals = FALSE,
    visible = TRUE
  ) %>%
  hc_tooltip(
    # hc_tooltip: Options for the tooltip that appears when the user hovers over a series or point.
  ) %>%
  hc_title(
    text = 'Distribution of Average Outcome Scores'
  ) %>%
  hc_subtitle(
    text = 'in the entire section by week'
  ) %>%
  hc_legend(enabled = F) 

dim(data)


####################### zooming in for each student and for each LO

list_of_students <- unique(data$Student_Name)

## [RHR] this will need to be automated

## creating a course objective

# data$Course_Objective <-
#   ifelse((data$LO == "mcanalysis") | (data$LO == "mcmodeling"), "MonteCarlo",
#          ifelse((data$LO == "interpretresults") | (data$LO == "professionalism") | (data$LO == "pythonimplementation"), "Simulations",
#                 ifelse((data$LO == "caanalysis") | (data$LO == "camodeling"),  "Cellular Automata",
#                        ifelse((data$LO == "networkanalysis") | (data$LO == "networkmodeling"),  "Networks", NA))))
# 
# 
data$Course_Objective <- mapvalues(data$LO, c("mcanalysis", "mcmodeling", "interpretresults", "professionalism", "pythonimplementation","caanalysis", "camodeling", "networkanalysis","networkmodeling" ),
          c("MonteCarlo","MonteCarlo","Simulations","Simulations","Simulations","Cellular Automata","Cellular Automata","Networks","Networks"))

data$Course_Objective <- as.factor(data$Course_Objective)


###################################################################################
############################ generic plotting function ############################ 
###################################################################################

plotting_scores <- function(df, student_name,number_of_LOs,
                          col1, col2, type_of_graph, title_of_graph) {
  df$col1 <- df[[col1]]
  df$col2 <- df[[col2]]
  print(str(df))
  print(number_of_LOs)
  highchart() %>%
    hc_add_series(
      data = df %>%
      arrange(col1),
      type = type_of_graph,
      hcaes(
        x = col1,
        y = col2
      ),
      name = "Average score",
      #color = viridis_pal(alpha = 0.5)(8)[1:number_of_LOs], 
      point_Width = 20
    ) %>%
    hc_xAxis(
      # visible = TRUE
      type = 'category'
      # opposite = TRUE
    ) %>%
    hc_yAxis(max=5, min=2,
      allowDecimals = FALSE,
      visible = TRUE
    ) %>%
    hc_tooltip(
    ) %>%
    hc_title(
      text = title_of_graph
    ) %>%
    hc_subtitle(
      text = paste0('for ', student_name)
    ) %>%
    hc_legend(enabled = F)
  #return(scores_data_chart)
}

num_students <- length(list_of_students)
list_scores_data_chart_student = list()
list_scores_data_chart_student_CO = list()
for (i in 1:num_students) {
  student_name = list_of_students[i]
  my_data <- data[data$Student_Name == student_name,]
  
  my_data <- my_data[!is.na(my_data$LO), ]
  my_data$Student_Name <- factor(my_data$Student_Name)
  dim(my_data)
  
  n_LOs <- length(unique(my_data$LO))
  
  n_COs <- length(unique(my_data$Course_Objective))
  
  
  my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
                      average_grade = mean(Score))
  my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
                                               average_grade = mean(Score))
  name_string <- paste('scores_data_chart_student', toString(i), sep="")
  list_scores_data_chart_student[[name_string]]<- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
  list_scores_data_chart_student_CO[[name_string]] <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
}
# 
# ##############student1
# 
# # 
# # test_func <- function() {
# #   for (i in 1:num_students) {
# #     student_name = list_of_students[i]
# #     
# #     my_data <- data[data$Student_Name == student_name,]
# #     
# #     my_data <- my_data[!is.na(my_data$LO), ]
# #     my_data$Student_Name <- factor(my_data$Student_Name)
# #     dim(my_data)
# #     
# #     n_LOs <- length(unique(my_data$LO))
# #     
# #     n_COs <- length(unique(my_data$Course_Objective))
# #     
# #     
# #     my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
# #                         average_grade = mean(Score))
# #     
# #     my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
# #                         average_grade = mean(Score))
# #     
# #     plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# #     
# #     
# #   }
# # }
# # scores_data_chart_students <- test_func()
# 
# 
# student_name = list_of_students[1]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# n_COs <- length(unique(my_data$Course_Objective))
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                      average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                                       average_grade = mean(Score))
# 
# scores_data_chart_student1 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student1_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# ##############student2
# student_name = list_of_students[2]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# scores_data_chart_student2 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student2_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# ##############student3
# student_name = list_of_students[3]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# scores_data_chart_student3 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student3_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# 
# ##############student4
# student_name = list_of_students[4]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# scores_data_chart_student4 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student4_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# 
# ##############student5
# student_name = list_of_students[5]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# scores_data_chart_student5 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student5_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# 
# ##############student6
# student_name = list_of_students[6]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# scores_data_chart_student6 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student6_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# 
# 
# ##############student7
# student_name = list_of_students[7]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# scores_data_chart_student7 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student7_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores") 
# 
# ##############student8
# student_name = list_of_students[8]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# 
# scores_data_chart_student8 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student8_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# ##############student9
# student_name = list_of_students[9]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# scores_data_chart_student9 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student9_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# 
# ##############student10
# student_name = list_of_students[10]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# scores_data_chart_student10 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student10_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")
# 
# 
# ##############student11
# student_name = list_of_students[11]
# 
# my_data <- data[data$Student_Name == student_name,]
# 
# my_data <- my_data[!is.na(my_data$LO), ]
# my_data$Student_Name <- factor(my_data$Student_Name)
# dim(my_data)
# 
# n_LOs <- length(unique(my_data$LO))
# 
# 
# my_data_LO <- ddply(my_data %>% arrange(Score), .(LO), summarise,
#                     average_grade = mean(Score))
# 
# my_data_CO <- ddply(my_data %>% arrange(Score), .(Course_Objective), summarise,
#                     average_grade = mean(Score))
# 
# scores_data_chart_student11 <- plotting_scores(my_data_LO, student_name, n_LOs, "LO", "average_grade", 'line', "Distribution of LO Scores")
# scores_data_chart_student11_CO <- plotting_scores(my_data_CO, student_name, n_COs, "Course_Objective", "average_grade", 'line', "Distribution of CO Scores")