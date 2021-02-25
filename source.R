## author: Raquel Ribeiro, ribeiro@minervaproject.com

## questions we want to address in this report:
## Q1: evolution of grades by week for each student
## Q2: grades by LO/HC for each student

##### PARAMETERS TO CHANGE
# Path to the directory that contains the data file
DATA_PATH <- "~/Documents/cs166/summer internship/grades_analysis/Grading Tracker test//"
# Select which LOs will be highlighted by default in the LO Evolution tab
DEFAULT_LO_TO_DISPLAY_IN_EVOLUTION <- c('networkanalysis', 'networkmodeling')
# Mapping the LOs to COs. Because the demo data file does not have the column CO
LO_IN_ORDER <- c("mcanalysis", "mcmodeling", "interpretresults", "professionalism", "pythonimplementation","caanalysis",       "camodeling",       "networkanalysis","networkmodeling" )
CO_IN_ORDER <- c("MonteCarlo", "MonteCarlo", "Simulations",      "Simulations",     "Simulations",         "Cellular Automata","Cellular Automata","Networks",       "Networks")

## importing helpful packages
library(dplyr)
library(plyr)
library(tidyr)
library(highcharter)
library(viridis)
library(stringr)
library(xts)
library(anytime)
library(htmlwidgets)
library(manipulateWidget)
## setting the working directory
setwd(DATA_PATH)

## loading data and basic cleaning up

data <- read.csv("synthetic_data.csv")
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

## generating week numbers

data$Week <- as.factor(round(as.numeric(as.character(data$Class_Code))))

## [RHR] this will need to be automated
data <- transform(data, 
                  Assignment= ifelse(
                    Assignment_Title=="Elevator simulation", 
                    "assignment_1", 
                    NA)
)


data$CO <- mapvalues(data$LO, c("mcanalysis", "mcmodeling", "interpretresults", "professionalism", "pythonimplementation","caanalysis", "camodeling", "networkanalysis","networkmodeling" ),
                     c("MonteCarlo","MonteCarlo","Simulations","Simulations","Simulations","Cellular Automata","Cellular Automata","Networks","Networks"))

data$CO <- as.factor(data$CO)

# Add weight column. Current data set does not have weights for scores.                                            
## Assumption in this demo: assignments have weight 6, and anything else has weight 1 
data <- transform(data, 
                  weight= ifelse(
                    Assessment_Type=="assignment", 
                    6, 
                    1)
)

# Add weighted score column                                    
data <- mutate(data,
       weighted_score = Score * weight)

# drop null in LO column
data <- data[!is.na(data$LO), ]


##########################################################################
##########################################################################
####### LO EVOLOTION TAB
##########################################################################
##########################################################################
split_date_func <- function(df) {
  # truncate the Updated Date column so that it contains only the date information
  df <- mutate(df, Updated_Date=substring(Updated_Date, 1,10))
  df <- mutate(df, Updated_Date=as.POSIXct(Updated_Date, format = "%Y-%m-%d"))
  lo_data <- ddply(df %>% arrange(Updated_Date), .(Updated_Date), summarise, 
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
  lo_data$cum_tot_weights <- cumsum(lo_data$tot_weights)
  lo_data$cum_sum_weighted <- cumsum(lo_data$sum_weighted)
  lo_data$running_avg <- lo_data$cum_sum_weighted/lo_data$cum_tot_weights
  return(lo_data)
}

calculate_LO_avg <- function(my_data, col_name_to_group_by) {
  # given a data for a specific Student, get the average LO over time
  data_with_avg_LO <- ddply(my_data, c(col_name_to_group_by), split_date_func)
  if (col_name_to_group_by != 'CO'){
    data_with_avg_LO$CO <- mapvalues(data_with_avg_LO$LO, LO_IN_ORDER, CO_IN_ORDER)
    
    data_with_avg_LO$CO <- as.factor(data_with_avg_LO$CO)
  }
  return(data_with_avg_LO)
}

plotting_LO_evolution <- function(my_data, student_name) {
  display_LOs <- DEFAULT_LO_TO_DISPLAY_IN_EVOLUTION
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
    hc_yAxis( max = 5, min=0,
              allowDecimals = FALSE,
              visible = TRUE
    ) %>%
    hc_tooltip(
      # hc_tooltip: Options for the tooltip that appears when the user hovers over a series or point.
      formatter= JS("function () {
                    return this.point.Updated_Date.substring(0,10).concat('<br/>', '<b>',this.point.LO, '</b>: ', parseFloat(this.point.y).toFixed(2));
                    // return parseFloat(this.point.y).toFixed(2).concat(this.point.Updated_Date);
                    }")
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
  my_data <- calculate_LO_avg(my_data, 'LO')
  name_string <- paste('scores_data_chart_student', toString(i), sep="")
  list_LO_evolution_student[[name_string]] <- plotting_LO_evolution(my_data, student_name)
}


##########################################################################
##########################################################################
####### LO AVERAGE SCORE TAB
##########################################################################
##########################################################################
list_of_students <- unique(data$Student_Name)
num_students <- length(list_of_students)
list_LO_score_student = list()

compute_course_avg <- function(data){
  data <- calculate_LO_avg(data, 'CO')
  data <- data %>%
    group_by(CO) %>%
    arrange(Updated_Date) %>%
    filter(row_number()==n())
  return(mean(data[['running_avg']]))
}

plotting_LO_avg <- function(data, student_name, col_name, plot_title, course_avg=-1, y_col='running_avg', min_y=0, max_y=5){

  data <- data %>%
    arrange_at(c('CO',col_name))
  # SET COLOR TO MATCH MINERVA SCHEME
  if (y_col=='running_avg'){
  data <- data %>%
    mutate(color = case_when(0 < running_avg  & running_avg< 2 ~ '#EC3A1E',
                             2 <= running_avg & running_avg< 3 ~ '#ECAC0D',
                             3 <= running_avg & running_avg< 4 ~ '#5FB40F',
                             4 <= running_avg & running_avg< 5 ~ '#227CD6',
                             5 <= running_avg & running_avg< 6 ~ '#430CCD'
           ))
  }
  else {
    data$color <- 'grey'
  }

  
  data$y <- data[[y_col]]
  data$course_avg <- course_avg
  data$col_name <- data[[col_name]]
  chart <- highchart()
  if (course_avg >0){
    chart <- chart %>% 
      hc_add_series(data=data$course_avg, type='line', name='Course average',
                    marker = list(
                      lineWidth = 1,
                      radius=0,
                      lineColor = "black"
                    ),
                    color= "black",
                    zIndex=10) 
    
  }
  chart <- chart %>%
    hc_chart(type = 'bar', polar = FALSE) %>%
    hc_xAxis(categories = data[[col_name]]) %>% 
    hc_add_series(data=data,
                 name='Average score',
                 showInLegend=FALSE) %>%
  
    hc_tooltip(
      formatter= JS("function () {
                    if (this.series.name == 'Average score'){
                        return this.point.col_name.concat(': ',parseFloat(this.point.y).toFixed(2));
                    } else{
                    return this.series.name.concat(': ',parseFloat(this.point.y).toFixed(2));}}")
    ) %>%
    hc_title(
      text = plot_title 
    ) %>% hc_subtitle(
      text = paste0('for ', student_name)
    ) %>%
    hc_yAxis(max = max_y, min=min_y,
               allowDecimals = FALSE,
               visible=TRUE
    ) %>%
    hc_legend(enabled=T) 
  
}

whole_class_LO_data <- data.frame()
for (i in 1:num_students) {
  student_name = list_of_students[i]
  my_data <- data[data$Student_Name == student_name,]
  
  my_data <- my_data[!is.na(my_data$LO), ]
  my_data$Student_Name <- factor(my_data$Student_Name)
  course_avg <- compute_course_avg(my_data)
  my_data <- calculate_LO_avg(my_data, 'LO')
  my_data <- my_data %>%
                  group_by(LO) %>%
                  arrange(Updated_Date) %>%
                  filter(row_number()==n())
  print(my_data)
  if (length(names(whole_class_LO_data)) == 0) {
    whole_class_LO_data <- my_data %>% select(LO, running_avg)
  }
  else {
    whole_class_LO_data <- rbind(whole_class_LO_data, my_data %>% select(LO, running_avg))
  }
  name_string <- paste('scores_data_chart_student', toString(i), sep="")
  list_LO_score_student[[name_string]] <- plotting_LO_avg(my_data, student_name, 'LO', 'Average LO score', course_avg)
}
whole_class_LO_data <- ddply(whole_class_LO_data, .(LO), summarise, 
                             avg_data = mean(running_avg),
                             min_data = min(running_avg),
                             max_data = max(running_avg))
whole_class_LO_data <- whole_class_LO_data %>% arrange(LO)
whole_class_LO_data$low <- whole_class_LO_data$avg_data - whole_class_LO_data$min_data
whole_class_LO_data$high <- whole_class_LO_data$max_data - whole_class_LO_data$avg_data
whole_class_LO_data$x <- whole_class_LO_data$LO
print(whole_class_LO_data)
# graph 
whole_class_LO <- highchart() %>% 
  hc_chart(type = "bar") %>% 
  hc_add_series(whole_class_LO_data, "errorbar", hcaes(x = x, low = min_data, high = max_data)) %>%
  hc_add_series(whole_class_LO_data, hcaes(x=x, y=avg_data), type="scatter", name="Mean", color="black") %>% 
  hc_xAxis(type='category') %>%
  hc_title(
    text = 'Range of LO average of a student' 
  ) %>% 
  hc_yAxis(max = 5, min=0,
           visible=TRUE
  ) %>%
  hc_tooltip(

    formatter= JS("function () {
                  if (this.series.name == 'Mean'){
                  return this.point.LO.concat('<br/><b>Mean</b>: ', parseFloat(this.point.y).toFixed(2), '<br/><b>Range</b>: [', parseFloat(this.point.min_data).toFixed(2), ', ', parseFloat(this.point.max_data).toFixed(2), ']');
                  }}")
  )
saveWidget(list_LO_score_student[['scores_data_chart_student1']], file="highchart.html", selfcontained = TRUE)


##########################################################################
##########################################################################
####### CO AVERAGE SCORE TAB
##########################################################################
##########################################################################

list_of_students <- unique(data$Student_Name)
num_students <- length(list_of_students)
list_CO_score_student = list()
unique_COs <- unique(data[!is.na(my_data$LO), ]$CO)
whole_class_CO_data <- data.frame()
for (i in 1:num_students) {
  student_name = list_of_students[i]
  my_data <- data[data$Student_Name == student_name,]

  my_data <- my_data[!is.na(my_data$LO), ]
  my_data$Student_Name <- factor(my_data$Student_Name)
  course_avg <- compute_course_avg(my_data)
  print(course_avg)
  my_data <- calculate_LO_avg(my_data, 'CO')
  # get the last row of each CO
  my_data <- my_data %>%
    group_by(CO) %>%
    arrange(Updated_Date) %>%
    filter(row_number()==n())
  if (length(names(whole_class_CO_data)) == 0) {
    whole_class_CO_data <- my_data %>% select(CO, running_avg)
  }
  else {
    whole_class_CO_data <- rbind(whole_class_CO_data, my_data %>% select(CO, running_avg))
  }
  name_string <- paste('scores_data_chart_student', toString(i), sep="")
  list_CO_score_student[[name_string]] <- plotting_LO_avg(my_data, student_name, 'CO', 'Average CO score', course_avg)
}

print(whole_class_CO_data)
whole_class_CO_data <- ddply(whole_class_CO_data, .(CO), summarise, 
                                  avg_data = mean(running_avg),
                                  min_data = min(running_avg),
                                  max_data = max(running_avg))
whole_class_CO_data <- whole_class_CO_data %>% arrange(CO)
whole_class_CO_data$low <- whole_class_CO_data$avg_data - whole_class_CO_data$min_data
whole_class_CO_data$high <- whole_class_CO_data$max_data - whole_class_CO_data$avg_data
whole_class_CO_data$x <- whole_class_CO_data$CO
print(whole_class_CO_data)
# graph 
whole_class_CO <- highchart() %>% 
  hc_chart(type = "bar") %>% 
  hc_add_series(whole_class_CO_data, "errorbar", hcaes(x = x, low = min_data, high = max_data)) %>%
  hc_add_series(whole_class_CO_data, hcaes(x=x, y=avg_data), type="scatter", name="Mean", color="black") %>% 
  hc_xAxis(type='category') %>%
  hc_title(
    text = 'Range of CO score of a student' 
  ) %>% 
  hc_yAxis(max = 5, min=0,
           visible=TRUE
  ) %>%
  hc_tooltip(
   
    formatter= JS("function () {
                  if (this.series.name == 'Mean'){
                  return this.point.CO.concat('<br/><b>Mean</b>: ', parseFloat(this.point.y).toFixed(2), '<br/><b>Range</b>: [', parseFloat(this.point.min_data).toFixed(2), ', ', parseFloat(this.point.max_data).toFixed(2), ']');
                  }}")
  )


##########################################################################
##########################################################################
####### LO CONTRIBUTION TAB
##########################################################################
##########################################################################
list_of_students <- unique(data$Student_Name)
num_students <- length(list_of_students)
list_CO_contrib_student = list()
unique_LOs <- unique(data[!is.na(my_data$LO), ]$LO)
whole_class_contrib_data <- data.frame()
for (i in 1:num_students) {
  student_name = list_of_students[i]
  my_data <- data[data$Student_Name == student_name,]

  my_data <- my_data[!is.na(my_data$LO), ]
  my_data$Student_Name <- factor(my_data$Student_Name)
  course_avg <- compute_course_avg(my_data)
  LO_data <- calculate_LO_avg(my_data, 'LO')
  # adter running the block below,
  # LO_data contains all LOs, each with tot_weights and weighted_sum.
  LO_data <- LO_data %>%
    group_by(LO) %>%
    arrange(Updated_Date) %>%
    filter(row_number()==n())
  CO_avg_data <- calculate_LO_avg(my_data, 'CO')
  # get the last row of each CO
  CO_avg_data <- CO_avg_data %>%
    group_by(CO) %>%
    arrange(Updated_Date) %>%
    filter(row_number()==n())
  CO_avg_data <- CO_avg_data %>% 
    select(CO, cum_tot_weights, running_avg)  
  names(CO_avg_data)[names(CO_avg_data) == "running_avg"] <- "CO_avg"
  names(CO_avg_data)[names(CO_avg_data) == "cum_tot_weights"] <- "CO_tot_weights"
  # if (i==1) {print(LO_data)}
  num_CO <- length(unique(CO_avg_data$CO))
  CO_num_data <- ddply(LO_data, .(CO), summarise,
                       N=length(LO))
  CO_data <- merge(x=CO_avg_data, y=CO_num_data, by="CO", all=TRUE)
  LO_contrib_data <- merge(x=LO_data, y=CO_data, by="CO", all=TRUE)
  LO_contrib_data$contrib <- LO_contrib_data$cum_sum_weighted/(num_CO*LO_contrib_data$CO_tot_weights*course_avg)
  if (length(names(whole_class_contrib_data)) == 0) {
    whole_class_contrib_data <- LO_contrib_data %>% select(LO, contrib)
  }
  else {
    whole_class_contrib_data <- rbind(whole_class_contrib_data, LO_contrib_data %>% select(LO, contrib))
  }
  if (i==1){
  print(CO_num_data)
  print(CO_avg_data)
  print(CO_data)
  print(LO_contrib_data)
  }
  # sanity check: all should print 1 
  print(sum(LO_contrib_data$contrib))
  
  name_string <- paste('scores_data_chart_student', toString(i), sep="")
  list_CO_contrib_student[[name_string]] <- plotting_LO_avg(data=LO_contrib_data, student_name=student_name, col_name ='LO', plot_title='Contribution of each LO to course grade',course_avg=0, y_col='contrib', min_y=0, max_y=1)

}

# get mean and range of contrib for the whole class for each LO
whole_class_contrib_data <- ddply(whole_class_contrib_data, .(LO), summarise, 
      avg_contrib = mean(contrib),
      min_contrib = min(contrib),
      max_contrib = max(contrib))
whole_class_contrib_data <- whole_class_contrib_data %>% arrange(LO)
whole_class_contrib_data$low <- whole_class_contrib_data$avg_contrib - whole_class_contrib_data$min_contrib
whole_class_contrib_data$high <- whole_class_contrib_data$max_contrib - whole_class_contrib_data$avg_contrib 
whole_class_contrib_data$x <- whole_class_contrib_data$LO
# graph 
whole_class_contrib <- highchart() %>% 
  hc_chart(type = "bar") %>% 
  hc_add_series(whole_class_contrib_data, "errorbar", hcaes(x = x, low = min_contrib, high = max_contrib)) %>%
  hc_add_series(whole_class_contrib_data, hcaes(x=x, y=avg_contrib), type="scatter", name="Mean", color="black") %>% 
  hc_xAxis(type='category') %>%
  hc_title(
    text = 'Range of contribution of each LO to course grade of a student' 
  ) %>% 
  hc_yAxis(max = 1, min=0,
           visible=TRUE
  ) %>%
  hc_tooltip(
    # headerFormat = "",
    # pointFormat=paste('Course average: ', course_avg),
    formatter= JS("function () {
                  if (this.series.name == 'Mean'){
                  return this.point.LO.concat('<br/><b>Mean</b>: ', parseFloat(this.point.y).toFixed(2), '<br/><b>Range</b>: [', parseFloat(this.point.min_contrib).toFixed(2), ', ', parseFloat(this.point.max_contrib).toFixed(2), ']');
                  }}")
    )

