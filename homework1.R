#Daphne Homework
#Don't need to set a wd beacause github does it for you. Never worry about file paths again
#Section I
teacher<-read.csv("~/Documents/GitHub/datascience/teachingRatings.csv", stringsAsFactors = TRUE)
summary(teacher)
#Section II: Descriptive Statistics

#Part A
teacher_stats<-get_summary_stats(teacher)
teacher_stats1<- teacher_stats[ ,c(1:4, 10:11, 5:8)]
flextable(teacher_stats1)
hist(teacher$eval)

#Part B

teachers_evalCat <- teacher %>% mutate(
  evalCateg=case_when(evalCat == 0 ~ "Low",
                     evalCat == 1 ~ "Average",
                     evalCat == 2 ~ "High"),
  evalCateg = factor(evalCateg, levels=c("Low",  "Average", "High")))

evalCateg_table<-table1( ~ evalCateg, data = teachers_evalCat)
t1flex(evalCateg_table) %>% 
  save_as_docx(path="homework1_frequencies.docx")

#Section III: Bivariate Associations

#Part A
apatheme <- theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='serif')) # setting up some of display options


ggplot(data = teacher, 
       mapping = aes(x = eval,
                     y = age)) +
  geom_point(colour = "black", 
             fill = "grey60", 
             size = 2, 
             shape = 21, 
             alpha = .60, 
             position = position_jitter()) + 
  geom_smooth(method = "lm", 
              se = TRUE, 
              fullrange = TRUE, 
              colour = "black", 
              fill = "grey60") + 
  scale_x_continuous(breaks = seq(1, 7, by = .5)) +  
  scale_y_continuous(breaks = seq(1, 7, by = 1)) + 
  labs(x = "Course Evaluation Score",
       y = "Course Age",
       title = "Course Evaluation Score by Instructor Age") +
  apatheme


cor(teacher$eval, teacher$age)



#Part B
teachers_evalCat2 <- teachers_evalCat %>% mutate(
  diffcat = case_when(
    difficulty == 0 ~ "Less_Difficult",
    difficulty == 1 ~ "Average",
    difficulty == 2 ~ "More_Difficult"),
  diffcat = factor(diffcat, levels=c("Less_Difficult", "Average", "More_Difficult")))


bar_plot <- ggplot(data = teachers_evalCat2, mapping = aes(x = diffcat, fill = evalCateg)) + 
  geom_bar(position = "dodge", aes(y = ..prop.., group = evalCateg)) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Course Evaluation by Difficulty Category",
       x = "Course Difficulty", 
       y = "Proportion of Evaluations", 
       fill = "Evaluation Category") +
  scale_fill_manual(values = c("Low" = "red", "Average" = "yellow", "High" = "green")) +
  theme_minimal() 

print(bar_plot)

options(scipen = 999) 

CrossTable(teachers_evalCat2$evalCateg, teachers_evalCat2$diffcat,
           prop.chisq = FALSE, chisq = TRUE, 
           expected = TRUE, sresid = TRUE, format = "SPSS", 
           dnn = c('Evaluation', 'Course Difficulty')) 


