library("tidyr")
library("dplyr")
library("ggplot2")
library("ggpubr")
library("data.table")
library("rcompanion")

setwd("C:/Users/chris/Dropbox/Work/Studies/2020/Document\ Engineering/Study\ Data")





df <- fread("2020-09-03.csv")

df <- tail(df, nrow(df)-2)

df <- df %>% select(-c(1:8, 10:18, 75, 78, 80))




df$first_eval <- "control"
df[FL_6_DO == "Modified|Control", first_eval := "modified"]

df <- df %>% select(-FL_6_DO)






# Scoring for the SUS
sus_score <- function(sus_answers, scoring) {
  sus_answers <- data.table(response = sus_answers, score = 0)
  if (scoring == "normal") {
    sus_answers[response == "Strongly disagree", score := 0]
    sus_answers[response == "Somewhat disagree", score := 1]
    sus_answers[response == "Neither agree nor disagree", score := 2]
    sus_answers[response == "Somewhat agree", score := 3]
    sus_answers[response == "Strongly agree", score := 4]
    
  } else if (scoring == "reversed") {
    sus_answers[response == "Strongly disagree", score := 4]
    sus_answers[response == "Somewhat disagree", score := 3]
    sus_answers[response == "Neither agree nor disagree", score := 2]
    sus_answers[response == "Somewhat agree", score := 1]
    sus_answers[response == "Strongly agree", score := 0]
  }
  return(sus_answers$score)
}


df$score_control <- 0
df$score_control <- df$score_control + sus_score(df$Q7_1, "normal")
df$score_control <- df$score_control + sus_score(df$Q7_2, "reversed")
df$score_control <- df$score_control + sus_score(df$Q7_3, "normal")
df$score_control <- df$score_control + sus_score(df$Q7_4, "reversed")
df$score_control <- df$score_control + sus_score(df$Q7_5, "normal")
df$score_control <- df$score_control + sus_score(df$Q7_6, "reversed")
df$score_control <- df$score_control + sus_score(df$Q7_7, "normal")
df$score_control <- df$score_control + sus_score(df$Q7_8, "reversed")
df$score_control <- df$score_control + sus_score(df$Q7_9, "normal")
df$score_control <- df$score_control + sus_score(df$Q7_10, "reversed")
df$score_control <- df$score_control * 2.5


df$score_modified <- 0
df$score_modified <- df$score_modified + sus_score(df$Q16_1, "normal")
df$score_modified <- df$score_modified + sus_score(df$Q16_2, "reversed")
df$score_modified <- df$score_modified + sus_score(df$Q16_3, "normal")
df$score_modified <- df$score_modified + sus_score(df$Q16_4, "reversed")
df$score_modified <- df$score_modified + sus_score(df$Q16_5, "normal")
df$score_modified <- df$score_modified + sus_score(df$Q16_6, "reversed")
df$score_modified <- df$score_modified + sus_score(df$Q16_7, "normal")
df$score_modified <- df$score_modified + sus_score(df$Q16_8, "reversed")
df$score_modified <- df$score_modified + sus_score(df$Q16_9, "normal")
df$score_modified <- df$score_modified + sus_score(df$Q16_10, "reversed")
df$score_modified <- df$score_modified * 2.5



#SUS breakdown
sus_score(df$Q7_1, "normal") %>% median()
sus_score(df$Q7_2, "normal") %>% median()
sus_score(df$Q7_3, "normal") %>% median()
sus_score(df$Q7_4, "normal") %>% median()
sus_score(df$Q7_5, "normal") %>% median()
sus_score(df$Q7_6, "normal") %>% median()
sus_score(df$Q7_7, "normal") %>% median()
sus_score(df$Q7_8, "normal") %>% median()
sus_score(df$Q7_9, "normal") %>% median()
sus_score(df$Q7_10, "normal") %>% median()


sus_score(df$Q16_1, "normal") %>% median()
sus_score(df$Q16_2, "normal") %>% median()
sus_score(df$Q16_3, "normal") %>% median()
sus_score(df$Q16_4, "normal") %>% median()
sus_score(df$Q16_5, "normal") %>% median()
sus_score(df$Q16_6, "normal") %>% median()
sus_score(df$Q16_7, "normal") %>% median()
sus_score(df$Q16_8, "normal") %>% median()
sus_score(df$Q16_9, "normal") %>% median()
sus_score(df$Q16_10, "normal") %>% median()



df <- df %>% select(-c(8:17, 24:32))



#df %>% select(score_control, score_modified) %>% View



# Task certainty
df <- df %>% rename(control_1_certainty = Q107_1) %>% 
  rename(control_2_certainty = Q114_1) %>% 
  rename(control_3_certainty = Q115_1) %>% 
  rename(modified_1_certainty = Q116_1) %>% 
  rename(modified_2_certainty = Q117_1) %>% 
  rename(modified_3_certainty = Q118_1)


# Task responses
df <- df %>% rename(control_1_answer = Q96) %>% 
  rename(control_2_answer = Q99) %>% 
  rename(control_3_answer = Q100) %>% 
  rename(modified_1_answer = Q101) %>% 
  rename(modified_2_answer = Q102) %>% 
  rename(modified_3_answer = Q103)





# Remove bad results
df <- subset(df, !(`Random ID` %in% c("45424", "24351", "57677", "92573", "45958")))









wilcox.print <- function(wc) {
  z = qnorm(wc$p.value/2)
  r = abs(z)/sqrt(nrow(df))
  print(paste0("Z = ", toString(round(z, 3))))
  print(paste0("p = ", toString(wc$p.value)))
  print(paste0("r = ", toString(round(r, 3))))
}





# Compare the SUS scores


# Overall
df %>% summarise(mdn = median(score_control), IQR = IQR(score_control))
df %>% summarise(mdn = median(score_modified), IQR = IQR(score_modified))
wilcox.test(df$score_control, df$score_modified, paired = T) 
wilcox.print(wilcox.test(df$score_control, df$score_modified, paired = T))

z <- wilcoxonZ(df$score_control, df$score_modified, paired = T); z
p <- wilcox.test(df$score_control, df$score_modified, paired = T)$p.value; p
r = abs(z)/sqrt(length(df$score_control)); r




# First attempt only
control_first <- filter(df, first_eval == "control")$score_control
modified_first <- filter(df, first_eval == "modified")$score_modified

median(control_first); IQR(control_first)
median(modified_first); IQR(modified_first)
wilcox.test(control_first, modified_first)


z <- wilcoxonZ(control_first, modified_first, paired = F); z
p <- wilcox.test(control_first, modified_first, paired = F)$p.value; p
r = abs(z)/sqrt(length(control_first) + length(modified_first)); r



# Non techie
control_first <- filter(df, first_eval == "control" & Q79_1 %in% c("No knowledge", "Some knowledge"))$score_control
modified_first <- filter(df, first_eval == "modified" & Q79_1 %in% c("No knowledge", "Some knowledge"))$score_modified

median(control_first); IQR(control_first)
median(modified_first); IQR(modified_first)
wilcox.test(control_first, modified_first)


z <- wilcoxonZ(control_first, modified_first, paired = F); z
p <- wilcox.test(control_first, modified_first, paired = F)$p.value; p
r = abs(z)/sqrt(length(control_first) + length(modified_first)); r


# Techie
control_first <- filter(df, first_eval == "control" & Q79_1 %in% c("Advanced knowledge", "Expert knowledge"))$score_control
modified_first <- filter(df, first_eval == "modified" & Q79_1 %in% c("Advanced knowledge", "Expert knowledge"))$score_modified

median(control_first); IQR(control_first)
median(modified_first); IQR(modified_first)
wilcox.test(control_first, modified_first)


z <- wilcoxonZ(control_first, modified_first, paired = F); z
p <- wilcox.test(control_first, modified_first, paired = F)$p.value; p
r = abs(z)/sqrt(length(control_first) + length(modified_first)); r













# Compare the task certainty
df$task_complete_control <- 0
df$task_complete_modified <- 0
df$task_certain_control <- 0
df$task_certain_modified <- 0


df[control_1_answer == "", control_1_certainty := "Couldn't answer"]
df[control_2_answer == "", control_2_certainty := "Couldn't answer"]
df[control_3_answer == "", control_3_certainty := "Couldn't answer"]
df[modified_1_answer == "", modified_1_certainty := "Couldn't answer"]
df[modified_2_answer == "", modified_2_certainty := "Couldn't answer"]
df[modified_3_answer == "", modified_3_certainty := "Couldn't answer"]


df[control_1_certainty != "Couldn't answer", task_complete_control := task_complete_control + 1]
df[control_2_certainty != "Couldn't answer", task_complete_control := task_complete_control + 1]
df[control_3_certainty != "Couldn't answer", task_complete_control := task_complete_control + 1]
df[modified_1_certainty != "Couldn't answer", task_complete_modified := task_complete_modified + 1]
df[modified_2_certainty != "Couldn't answer", task_complete_modified := task_complete_modified + 1]
df[modified_3_certainty != "Couldn't answer", task_complete_modified := task_complete_modified + 1]

df[control_1_certainty == "Very certain", task_certain_control := task_certain_control + 1]
df[control_2_certainty == "Very certain", task_certain_control := task_certain_control + 1]
df[control_3_certainty == "Very certain", task_certain_control := task_certain_control + 1]
df[modified_1_certainty == "Very certain", task_certain_modified := task_certain_modified + 1]
df[modified_2_certainty == "Very certain", task_certain_modified := task_certain_modified + 1]
df[modified_3_certainty == "Very certain", task_certain_modified := task_certain_modified + 1]



# Answered
control_first <- filter(df, first_eval == "control")$task_complete_control
modified_first <- filter(df, first_eval == "modified")$task_complete_modified

median(control_first); IQR(control_first)
median(modified_first); IQR(modified_first)
wilcox.test(control_first, modified_first)


z <- wilcoxonZ(control_first, modified_first, paired = F); z
p <- wilcox.test(control_first, modified_first, paired = F)$p.value; p
r = abs(z)/sqrt(length(control_first) + length(modified_first)); r


# Certain
control_first <- filter(df, first_eval == "control")$task_certain_control
modified_first <- filter(df, first_eval == "modified")$task_certain_modified

median(control_first); IQR(control_first)
median(modified_first); IQR(modified_first)
wilcox.test(control_first, modified_first)


z <- wilcoxonZ(control_first, modified_first, paired = F); z
p <- wilcox.test(control_first, modified_first, paired = F)$p.value; p
r = abs(z)/sqrt(length(control_first) + length(modified_first)); r





# Preferences
df %>% group_by(Q31) %>% summarise(n())



chisq.test(data)












# Reasons for disclosures

df <- df %>% rename("reason_general_interest" = Q59_4)
df <- df %>% rename("reason_particular_info" = Q59_10)
df <- df %>% rename("reason_concern" = Q59_5)
df <- df %>% rename("reason_legal" = Q59_8)
df <- df %>% rename("reason_archival" = Q59_9)
df <- df %>% rename("reason_portability" = Q59_11)
df <- df %>% rename("reason_verification" = Q59_12)
df <- df %>% rename("reason_ensure_no_unwanted_data" = Q59_14)
df <- df %>% rename("reason_other" = Q59_7)






reasons_top <- tibble(reason = character(), percentage = numeric())
reasons_bottom<- tibble(reason = character(), percentage = numeric())

temp <- df %>% filter(reason_general_interest %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "General interest", percentage =  temp)
temp <- df %>% filter(reason_particular_info %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "Find a particular \nbit of information", percentage =  temp)
temp <- df %>% filter(reason_concern %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "Concern or distrust \nof the organisation", percentage =  temp)
temp <- df %>% filter(reason_legal %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "Legal Reasons", percentage =  temp)
temp <- df %>% filter(reason_archival %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "Archival purposes", percentage =  temp)
temp <- df %>% filter(reason_portability %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "To transfer that data \nto another service", percentage =  temp)
temp <- df %>% filter(reason_verification %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "Verify correct information", percentage =  temp)
temp <- df %>% filter(reason_ensure_no_unwanted_data %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "Ensure that no unwanted \ndata is being stored", percentage =  temp)
temp <- df %>% filter(reason_other %in% c(1, 2, 3)) %>% nrow()/nrow(df); reasons_top <- add_row(reasons_top, reason = "Other", percentage =  temp)

temp <- df %>% filter(reason_general_interest %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "General interest", percentage =  temp)
temp <- df %>% filter(reason_particular_info %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "Find a particular \nbit of information", percentage =  temp)
temp <- df %>% filter(reason_concern %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "Concern or distrust \nof the organisation", percentage =  temp)
temp <- df %>% filter(reason_legal %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "Legal Reasons", percentage =  temp)
temp <- df %>% filter(reason_archival %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "Archival purposes", percentage =  temp)
temp <- df %>% filter(reason_portability %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "To transfer that data \nto another service", percentage =  temp)
temp <- df %>% filter(reason_verification %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "Verify correct information", percentage =  temp)
temp <- df %>% filter(reason_ensure_no_unwanted_data %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "Ensure that no unwanted \ndata is being stored", percentage =  temp)
temp <- df %>% filter(reason_other %in% c(7, 8, 9)) %>% nrow()/nrow(df); reasons_bottom <- add_row(reasons_bottom, reason = "Other", percentage =  temp)

top_graph <- reasons_top %>%
  filter(reason != "Other") %>%
  ggplot(aes(reorder(reason, -percentage), percentage)) +
  geom_bar(stat="identity") +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name = "\n\n", labels=scales::percent, limits = c(0,1), breaks = seq(0, 1, 0.2), expand = c(0.001, 0.001)) +
  theme_minimal() +
  theme(axis.ticks = element_line(color = "grey80"), plot.title = element_text(hjust = 0.5), legend.title.align = 0.5, legend.position = "bottom", legend.direction = "vertical") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Percentage of participants to list a reason\n in their top-three list of choices\n")

bottom_graph <- reasons_bottom %>%
  filter(reason != "Other") %>%
  ggplot(aes(reorder(reason, -percentage), percentage)) +
  geom_bar(stat="identity") +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name = "\n\n", labels=scales::percent, limits = c(0,1), breaks = seq(0, 1, 0.2), expand = c(0.001, 0.001)) +
  theme_minimal() +
  theme(axis.ticks = element_line(color = "grey80"), plot.title = element_text(hjust = 0.5), legend.title.align = 0.5, legend.position = "bottom", legend.direction = "vertical") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Percentage of participants to list a reason\n in their bottom-three list of choices\n")


ggarrange(top_graph, bottom_graph)




# Past disclosures
df %>% group_by(Q77) %>% summarise(n())




#demographics


df %>% filter(Q79_1 %in% c("Expert knowledge")) %>% nrow()/123
df %>% filter(Q79_1 %in% c("Advanced knowledge")) %>% nrow()/123
df %>% filter(Q79_1 %in% c("Good level of knowledge")) %>% nrow()/123
df %>% filter(Q79_1 %in% c("Some knowledge")) %>% nrow()/123
df %>% filter(Q79_1 %in% c("No knowledge")) %>% nrow()/123


