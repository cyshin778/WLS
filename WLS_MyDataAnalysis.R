library(readr)
df <- read_csv("C:/Users/ChaeYoon Shin/OneDrive/Desktop/WLS project/wls_phenotype_data.csv")

summary(df$Neuroticism_GPS) # 최소, 최대, 중앙값, 사분위수 확인

# 1. Neuroticism GPS에 따라 세 그룹으로 나누기

df$Neuroticism_GPS_scaled <- scale(df$Neuroticism_GPS)  # PRS를 Z-score로 변환
summary(df$Neuroticism_GPS_scaled)

hist(df$Neuroticism_GPS_scaled)      # 히스토그램으로 분포 확인
boxplot(df$Neuroticism_GPS_scaled) # 이상치(outliers) 확인
length(df$Neuroticism_GPS_scaled)


# 백분위수 계산
q20 <- quantile(df$Neuroticism_GPS_scaled, 0.20, na.rm = TRUE)  # 하위 20%
q80 <- quantile(df$Neuroticism_GPS_scaled, 0.80, na.rm = TRUE)  # 상위 20%

# GPS 그룹 나누기
df$Neuroticism_GPS_group <- cut(Neuroticism_GPS_scaled, 
                            breaks = c(-Inf, q20, q80, Inf), 
                            labels = c("GPS_Bottom20", "GPS_Middle60", "GPS_Top20"))

table(df$Neuroticism_GPS_group)  # 각 그룹별 개수 출력

df$Neuroticism_GPS_group


# Childhood_ELS_R5 (독립변수) 1~4로 값 저장하기

breaks <- seq(min(df$Childhood_ELS_R5), max(df$Childhood_ELS_R5), length.out = 5)  # 5는 4개의 구간을 나누기 위한 경계

# 각 값이 속한 그룹을 찾기
grouped_data <- cut(df$Childhood_ELS_R5, breaks = breaks, labels = c(1, 2, 3, 4), include.lowest = TRUE)

# 데이터프레임에 그룹 열 추가
df$Childhood_ELS_R5_group <- grouped_data

# 아동학대 경험 그룹 나누기- 1(low), 2(moderate), 3,4(high)
df$Childhood_ELS_R5_group_level <- factor(
  ifelse(df$Childhood_ELS_R5_group == 1, "Low",
         ifelse(df$Childhood_ELS_R5_group == 2, "Moderate", "High")),
  levels = c("Low", "Moderate", "High") 
)

model <- lm(Depression_score_R6 ~ Childhood_ELS_R5_group_level * Neuroticism_GPS_scaled, data = df)
summary(model)


model <- lm(Depression_score_R6 ~ Childhood_ELS_R5_group_level * Depression_GPS_scaled, data = df)
summary(model)

# Childhood_ELS_R5 : split into tertiles

library(dplyr)

df$Childhood_ELS_R5_tertile <- ntile(df$Childhood_ELS_R5,3)

df$Childhood_ELS_R5_tertile <- as.factor(df$Childhood_ELS_R5_tertile)

model <- lm(Depression_score_R6 ~ Childhood_ELS_R5_tertile * Neuroticism_GPS_scaled, data = df)
summary(model)

# # Childhood_ELS_R5 : split into 20,60,20

df$percentile <- cut(df$Childhood_ELS_R5, 
                  breaks = quantile(df$Childhood_ELS_R5, probs = c(0, 1/5, 4/5, 1), na.rm = TRUE), 
                  labels = c("Bottom20", "Intermediate60", "High20"), 
                  include.lowest = TRUE)

df$percentile <- as.factor(df$percentile)

model <- lm(Depression_score_R6 ~ percentile * Neuroticism_GPS_scaled, data = df)
summary(model)




# 2. GPS 수준마다 neuroticism GPS, Childhood_ELS_R5_group, Depression features만 모아놓은 새로운 dataframe (R5, R6) 만들기

GPS_bottom_20 <- subset(df, Neuroticism_GPS_group == "GPS_Bottom20")[, c("Neuroticism_GPS_scaled", "SF12_mental_R6", "Depression_num_R6", "Depression_score_R6", "SF12_mental_d", "Depression_num_d2", "Depression_score_d2",
                                                                        "SF12_mental_R5","Depression_num_R5","Depression_score_R5","Depression_num_d1","Depression_score_d1","Childhood_ELS_R5_group")]
GPS_middle_60 <- subset(df, Neuroticism_GPS_group == "GPS_Middle60")[, c("Neuroticism_GPS_scaled", "SF12_mental_R6", "Depression_num_R6", "Depression_score_R6", "SF12_mental_d", "Depression_num_d2", "Depression_score_d2",
                                                                        "SF12_mental_R5","Depression_num_R5","Depression_score_R5","Depression_num_d1","Depression_score_d1","Childhood_ELS_R5_group")]
GPS_top_20 <- subset(df, Neuroticism_GPS_group == "GPS_Top20")[, c("Neuroticism_GPS_scaled", "SF12_mental_R6", "Depression_num_R6", "Depression_score_R6", "SF12_mental_d", "Depression_num_d2", "Depression_score_d2",
                                                                  "SF12_mental_R5","Depression_num_R5","Depression_score_R5","Depression_num_d1","Depression_score_d1","Childhood_ELS_R5_group")]


# 각 GPS 그룹에서 아동학대 경험 low vs high 로 나눔
GPSbottom20_childlow <- subset(GPS_bottom_20, Childhood_ELS_R5_group == 1 | Childhood_ELS_R5_group == 2)
GPSbottom20_childhigh <- subset(GPS_bottom_20, Childhood_ELS_R5_group == 3 | Childhood_ELS_R5_group == 4)
GPSmiddle60_childlow <- subset(GPS_middle_60, Childhood_ELS_R5_group == 1 | Childhood_ELS_R5_group == 2)
GPSmiddle60_childhigh <- subset(GPS_middle_60, Childhood_ELS_R5_group == 3 | Childhood_ELS_R5_group == 4)
GPStop20_childlow <- subset(GPS_top_20, Childhood_ELS_R5_group == 1 | Childhood_ELS_R5_group == 2)
GPStop20_childhigh <- subset(GPS_top_20, Childhood_ELS_R5_group == 3 | Childhood_ELS_R5_group == 4)

sum(df$Childhood_ELS_R5_group %in% c(1, 2))
sum(df$Childhood_ELS_R5_group %in% c(3, 4))




# 각 그룹별 Childhood_ELS_R5_group의 빈도수 출력
list("GPS_bottom_20" = table(GPS_bottom_20$Childhood_ELS_R5_group))
list("GPS_middle_60" = table(GPS_middle_60$Childhood_ELS_R5_group))
list("GPS_top_20" = table(GPS_top_20$Childhood_ELS_R5_group))


count_3_4 <- sum(GPS_bottom_20$Childhood_ELS_R5_group %in% c(3, 4))
total_count <- length(GPS_bottom_20$Childhood_ELS_R5_group)
ratio_3_4 <- count_3_4 / total_count
cat("3,4 비율: ", ratio_3_4 * 100, "%")

count_3_4 <- sum(GPS_middle_60$Childhood_ELS_R5_group %in% c(3, 4))
total_count <- length(GPS_middle_60$Childhood_ELS_R5_group)
ratio_3_4 <- count_3_4 / total_count
cat("3,4 비율: ", ratio_3_4 * 100, "%")

count_3_4 <- sum(GPS_top_20$Childhood_ELS_R5_group %in% c(3, 4))
total_count <- length(GPS_top_20$Childhood_ELS_R5_group)
ratio_3_4 <- count_3_4 / total_count
cat("3,4 비율: ", ratio_3_4 * 100, "%")




depression_df <- list(GPS_bottom_20 = GPS_bottom_20, GPS_middle_60 = GPS_middle_60, GPS_top_20 = GPS_top_20)


# ANOVA- 세 그룹 간 Depression_R6_score가 유의미한 차이가 있는가 (yes, p<0 수준에서)

df_combined <- rbind(
  data.frame(Group = "GPS_bottom_20", depression_score_R6 = depression_df$GPS_bottom_20$Depression_score_R6),
  data.frame(Group = "GPS_middle_60", depression_score_R6 = depression_df$GPS_middle_60$Depression_score_R6),
  data.frame(Group = "GPS_top_20", depression_score_R6 = depression_df$GPS_top_20$Depression_score_R6)
)

# ANOVA
anova_result <- aov(depression_score_R6 ~ Group, data = df_combined)
summary(anova_result)

# 사후 분석 (Tukey HSD)
tukey_result <- TukeyHSD(anova_result)
tukey_result



# Depression score threshold = 1, 1.5

R5_depression <- subset(df, Depression_score_R5 >= 1.5)
R5_notdepression <- subset(df, Depression_score_R5 < 1.5)
R6_depression <- subset(df, Depression_score_R6 >= 1.5)
R6_notdepression <- subset(df, Depression_score_R6 < 1.5)

R5oR6o <- subset(df, (Depression_score_R5 >= 1.5) & (Depression_score_R6 >= 1.5))
R5xR6o <- subset(df, (Depression_score_R5 < 1.5) & (Depression_score_R6 >= 1.5))
R5oR6x <- subset(df, (Depression_score_R5 >= 1.5) & (Depression_score_R6 < 1.5))
R5xR6x <- subset(df, (Depression_score_R5 < 1.5) & (Depression_score_R6 < 1.5))

conf_matrix <- table(R5_depression = df$Depression_score_R5 >= 1.5,
                     R6_depression = df$Depression_score_R6 >= 1.5)


library(ggplot2)

counts <- data.frame(group = c("R5oR6o","R5xR6o","R5oR6x","R5xR6x"),
                                count = c(nrow(R5oR6o),nrow(R5xR6o),nrow(R5oR6x),nrow(R5xR6x)))
ggplot(counts, aes(x=group, y= count)) +
  geom_bar(stat = "identity", fill="blue") +
  geom_text(aes(label = count), vjust = -0.3) +
  labs(title = "Depression Change (Threshold: Depression_R6_score = 1.5)", x="Group", y="Count")


# depression 변수 모두 적용

R5_depression <- with(df, Depression_score_R5 >= 1 & SF12_mental_R5 < 0 & Depression_num_R5 >= 1)
R6_depression <- with(df, Depression_score_R6 >= 1 & SF12_mental_R6 < 0 & Depression_num_R6 >= 1)

conf_matrix = table(R5_depression = R5_depression, R6_depression = R6_depression)

library(ggplot2)
library(dplyr)

conf_df <- as.data.frame(conf_matrix)

conf_df$group <- case_when(
  conf_df$R5_depression == TRUE  & conf_df$R6_depression == TRUE  ~ "Prevalent: R5o -> R6o",
  conf_df$R5_depression == TRUE  & conf_df$R6_depression == FALSE ~ "R5o -> R6x",
  conf_df$R5_depression == FALSE & conf_df$R6_depression == TRUE  ~ "Incident: R5x -> R6o",
  conf_df$R5_depression == FALSE & conf_df$R6_depression == FALSE ~ "R5x -> R6x"
)


conf_df$group <- gsub("Incident", "Incident\n", conf_df$group)  # "R5o" 뒤에 줄바꿈 추가
conf_df$group <- gsub("Prevalent", "Prevalent\n", conf_df$group)  # "R5x" 뒤에 줄바꿈 추가
        

ggplot(conf_df, aes(x = group, y = Freq, fill = group)) +
  geom_bar(stat = "identity") + 
  labs(title = "Depression Change",
       x = "Depression Status (R5 vs R6)",
       y = "Count") +
  geom_text(aes(label=Freq), vjust=-0.3)
  theme_minimal() +
  theme(axis.text.x = element_text(size=1, angle = 90, hjust = 1))




# GPS 그룹 내에서 depression O,X 나누기


library(ggplot2)
library(gridExtra)

dfs <- list(GPS_bottom_20 = GPS_bottom_20, 
            GPS_middle_60 = GPS_middle_60, 
            GPS_top_20 = GPS_top_20)

# 각 데이터프레임에 대해 confusion matrix 계산 및 그래프 생성
plots <- lapply(names(dfs), function(name) {
  df <- dfs[[name]]
  
  R5_depression <- with(df, Depression_num_R5 >= 1) 
  R6_depression <- with(df, Depression_num_R6 >= 1)
  
  # confusion matrix
  cm <- table(R5_depression = R5_depression, R6_depression = R6_depression)
  cm_df <- as.data.frame(cm)
  
  # bar graph
  bar_plot <- ggplot(cm_df, aes(x = interaction(R5_depression, R6_depression), y = Freq)) +
    geom_bar(stat = "identity", fill='blue') +
    geom_text(aes(label = Freq), vjust = -0.3) +
    labs(title = name,
         x = "Depression Status (R5 vs R6)",
         y = "Count") +
    theme_minimal() +
    scale_x_discrete(labels = c("R5x -> R6x", "R5o -> R6x", "Incident : R5x -> R6o", "Prevalent : R5o -> R6o")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
})

plots[[1]]
plots[[2]]
plots[[3]]


# 조절효과 분석 with Caregiving_give_advice_R5, Caregiving_get_advice_R5

model <- lm(df$Depression_score_R6 ~ df$Neuroticism_GPS_scaled*df$Caregiving_give_advice_R6*df$Caregiving_get_advice_R6, data=df)


model <- lm(df$Depression_score_R6 ~ df$Neuroticism_GPS_scaled*df$Love_social_R5*df$Support_family_R5*df$Support_problem_R5)
summary(model)




# 

df$Depression_GPS_scaled <- scale(df$Depression_GPS)  # PRS를 Z-score로 변환
summary(df$Depression_GPS_scaled)


model <- lm(Depression_score_R6 ~ Depression_GPS_scaled * Childhood_ELS_R5, data=df)
summary(model)



model <- lm(Depression_score_R6 ~ Depression_GPS_scaled * Caregiving_give_advice_R6, data=df)
summary(model)









