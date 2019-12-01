df_questions <- read.csv('../questions_only.csv')
head(df_questions[,1:10])
head(df_questions[,11:20])
head(df_questions[,21:30])
head(df_questions[,31:35])
# load main data file
t1 <- Sys.time()
df <- read.csv('../multiple_choice_responses.csv')
t2 <- Sys.time()
print(t2-t1)
# remove 1st data row containing the question itself
n <- nrow(df)
df <- droplevels(df[2:n,])
# show salary levels
cc <- levels(df$Q10)
print(cc)
# change not sorted ranges into numeric representations (here using mid of range)
cc_new <- c(-1,750000,500,1500,12500,112500,137500,17500,175000,2500,22500,225000,27500,275000,3500,35000,400000,4500,45000,6250,55000,65000,8750,75000,85000,95000)
# combine both worlds in a data frame to check if everything is in the right place
df_salary <- data.frame(range=levels(df$Q10), value=cc_new)
print(df_salary)
# now replace the ranges by those numeric values
levels(df$Q10) <- cc_new
summary(df$Q10)
# remove missings...
df_select <- dplyr::filter(df, df$Q10 != '-1')
df_select <- droplevels(df_select)
# ... and generate right order
df_select$Q10num <- as.numeric(as.character(df_select$Q10))
df_select$Q10ord <- as.factor(df_select$Q10num)
summary(df_select$Q10ord)
# first visualization
plot(as.factor(df_select$Q10ord), las=2)
sel_country <- 'United States of America'
sel_role <- 'Data Scientist'
df_filter <- dplyr::filter(df_select, Q3==sel_country & Q5==sel_role)
plot(as.factor(df_filter$Q10ord), las=2, main='USA')
# numeric evaluation
salary_eval_num <- df_filter$Q10num
summary(salary_eval_num)
hist(salary_eval_num,25)
plot(ecdf(salary_eval_num))
grid()
# another example
sel_country <- 'Germany'
sel_role <- 'Data Scientist'
df_filter <- dplyr::filter(df_select, Q3==sel_country & Q5==sel_role)
plot(as.factor(df_filter$Q10ord), las=2, main='Germany')
salary_eval_num <- df_filter$Q10num
summary(salary_eval_num)
hist(salary_eval_num,50)
plot(ecdf(salary_eval_num))
grid()
# yet another example
sel_country <- 'Japan'
sel_role <- 'Data Scientist'
df_filter <- dplyr::filter(df_select, Q3==sel_country & Q5==sel_role)
plot(as.factor(df_filter$Q10ord), las=2, main='Japan')
salary_eval_num <- df_filter$Q10num
summary(salary_eval_num)
hist(salary_eval_num,25)
plot(ecdf(salary_eval_num))
grid()

all_countries <- levels(df$Q3)
sel_role <- 'Data Scientist'

for(country in all_countries) {
  print(country)
  df_now <- dplyr::filter(df_select, Q3==country & Q5==sel_role)
  salary_now <- df_now$Q10num
  print(paste0('# values: ', length(salary_now)))
  print(summary(salary_now))
  cat('\n')
}

sel_role <- 'Data Scientist'

df_select_role <- dplyr::filter(df_select, Q5==sel_role)

# count frequencies per country
tab_country <- as.data.frame(table(df_select_role$Q3))
# select only countries having certain number of values
tab_country_sel <- dplyr::filter(tab_country, Freq>60)
countries_sel <- as.character(tab_country_sel$Var1)
# remove "other" category which is misleading here
countries_sel <- setdiff(countries_sel, 'Other')

df_now <- dplyr::filter(df_select_role, Q3 %in% countries_sel)
# prepare data for boxplot etc.
data_for_boxplot <- data.frame(country = df_now$Q3,
                               value = df_now$Q10num)

# reduce long names for USA and UK
levels(data_for_boxplot$country) <- c(levels(data_for_boxplot$country),'USA','UK')
data_for_boxplot$country[data_for_boxplot$country=='United States of America'] <- 'USA'
data_for_boxplot$country[data_for_boxplot$country=='United Kingdom of Great Britain and Northern Ireland'] <- 'UK'
# boxplot
bxplt <- ggplot(data_for_boxplot, aes(country, value, fill=country)) + geom_boxplot()
bxplt

# violin plot
vlplt <- ggplot(data_for_boxplot, aes(country, value, fill=country)) + geom_violin()
vlplt

sel_country <- 'United States of America'

df_now <- dplyr::filter(df_select, Q3==sel_country)

data_for_boxplot <- data.frame(role = df_now$Q5,
                               value = df_now$Q10num)
# violin plot
vlplt <- ggplot(data_for_boxplot, aes(role, value, fill=role)) + geom_violin()
vlplt + ggtitle('Comparison by role') + theme(axis.text.x = element_text(angle = 90))

# box plot
bxplt <- ggplot(data_for_boxplot, aes(role, value, fill=role)) + geom_boxplot()
bxplt + ggtitle('Comparison by role') + theme(axis.text.x = element_text(angle = 90))

# zoom in
bxplt + ylim(0, 250000) + ggtitle('Comparison by role') + theme(axis.text.x = element_text(angle = 90))

df_DS <- dplyr::filter(df_now, Q5=='Data Scientist')
df_DE <- dplyr::filter(df_now, Q5=='Data Engineer')
print(paste0('Data Scientist #: ', nrow(df_DS)))
print(paste0('Data Engineer  #: ', nrow(df_DE)))

summary(df_DS$Q10num)

summary(df_DE$Q10num)

hist(df_DS$Q10num, 30, freq=FALSE, col='#00FF0040', ylim=c(0,1.5e-5), main='Data Scientist (green) vs Data Engineer (blue)')
hist(df_DE$Q10num, 30, freq=FALSE, add=TRUE, col='#0000FF40')

# zoom in range (0,200k)
hist(df_DS$Q10num, 120, freq=FALSE, col='#00FF0040', xlim=c(0,200000), ylim=c(0,4e-5), main='Data Scientist (green) vs Data Engineer (blue)')
hist(df_DE$Q10num, 120, freq=FALSE, add=TRUE, col='#0000FF40')

# compare empirical CDFs
plot(ecdf(df_DS$Q10num), col='purple')
plot(ecdf(df_DE$Q10num), col='orange', add=TRUE)
grid()


