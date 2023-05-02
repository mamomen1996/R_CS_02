# Case-Study Title: Story Telling with Data (decision making and reasoning based-on data visualization)
# Data Analysis methodology: CRISP-DM
# Dataset: research academic paper in economics (Harvard business review magazine)
# Case Goal: Try to replicate CS_02_03_1.png: compare distribution and mean of annual income levels of population in 3 class at two years


### Required Library ----
install.packages('ggplot2')
install.packages('readxl')
library('ggplot2')
library('readxl')


### Read Data from File ----
readxl::excel_sheets("CS_02_03.xlsx")
data <- readxl::read_xlsx(path = "CS_02_03.xlsx", sheet = "3. Median HH income, metro")
dim(data)
summary(data)
str(data)


### Data PreProcessing ----
# Data Preparation
data_1 <- data[-c(1:5,8), -6]
data_1 <- as.data.frame(data_1)
colnames(data_1)[1] <- "Metropolitan area"
View(head(data_1))

data_2 = data.frame(metropolitan_area = c(''), median_HH_income = c(0), income_class = c(''), year = c(''))
for(i in 3:nrow(data_1)){
  for(j in 2:ncol(data_1)){
    data_2 <- rbind(data_2, list(data_1[i,1], data_1[i,j], data_1[2,j], data_1[1,ifelse(j < 6, 2, 6)]))
  }
}
data_2 <- data_2[-1,]

data_2$year <- factor(data_2$year)
data_2$median_HH_income <- as.numeric(data_2$median_HH_income)

View(head(data_2, 10))
dim(data_2)
colnames(data_2)
summary(data_2)

data_2$median_HH_income_per_1000 <- round(data_2$median_HH_income / 1000, 2)
data_2 <- data_2[-which(data_2$income_class == "All"),]
data_2$income_class <- factor(data_2$income_class,
                              levels = c("Lower","Middle","Upper"),
                              labels = c("Lower Class", "Middle Class", "Upper Class"))
View(head(data_2, 10))

data_means <- round(tapply(data_2$median_HH_income, list(data_2$income_class, data_2$year), mean))
data_means
class(data_means)
year <- rep(levels(data_2$year),3)
income_class <- rep(levels(data_2$income_class),each = 2)
data_means_df <- data.frame(income_class = income_class, year = year, average = as.vector(t(data_means)))
View(data_means_df)


### Data Visualization ----
ggplot(data_2, aes(x = median_HH_income_per_1000)) +
  ggtitle("Median Household Income by Income Tier Across U.S. Metropolitan Areas",
  subtitle = "Average median income across 229 metros decreased from $67,863 in 1999 to $62,662 in 2014, representing an 8% loss in\nincome, The lower income class experienced the largest impact with a 11% decrease while the middle and upper class median\nhousehold income decreased by 6% and 8% respectively.") +
  labs(caption = "Source: Pew Research Center analysis of the\n2000 decennial census and 2014 American\nCommunity Survey (IPUMS)") +
  geom_histogram(bins = 30, aes(fill = year), color = 'white') +
  facet_grid(cols = vars(income_class), rows = vars(year), scales = "free_x") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(breaks = seq(0, 50, 10),
                     limits = c(0, 60)) +
  scale_fill_discrete(guide = "none") +
  geom_vline(data = data_means_df, aes(xintercept = round(average / 1000, 2)), linetype = 'dashed') +
  geom_text(data = data_means_df,
            aes(label = paste0(format("$", width = 17, justify = "right"), trimws(format(average, big.mark = ",")))), 
            x = (round(data_means_df$average / 1000, 2)), 
            y = 52) +
  xlab("Median Household Income (thousands)") +
  ylab("Frequency") +
  theme_light() +
  theme(
        plot.title = element_text(hjust = 0, color = "black", size = 17, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(color = "gray70"),
        axis.title.x = element_text(size = 13, color = "gray20", vjust = -3),
        axis.title.y = element_text(size = 13, color = "gray70", vjust = 4, margin = margin(l = 5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(size = 7, hjust = 0, margin = margin(t = 10), color = "gray70"),
        plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15)),
        panel.spacing.x = grid::unit(x = 1.5, units = "lines"),
        panel.spacing.y = grid::unit(x = 0.5, units = "lines"),
        strip.text = element_text(color = "black"))
