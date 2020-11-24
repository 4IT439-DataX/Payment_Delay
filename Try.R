
### Dependencies ---------------------------------------------------------------
rm(list = ls()) # clear

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(naniar)) install.packages("naniar")
if (!require(styler)) install.packages("styler")
if (!require(GGally)) install.packages("GGally")
if (!require(skimr)) install.packages("skimr")
if (!require(ggcorrplot)) install.packages("ggcorrplot")
if (!require(gridExtra)) installed.packages("gridExtra")

library(tidyverse)
library(naniar)
library(styler)
library(GGally)
library(skimr)
library(ggcorrplot)
library(gridExtra)


# Load the initial data --------------------------------------------------------

# Put data files outside of the git folder in order to avoid pushing too large
# files to repository
# path_to_data <- 'D:/..../payment_dates_final.csv'
path_to_data <- "D:/01 Skola VSE Statistika/DataX/zaverecny projekt/payment_dates_final.csv"
# path_to_data <- "..\\payment_dates_final.csv"

data_collection <- read.csv(path_to_data)

# Data understanding -----------------------------------------------------------

# Data description =============================================================
# Data volume (number of rows and columns)
nrow <- nrow(data_collection)
ncol <- ncol(data_collection)

# Convert columns to the correct data type
data_collection <- data_collection %>%
  mutate(due_date = as.Date(due_date, format = "%Y-%m-%d"))
data_collection <- data_collection %>%
  mutate(payment_date = as.Date(payment_date, format = "%Y-%m-%d"))
data_collection <- data_collection %>%
  mutate(product_type = as.factor(product_type))
data_collection <- data_collection %>%
  mutate(contract_status = as.factor(contract_status))
data_collection <- data_collection %>%
  mutate(business_discount = as.factor(business_discount))
data_collection <- data_collection %>%
  mutate(gender = as.factor(gender))
data_collection <- data_collection %>%
  mutate(marital_status = as.factor(marital_status))
data_collection <- data_collection %>%
  mutate(clients_phone = as.factor(clients_phone))
data_collection <- data_collection %>%
  mutate(client_mobile = as.factor(client_mobile))
data_collection <- data_collection %>%
  mutate(client_email = as.factor(client_email))
data_collection <- data_collection %>%
  mutate(total_earnings = factor(total_earnings, labels = c(
    "level1", "level2", "level3", "level4",
    "level5", "level6", "level7", "level8",
    "level9", "level10", "not_declared"
  )))
data_collection <- data_collection %>%
  mutate(living_area = as.factor(living_area))
data_collection <- data_collection %>%
  mutate(different_contact_area = as.factor(different_contact_area))
data_collection <- data_collection %>%
  mutate(kc_flag = as.factor(kc_flag))
# Problem! cf_val appears to not be a factor, despite the description file !!!
data_collection <- data_collection %>%
  mutate(cf_val = as.numeric(cf_val))
data_collection <- data_collection %>%
  mutate(kzmz_flag = as.factor(kzmz_flag))
data_collection <- data_collection %>%
  mutate(due_amount = as.numeric(due_amount))
data_collection <- data_collection %>%
  mutate(paid_amount = as.numeric(payed_ammount))

# Remove feature "payed_ammount" which was replaced by feature "paid_amount"
data_collection <- subset(data_collection, select = -payed_ammount)

# Create a feature for delay in days
data_collection$delay <- difftime(data_collection$payment_date,
  data_collection$due_date, tz,
  units = "days"
)
data_collection <- data_collection %>%
  mutate(delay = as.numeric(delay))

# Display the internal structure of the data
str(data_collection)

# Summary statistics of the data
summary <- summary(data_collection)
detailed_statistics <- skim(data_collection)

# Analyze correlations between variables #######################################
# Compute a matrix of correlation p-values and plot the correlation matrix
p.mat <- data_collection %>%
  select_if(is.numeric) %>%
  cor_pmat()

correlogram <- data_collection %>%
  drop_na() %>%
  select_if(is.numeric) %>%
  cor() %>%
  ggcorrplot(
    p.mat = p.mat,
    type = "lower", hc.order = T, ggtheme = theme_minimal,
    colors = c("#6D9EC1", "white", "#E46726"),
    show.diag = T, lab_size = 5, title = "Correlation Matrix",
    legend.title = "Correlation Value",
    outline.color = "white"
  )

# Compute correlation matrix between all possible pairs of variables and select
#   only rows containing moderate or strong values of correlation coefficient
data_collection %>%
  select_if(is.numeric) %>%
  cor() %>%
  round(., 2) %>%
  data.frame() %>%
  rownames_to_column("rows") %>%
  mutate(across(), replace(., . == 1, 0)) %>%
  column_to_rownames("rows") %>%
  filter_all(any_vars(abs(.) >= 0.3))

# Test the significance of found correlation
correlation <- cor.test(data_collection$due_amount, data_collection$paid_amount,
  method = "pearson"
)

# Examine relationship between categorical features using chi-squared test with
#   the significance level of 0.05
# Overestimating the matrix size saves time compared to building the matrix one
#  row at a time:
categorical_rel <- matrix(nrow = choose(ncol(data_collection), 2), ncol = 2)
cont_vector <- vector("integer")
iteration <- 0
for (i in c(1:(ncol(data_collection) - 1))) {
  if (is.factor(data_collection[, i])) {
    for (j in c((i + 1):ncol(data_collection))) {
      iteration <- iteration + 1
      if (is.factor(data_collection[, j])) {
        contingency_table <- table(data_collection[, i], data_collection[, j])
        chisq <- (chisq.test(contingency_table, correct = FALSE))
        if (chisq$p.value <= 0.05) {
          categorical_rel[iteration, ] <- c(i, j)
          cont_vector <- c(cont_vector, i, j)
        }
      }
    }
  }
}

# Save the pairs of categorical features that are correlated into a data frame
categorical_rel <- as.data.frame(categorical_rel)
categorical_rel <- categorical_rel %>%
  filter_all(any_vars(!is.na(.)))

cont_vector <- unique(cont_vector)

data_collection %>%
  ggplot(aes(data_collection[, 6], fill = data_collection[, 6])) +
  geom_bar() +
  facet_grid(~ data_collection[, 5]) +
  labs(
    title = paste0(
      "Distribution of ", names(data_collection)[6], " by ",
      names(data_collection)[5]
    ),
    fill = paste0(names(data_collection)[6]),
    x = paste0(names(data_collection)[6]),
    y = "Count",
    color = paste0()
  ) +
  theme_minimal()

data_collection %>%
  ggplot(aes(data_collection[, 5])) +
  geom_bar(aes(fill = data_collection[, 6]), positon = "fill")

# Univariate analysis of numeric variables #####################################
# Summary for each attribute
headofTable <- c(
  "Num. of Children", "Num. Other Product", "Year of Birth",
  "Due amount", "Paid amount", "Delay"
)
EX <- c(
  mean(data_collection$number_of_children),
  mean(data_collection$number_other_product), mean(data_collection$birth_year),
  mean(data_collection$due_amount), mean(data_collection$paid_amount),
  mean(data_collection$delay)
)
VarX <- c(
  var(data_collection$number_of_children),
  var(data_collection$number_other_product), var(data_collection$birth_year),
  var(data_collection$due_amount), var(data_collection$paid_amount),
  var(data_collection$delay)
)
Median <- c(
  median(data_collection$number_of_children),
  median(data_collection$number_other_product),
  median(data_collection$birth_year), median(data_collection$due_amount),
  median(data_collection$paid_amount), median(data_collection$delay)
)
Q1 <- c(
  quantile(data_collection$number_of_children, probs = 1 / 4),
  quantile(data_collection$number_other_product, probs = 1 / 4),
  quantile(data_collection$birth_year, probs = 1 / 4),
  quantile(data_collection$due_amount, probs = 1 / 4),
  quantile(data_collection$paid_amount, probs = 1 / 4),
  quantile(data_collection$delay, probs = 1 / 4)
)
Q3 <- c(
  quantile(data_collection$number_of_children, probs = c(3 / 4)),
  quantile(data_collection$number_other_product, probs = c(3 / 4)),
  quantile(data_collection$birth_year, probs = c(3 / 4)),
  quantile(data_collection$due_amount, probs = c(3 / 4)),
  quantile(data_collection$paid_amount, probs = c(3 / 4)),
  quantile(data_collection$delay, probs = 3 / 4)
)
Min <- c(
  min(data_collection$number_of_children),
  min(data_collection$number_other_product),
  min(data_collection$birth_year), min(data_collection$due_amount),
  min(data_collection$paid_amount), min(data_collection$delay)
)
Max <- c(
  max(data_collection$number_of_children),
  max(data_collection$number_other_product), max(data_collection$birth_year),
  max(data_collection$due_amount), max(data_collection$paid_amount),
  max(data_collection$delay)
)

summaryData <- distinct(data.frame(headofTable, EX, VarX, Median, Q1, Q3, Min,
  Max,
  check.rows = FALSE, check.names = FALSE
))

# Density plots for numeric attributes and dates
density_plots <- list()

density_plots[[1]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_bar(aes(contract_id), fill = "grey70") +
  geom_vline(
    xintercept = mean(data_collection$contract_id),
    color = "blue", linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$contract_id),
    color = "red", linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of contract_id",
    x = "Contract ID",
    y = "Count"
  ) +
  annotate(
    geom = "text", x = mean(data_collection$contract_id),
    y = 25, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$contract_id),
    y = 75, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[2]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(payment_order)) +
  geom_density() +
  geom_vline(
    xintercept = mean(data_collection$payment_order, na.rm = TRUE),
    color = "blue", linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$payment_order, na.rm = T),
    color = "red", linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of payment_order",
    x = "Payment order",
    y = "Count"
  ) +
  annotate(
    geom = "text", x = mean(data_collection$payment_order, na.rm = T),
    y = 0.01, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$payment_order, na.rm = T),
    y = 0.03, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[3]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(number_of_children)) +
  geom_bar(fill = "grey70") +
  geom_vline(
    xintercept = mean(data_collection$number_of_children),
    color = "blue", linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$number_of_children),
    color = "red", linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of number_of_children",
    x = "Number of children",
    y = "Count"
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 1)) +
  annotate(
    geom = "text", x = mean(data_collection$number_of_children),
    y = 500000, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$number_of_children),
    y = 1000000, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[4]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(number_other_product)) +
  geom_bar(fill = "grey70") +
  geom_vline(
    xintercept = mean(data_collection$number_other_product),
    color = "blue", linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$number_other_product),
    color = "red", linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of number_other_product",
    x = "Number other products",
    y = "Count"
  ) +
  scale_x_continuous(breaks = seq(from = 1, to = 13, by = 1)) +
  annotate(
    geom = "text", x = mean(data_collection$number_other_product),
    y = 250000, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$number_other_product),
    y = 600000, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[5]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(birth_year)) +
  geom_bar(fill = "grey70") +
  geom_vline(
    xintercept = mean(data_collection$birth_year), color = "blue",
    linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of birth_year",
    x = "Birth year",
    y = "Count"
  ) +
  scale_x_discrete(breaks = seq(from = 1920, to = 2000, by = 10)) +
  annotate(
    geom = "text", x = mean(data_collection$birth_year),
    y = 20000, label = "mean = median", color = "blue"
  ) +
  theme_minimal()

density_plots[[6]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(birth_month)) +
  geom_bar(fill = "grey70") +
  geom_vline(
    xintercept = mean(data_collection$birth_month), color = "blue",
    linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$birth_month), color = "red",
    linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of birth_month",
    x = "Birth month",
    y = "Count"
  ) +
  scale_x_discrete(breaks = seq(from = 1, to = 12, by = 1)) +
  annotate(
    geom = "text", x = mean(data_collection$birth_month),
    y = 100000, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$birth_month),
    y = 50000, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[7]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(cf_val)) +
  geom_density() +
  geom_vline(
    xintercept = mean(data_collection$cf_val, na.rm = TRUE), color = "blue",
    linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of cf_val",
    x = "CF value",
    y = "Count"
  ) +
  annotate(
    geom = "text", x = median(data_collection$cf_val, na.rm = TRUE),
    y = 1, label = "median = mean", color = "red"
  ) +
  theme_minimal()

density_plots[[8]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(due_amount)) +
  geom_density() +
  geom_vline(
    xintercept = mean(data_collection$due_amount), color = "blue",
    linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$due_amount), color = "red",
    linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of due_amount",
    x = "Due amount",
    y = "Count"
  ) +
  annotate(
    geom = "text", x = mean(data_collection$due_amount),
    y = 0.0001, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$due_amount),
    y = 0.0003, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[9]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(paid_amount)) +
  geom_density() +
  geom_vline(
    xintercept = mean(data_collection$paid_amount), color = "blue",
    linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$paid_amount), color = "red",
    linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of paid amount",
    x = "Paid amount",
    y = "Count"
  ) +
  annotate(
    geom = "text", x = mean(data_collection$paid_amount),
    y = 0.0002, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$paid_amount),
    y = 0.0004, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[10]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(delay)) +
  geom_density() +
  geom_vline(
    xintercept = mean(data_collection$delay, na.rm = TRUE),
    color = "blue",
    linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$delay, na.rm = TRUE),
    color = "red",
    linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of delay",
    x = "Delay",
    y = "Count"
  ) +
  annotate(
    geom = "text", x = mean(data_collection$delay, na.rm = TRUE),
    y = 0.04, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$delay, na.rm = TRUE),
    y = 0.02, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[11]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(due_date)) +
  geom_density() +
  geom_vline(
    xintercept = mean(data_collection$due_date),
    color = "blue", linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$due_date),
    color = "red", linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of due_date",
    x = "Due date",
    y = "Count"
  ) +
  scale_x_date(date_labels = "%Y") +
  annotate(
    geom = "text", x = mean(data_collection$due_date),
    y = 0.0003, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$due_date),
    y = 0.0006, label = "median", color = "red"
  ) +
  theme_minimal()

density_plots[[12]] <- data_collection %>%
  drop_na() %>%
  ggplot(aes(payment_date)) +
  geom_density() +
  geom_vline(
    xintercept = mean(data_collection$payment_date, na.rm = T),
    color = "blue", linetype = "dotted", size = 1
  ) +
  geom_vline(
    xintercept = median(data_collection$payment_date, na.rm = T),
    color = "red", linetype = "dotted", size = 1
  ) +
  labs(
    title = "Distribution of payment_date",
    x = "Payment date",
    y = "Count"
  ) +
  scale_x_date(date_labels = "%Y") +
  annotate(
    geom = "text", x = mean(data_collection$payment_date, na.rm = T),
    y = 0.0006, label = "mean", color = "blue"
  ) +
  annotate(
    geom = "text", x = median(data_collection$payment_date, na.rm = T),
    y = 0.0003, label = "median", color = "red"
  ) +
  theme_minimal()

# Plot distribution for each numeric attribute
density_plots <- grid.arrange(grobs = density_plots, ncol = 2)

#  Boxplots for numeric attributes
boxplots <- list()
boxplots[[1]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(y = number_of_children)) +
  scale_x_discrete() +
  labs(title = "Number of children")

boxplots[[2]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(y = number_other_product)) +
  scale_x_discrete() +
  labs(title = "Number other product")

boxplots[[3]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(y = birth_year)) +
  scale_x_discrete() +
  labs(title = "Birth year")

boxplots[[4]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(y = birth_month)) +
  scale_x_discrete() +
  labs(title = "Birth year")

boxplots[[5]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(y = cf_val)) +
  scale_x_discrete() +
  labs(title = "CF value")

boxplots[[6]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(y = due_amount)) +
  labs(title = "Due amount")

boxplots[[7]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(y = paid_amount)) +
  labs(title = "Paid amount")

boxplots[[8]] <- data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(y = delay)) +
  scale_x_discrete() +
  labs(title = "Delay")

# Plot boxplot for each numeric attribute
boxplots <- grid.arrange(grobs = boxplots, ncol = 4)

# Univariate analysis of categorical variables #################################
frequencies <- list()

frequencies[[1]] <- data_collection %>%
  group_by(product_type) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[2]] <- data_collection %>%
  group_by(contract_status) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[3]] <- data_collection %>%
  group_by(business_discount) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[4]] <- data_collection %>%
  group_by(gender) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[5]] <- data_collection %>%
  group_by(marital_status) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[6]] <- data_collection %>%
  group_by(clients_phone) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[7]] <- data_collection %>%
  group_by(client_mobile) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[8]] <- data_collection %>%
  group_by(client_email) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[9]] <- data_collection %>%
  group_by(total_earnings) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[10]] <- data_collection %>%
  group_by(living_area) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[11]] <- data_collection %>%
  group_by(different_contact_area) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[12]] <- data_collection %>%
  group_by(kc_flag) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )
frequencies[[13]] <- data_collection %>%
  group_by(kzmz_flag) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_cumulative_frequency = cumsum(relative_frequency),
    relative_frequency = round(100 * relative_frequency, 2),
    relative_cumulative_frequency = round(
      100 * relative_cumulative_frequency,
      2
    ), nr = row_number(-frequency)
  )

# Data exploration =============================================================
# How much due amount equals to paid amount (check if cause of correlation)
n <- data_collection %>%
  group_by(contract_id) %>%
  summarise(result = sum(data_collection$due_amount == data_collection$paid_amount)/nrow)



# Check interesting coverage
# most products are type 1
coverage <- list()

coverage[[1]] <- ggplot(data = data_collection, aes(x = product_type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# contract status mostly 1, then 5,6,8,7 some 2,3,4...What does it mean??
coverage[[2]] <- ggplot(data = data_collection, aes(x = contract_status)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# most payment orders have discount
coverage[[3]] <- ggplot(data = data_collection, aes(x = business_discount)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

coverage[[4]] <- ggplot(data = data_collection, aes(x = gender)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# marital status mostly 3, some 4,2,6 5 and 1 mostly not...What does it mean?
coverage[[5]] <- ggplot(data = data_collection, aes(x = marital_status)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# mostly 0 children, drops with number
prop.table(table(data_collection$number_of_children))

#  mostly 1 other product, drops with number
prop.table(table(data_collection$number_other_product))

coverage[[6]] <- ggplot(data = data_collection, aes(x = clients_phone)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

coverage[[7]] <- ggplot(data = data_collection, aes(x = client_mobile)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# almost no email contact
coverage[[8]] <- ggplot(data = data_collection, aes(x = client_email)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# total earning level mostly not declared
coverage[[9]] <- ggplot(data = data_collection, aes(x = total_earnings)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  coord_flip()

coverage[[10]] <- data_collection %>%
  group_by(living_area) %>%
  summarize(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(
    relative_frequency = frequency / sum(frequency),
    relative_frequency = round(100 * relative_frequency, 2)
    ) %>%
  head() %>%
  as.data.frame() %>%
  tableGrob(theme = ttheme_default(base_size = 8, padding = unit(c(2,2), "mm")))

# mostly not different contact area
coverage[[11]] <- ggplot(data = data_collection, aes(x = different_contact_area)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# mostly false KC flag - mostly owns local citizenship
coverage[[12]] <- ggplot(data = data_collection, aes(x = kc_flag)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# mostly false KZMZ flag  mostly did not fill employer
coverage[[13]] <- ggplot(data = data_collection, aes(x = kzmz_flag)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

coverage <- grid.arrange(grobs = coverage, ncol = 3)

# Bivariate analysis of continuous variables wrt categorical variables #########

# Statistical dependence of delay on gender
meanG_D <- data_collection %>%
  group_by(gender) %>%
  summarise(mean = mean(delay))

medG_D <- data_collection %>%
  group_by(gender) %>%
  summarise(med = median(delay))

maxG_D <- data_collection %>%
  group_by(gender) %>%
  summarise(max = max(delay))

minG_D <- data_collection %>%
  group_by(gender) %>%
  summarise(min = min(delay))

Q1G_D <- data_collection %>%
  drop_na() %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(delay, probs = 1 / 4))

Q3G_D <- data_collection %>%
  drop_na() %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(delay, probs = 3 / 4))

data_GD <- data.frame(meanG_D, medG_D[, 2], minG_D[, 2], maxG_D[, 2],
  Q1G_D[, 2], Q3G_D[, 2],
  check.names = FALSE
)

data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(gender, delay)) +
  labs(
    title = "Statistical dependence of delay on gender",
    x = "Gender",
    y = "Delay"
  ) +
  theme_minimal()

# Statistical dependence of paid amount on gender
meanG_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(mean = mean(paid_amount))

medG_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(med = median(paid_amount))

maxG_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(max = max(paid_amount))

minG_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(min = min(paid_amount))

Q1G_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(paid_amount, probs = 1 / 4))

Q3G_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(paid_amount, probs = 3 / 4))

data_GPA <- data.frame(meanG_PA, medG_PA[, 2], minG_PA[, 2], maxG_PA[, 2],
  Q1G_PA[, 2], Q3G_PA[, 2],
  check.names = FALSE
)

data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(gender, paid_amount)) +
  labs(
    title = "Statistical dependence of paid amount on gender",
    x = "Gender",
    y = "Paid amount"
  ) +
  theme_minimal()

# Statistical dependence of due amount on gender
meanG_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(mean = mean(due_amount))

medG_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(med = median(due_amount))

maxG_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(max = max(due_amount))

minG_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(min = min(due_amount))

Q1G_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(due_amount, probs = 1 / 4))

Q3G_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(due_amount, probs = 3 / 4))

data_GDA <- data.frame(meanG_DA, medG_DA[, 2], minG_DA[, 2], maxG_DA[, 2],
  Q1G_DA[, 2], Q3G_DA[, 2],
  check.names = FALSE
)

data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(gender, due_amount)) +
  labs(
    title = "Statistical dependence of due amount on gender",
    x = "Gender",
    y = "Due amount"
  ) +
  theme_minimal()

# Dependence of paid amount on gender, product type and business discount
data_collection %>%
  group_by(gender, product_type, business_discount) %>%
  summarise(paidAmount = mean(paid_amount)) %>%
  spread(gender, paidAmount)

data_collection %>%
  group_by(gender, number_of_children) %>%
  summarise(delay = mean(delay)) %>%
  spread(gender, delay)

data_collection %>%
  drop_na() %>%
  ggplot() +
  geom_boxplot(aes(gender, paid_amount, color = product_type)) +
  facet_grid(~business_discount) +
  labs(
    title = "Statistical dependence of paid amount on gender, product type and business discount",
    x = "Gender",
    y = "Paid amount"
  ) +
  theme_minimal()

# Distribution of paid amount according to total earnings
data_collection %>%
  drop_na() %>%
  ggplot(aes(paid_amount, color = total_earnings)) +
  geom_density() +
  geom_vline(
    xintercept = mean(data_collection$paid_amount),
    linetype = "dashed", size = 1
  ) +
  labs(
    title = "Distribution of paid amount by total earnings",
    x = "Paid amount",
    y = "Count"
  ) +
  theme_minimal()

# Boxplot of paid amount according to product type
data_collection %>%
  drop_na() %>%
  ggplot(aes(
    x = product_type, y = paid_amount,
    color = product_type
  )) +
  geom_boxplot() +
  geom_hline(
    yintercept = mean(data_collection$paid_amount, na.rm = TRUE),
    linetype = "dashed", size = 1
  ) +
  labs(
    title = "Paid amount by product type",
    x = "Product type",
    y = "Paid amount"
  ) +
  theme_minimal()

# Density plot of due amount according to contract status
data_collection %>%
  drop_na() %>%
  ggplot(aes(due_amount, color = contract_status)) +
  geom_density() +
  facet_grid(contract_status ~ .) +
  geom_vline(
    xintercept = mean(data_collection$due_amount, na.rm = TRUE),
    linetype = "dashed", size = 1
  ) +
  labs(
    title = "Distribution of due amount by contract status",
    x = "Due amount",
    y = "Count"
  ) +
  theme_minimal()

# Verify data quality ==========================================================

# Are there missing values in the data? If so, how are they represented, where
# do they occur, and how common are they?

variables_miss <- miss_var_summary(data_collection)
print(variables_miss)
# different_contract_area missing 20%, cf_val living_area kc_flag missing 19,9%
# 1173 payment date missing, not yet paid
gg_miss_var(data_collection)

vis_miss()

# more characteristics missing at the same time
data_collection %>%
  gg_miss_var(facet = total_earnings)

# what with NAs in payment order
# to do payment order id!!

sum(is.na(data_collection$different_contact_area) == T &
      is.na(data_collection$cf_val) == T &
      is.na(data_collection$living_area) == T & 
      is.na(data_collection$kc_flag) == T)

sum(is.na(data_collection$kc_flag) == T)

# Zaver: ze ctyr promennych s nejvice NA zaznamy je vetsina NA pozorovani
# na stejnych radcich, to muze indikovat "missing not at random".


# Check for plausibility of values
for (i in c(1:ncol)) {
  if (is.factor(data_collection[, i])) {
    print(colnames(data_collection[i]))
    print(prop.table(table(data_collection[, i])))
    cat(sep = "\n\n")
  }
}

# Data preparation -------------------------------------------------------------
# Clean the data - estimation of missing data

# Feature engineering ==========================================================
# This function takes a vector and shifts its values by 1. This means that 
#   on the second position is the first value, on the third position is 
#   the second value etc. This is necessary for feature engineering. 
#   We want a cumulative sum/cumulative mean for previous payments, but 
#   functions cummean/cumsum applied to a row[i] make the calculations using
#   also the number on this line. This is where this function comes in handy.
f_vec_shift <- function(x){
  new_vec <- c(0, x[1:length(x)-1])
  return(new_vec)
}

# Creating new features: was the delay larger than 21(140) days? 
data_collection <- data_collection %>%
  mutate(delay_21_y = ifelse(delay > 21, 1, 0)) %>%
  mutate(delay_140_y = ifelse(delay > 140, 1, 0))

# Creating new features: mean delay for the whole client's history
data_collection <- data_collection %>%
  nest(-contract_id) %>%
  mutate(delay_indiv_help = map(.x = data, .f = ~cummean(.x$delay))) %>%
  mutate(delay_indiv = map(.x = delay_indiv_help, .f = ~f_vec_shift(.x))) %>%
  select(-delay_indiv_help) %>%
  unnest(c(data, delay_indiv))

# Creating new features: number of delays larger than 21(140) days
# for the whole client's history. 
data_collection <- data_collection %>%
  nest(-contract_id) %>%
  mutate(delay_indiv_21_help = map(.x = data, .f = ~cumsum(.x$delay_21_y))) %>%
  mutate(delay_indiv_21 = map(.x = delay_indiv_21_help, .f = ~f_vec_shift(.x))) %>%
  mutate(delay_indiv_140_help = map(.x = data, .f = ~cumsum(.x$delay_140_y))) %>%
  mutate(delay_indiv_140 = map(.x = delay_indiv_140_help, .f = ~f_vec_shift(.x))) %>%
  select(-delay_indiv_21_help, -delay_indiv_140_help) %>%
  unnest(c(data, delay_indiv_21, delay_indiv_140))

# This part of the feature engineering process is the longest, but the syntax
# is not that complicated (compared to map/nest etc.).
# It works like this: 
# First, we create new features (variables, columns) for storing 
# the average payment delay for last 12/6/3/1 month.
# In this section, I am also creating a set of new features with suffix _help, 
# they serve only as a temporary helper. 

# It was necessasry rep() NA values, since 0 or any other number
# would be misleading. 
data_collection <- data_collection %>%
  mutate(mean_delay_12m = rep(NA, nrow(data_collection))) %>% 
  mutate(mean_delay_12m_help = rep(NA, nrow(data_collection))) %>%
  mutate(mean_delay_6m = rep(NA, nrow(data_collection))) %>% 
  mutate(mean_delay_6m_help = rep(NA, nrow(data_collection))) %>%
  mutate(mean_delay_3m = rep(NA, nrow(data_collection))) %>% 
  mutate(mean_delay_3m_help = rep(NA, nrow(data_collection))) %>%
  mutate(mean_delay_1m = rep(NA, nrow(data_collection))) %>% 
  mutate(mean_delay_1m_help = rep(NA, nrow(data_collection))) %>%
  filter(is.na(payment_order) == F) %>% # POZOR, OVERIT!!!!!!!! 
  filter(is.na(delay) == F) # POZOR, OVERIT!!!!!!!!

# Second we nest data again
data_collection <- data_collection %>%
  nest(-contract_id) 

# And third, we loop through the data. A lot. 
# The first loop loops through the nested data for each contract. 
# The purpose of this main loop is similar to map() function. 
# Maybe the inner loop could be written as a function and passed to
# map() in .f argument, but not sure how big the runtime gains would be. 
for(i in 1:nrow(data_collection)){
  df_actual <- data_collection$data[[i]]
  
  # The second loop loops through the dataframe for each contract. 
  # There are 4 if conditions in this for loop -> 12/6/3/1 M delay.
  # The logic is this: 
  # The algorithm identifies rows with due_date in the given date range, which
  # means last 12/6/3/1 months. These rows are marked with 1 in the _help column. 
  # Then, the average is calculated simply by multiplying delay column by 
  # _help column and divided by the sum of _help. This works because the 
  # _help column has only zeroes and ones. 
  # Lastly, the _help columned is restarted for the next round. 
  for(j in 1:nrow(df_actual)){
    
    # Mean for the last 12 months
    if(j > 12){
      # Mark relevant rows with 1
      df_actual <- df_actual %>%
        mutate(mean_delay_12m_help = ifelse(due_date >= df_actual$due_date[j] - months(12) &
                                              due_date < df_actual$due_date[j], 1, 0))
      
      # Calculate mean
      df_actual$mean_delay_12m[j] <-sum(df_actual$mean_delay_12m_help*df_actual$delay)/sum(df_actual$mean_delay_12m_help) 
      # Restart helper column
      df_actual$mean_delay_12m_help = rep(NA, nrow(df_actual))
    }
    
    # Mean for the last 6 months
    if(j > 6){
      # Mark relevant rows with 1 
      df_actual <- df_actual %>%
        mutate(mean_delay_6m_help = ifelse(due_date >= df_actual$due_date[j] - months(6) &
                                             due_date < df_actual$due_date[j], 1, 0))
      
      # Calculate mean
      df_actual$mean_delay_6m[j] <-sum(df_actual$mean_delay_6m_help*df_actual$delay)/sum(df_actual$mean_delay_6m_help) 
      # Restart helper column
      df_actual$mean_delay_6m_help = rep(NA, nrow(df_actual))
    }
    
    # Mean for the last 3 months
    if(j > 3){
      # Mark relevant rows with 1 
      df_actual <- df_actual %>%
        mutate(mean_delay_3m_help = ifelse(due_date >= df_actual$due_date[j] - months(3) &
                                             due_date < df_actual$due_date[j], 1, 0))
      
      # Calculate mean
      df_actual$mean_delay_3m[j] <-sum(df_actual$mean_delay_3m_help*df_actual$delay)/sum(df_actual$mean_delay_3m_help) 
      # Restart helper column
      df_actual$mean_delay_3m_help = rep(NA, nrow(df_actual))
    }
    
    # Mean for the last 1 month
    if(j > 1){
      # Mark relevant rows with 1
      df_actual <- df_actual %>%
        mutate(mean_delay_1m_help = ifelse(due_date >= df_actual$due_date[j] - months(1) &
                                             due_date < df_actual$due_date[j], 1, 0))
      
      # Calculate mean
      df_actual$mean_delay_1m[j] <-sum(df_actual$mean_delay_1m_help*df_actual$delay)/sum(df_actual$mean_delay_1m_help) 
      # Restart helper column
      df_actual$mean_delay_1m_help = rep(NA, nrow(df_actual))
    }
    
  }
  data_collection$data[[i]] <- df_actual 
  
  # Progress bar might be more elegant, someone can add later 
  # There is 86 980 observations to loop through, so printing [i] gives a good idea of progress. 
  print(i)
}

# Unnest the data
# Remove helper columns
data_collection <- data_collection %>%
  unnest(-data) %>%
  select(-mean_delay_12m_help, -mean_delay_6m_help, -mean_delay_3m_help, -mean_delay_1m_help)


# Write data to .txt for the model creation
write.table(data_collection, file = "data_collection_prepared.txt", sep = ";")


