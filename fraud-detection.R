## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(eval = TRUE)
knitr::opts_chunk$set(cache = TRUE)
knitr::opts_chunk$set(autodep = TRUE)


## ----Naming report sections for cross-reference, include=FALSE, eval=TRUE---------------------------------------------------------
# This is not used for the model or for any computation, it is only use to build the report in a dynamic way and
# centralise section titles in one place
sec_one <- "Workflow and methods"
sec_two <- "Preliminary analysis"
sec_three <- "Modelling"
sec_four <- "Results and conclusion"


## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------
# Define the size of the smaller dataset used for the initial 
# tuning of the model
small_sample_size <- 200000


## ----Load packages, results='hide', message = FALSE, warning = FALSE, cache=FALSE-------------------------------------------------
# Required packages
if (!require(parallel)) install.packages('parallel')
library(parallel)
if (!require(doParallel)) install.packages('doParallel')
library(doParallel)
if (!require(class)) install.packages('class')
library(class)
if (!require(tictoc)) install.packages('tictoc')
library(tictoc)
if (!require(knitr)) install.packages('knitr')
library(knitr)
if (!require(data.table)) install.packages('data.table')
library(data.table)
if (!require(stringr)) install.packages('stringr')
library(stringr)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(ggridges)) install.packages('ggridges')
library(ggridges)
if (!require(ggthemes)) install.packages('ggthemes')
library(ggthemes)
if (!require(scales)) install.packages('scales')
library(scales)
if (!require(corrplot)) install.packages('corrplot')
library(corrplot)
if (!require(e1071)) install.packages('e1071')
library(e1071)
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
if (!require(xgboost)) install.packages('xgboost')
library(xgboost)
if (!require(PRROC)) install.packages('PRROC')
library(PRROC)
if (!require(caret)) install.packages('caret')
library(caret)

# Set the theme for all the graphs
theme_set(theme_fivethirtyeight(base_size = 10))
theme_update(axis.title = element_text())
theme_update(plot.title = element_text(margin=ggplot2::margin(0,0,15,0), 
                                       hjust = 0.5))

# Creates "data" directory in the current directory if it doesn't exist 
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)


## ----Data origin parameters, include=FALSE, eval=TRUE-----------------------------------------------------------------------------
# If "no_local_data" is set to "TRUE", code will download and process data 
# as if it doesn't exist locally.
# If set to "FALSE", code will load local data to speed up runtime.
no_local_data <- TRUE
models_rerun <- TRUE


## ----Download dataset from GitHub, eval=no_local_data-----------------------------------------------------------------------------
# 1) Download the .rds file
url <- paste0("https://media.githubusercontent.com/media/vgkienzler/",
              "mob-money-fraud-detect/master/data/transactions-raw.rds")
download.file(url, destfile = "data/transactions-raw.rds")
# 2) Load the rds file
transactions.raw <- readRDS("data/transactions-raw.rds")


# ## ----Download dataset from Kaggle, eval=no_local_data-----------------------------------------------------------------------------
# # 1) Download and save the file from kaggle (archive.zip) in folder "data".
# # 2) Unzip and save the csv file in the /data folder.
# file.rename(unzip("data/archive.zip"),"data/transactions.csv")
# # 3) Load the csv file as a data frame
# transactions.raw <- read.csv(file="data/transactions.csv")
# 
# 
# ## ----Save transactions-raw dataset, echo=FALSE, include=FALSE, eval=no_local_data, cache=FALSE------------------------------------
# # Save the transactions dataset in directory "data"
# # Creates "data" directory in the current directory if it doesn't exist 
# ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
# saveRDS(transactions.raw, file = "data/transactions-raw.rds")


## ----Load local datasets if any, include=FALSE, eval=!no_local_data---------------------------------------------------------------
## # Load this dataset if it is locally available.
## # Update the path to match your own.
## # Loading locally available dataset is faster in case of re-run.
## transactions.raw <- readRDS("data/transactions-raw.rds")


## ----Check data table, message = FALSE, warning = FALSE---------------------------------------------------------------------------
# Display the current column names
colnames(transactions.raw)


## ----Renaming---------------------------------------------------------------------------------------------------------------------
# Correct spelling error and apply consistant naming scheme (camelCase) to 
# all features:
transactions <- transactions.raw %>% rename(oldBalanceDest = oldbalanceDest, 
                                            oldBalanceOrig = oldbalanceOrg, 
                                            newBalanceOrig = newbalanceOrig, 
                                            newBalanceDest = newbalanceDest)


## ----Check NAs--------------------------------------------------------------------------------------------------------------------
message("Is there any NA in the dataset? ", anyNA(transactions))


## ---------------------------------------------------------------------------------------------------------------------------------
str(transactions, vec.len = 1)


## ----Column type update, results=FALSE--------------------------------------------------------------------------------------------
# Update column type
# Update isFraud to factor, positive class = fraud (F1), 
# negative class = genuine (G0)
# Using "0" and "1" as factors risks being confusing and triggers errors
# with some caret functions
transactions <- transactions %>%
  mutate(nameDest=as.character(nameDest), 
         nameOrig=as.character(nameOrig)) %>%
  mutate(isFraud = factor(isFraud, levels = c(1, 0), labels = c("F1", "G0")), 
         isFlaggedFraud = as.factor(isFlaggedFraud))


## ----Split names from types M and C-----------------------------------------------------------------------------------------------
# Add type columns for nameOrig and nameDest and relocate them
transactions <- transactions %>% 
  mutate(typeOrig = as.factor(str_sub(nameOrig,1,1)),
         typeDest = as.factor(str_sub(nameDest,1,1))) %>%
  relocate(typeOrig, .after=nameOrig) %>% 
  relocate(typeDest, .after=nameDest)


## ----Split playset/validation, warning=FALSE, eval=no_local_data, cache=FALSE-----------------------------------------------------
# Validation set is 10% of transactions dataset
# Set seed for reproducibility
set.seed(1)

in_validation <- createDataPartition(y = transactions$isFraud, 
                                     times = 1, p = 0.1, list = FALSE)
playset <- transactions[-in_validation,]
validation <- transactions[in_validation,]

# Save datasets for future use
saveRDS(playset, file = "data/playset.rds")
saveRDS(validation, file = "data/validation.rds")

# Do some cleaning
rm(in_validation)


## ----Load playset, results=FALSE, echo=FALSE, eval=!no_local_data-----------------------------------------------------------------
## # If local data available, download playset
## playset <- readRDS("data/playset.rds")
## validation <- readRDS("data/validation.rds")


## ----Display playset/validation length, warning=FALSE-----------------------------------------------------------------------------
# Declare function "formated_nrow" to display number of rows using ","
formated_nrow <- function(x){
  x <- format(nrow(x), big.mark = ",")
  return(x)
}

# Display the number of observations in the different sets for control
message("'playset' contains ", formated_nrow(playset)," observations.")
message("'validation' contains ", formated_nrow(validation), " observations.")


## ----Class prevalence, warning=FALSE, message=FALSE-------------------------------------------------------------------------------
# Look at the number of fraudulent/genuine transactions
freq_count <- data.frame(
  table(playset$isFraud, 
        playset$isFlaggedFraud),
  prop.table(table(playset$isFraud, 
                   playset$isFlaggedFraud)))[c(1,2,3,6)]

freq_count <- freq_count %>%
  mutate(Freq.1 = round(Freq.1, 4)) %>%
  rename(
    isFraud = Var1,
    isFlaggedFraud = Var2,
    Count = Freq,
    Prop. = Freq.1)

kable(freq_count, caption="Class prevalence", 
      col.names=c("isFraud","isFlaggedFraud","Count", "Prop."))


## ----isFlaggedFraud above 200k----------------------------------------------------------------------------------------------------
# Count and print the number of transactions above 200,000
trans_above <- nrow(transactions %>% filter(amount > 200000))
message("Number of transactions above 200,000: "
  , format(trans_above, big.mark = ","))

# Count and print the number of transactions above 200,000
# and with isFlaggedFraud == 1
trans_both <- nrow(transactions %>% 
                     filter(amount > 200000) %>% 
                     filter(isFlaggedFraud == 1))
message(
  "Number of transactions above 200,000 and with isFlaggedFraud == 1: "
  , trans_both
  )

# Do some cleaning
rm(freq_count, trans_above, trans_both)


## ----Transactions per type, message=FALSE, warning=FALSE--------------------------------------------------------------------------
# Table of number of transactions per type
kable(sort(
  table(playset$type), decreasing = TRUE), 
  caption="Number of transactions per type", 
  col.names=c("Type","Count"))


## ----Histogram genuine per type, warning=FALSE, message=FALSE---------------------------------------------------------------------
playset %>% 
  mutate(bin = cut_number(amount, 50, labels=FALSE)) %>%
  filter(isFraud == "G0") %>%
  ggplot(aes(bin, fill = type)) +
  geom_histogram(stat = "count", color="White") +
  facet_grid(typeOrig ~ typeDest) +
  labs(
    fill = "Trans. type:"
    , x = "Amount bin (equal number of transactions per bin)"
    , title = "Histogram of transactions, genuine transactions")


## ----Density of transaction amount per type for genuine transactions, warning=FALSE, message=FALSE--------------------------------
# Plot the density vs. transaction amount using a log10 scale
# for non fraudulent transactions
playset %>%
  filter(isFraud == "G0") %>%
  ggplot(aes(amount, fill = type)) +
  geom_density(alpha = 0.5) +
  scale_x_log10(
    limits = c(10, NA)
    , n.breaks = 8
    , labels = trans_format("log10", math_format(10^.x))) +
  labs(
    fill = "Trans. type:"
    , x = "amount (log10 scale, local currency)"
    , title = "Transaction amount densities, genuine transactions") +
  annotation_logticks(sides = "b") +
  theme(axis.text.x= element_text(size=10))


## ----Histogram fraud per type and amount bins, warning=FALSE, message=FALSE-------------------------------------------------------
# Histogram of fraudulent transactions per amount and type, 
# typeDest and typeOrig categories.
# Amount is split between 50 bins with the same number of transactions.
playset %>% 
  mutate(bin = cut_number(amount, 50, labels=FALSE)) %>%
  filter(isFraud == "F1") %>%
  ggplot(aes(bin, fill = type)) +
  geom_histogram(stat = "count", color = "white") +
  facet_grid(typeDest ~ typeOrig) +
  labs(
    fill = "Trans. type:"
    , x = "Amount bin (equal number of transactions per bin)"
    , title = "Histogram of transactions, frauds")


## ----Fraud category per typeDest and typeOrig, warning=FALSE, message=FALSE-------------------------------------------------------
# Distribution table of fraudulent and genuine transactions
# between two perdictors: typeOrig, typeDest
kable(playset %>% 
  group_by(isFraud, typeOrig, typeDest) %>% 
  summarise(Count = n()))


## ----Compare density of transaction amount per type, warning=FALSE, message=FALSE-------------------------------------------------
# Plot the density vs. transaction amount using a log10 scale,
# for fraudulent and non-fraudulent transactions

# Define the labeller to display "Genuine" and "Fraud"
is_fraud <- list("0" = "Genuine", "1" = "Fraud")

my_labeller <- function(variable,value){
  return(is_fraud[value])
}

playset %>%
  ggplot(aes(x = amount, y = type, height = stat(density),
             fill = type)) +
  geom_density_ridges(alpha = 0.5, 
                      stat = "density",
                      show.legend = FALSE,
                      scale = 4,
                      draw_baseline = FALSE,
                      rel_min_height = 0.01) +
  scale_x_log10(
    limits = c(10^2, 3*10^7), 
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
    ) +
  scale_y_discrete(expand = expand_scale(mult = c(0.3, 1))) +
  annotation_logticks(sides = "b") +
  xlab("Transaction amount (log10 scale)") +
  ggtitle("Densities of transaction amounts") +
  theme(plot.title = element_text(margin=ggplot2::margin(0,0,25,0),
                                  hjust = 0.3)) +
  facet_grid(isFraud ~., labeller = my_labeller)


## ----Display cash-in transactions, cache=TRUE-------------------------------------------------------------------------------------
# Display 10 cash-in transactions 
head(
  playset %>% 
    select(-step, -isFlaggedFraud, -isFraud,
           -typeDest, -typeOrig) %>% 
    filter(type == "CASH_IN")
  , 10)


## ----Cash-in transactions balance errors------------------------------------------------------------------------------------------
# Filter cash-in transactions
playset_CASH_IN <- playset %>% filter(type == "CASH_IN")

# Display total number of cash-in transactions
count_cash_in <- formated_nrow(playset_CASH_IN)
message("Total number of cash-in transactions in the dataset: " 
  , count_cash_in)

# Display number of transactions with a balance error on the origin account
# that is greater than 0.1 to omit rounding errors.
orig_error <- formated_nrow(
  playset_CASH_IN %>% 
    mutate(errBalanceOrig = oldBalanceOrig + amount - newBalanceOrig) %>% 
    filter(abs(errBalanceOrig) > 0.1)
  )

message(
  "Nb. of cash-in transactions with an error in the origin balance: "
  , orig_error
  )

# Display number of transactions with a balance error on the destination account
dest_error <- formated_nrow(
  playset_CASH_IN %>% 
    mutate(errBalanceDest = oldBalanceDest - amount - newBalanceDest) %>% 
    filter(abs(errBalanceDest) > 0.1)
  )

message(
  "Nb. of cash-in transactions with an error in the destination balance: "
  , dest_error
  )


## ----Cash-in transactions balance errors on the destination account---------------------------------------------------------------
# Display the 10 largest errors on the destination account
kable(head(
  playset_CASH_IN %>% 
  mutate(errBalanceDest = - newBalanceDest - amount + oldBalanceDest) %>% 
  filter(abs(errBalanceDest) > 0.1) %>% 
  select(-step, -type, -nameDest, -nameOrig, -typeOrig, -typeDest, 
         -isFraud, -isFlaggedFraud) %>% 
  arrange(desc(abs(errBalanceDest))), 10))


## ----Cash-in balance error on the origin account----------------------------------------------------------------------------------
# Filter only transactions with a balance error on the origin account
origin_error_df <- playset_CASH_IN %>% 
       select(-step, -type, -oldBalanceDest, -newBalanceDest, -nameDest, 
              -isFlaggedFraud, -isFraud) %>% 
       mutate(errBalanceOrig = newBalanceOrig - amount - oldBalanceOrig) %>% 
       filter(abs(errBalanceOrig) > 0.1)

# Display transactions displaying a balance error on the origin account
kable(head(origin_error_df, 10))

# Check if all the errors are due to amount not being substracted
# from oldBalanceOrig
amount_not_reflected <- formated_nrow(
  origin_error_df %>% filter(oldBalanceOrig == newBalanceOrig)
  )

message(
  "Nb. of cash-in transactions displaying an error on the origin balance: "
  , orig_error
  )

# Display the number of transactions with balance error
message("Nb. of these transactions where 'newBalanceOrig' is not updated: "
  , amount_not_reflected)

# Do some cleaning
rm(dest_error, orig_error, origin_error_df, count_cash_in, 
   playset_CASH_IN, amount_not_reflected)


## ----Display payment transactions-------------------------------------------------------------------------------------------------
# Display 10 payment transactions 
kable(head(playset %>% 
       select(-step, -isFlaggedFraud, -isFraud, -nameOrig, -nameDest, 
              -typeDest, -typeOrig) %>% 
       filter(type == "PAYMENT"), 10))

# Filter payment transactions
playset_PAYMENT <- playset %>% filter (type == "PAYMENT")

# Count how many payment transactions have oldBalanceDest > 0
number_oldBalanceDest <- nrow(playset_PAYMENT %>% filter(oldBalanceDest > 0))
message("Nb. of payment transactions where oldBalanceDest > 0: "
  , number_oldBalanceDest)

# Count how many payment transactions have newBalanceDest > 0
number_newBalanceDest <- nrow(playset_PAYMENT %>% filter(newBalanceDest > 0))
message("Nb. of payment transactions where oldBalanceDest > 0: "
  , number_newBalanceDest)


## ----Error balance origin payment-------------------------------------------------------------------------------------------------
# Display total number of payment transactions
count_payment <- formated_nrow(playset_PAYMENT)
message("Total number of payment transactions in the dataset: "
  , count_payment)

# Caculate the number of transactions with
# balance error on the origin account
orig_error <- formated_nrow(
  playset_PAYMENT %>% 
    mutate(errBalanceOrig = oldBalanceOrig - amount - newBalanceOrig) %>% 
    filter(abs(errBalanceOrig) > 0.1)
  )

# Display the number of such transactions
message(
  "Nb. of payment transactions displaying an error on the origin balance: "
  , orig_error
  )


## ----Display payment error transactions-------------------------------------------------------------------------------------------
# Select what to display
payment_errors_df <- playset_PAYMENT %>% 
  select(- step, -isFraud, -isFlaggedFraud, -nameDest, -typeDest, 
         -oldBalanceDest, -newBalanceDest) %>%
  mutate(errBalanceOrig = oldBalanceOrig - amount - newBalanceOrig) %>% 
  filter(abs(errBalanceOrig) > 0.1)

# Display payment transactions with errors 
head(payment_errors_df, 5)

not_enough <- formated_nrow(
  payment_errors_df %>% filter(amount > oldBalanceOrig)
  )

message("Nb. of payment transactions where 'amount' > initial balance: "
  , not_enough)

# Do some cleaning
rm(playset_PAYMENT, orig_error, count_payment, number_oldBalanceDest, 
   number_newBalanceDest, payment_errors_df, not_enough)


## ----Debit transactions balance errors--------------------------------------------------------------------------------------------
# Filter debit transactions
playset_DEBIT <- playset %>% filter(type == "DEBIT")

# Display 10 DEBIT transactions
head(
  playset_DEBIT %>% select(-step, -isFraud, -isFlaggedFraud)
  , 10)

# Display total number of debit transactions
count_debit <- formated_nrow(playset_DEBIT)
message("Total number of debit transactions in the dataset: "
  , count_debit)

# Add errBalanceOrig and errBalanceDest to playset_DEBIT
playset_DEBIT <- playset_DEBIT %>% 
  mutate(errBalanceOrig = oldBalanceOrig - amount - newBalanceOrig) %>%
  mutate(errBalanceDest = oldBalanceDest + amount - newBalanceDest)

# Display the number of transactions showing a balance error
# on the origin account
orig_error <- formated_nrow(
  playset_DEBIT %>% filter(abs(errBalanceOrig) > 0.1)
  )
message(
  "Nb. of debit transactions with an error on the origin balance: ", 
  orig_error
  )

# Display the number of transactions showing a balance error 
# on the destination account
dest_error <- formated_nrow(
  playset_DEBIT %>% filter(abs(errBalanceDest) > 0.1)
  )
message(
  "Nb. of debit transactions with an error on the destination balance: ", 
  dest_error
  )


## ----Debit transactions balance errors - Origin account---------------------------------------------------------------------------
# Display the 10 largest error on the origin account
orig_errors_df <- playset_DEBIT %>% 
       filter(abs(errBalanceOrig) > 0.1) %>% 
       arrange(desc(abs(errBalanceOrig)))

kable(head(
  orig_errors_df %>% 
    select(-type, -step, -isFraud, -isFlaggedFraud, -nameOrig,
           -nameDest, -typeDest, -typeOrig, - errBalanceDest)
  , 10))

not_enough <- formated_nrow(
  orig_errors_df %>% filter(amount > oldBalanceOrig)
  )

message(
  "Nb. of debit transactions where 'amount' > account initial balance: ", 
  not_enough
  )


## ----Debit transactions balance errors - Destination account----------------------------------------------------------------------
# Display the 10 largest error on the destination account
dest_errors_df <- playset_DEBIT %>% 
       mutate(errBalanceDest = newBalanceDest - amount - oldBalanceDest) %>% 
       filter(abs(errBalanceDest) > 0.1)

# Display the 10 largest error on the destination account
kable(head(
  dest_errors_df %>% 
    select(-type, -step, -isFraud, -isFlaggedFraud, -typeDest, -typeOrig,
           -nameOrig, -nameDest, -oldBalanceOrig, -newBalanceOrig) %>% 
    arrange(desc(abs(errBalanceDest))), 10))

# Do some cleaning
rm(dest_error, orig_error, count_debit, playset_DEBIT, 
   dest_errors_df, orig_errors_df)


## ----Cash-out transactions balance errors-----------------------------------------------------------------------------------------
# Filter cash-out transactions
playset_CASH_OUT <- playset %>% filter(type == "CASH_OUT")

# Display total number of cash-out transactions
count_cash_out <- formated_nrow(playset_CASH_OUT)
message("Total number of cash-out transactions in the dataset: "
  , count_cash_out)

# Add errBalanceOrig and errBalanceDest
playset_CASH_OUT <- playset_CASH_OUT %>% 
  mutate(errBalanceOrig = oldBalanceOrig - amount - newBalanceOrig) %>%
  mutate(errBalanceDest = oldBalanceDest + amount - newBalanceDest)

# Number of transactions displaying a balance error on the origin account
orig_error <- formated_nrow(
  playset_CASH_OUT %>%filter(abs(errBalanceOrig) > 0.1)
  )

message(
  "Nb. of cash-out transactions with an error on the origin balance: "
  , orig_error
  )

# Number of transactions displaying a balance error on the destination account
dest_error <- formated_nrow(
  playset_CASH_OUT %>% filter(abs(errBalanceDest) > 0.1)
  )

message(
  "Nb. of cash-out transactions with an error on the dest. balance: ", 
  dest_error
  )


## ----Cash-out transactions balance errors - Origin account------------------------------------------------------------------------
# Select the transactions with errors on the origin account
orig_errors_df <- playset_CASH_OUT %>% 
       filter(abs(errBalanceOrig) > 0.1)

# Display the largest error on the origin account
kable(head(
  orig_errors_df %>% 
    select(-type, -step, -isFraud, -isFlaggedFraud, -nameOrig,
    -nameDest, -typeDest, -typeOrig, - errBalanceDest)
  , 10))

# Look at how many errors are explained by a transaction amount greater than
# the initial account balance
not_enough <- formated_nrow(
  orig_errors_df %>% filter(amount > oldBalanceOrig)
  )

message(
  "Nb. of cash-out transactions where 'amount' > initial balance: "
  , not_enough
  )

# Look at the errors which are not explained by the previous explanation
orig_errors_df %>% 
  filter(amount < oldBalanceOrig) %>% 
  select(-step, -type, -nameOrig, -nameDest, -typeOrig, -typeDest, 
         -isFlaggedFraud, -errBalanceDest)


## ----Cash-out transactions balance errors - Destination account-------------------------------------------------------------------
# Display the 10 largest error on the destination account
dest_errors_df <- playset_CASH_OUT  %>% 
       filter(abs(errBalanceDest) > 0.1)

# Display the 10 largest error on the destination account
kable(head(
  dest_errors_df %>%
    select(-type, -step, -isFraud, -isFlaggedFraud, 
           -typeDest, -typeOrig, -nameOrig, -nameDest,
           -oldBalanceOrig, -newBalanceOrig, -errBalanceOrig) %>% 
    arrange(desc(abs(errBalanceDest)))
  , 10))

count_twice_amount <- formated_nrow(
  playset_CASH_OUT %>% filter(abs(errBalanceDest) > 2*amount)
  )

message("Nb. of transactions where abs(errBalanceDest) > 2 * amount: "
  , count_twice_amount)


## ---- Fraudulent cash-out transactions--------------------------------------------------------------------------------------------
# Display total number of fraudulent cash-out transactions
count_cash_out_fraud <- formated_nrow(
  playset_CASH_OUT %>% filter(isFraud == "F1")
  )

message(
  "Total number of fraudulent cash-out transactions in the dataset: "
  , count_cash_out_fraud
  )

# Visualise a few fraudulent cash-out transactions
head(
  playset_CASH_OUT %>% 
    filter(isFraud == "F1") %>% 
    select(-step, -type, -typeOrig, -typeDest, -nameOrig,
           -nameDest, -isFlaggedFraud)
  , 10)

# Confirm how many cash-out transactions have newBalanceOrig == 0
fraud_newBal <- formated_nrow(
  playset_CASH_OUT %>% 
    filter(isFraud == "F1") %>% 
    filter(newBalanceOrig == 0)
  )

message(
  "Total number of fraudulent cash-out transactions with 'newBalanceOrig' = 0: "
  , fraud_newBal
  )

# Compare this to the total number of genuine cash-out transactions with
# newBalanceOrig == 0
genuine_newBal <- formated_nrow(
  playset_CASH_OUT %>%
    filter(isFraud == "G0") %>% 
    filter(newBalanceOrig == 0)
  )

message(
  "Total number of genuine cash-out transactions with 'newBalanceOrig' = 0: "
  , genuine_newBal
  )

# Do some cleaning
rm(dest_error, orig_error, count_cash_out, not_enough, txt, 
   orig_errors_df, playset_CASH_OUT, count_cash_out_fraud, genuine_newBal,
   fraud_newBal, dest_errors_df, count_twice_amount)


## ----Transfer transactions balance errors-----------------------------------------------------------------------------------------
# Filter transfer transactions
playset_TRANSFER <- playset %>% filter(type == "TRANSFER")

# Display total number of transfer transactions
count_transfer <- formated_nrow(playset_TRANSFER)
message("Total number of transfer transactions in the dataset: "
  , count_transfer)

# Add errBalanceOrig and errBalanceDest
playset_TRANSFER <- playset_TRANSFER %>% 
  mutate(errBalanceOrig = oldBalanceOrig - amount - newBalanceOrig) %>%
  mutate(errBalanceDest = oldBalanceDest + amount - newBalanceDest)

# Number of transactions displaying a balance error on the origin account
orig_error <- formated_nrow(
  playset_TRANSFER %>% filter(abs(errBalanceOrig) > 0.1)
  )

message(
  "Nb. of transfer transactions displaying an error on the origin balance: "
  , orig_error
  )

# Display number of transactions displaying a balance error on the destination account
dest_error <- formated_nrow(
  playset_TRANSFER %>% filter(abs(errBalanceDest) > 0.1)
  )

message(
  "Nb. of transfer transactions displaying an error on the destination balance: "
  , dest_error
  )



## ----Transfer transactions balance errors - Origin account, cache=TRUE------------------------------------------------------------
# Select the transactions with errors on the origin account
orig_errors_df <- playset_TRANSFER %>% 
       filter(abs(errBalanceOrig) > 0.1)

# Display the largest error on the origin account
kable(head(
  orig_errors_df %>% 
    select(-type, -step, -isFraud, -isFlaggedFraud, -nameOrig,
    -nameDest, -typeDest, -typeOrig, - errBalanceDest)
  , 20))

# Look at how many errors are explained by a transaction amount greater than
# the initial account balance
not_enough <- formated_nrow(
  orig_errors_df %>% filter(amount > oldBalanceOrig)
  )

message("Nb. of cash-out transactions where 'amount' > initial balance: "
  , not_enough)

# Look at the errors which are not explained by the previous explanation
orig_errors_df %>% 
  filter(amount < oldBalanceOrig) %>% 
  select(-step, -type, -nameOrig, -nameDest, -typeOrig, -typeDest, 
         -isFlaggedFraud, -errBalanceDest)


## ----Transfer transactions balance errors - Destination account-------------------------------------------------------------------
# Display the 10 largest error on the destination account
dest_errors_df <- playset_TRANSFER  %>% 
       filter(abs(errBalanceDest) > 0.1)

# Display the 10 largest error on the destination account
kable(head(
  dest_errors_df %>% 
    select(-type, -step, -isFraud, -isFlaggedFraud, 
           -typeDest, -typeOrig, -nameOrig, -nameDest,
           -oldBalanceOrig, -newBalanceOrig, -errBalanceOrig) %>% 
    arrange(desc(abs(errBalanceDest)))
  , 10))

count_twice_amount <- formated_nrow(
  playset_TRANSFER %>%
  filter(abs(errBalanceDest) > 2 * amount)
  )

message("Nb. of transactions where abs(errBalanceDest) > 2*amount: "
  , count_twice_amount)


## ---- Fraudulent Transfer transactions--------------------------------------------------------------------------------------------
# Display total number of fraudulent transfer transactions
count_transfer <- formated_nrow(
  playset_TRANSFER %>% filter(isFraud == "F1")
  )
message("Total number of fraudulent transfer transactions in the dataset: "
  , count_transfer)

# Visualise a few fraudulent transfer transactions
head(
  playset_TRANSFER %>% 
    filter(isFraud == "F1") %>% 
    select(-step, -type, -typeOrig, -typeDest, -nameOrig,
           -nameDest, -isFlaggedFraud)
  , 10)

# Confirm how many transfer transactions have newBalanceOrig == 0
# and errBalanceDest == amount
fraud_selection <- formated_nrow(
  playset_TRANSFER %>% filter(isFraud == "F1" & 
                                newBalanceOrig == 0 & 
                                errBalanceDest == amount)
  )
message(
  "Fraudulent transfers with 'newBalanceOrig' and 'errBalanceDest' = 'amount': "
  , fraud_selection
  )

# Compare this to the total number of genuine transfer transactions with
# newBalanceOrig == 0
genuine_selection <- formated_nrow(
  playset_TRANSFER %>% filter(isFraud == "G0" & 
                              newBalanceOrig == 0 & 
                              errBalanceDest == amount)
  )
message(
  "Genuine transfer transactions with these characteristics: "
  , genuine_selection
  )

# Do some cleaning
rm(dest_error, orig_error, count_transfer, not_enough, orig_errors_df, 
   playset_TRANSFER, dest_errors_df, count_twice_amount, fraud_selection, 
   genuine_selection, txt)


## ----Analysis of fraudulent transactions, warning=FALSE, cache=TRUE---------------------------------------------------------------
# Filter all fraud transactions
playset_FRAUD <- playset %>% filter(isFraud == "F1")

# Display the number of fraudulent transactions
fraud_count <- formated_nrow(playset_FRAUD)
message("Total number of fraudulent transactions: ", fraud_count)


## ----Analysis of fraudulent transactions -continued, message=FALSE, warning=FALSE-------------------------------------------------
# Display a few of them
kable(head(
  playset_FRAUD %>% 
    select(-step, -typeOrig, -typeDest, -isFraud, -isFlaggedFraud, 
           - nameDest, -oldBalanceDest, -newBalanceDest)
  , 15), caption = "Analysis of fraudulent transactions - Origin side")

kable(head(
  playset_FRAUD %>% 
    select(-step, -typeOrig, -typeDest, -isFraud, -isFlaggedFraud, 
           - nameOrig, -oldBalanceOrig, -newBalanceOrig)
  , 15), caption = "Analysis of fraudulent transactions - Destination side")

# Repetition of fraud amount
same_amount_summary <- playset_FRAUD %>% 
  group_by(amount) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Display the most frequent amounts and their respective counts
kable(head(same_amount_summary, 10))

# Display how many times these counts are repeated
kable(table(
  same_amount_summary %>% pull(count)
  ), caption = "Number of times a given count is repeated"
  , col.names = c("count","frequency"))

# Do some cleaning
rm(fraud_count, playset_FRAUD, same_amount_summary, fraud_count)


## ---- Adding countOrig and countDest features, message=FALSE, warning=FALSE, eval=no_local_data-----------------------------------
# Create two new features: countOrig and countDest
playset <- playset %>% group_by(nameOrig) %>% mutate(countOrig = n())
playset <- playset %>% group_by(nameDest) %>% mutate(countDest = n())

validation <- validation %>% group_by(nameOrig) %>% mutate(countOrig = n())
validation <- validation %>% group_by(nameDest) %>% mutate(countDest = n())

# Move the features to a more relevant location in the dataframe
playset <- data.frame(playset %>% 
  relocate(countOrig, .after=nameOrig) %>% 
  relocate(countDest, .after=nameDest) %>%
  mutate(countOrig = as.integer(countOrig), countDest = as.integer(countDest)))
validation <- data.frame(validation %>% 
  relocate(countOrig, .after=nameOrig) %>% 
  relocate(countDest, .after=nameDest) %>%
  mutate(countOrig = as.integer(countOrig), countDest = as.integer(countDest)))


## ---- Creating errBalanceOrig and errBalanceDest features, eval=no_local_data-----------------------------------------------------
# Cash-out, Debit, transfer, payment transactions: 
# money moving out of origin account
# Cash-in: money moving in origin account, out of destination account
# use 'ifelse' clause to distinguish the two cases
playset <- playset %>% 
  mutate(errBalanceOrig = ifelse(type == "CASH_IN", 
                                 oldBalanceOrig + amount - newBalanceOrig, 
                                 oldBalanceOrig - amount - newBalanceOrig)) %>%
  mutate(errBalanceDest = ifelse(type == "CASH_IN", 
                                 oldBalanceDest - amount - newBalanceDest, 
                                 oldBalanceDest + amount - newBalanceDest))

validation <- validation %>% 
  mutate(errBalanceOrig = ifelse(type == "CASH_IN", 
                                 oldBalanceOrig + amount - newBalanceOrig, 
                                 oldBalanceOrig - amount - newBalanceOrig)) %>%
  mutate(errBalanceDest = ifelse(type == "CASH_IN", 
                                 oldBalanceDest - amount - newBalanceDest, 
                                 oldBalanceDest + amount - newBalanceDest))


## ---- Feature selection, eval=no_local_data---------------------------------------------------------------------------------------
# Dropping uninformative features
playset <- playset %>% 
  select(-typeOrig, -typeDest, -nameOrig, -nameDest, -isFlaggedFraud)
validation <- validation %>% 
  select(-typeOrig, -typeDest, -nameOrig, -nameDest, -isFlaggedFraud)

# Save the clean playset and validation dataset in directory "data"
# Creates "data" directory in the current working directory if it doesn't exist 
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
saveRDS(validation, file = "data/validation-clean.rds")
saveRDS(playset, file = "data/play-clean.rds")


## ---- results=FALSE, echo=FALSE, eval=!no_local_data------------------------------------------------------------------------------
## # If local data is available, then load sets
## # The next code blocks are not evaluated if there is local data
## playset <- readRDS("data/play-clean.rds")
## validation <- readRDS("data/validation-clean.rds")


## ----Function correlation heatmap, message = FALSE, warning = FALSE---------------------------------------------------------------
# Define function to run correlation and plot correlation heatmap
# Takes as argument the dataset as dataframe and the vector of features
# to include in the heatmap (vector of integer)
corr_heat <- function(data_frame, features_vect, add_to = FALSE){
  # Keep from data_frame the features of interest (features_vect)
  # and convert them to integer so cor() can run.
  
  df_correl <- sapply(
    as.data.frame(data_frame[, features_vect]), 
    as.integer)
  
  # Calculate correlation matrix
  correl_mat <- cor(df_correl)
  
  # visualize correlation matrix
  corrplot(correl_mat, method = "color", add = add_to,
           col = colorRampPalette(c("blue","white","red"))(200), 
           type = "upper", diag = F, 
           tl.cex=0.80, tl.col = "black", 
           tl.offset = 0.6, cl.cex = 0.7)
}



## ----Correlation heatmap all features, message = FALSE, warning = FALSE-----------------------------------------------------------
# Define what predictors to include
# 1: step, 2: type, 3: amount,
# 4: countOrig, 5: oldBalanceOrig, 6: newBalanceOrig,
# 7: count Dest, 8: oldBalanceDest, 9: newBalanceDest,
# 10: isFraud, 11: errBalanceOrig, 12: errBalanceDest
#
features_select <- c(1:12)

# Run function corr_heat()
corr_heat(playset, features_select)


## ----correlation heatmap fraud, message = FALSE, warning = FALSE------------------------------------------------------------------
# Keep only fraudulent transactions
playset_fraud <- playset %>% filter(isFraud == "F1")

# Keep only genuine transactions
playset_genuine <- playset %>% filter(isFraud == "G0")

# Select predictors of interest
features_select <- c(1:9, 11, 12)

# Splitting the graph are in two to allow account-by-account comparison
par(mfrow=c(1,2))

# Call corr_heat() on fraudulent transactions
corr_heat(playset_fraud, features_select)

# Call corr_heat() on regular transactions
corr_heat(playset_genuine, features_select)

mtext("Correlation heatmaps comparisons", 
      account = 3, line = -2, outer = TRUE, cex = 1)


## ---- include=FALSE---------------------------------------------------------------------------------------------------------------
# Do some cleaning
rm(playset_fraud, playset_genuine, features_select)


## ----Split playset, warning=FALSE, eval=no_local_data, cache=FALSE----------------------------------------------------------------
# Set seed for reproducibility
set.seed(1)

# The test set will be 20% of 'playset'
in_test <- createDataPartition(y = playset$isFraud, 
                               times = 1, p = 0.2, list = FALSE)
train.set <- playset[-in_test,]
test.set <- playset[in_test,]

# Do some cleaning
rm(in_test, transactions, transactions.raw)

## ----Load train.set and test.set, results=FALSE, echo=FALSE, eval=!no_local_data, cache=FALSE-------------------------------------
## # If there are local data, download train.set and test.set
## train.set <- readRDS("data/train.rds")
## test.set <- readRDS("data/test.rds")


## ----Display sets length, warning=FALSE-------------------------------------------------------------------------------------------
# Display the number of observations in all sets for control
message("'train.set' contains ", formated_nrow(train.set), " observations.")
message("'test.set' contains ", formated_nrow(test.set), " observations.")
message("'validation' contains ", formated_nrow(validation), " observations.")


## ----Save transactions dataset, message=FALSE, warning=FALSE, eval=no_local_data, cache=FALSE-------------------------------------
# Save the clean transactions dataset in directory "data"
# Creates "data" directory in the current directory if it doesn't exist 
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
saveRDS(train.set, file = "data/train.rds")
saveRDS(test.set, file = "data/test.rds")


## ----Cross-validation-------------------------------------------------------------------------------------------------------------

# Create smaller sub-set for training to lower execution time
# Set seed for repeatability and comparability
seed_general <- 123
set.seed(seed_general)
train.small <- sample_n(train.set, small_sample_size)

message("'train.small' contains ", formated_nrow(train.small), " observations.")

# Set-up k-fold and nb of repeats for repeated cross-validation
k_fold <- 5
n_repeat <- 3


## ----Models set up, message = FALSE-----------------------------------------------------------------------------------------------
# Create list of forumla
# Create the 3 different formulas for the 3 versions
form.base <- isFraud ~ .-countOrig -countDest -errBalanceDest -errBalanceOrig
form.err <- isFraud ~ .-countOrig -countDest
form.all <- isFraud ~ .

# Create list with all the formulas
form.list <- c("base" = form.base, "err" = form.err, "all" = form.all)

# Do some cleaning
rm(form.base, form.err, form.all)


## ----Start parallel computing, message=FALSE, warning=FALSE, cache=FALSE----------------------------------------------------------
# Start parallel computing
# Detect max number of cores and leave 1 out so
# the computer can still work on other things
nb_cores <- detectCores()-2
# Create cluster
parallel_clusters <- makePSOCKcluster(nb_cores)
# Start parallel processing
registerDoParallel(parallel_clusters)


## ----Create classes, warning=FALSE, message=FALSE---------------------------------------------------------------------------------
# Create S3 class "LabParam"
# Just a way to make sure the list structure is as required
LabParam <- function(seed, train_set, test_set){
  # Create LabParam list/object with the proper structure
  object <- list(
    "Seed" = seed,
    "TrainSet" = train_set,
    "TestSet" = test_set)

  # Set class name
  class(object) <- append(class(object), "LabParam")
  return(object)
}

# Create S3 class "ModelParam"
# Just a way to make sure the list structure is as required
ModelParam <- function(model_name, raw_prob, form_list, args_list){
  # Create ModelParam list/object with the proper structure
  object <- list("ModName" = model_name,
                 "RawProb" = raw_prob,
                 "FormulaList" = form_list,
                 "TrainArgs" = args_list)
  
  # Set class name
  class(object) <- append(class(object), "ModelParam")
  return(object)
}

# Create S3 class "LabEnv"
LabEnv <- function(lab_param){
  # Will contain all the model objects and common parameters
  object <- list()
  
  object["Params"] <- list(lab_param)
  
  # Set class name
  class(object) <- append(class(object), "LabEnv")
  return(object)
}

# Create S3 class "Model"
Model <- function(param_list){
  # Function to create S3 class with name "ModelObject"
  object <- list()
  
  object["Params"] <- list(param_list)
  object["ModFits"] <- list("Model details not updated yet.")
  object["Preds"] <- list("Predictions not updated yet.")
  object["ExecTimes"] <- list("Execution times not updated yet.")
  object["Perfs"] <- list("Performances not updated yet.")

  # Set the name for the class
  class(object) <- append(class(object), "Model")
  return(object)
}


## ----Create LabEnv object---------------------------------------------------------------------------------------------------------

# Create list of lab parameters using class LabParam.
# Seed, train_set, test_set will be used with all models
# trained in this lab environment.
lab_parameters <- LabParam(seed = seed_general,
                       train_set = train.small,
                       test_set = test.set)

# Create LabEnv object
lab_env <- LabEnv(lab_param = lab_parameters)

# Do some cleaning
rm(lab_parameters, train.small, test.set)


## ----import functions, code = readLines("./functions-model.r"), echo=FALSE, eval=TRUE---------------------------------------------
## These functions are printed in appendix in fraud-detection.pdf in order to not disturb the flow of the document.
## However, they must be run before they are called when running the models.

# Function pr_auc() to return the area under the precision-recall curve
pr_auc <- function(true_classes, pred_pc, score_type, include_curve = FALSE){
  # Depending on pred_type ("raw" or "prob"), pred_pc is either probabilities or classes
  # true_classes are the classes of the observations, "G0" or "F1"
  # scores.weights0 must be numerical values (1 for positive class, 0 for other)
  if(score_type == "prob"){
    # Here pred_pc are the probabilities for the positive class (classification scores)
    # given by the classifier
    PRAUC <- pr.curve(scores.class0 = pred_pc$F1
                      , weights.class0 = ifelse(true_classes == "F1", 1, 0) 
                      , curve = include_curve)
  }
  if(score_type == "raw"){
    # Here pred_pc is the class prediction for all classes
    PRAUC <- pr.curve(scores.class0 = ifelse(pred_pc == "F1", 1, 0)
                      , weights.class0 = ifelse(true_classes == "F1", 1, 0) 
                      , curve = include_curve)
  }
  return(PRAUC)
}


# Function to try each type of model on the formula list, based on caret::train()
# formula_list must be a list of formula with names to work properly
# pred_type can be "prob" or "raw"
run_model <- function(model_object, lab_object)
{
  # print("Calling the base run_model function")
  UseMethod("run_model", model_object)
}


run_model.default <- function(model_object, lab_object)
{
  print("Wrong object type.")
}


run_model.Model <- function(model_object, lab_object){
  # Initiate the three lists which will receive the fits,
  # predictions and performance
  fit.list <- list()
  pred.list <- list()
  time.list <- list()
  
  # Get parameters from model_object
  formula_list <- model_object$Params$FormulaList
  args_caret_train <- model_object$Params$TrainArgs
  pred_type <- model_object$Params$RawProb
  
  # Get general parameters from lab_object
  data_test <- lab_object$Params$TestSet
  data_train <- lab_object$Params$TrainSet
  
  set.seed(lab_object$Params$Seed)
  
  # Set the training dataset and fit the model
  # 1) Without engineered features
  # 2) With balance error features only
  # 3) With all features
  tic("Train 3 models")
  for (i in 1:length(formula_list)){
    # Update args_caret_train with formula and data to run model
    args_train <- c(list("form" = as.formula(formula_list[[i]]))
                    , "data" = list(data_train), args_caret_train)
    
    # In order to allow running caret::train() on different lists of arguments
    # the base::do.call() function is used.
    # Call train on list of arguments args_caret_train
    fit <- do.call(train, args_train)
    
    # Update list of fits with training results
    fit.list[names(formula_list)[i]] <- list(fit)
  }
  time_to_train <- toc()
  time.list["TrainTime"] <- list(
    c("msg" = list(time_to_train$msg)
      , "time.sec" = list(time_to_train$toc-time_to_train$tic))
  )    
  
  # Run the 3 models on the test dataset to get predictions
  # Request probabilities with type = "prob" to make it possible to estimate PRAUC
  # Measure exectuation time and update time.list with elapsed time
  tic("Predict 3 models")
  for (i in 1:length(formula_list)){
    pred.list[names(formula_list)[i]] <- list(
      predict.train(fit.list[[i]], data_test, type = pred_type)
    )
  }
  time_to_predict <- toc()
  time.list["PredTime"] <- list(
    c("msg" = list(time_to_predict$msg)
      , "time.sec" = list(time_to_predict$toc-time_to_predict$tic))
  )
  
  # Update model_object with new attributes
  model_object["ModFits"] <- list(fit.list)
  model_object["Preds"] <- list(pred.list)
  model_object["ExecTimes"] <- list(time.list)
  
  # Return all information as a list
  return (model_object)
}


add_mod_to_lab <- function(model_object, lab_object){
  lab_object[[model_object$Params$ModName]] <- model_object
  return(lab_object)
}


# Define function measure_perf() that returns PR-AUC and the confusion matrix
# of a given model a list of formulas. Performance is measured using the predictions
# calculated based on the test set. Takes into account a classification threshold when 
# predictions are probabilities (as opposed to classes)
measure_perf <- function(model_object, lab_object, threshold = 0.5){
  
  # Get parameters from model_object
  model_name <- model_object$Params$ModName
  formula_list <- model_object$Params$FormulaList
  pred_type <- model_object$Params$RawProb 
  
  # Get parameters from lab_object
  data_test <- lab_object$Params$TestSet
  
  # Initiate performance list
  perf.list <- list()
  
  # Measure performance and update the performance list for each of
  # the three model versions
  for (i in 1:length(formula_list)){
    # 1) Calculate PRAUC performance measure
    PRAUC <- pr_auc(true_classes = data_test$isFraud
                    , pred_pc = model_object$Preds[[i]]
                    , score_type = pred_type
                    , include_curve = TRUE)
    
    # 2) Depending on pred_type, measure performance using either
    # probabilities or class
    if(pred_type == "prob"){
      # Confusion Matrix is calculated from probabilities with the threshold
      # passed as argument
      cm <- confusionMatrix(
        factor(ifelse(model_object$Preds[[i]]$F1 > threshold
                      , "F1"
                      , "G0")
               , levels = c("F1", "G0")
               , labels = c("F1", "G0"))
        , data_test$isFraud
        , positive = "F1"
      )
      Threshold <- threshold
    } else if(pred_type == "raw"){
      # Confusion Matrix is calculated from predicted classes
      cm <- confusionMatrix(model_object$Preds[[i]]
                            , data_test$isFraud
                            , positive = "F1")
      Threshold <- "None (class)"
    } else {
      warning("Error: only 'raw' or 'prob' possible.")
    }
    # Update perf.list with performance for formula i
    perf.list[names(formula_list)[i]] <- list(
      c("PRAUC" = list(PRAUC)
        , "ConfusionMatrix" = list(cm)
        , "Threshold" = list(Threshold))
    )
  }
  # Update model_object with performance
  model_object["Perfs"] <- list(perf.list)
  
  # Return model_object updated with performance
  return (model_object)
}


# Function to display variable importance
# Returns a ggplot graph object
plot_var_imp <- function(model_object,
                         model_version,
                         lab_env){
  var_imp <- cbind(
    Variable = row.names(varImp(model_object$ModFits[[model_version]])[[1]])
    , varImp(model_object$ModFits[[model_version]])[[1]]
  ) %>% 
    arrange(desc(Overall))
  
  row.names(var_imp) <- NULL
  
  # Set the ggplot object
  g <- var_imp %>% 
    ggplot(aes(x = Overall, y = reorder(Variable, Overall))) +
    geom_bar(color = "white", fill="lightblue", stat = "identity") +
    xlab("Importance") +
    ylab("") +
    ggtitle("Variable importance")
  return(g)
}


# Return the performance of a model when different thresholds are used for 
# classification. savePredictions=TRUE is required in trainControl() when 
# training the models since this function re-uses the probabilities computed 
# by train() to save time. These probabilities are saved under in 
# "ModFits[[model_version]]$pred" in the Model object
threshold_effect <- function(model_object, 
                             model_version,
                             lab_env,
                             th.lim = c(0.1, 0.9)){
  
  # Get variables from Model and LabEnv objects:
  model_method = model_object$Params$TrainArgs$method
  nb_repeats <- model_object$Params$TrainArgs$trControl$repeats
  nb_folds <- model_object$Params$TrainArgs$trControl$number
  
  threshold <- seq(from = th.lim[1], to = th.lim[2], by = 0.1)
  df <- data.frame("Threshold" = numeric()
                   , "Type" = factor(levels=c("FP", "FN"))
                   , "Count" = integer()
                   , "Rep_ID" = integer())
  
  # Extract the probabilities for the model and version for all
  # nb_fold and nb_repeats of the cross-validation process
  # Select tunning parameters of bestTune
  
  best_tune <- model_object$ModFits[[model_version]]$bestTune
  
  if(model_method == "rf"){
    best_mtry <- best_tune$mtry
    preds_df <- model_object$ModFits[[model_version]]$pred %>%
      filter(mtry == best_mtry)
  } else if(model_method == "xgbTree"){
    best_nrounds <- best_tune$nrounds
    best_maxdepth <- best_tune$max_depth
    best_eta <- best_tune$eta
    best_gamma <- best_tune$gamma
    best_colsample <- best_tune$colsample_bytree
    best_minchildweight <- best_tune$min_child_weight
    best_subsample <- best_tune$subsample
    
    preds_df <- model_object$ModFits[[model_version]]$pred %>% 
      filter(nrounds == best_nrounds 
             & max_depth == best_maxdepth 
             & eta == best_eta 
             & gamma == best_gamma 
             & colsample_bytree == best_colsample 
             & min_child_weight == best_minchildweight 
             & subsample == best_subsample)
  } else {
    warning("No acceptable model method found.")
  }
  
  # Create predictions based on probabilities and thresholds for all thresholds
  for(th in threshold){
    rep_ID <- 0
    # Loop through each rep of nb_repeats
    for(rep in seq(1, nb_repeats, 1)){
      rep_ID <- rep_ID + 1
      # Loop through each fold of nb_folds
      for(fold in seq(1, nb_folds, 1)){
        name_resample <- paste("Fold", fold, ".Rep", rep, sep="")
        # Extract probabilities
        F1_prob <- (preds_df %>% filter(Resample == name_resample))$F1
        # Extract observations
        obs <- (preds_df %>% filter(Resample == name_resample))$obs
        # Make predictions based on threshold value and save as factor
        preds_th <- factor(ifelse(F1_prob > th, "F1", "G0") 
                           , levels = c("F1", "G0")
                           , labels = c("F1", "G0"))
        # Binds observations and predictions and compute false positives 
        # and false negatives
        # When using binding, factors are lost, but factor indexes remain
        bind <- data.frame(cbind(preds_th, obs))
        # Save false positives (fp)
        fp <- nrow(bind %>% filter(preds_th == 1 & obs == 2))
        df <- df %>% add_row("Threshold" = th
                             , "Type" = "FP"
                             , "Count" = fp
                             , "Rep_ID" = rep_ID)
        # save false negatives (fn)
        fn <- nrow(bind %>% filter(preds_th == 2 & obs == 1))
        df <- df %>% add_row("Threshold" = th
                             , "Type" = "FN"
                             , "Count" = fn
                             , "Rep_ID" = rep_ID)
      }
    }
  }
  return(df)
}  


# Plot threshold effect
plot_th_effect <- function(dataframe, threshold.lim = c(0.1, 0.8)){
  
  df <- dataframe %>% 
    filter((Threshold >= threshold.lim[1]) 
           & (Threshold <= threshold.lim[2])) %>%
    mutate(Threshold = factor(Threshold))
  
  max_count <- max(df$Count)
  
  df %>% ggplot(aes(x = Threshold, y = Count, fill = Type)) +
    geom_dotplot(binaxis='y'
                 , method = "histodot"
                 , stackdir='center'
                 , binwidth = max_count/30
                 , dotsize= 0.8
                 , position=position_dodge(1)
                 , alpha = 1) +
    xlab("Threshold") +
    ylab("Count") +
    ggtitle("Number of FP and FN for different thresholds") +
    theme(legend.position = "bottom")
}


# Return confusion matrix based on probablity observations and class of reference
# for the specified threshold
preds_to_cm <- function(model_object,
                        model_version,
                        lab_env,
                        threshold){
  # Get the test set
  test_set <- lab_env$Params$TestSet
  
  # Get the predictions
  F1_prob <- model_object$Preds[[model_version]]$F1
  preds_th <- factor(ifelse(F1_prob > threshold, "F1", "G0") 
                     , levels = c("F1", "G0") 
                     , labels = c("F1", "G0"))
  
  obs <- test_set$isFraud
  
  # Using table calculates the confusion matrix
  df <- data.frame("Predictions" = preds_th 
                   , "Reference" = obs
                   , stringsAsFactors = FALSE)
  return(table(df))
}


# Delete the parts of ModFits which are not needed for the version of the
# models that is not "version_keep". Helps manage memory space.
shrink_model <- function(model_object,
                         version_keep){
  model_fits <- model_object$ModFits
  for (i in 1:length(model_fits)){
    if (names(model_fits)[[i]] != version_keep){
      # Remove less important details
      model_object$ModFits <- "Removed to reduce size."
    }
  }
  return(model_object)
}


# Returns the number of reclassifiable false positives for a specific model and version
# based on a specific threshold
get_reclass_FP <- function(model_object, 
                           model_version,
                           lab_env,
                           threshold = 0.5){
  # Get parameters from model and lab_env
  type <- model_object$Params$RawProb
  test_set <- lab_env$Params$TestSet
  
  pred <- model_object$Preds[[model_version]]
  new_df <- cbind(test_set, pred)
  # Let's see how many of the positives are of type payment, debit or cash-in
  if (type == "prob"){
    nfp <- nrow(new_df %>% 
                  filter(F1 > 0.5) %>% 
                  filter(type=="PAYMENT"|type=="CASH_IN" | type=="DEBIT"))
  } else if (type == "raw"){
    nfp <- nrow(new_df %>% 
                  filter(pred == "F1") %>% 
                  filter(type=="PAYMENT"|type=="CASH_IN" | type=="DEBIT"))    
  } else {
    warning(
      "Type argument error in get_reclass_FP(): only 'raw' or 'prob' expected.\n"
    )
  }
  return(nfp)
}


# Function to extract and return performance based on the name of the model
get_performance <- function(model_object, lab_env){
  # Create data frame that will be returned
  return_df <- data.frame("Features" = character() 
                          , "PRAUC" = numeric() 
                          , "Recall" = numeric()
                          , "Precision" = numeric()
                          , "Accuracy" = numeric()
                          , "F1" = numeric() 
                          , stringsAsFactors=FALSE)
  
  # Extract the relevant information from lab_env object
  for (i in 1:length(model_object$Perfs)){
    model_version <- names(model_object$Perfs)[i]
    prauc <- round(
      model_object$Perfs[[i]]$
        PRAUC$auc.integral, 5)
    recall <- round(
      model_object$Perfs[[i]]$
        ConfusionMatrix$byClass["Recall"], 5)
    precision <- round(
      model_object$Perfs[[i]]$
        ConfusionMatrix$byClass["Precision"], 5)
    accuracy <- round(
      model_object$Perfs[[i]]$
        ConfusionMatrix$overall["Accuracy"], 5)    
    F1_perf <- round(
      model_object$Perfs[[i]]$
        ConfusionMatrix$byClass["F1"], 5)
    return_df <- return_df %>%
      add_row(Features = model_version
              , PRAUC = prauc
              , Recall = recall
              , Precision = precision
              , Accuracy = accuracy
              , F1 = F1_perf)
  }
  return(return_df)
}


# Function to extract and return execution time based on the name of the model
# Execution time expressed in hours, minutes, seconds
# Returns a named list
get_exec_time <- function(model_object, lab_env){
  exec_time <- round(model_object$ExecTime$TrainTime$time.sec + 
                       model_object$ExecTime$PredTime$time.sec, 5)
  # Get numbers of hours by using %/% to get quotient
  exec_time_hour <- exec_time %/% 3600 
  # Get numbers of minutes using %% (modulo)
  exec_time_min <- (exec_time - exec_time_hour*3600) %/% 60
  # Get remaining seconds
  exec_time_sec <- (exec_time - exec_time_hour*3600) %% 60
  
  exec_time <- list("hour" = exec_time_hour 
                    , "min" = exec_time_min 
                    , "sec" = exec_time_sec)
  return (exec_time)
}


# Function to extract the confusion matrix based on the name of the model
# and it's option ("base", "err", "all" or "best" - will select the best model)
# Metric for selection of best model can be specified, default value: PRAUC
get_conf_matrix <- function(model_object,
                            model_option, 
                            lab_env, 
                            comparison_metric="PRAUC"){
  best_perf <- 0
  
  # If "best" is passed as model_option this loops finds the best version
  # using comparison_metric for the comparison
  if(model_option == "best"){
    for (i in 1:length(model_object$Perfs)){
      model_version <- names(model_object$Perfs)[i]
      
      # Get performance measure
      if(comparison_metric == "PRAUC"){
        perf_measure <- model_object$Perfs[[i]]$
          PRAUC$auc.integral
      } else if(comparison_metric == "Accuracy"){
        perf_measure <- model_object$Perfs[[i]]$
          ConfusionMatrix$overall[["Accuracy"]]        
      } else {
        perf_measure <- model_object$Perfs[[i]]$
          ConfusionMatrix$byClass[[comparison_metric]]        
      }
      
      # If performance version i better than previous, update model_option
      model_option <- ifelse(perf_measure > best_perf
                             , model_version
                             , model_option)
      # If performance version i better than previous, update performance
      best_perf <- ifelse(perf_measure > best_perf
                          , perf_measure
                          , best_perf)
    }
  }
  
  return (list("Version" = model_option
               , "Metric" = comparison_metric
               , "Table" = model_object$Perfs[[model_option]]$ConfusionMatrix$table))
}


# Function to display the performance of a model
# "comparison_metric" required to compare the versions to get
# the best version
# Returns the best version of the model

display_best_perf <- function(model_object, 
                              lab_env, 
                              comparison_metric){
  # Retrieve performance data
  print(get_performance(model_object, lab_env))
  
  # Dispaly execution time list
  time_list <- get_exec_time(model_object)
  cat("Execution time: " 
      , time_list[[1]], "h "
      , time_list[[2]], "m "
      , time_list[[3]], "s.\n" 
      , sep = "")
  
  # Display confusion matrix for the best version of the
  # specified model
  conf_matrix <- get_conf_matrix(model_object = model_object
                                 , model_option = "best"
                                 , lab_env = lab_env
                                 , comparison_metric = comparison_metric)
  cat(
    "Best version: '"
    , conf_matrix[["Version"]]
    , "' when using '"
    , conf_matrix[["Metric"]]
    , "' as comparison metric.\n"
    , "Confusion matrix of best version: \n"
    , sep = ""
  )
  print(conf_matrix[["Table"]])
  return(conf_matrix[["Version"]])
}



## ----Run logistic algorithm, results=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------

# Set the general trainControl parameters for the train() function
trc <- trainControl("repeatedcv", 
                    number = k_fold, 
                    repeats = n_repeat,
                    allowParallel = TRUE,
                    classProbs = TRUE)

# Set model name for data retrieval and storage
model_name <- "Logistic"
raw_prob <- "prob"

# Set the arguments list for the train() function.
args_list <- list(
      method = "glm", 
      # data = train.small,
      family = "binomial",
      trControl = trc)

# Aggregate all these parameters into a "ModelParam" object
parameters <- ModelParam(model_name = model_name,
                   raw_prob = raw_prob,
                   form_list = form.list,
                   args_list = args_list)

# Initiate a new Model object
model_object <- Model(parameters)

# Update Model object with execution time,
# predictions and model fit
model_object <- run_model(model_object, lab_env)

# Update Model object with performances
model_object <- measure_perf(model_object, lab_env)


## ----Display logistic algorithm, warning=FALSE------------------------------------------------------------------------------------
# Display performance of the different versions
best_version <- display_best_perf(model_object = model_object,
                                  lab_env = lab_env,
                                  comparison_metric = "Accuracy")
# Shrink model to save memory space and add to lab_env for easy tracking
lab_env <- add_mod_to_lab(shrink_model(model_object = model_object,
                                       version_keep = best_version),
                          lab_object = lab_env)


## ----Count FP of type payment, debit and cash-in, warning=TRUE--------------------------------------------------------------------
nfp <- get_reclass_FP(model_object = model_object,
                      model_version = best_version,
                      lab_env = lab_env)

message("Number of positives of type 'CASH_IN', 'DEBIT' or 'PAYMENT': ", nfp)

# Do some cleaning
# saveRDS(model_object, "data/log-mod.rds")
rm(trc, model_name, model_object, args_list, 
   raw_prob, parameters, nfp, best_version)


## ----Run logistic algorithm with weights, results=FALSE, message=FALSE, warning=FALSE---------------------------------------------
# Set model name for data retrieval and storage
model_name <- "Logistic_weighted"
raw_prob <- "prob"

# Set the general trainControl parameters for the train() function
trc <- trainControl("repeatedcv", 
                    number = k_fold, 
                    repeats = n_repeat, 
                    classProbs = TRUE)

# Define model weights vector in line with the proportion of each class
model_weights <- ifelse(lab_env$Params$TrainSet$isFraud == "F1", 1000, 1)

# Define the arguments list for train()
args_list <- list(
      method = "glm", 
      # data = train.small,
      weights = model_weights,
      family = "binomial",
      trControl = trc)

# Aggregate all these parameters into a "ModelParam" object
parameters <- ModelParam(model_name = model_name,
                   raw_prob = raw_prob,
                   form_list = form.list,
                   args_list = args_list)

# Initiate a new Model object
model_object <- Model(parameters)

# Update Model object with execution time,
# predictions and model fit
model_object <- run_model(model_object, lab_env)

# Update Model object with performances
model_object <- measure_perf(model_object, lab_env)


## ----Display perf. weighted logistic, warning=FALSE-------------------------------------------------------------------------------
model_name <- "Logistic_weighted"
# Display performance of the models
best_version <- display_best_perf(model_object = model_object,
                                  lab_env = lab_env,
                                  comparison_metric = "Accuracy")

# Shrink model to save memory space and add to lab_env for easy tracking
lab_env <- add_mod_to_lab(shrink_model(model_object = model_object,
                                       version_keep = best_version),
                          lab_object = lab_env)

# Identify positives candidate for re-classification
message("Number of positives of type 'CASH_IN', 'DEBIT' or 'PAYMENT': ",
        get_reclass_FP(model_object = model_object,
                       model_version = best_version,
                       lab_env = lab_env,
                       threshold = 0.5))


## ---- results=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
# Back-up Model object for later use
# saveRDS(model_object, "data/log_weighted_mod.rds")

# Do some cleaning - remove unused object and run garbage collection
rm(trc, model_name, model_object, args_list, raw_prob, 
   parameters, model_weights, best_version)


## ----Run logistic algorithm with SMOTE, results=FALSE, warning=FALSE--------------------------------------------------------------
model_name <- "Logistic_SMOTE"
raw_prob <- "prob"

# Set the general trainControl parameters for the train() function
trc <- trainControl("repeatedcv", 
                    number = k_fold, 
                    repeats = n_repeat, 
                    sampling = "smote",
                    classProbs = TRUE)

# Define the arguments list for train()
args_list <- list(
      method = "glm", 
      # data = train.small,
      family = "binomial",
      trControl = trc)

# Aggregate all these parameters into a "ModelParam" object
parameters <- ModelParam(model_name = model_name,
                   raw_prob = raw_prob,
                   form_list = form.list,
                   args_list = args_list)

# Initiate a new Model object
model_object <- Model(parameters)

# Update Model object with execution time,
# predictions and model fit
model_object <- run_model(model_object, lab_env)

# Update Model object with performances
model_object <- measure_perf(model_object, lab_env)


## ----Display perf logistic SMOTE, warning=FALSE, cache=FALSE----------------------------------------------------------------------
model_name <- "Logistic_weighted"
# Display performance of the models
best_version <- display_best_perf(model_object, 
                                  lab_env,
                                  comparison_metric = "Accuracy")
# Shrink saved model to save memory space
lab_env <- add_mod_to_lab(shrink_model(model_object = model_object,
                                       version_keep = best_version),
                          lab_object = lab_env)
# Identify an positive candidate for reclassification
message("Number of positives of type 'CASH_IN', 'DEBIT' or 'PAYMENT': ", 
        get_reclass_FP(model_object = model_object,
                       model_version = best_version,
                       lab_env = lab_env,
                       threshold = 0.5))


## ---- results=FALSE, warning=FALSE, cache=FALSE, message=FALSE--------------------------------------------------------------------
# Back-up Model object for later use
# saveRDS(model_object, "data/log_smote_mod.rds")
# Do some cleaning
rm(trc, model_name, model_object, args_list, raw_prob, 
   parameters, best_version)
 


## ----Run Random Forest, message=FALSE, warning=TRUE, eval=models_rerun, cache=FALSE-----------------------------------------------
# Set model name
model_name <- "Random_Forest_ACC"
raw_prob <- "prob"

# Set the general trainControl parameters for the train() function
trc <- trainControl("repeatedcv",
                    number = k_fold,
                    repeats = n_repeat,
                    classProbs = TRUE,
                    savePredictions = TRUE,
                    allowParallel = TRUE,
                    verboseIter = TRUE)

tune_grid <- expand.grid(mtry = c(2, 4, 7))

# Define the arguments list for train()
args_list <- list(
      method = "rf",
      tuneGrid = tune_grid,
      # The models have run with up to 500 trees
      # 150 is largely enough
      ntree = 150,
      trControl = trc)

# Aggregate all these parameters into a "ModelParam" object
parameters <- ModelParam(model_name = model_name,
                   raw_prob = raw_prob,
                   form_list = form.list,
                   args_list = args_list)

# Initiate a new Model object
model_object <- Model(parameters)

# Update Model object with execution time,
# predictions and model fit
model_object <- run_model(model_object, lab_env)

# Update Model object with performances
model_object <- measure_perf(model_object, lab_env)

## ----echo=FALSE, results=FALSE, eval=!models_rerun--------------------------------------------------------------------------------
## # Load model_object from saved model if models_rerun = FALSE
## model_object <- readRDS("data/rf_acc_mod.rds")


## ----Display Random Forest ACC performance, warning=FALSE-------------------------------------------------------------------------
# Display performance of the models
model_name <- "Random_Forest_ACC"
# Display performance of the models
best_version <- display_best_perf(model_object, 
                                  lab_env,
                                  comparison_metric = "Accuracy")
# Shrink saved model to save memory space
lab_env <- add_mod_to_lab(shrink_model(model_object = model_object,
                                       version_keep = best_version),
                          lab_object = lab_env)
# Identify an positive candidate for reclassification
message("Number of positives of type 'CASH_IN', 'DEBIT' or 'PAYMENT': ", 
        get_reclass_FP(model_object = model_object,
                       model_version = best_version,
                       lab_env = lab_env,
                       threshold = 0.5))

# Calcualte the number of FP and FN for various value of the classification
# threhold
th_effect_df <- threshold_effect(model_object = model_object,
                                 lab_env = lab_env,
                                 model_version = best_version)
# Plot the graph of FN and FP vs. threshold value
plot_th_effect(th_effect_df)


## ----Get CM for a threshold of 0.6------------------------------------------------------------------------------------------------
preds_to_cm(model_object = model_object,
            model_version = best_version, 
            lab_env = lab_env, 
            threshold = 0.6)


## ----Feature importance check RF ACC, message=FALSE, warning=FALSE, cache=FALSE---------------------------------------------------
# Use function plot_var_imp() (see code in annex) to draw
# graph of variable importance.
plot_var_imp(model_object = model_object,
             model_version = best_version,
             lab_env = lab_env)


## ---- results=FALSE, warning=FALSE, message=FALSE, cache=FALSE, eval=models_rerun-------------------------------------------------
# Back-up Model object for later use
saveRDS(model_object, "data/rf_acc_mod.rds")

# Do some cleaning
rm(trc, model_name, model_object, args_list, raw_prob, 
   parameters, tune_grid, th_object_df, best_version)


## ----Run random forest PR, warning=FALSE, eval=models_rerun, cache=FALSE----------------------------------------------------------
# Set model name
model_name <- "Random_Forest_PR"
raw_prob <- "prob"

# Set the general trainControl parameters for the train() function
# This time we specify "prSummary" as summaryFunction to make it
# possible to tune parameters on PRAUC
trc <- trainControl("repeatedcv",
                    number = k_fold,
                    repeats = n_repeat,
                    classProbs = TRUE,
                    savePredictions = TRUE,
                    allowParallel = TRUE,
                    verboseIter = TRUE,
                    summaryFunction = prSummary)

tune_grid <- expand.grid(mtry = c(2, 4, 7))

# Define the arguments list for train()
# This time metric is "AUC" instead of default "Accuracy"
args_list <- list(
      method = "rf",
      tuneGrid = tune_grid,
      ntree = 150,
      metric = "AUC",
      # data = train.small,
      trControl = trc)

# Aggregate all these parameters into a "ModelParam" object
parameters <- ModelParam(model_name = model_name,
                   raw_prob = raw_prob,
                   form_list = form.list,
                   args_list = args_list)

# Initiate a new Model object
model_object <- Model(parameters)

# Update Model object with execution time,
# predictions and model fit
model_object <- run_model(model_object, lab_env)

# Update Model object with performances
model_object <- measure_perf(model_object, lab_env)

## ----echo=FALSE, results=FALSE, eval=!models_rerun--------------------------------------------------------------------------------
## # Load model_object from saved model if models_rerun = FALSE
## model_object <- readRDS("data/rf_pr_mod.rds")

## ----Display Random Forest PR performance, warning=FALSE--------------------------------------------------------------------------
# Display performance of the models
model_name <- "Random_Forest_PR"
# Display performance of the models
best_version <- display_best_perf(model_object, 
                                  lab_env,
                                  comparison_metric = "PRAUC")
# Shrink saved model to save memory space
lab_env <- add_mod_to_lab(shrink_model(model_object = model_object,
                                       version_keep = best_version),
                          lab_object = lab_env)
# Identify an positive candidate for reclassification
message("Number of positives of type 'CASH_IN', 'DEBIT' or 'PAYMENT': ", 
        get_reclass_FP(model_object = model_object,
                       model_version = best_version,
                       lab_env = lab_env,
                       threshold = 0.5))

# Calculate the number of FP and FN for various value of the classification
# threhold
th_effect_df <- threshold_effect(model_object = model_object,
                                 lab_env = lab_env,
                                 model_version = best_version)
# Plot the graph of FN and FP vs. threshold value
plot_th_effect(th_effect_df)


## ----Get CM for best version RF PR, cache = FALSE---------------------------------------------------------------------------------
preds_to_cm(model_object = model_object,
            model_version = best_version,
            lab_env = lab_env,
            threshold = 0.4)


## ---- results=FALSE, warning=FALSE, message=FALSE, cache=FALSE, eval=models_rerun-------------------------------------------------
# Back-up Model object for later use
saveRDS(model_object, "data/rf_pr_mod.rds")

# Do some cleaning
rm(trc, model_name, model_object, args_list, raw_prob, 
   parameters, tune_grid, th_object_df, best_version)


## ----Run GBM algorithm, results=FALSE, message=FALSE, warning=FALSE, eval=models_rerun, cache=FALSE-------------------------------

# Set model name
model_name <- "GBM_ACC"
raw_prob <- "prob"

# Set the general trainControl parameters for the train() function
# This time we specify "prSummary" as summaryFunction to make it
# possible to tune parameters on PRAUC
trc <- trainControl("repeatedcv",
                    number = k_fold,
                    repeats = n_repeat,
                    verboseIter = TRUE,
                    savePredictions = TRUE,
                    allowParallel = TRUE,
                    classProbs = TRUE)

# Grid space for the search for best hyperparameters
tune_grid <- expand.grid(nrounds = c(100, 150, 180),
                       max_depth = c(10, 15, 20),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
                      )

# Define the arguments list for train()
args_list <- list(
      method = "xgbTree",
      trControl = trc,
      tuneGrid = tune_grid)

# Aggregate all these parameters into a "ModelParam" object
parameters <- ModelParam(model_name = model_name,
                   raw_prob = raw_prob,
                   form_list = form.list,
                   args_list = args_list)

# Initiate a new Model object
model_object <- Model(parameters)

# Update Model object with execution time,
# predictions and model fit
model_object <- run_model(model_object, lab_env)

# Update Model object with performances
model_object <- measure_perf(model_object, lab_env)

## ----echo=FALSE, results=FALSE, eval=!models_rerun, cache = FALSE-----------------------------------------------------------------
## # Load model_object from saved model if models_rerun = FALSE
## model_object <- readRDS("data/gbm_acc_mod.rds")

## ----Display GBM algorithm and optimisation, message=FALSE, warning=FALSE---------------------------------------------------------
# Display performance of the models
best_version <- display_best_perf(model_object, 
                                  lab_env,
                                  comparison_metric = "Accuracy")
# Look at variable importance
plot_var_imp(model_object = model_object,
             model_version = best_version,
             lab_env = lab_env)

# Shrink saved model to save memory space
lab_env <- add_mod_to_lab(shrink_model(model_object = model_object,
                                       version_keep = best_version),
                          lab_object = lab_env)
# Identify an positive candidate for reclassification
message("Number of positives of type 'CASH_IN', 'DEBIT' or 'PAYMENT': ", 
        get_reclass_FP(model_object = model_object,
                       model_version = best_version,
                       lab_env = lab_env,
                       threshold = 0.5))

# Calcualte the number of FP and FN for various value of the classification
# threhold
th_effect_df <- threshold_effect(model_object = model_object,
                                 lab_env = lab_env,
                                 model_version = best_version)
# Plot the graph of FN and FP vs. threshold value
plot_th_effect(th_effect_df)


## ----Get CM for GBM_ACC best version, cache = FALSE-------------------------------------------------------------------------------
preds_to_cm(model_object = model_object,
            model_version = best_version,
            lab_env, 0.3)


## ---- warning=FALSE, echo=FALSE, eval=models_rerun, cache=FALSE-------------------------------------------------------------------
# Back-up Model object for later use
saveRDS(model_object, "data/gbm_acc_mod.rds")

# Do some cleaning
rm(trc, model_name, model_object, args_list, raw_prob, 
   parameters, tune_grid, th_object_df, best_version)


## ----Run GBM with PR metric, warning=FALSE, message=FALSE, cache=FALSE, eval=models_rerun-----------------------------------------

# Set model name
model_name <- "GBM_PR"
raw_prob <- "prob"

# Set the general trainControl parameters for the train() function
# This time we specify "prSummary" as summaryFunction to make it
# possible to tune parameters on PRAUC
trc <- trainControl("repeatedcv",
                    number = k_fold,
                    repeats = n_repeat,
                    savePredictions = TRUE,
                    allowParallel = TRUE,
                    classProbs = TRUE,
                    verboseIter = TRUE,
                    summaryFunction = prSummary)

# Grid space for the search for best hyperparameters
tune_grid <- expand.grid(nrounds = c(100, 150, 180),
                       max_depth = c(10, 15, 20),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
                      )

# Define the arguments list for train()
# This time metric is "AUC" instead of default "Accuracy"
args_list <- list(
      method = "xgbTree",
      metric = "AUC",
      trControl = trc,
      tuneGrid = tune_grid)

# Aggregate all these parameters into a "ModelParam" object
parameters <- ModelParam(model_name = model_name,
                   raw_prob = raw_prob,
                   form_list = form.list,
                   args_list = args_list)

# Initiate a new Model object
model_object <- Model(parameters)

# Update Model object with execution time,
# predictions and model fit
model_object <- run_model(model_object, lab_env)

# Update Model object with performances
model_object <- measure_perf(model_object, lab_env)

## ----echo=FALSE, results=FALSE, eval=!models_rerun, cache = FALSE-----------------------------------------------------------------
## # Load model_object from saved model if models_rerun = FALSE
## model_object <- readRDS("data/gbm_pr_mod.rds")

## ----Display GBM PR and optimisation----------------------------------------------------------------------------------------------
# Display performance of the models
best_version <- display_best_perf(model_object, 
                                  lab_env,
                                  comparison_metric = "PRAUC")
# Look at variable importance
plot_var_imp(model_object = model_object,
             model_version = best_version,
             lab_env = lab_env)

# Shrink saved model to save memory space
lab_env <- add_mod_to_lab(shrink_model(model_object = model_object,
                                       version_keep = best_version),
                          lab_object = lab_env)
# Identify an positive candidate for reclassification
message("Number of positives of type 'CASH_IN', 'DEBIT' or 'PAYMENT': ", 
        get_reclass_FP(model_object = model_object,
                       model_version = best_version,
                       lab_env = lab_env,
                       threshold = 0.5))

# Calcualte the number of FP and FN for various value of the classification
# threhold
th_effect_df <- threshold_effect(model_object = model_object,
                                 lab_env = lab_env,
                                 model_version = best_version)
# Plot the graph of FN and FP vs. threshold value
plot_th_effect(th_effect_df)


## ----Get best CM for GBM_PR best version, cache = FALSE---------------------------------------------------------------------------
preds_to_cm(model_object = model_object,
            lab_env = lab_env,
            model_version = best_version,
            threshold = 0.4)


## ---- warning=FALSE, cache=FALSE--------------------------------------------------------------------------------------------------
#{r, warning=FALSE, cache=FALSE, eval=models_rerun}
# Back-up Model object for later use
saveRDS(model_object, "data/gbm_pr_mod.rds")

# Do some cleaning
rm(trc, model_name, model_object, args_list, raw_prob, 
   parameters, tune_grid, th_object_df, best_version)


## ----Stop parallel computing, message=FALSE, warning=FALSE, cache=FALSE-----------------------------------------------------------
# Stop parallel computing
stopCluster(parallel_clusters)


## ----Validation best model, echo = FALSE, message = FALSE, warning = FALSE, cache = FALSE-----------------------------------------

model_object <- readRDS("data/gbm_acc_mod.rds")

best_version <- get_conf_matrix(model_object = model_object
                                , model_option = "best"
                                , lab_env = lab_env
                                , comparison_metric = "PRAUC")

# Extract the best model from lab_env
best_fit <- model_object$ModFits[[best_version$Version]]

# Get the predictions on the validation using the best model
preds <- predict.train(best_fit, validation)

# Calculate confusion matrix and F1-score
cm <- confusionMatrix(preds, validation$isFraud)

kable(cm$table, caption = "Confusion matrix validation set")

cm$byClass["F1"]

rm(model_object)


## ----import code annex, code = readLines("./functions-model.r"), echo=TRUE, eval=FALSE--------------------------------------------
## 

