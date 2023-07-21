#____________________________________________

#TASK 1: Split the Transaction Code to extract the letters at the start of the transaction code. These identify the bank who processes the transaction
#Rename the new field with the Bank code 'Bank'.

--------------------------------------------

  #Create the Output data.frame for comparison
Output <- Input
View(Output) 

#Extract words in the Input$Transaction.Code divided by "-"
Bank <- data.frame(Bank = sapply(strsplit
                      (Input$Transaction.Code, "-"), function(x) x[1]),
                       Input$Transaction.Code)

#This is only for backing up
View(Bank)
Output_t1 <- Bank
View(Output_t1)

#Combine the new data.frame to the Input to create an output_og
Output_og <- cbind(Bank[1], Input)
View(Output_og)

#____________________________________________

#TASK 2: Rename the values in the Online or In-person field, 
#Online of the 1 values and In-Person for the 2 values.

--------------------------------------------
Output_og <- Output_og %>%
  mutate(Online.or.In.Person = case_when(
    Output_og$Online.or.In.Person == 1 ~ "Online",
    Output_og$Online.or.In.Person == 2 ~ "In-person",
    TRUE ~ as.character(Output_og$Online.or.In.Person)    
    ))
    # If there are other values, keep them as they are

#____________________________________________

#TASK 3: Change the date to be the day of the week

--------------------------------------------
install.packages("lubridate")
library("lubridate")

Output_t3 <- wday(Output_og$Transaction.Date)
View(Output_t3)

#
Output_og <- Output_og %>%
  mutate(Transaction.Day = wday(Output_og$Transaction.Date))

Output_og <- Output_og %>%
  mutate(Transaction.Dayv2 = case_when(
         Output_og$Transaction.Day == 1 ~ "Sunday",
          Output_og$Transaction.Day == 2 ~ "Monday",
           Output_og$Transaction.Day == 3 ~ "Tuesday",
            Output_og$Transaction.Day == 4 ~ "Wednesday",
             Output_og$Transaction.Day == 5 ~ "Thursday",
              Output_og$Transaction.Day == 6 ~ "Friday",
               Output_og$Transaction.Day == 7 ~ "Saturday"))

#____________________________________________

#TASK 4: #Different levels of detail are required in the outputs. 
#You will need to sum up the values of the transactions in three ways
#1. Total Values of Transactions by each bank
#2. Total Values by Bank, Day of the Week and Type of Transaction (Online or In-Person)
#3. Total Values by Bank and Customer Code

--------------------------------------------

#4.1 Total value each bank
Value_sum_4.1 <- data.frame(Bank = Output_og$Bank,Value = Input$Value)
Value_sum_4.1 <- Value_sum_4.1 %>%
  group_by(Bank) %>%
  summarise(Value = sum(Value))

#4.2 Total Values by Bank, Day of the Week and Type of Transaction (Online or In-Person)
Value_sum_4.2 <- data.frame(Bank = Output_og$Bank,
                            Value = Input$Value,
                            Day = Output_og$Transaction.Dayv2,
                            Type = Output_og$Online.or.In.Person)
Value_sum_4.2 <- Value_sum_4.2 %>%
  group_by(Bank, Day, Type) %>%
  summarise(Value = sum(Value))

#4.3 Total Values by Bank and Customer Code
Value_sum_4.3 <- data.frame(Bank = Output_og$Bank,
                            Value = Input$Value,
                            Customer = Output_og$Customer.Code)
Value_sum_4.3 <- Value_sum_4.3 %>%
  group_by(Bank, Customer) %>%
  summarise(Value = sum(Value))


