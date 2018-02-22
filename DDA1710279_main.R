############################### LOAN DATA CASE STUDY########################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(outliers)
library(zoo)
library(corrplot)
###########################################################################################

#-------------Data sourcing

#Importing the dataset
loan <- read.csv("loan.csv")   # Contains 39717 obs with 111 variables


summary(loan) #Shows that there are several columns which have mode as Logical and all of them as NA's
#################################################################################################
#------------Data Cleaning

#Removing columns with NA's greater than 40% of the column data

loan1 <- loan[,which(colMeans(is.na(loan)) <=0.4)]  #drops nearly 50% of columns, there are now 55 columns

#-----------------------------------------------------------------------------------------
#Looking for duplicate rows

sum(duplicated(loan1))  #0, there are no duplicates

#------------------------------------------------------------------------------------------

#Logic for removing columns with 0's where median is 0.

nums <- sapply(loan1, is.numeric)  #Extracting only numeric columns
nums1 <- loan1[,nums]

zero_columns<- sapply(nums1,function(x) median(x,na.rm=T)==0)  #Apply function to find which numeric columns has median as 0

remove_col <- names(which(zero_columns==T)) #getting the column names which has Median 0
remove_col

loan1 <- loan1[,-which(names(loan1) %in% remove_col)] # removing the those columns which has median 0

#---------------------------------------------------------------------------------------------

#Removing columns by observations

summary(loan1)

#removing these columns:title, next_pymnt_d,emp_title ,id,member_id,desc, pymnt_plan,url,zip_code,initial_list_status,policy_code,application_type
#the columns total_pymnt and total_pymnt_inv are the same and the latter has the rounded version,hence dropping total_pymnt

remove_col <- c("title","total_pymnt","next_pymnt_d","emp_title","id","member_id","desc","pymnt_plan","zip_code","initial_list_status","policy_code","application_type","url")
loan1 <- loan1[,-which(names(loan1) %in% remove_col)] # removing the unwanted columns

#-----------------------------------------------------------------------------------------------

#Finding the NA's

sum(is.na(loan1))  # 1

#There is just 1 NA.


#------------------------------------------------------------------------------------------
#Finding the number of blanks

num_of_blanks <- sapply(loan1,function(x) length(which(x =="")))
num_of_blanks #all the values are within thresholds

#-------------------------------------------------------------------------------------------

# Treating the date columns (Please note the failing to parse warning messages are expected)

columns_date <- c("issue_d","earliest_cr_line","last_pymnt_d","last_credit_pull_d") #columns containing date

#To handle the dates thats before 1970
date_function <- function(x){   
  formatted_date <- paste("01-",as.character(x))
   y <- dmy(formatted_date)
   corrected_Date <- as.Date(ifelse(y > Sys.Date(), format(y, "19%y-%m-%d"), format(y)))
   return(corrected_Date)
}

loan1_a <- loan1[,columns_date] #Subsetting only the date columns

loan1[,columns_date] <- lapply(loan1_a,function(x) date_function(x))


summary(loan1)
#--------------------------------------------------------------------------------------------------

#Removing the % sign from interest rate columns

interest_cols <- c("int_rate","revol_util") #columns containing interest rates, but as strings
loan1_b <- loan[,interest_cols] #subsetting only the interest rate columns
loan1[,interest_cols] <- lapply(loan1_b,function(x) as.numeric(sub("%","",x)))  #storing as integer

#-----------------------------------------------------------------------------------------------------------------------

#We are conidering only 2 loan status "Charged off" and "Fully paid" for our analysis and removing the status "current" as its an intermittant indicator

loan1 <- filter(loan1,loan_status %in% c("Fully Paid","Charged Off"))

#----------------------------------------------------------------------------------------------------------------

#----------Outlier treatment:

#Applying the standard boxplot technique to remove the outliers
# Standard Box plot : [ Q1- c*IQD, Q3+c*IQD] , c=3

# Removing the outlier from the annual income column as this is the primary attribute while applying loan.

summary(loan1$annual_inc) #before outlier removal

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4000   40000   58870   68780   82000 6000000 
boxplot(loan1$annual_inc) #before outlier removal

#removing outlier using standard boxplot formula
loan1 <- loan1 %>% filter(loan1$annual_inc > quantile(loan1$annual_inc, .25) - 3*IQR(loan1$annual_inc) & loan1$annual_inc < quantile(loan1$annual_inc, .75) + 3*IQR(loan1$annual_inc))

summary(loan1$annual_inc) #After outlier removal

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4000   40000   56000   61080   78000  145000 

boxplot(loan1$annual_inc) # After outlier removal

##################################################################################################

#Strategy for the EDA:
#Now that We have a clean data in hand:We are breaking down our analysis in following ways:

# 1. Identify the Loan, Borrower, Credit line Characteristics.
# 2.  Bin the continuous variables into categorical variables for each of the 3 segments
# 3. To establish the level of dependency with respect the loan status (Fully paid or Charged off), perform the proportion test and extract the chi square statistic
# 4. Compare the chi square statistic within each segment and pick the top 5 variables with the least chi square statistic, lower the number , higher is the dependency.
# 5.For the chosen variable , perform the ggplot visualization to know the various attributes of the variable.


#------------------------------------------------------------------------------------------------------

# Variables that describe the loan Characteristics:loan_amnt,funded_amnt,funded_amnt_inv,term,int_rate,installment,grade,sub_grade

#Variables that describe the Borrower Characteristics:  emp_length,home_ownership,annual_inc,verification_status,issue_d,loan_status,purpose,addr_state,dti,

#Variables that describe the Credit line Characteristics: earliest_cr_line,inq_last_6_mths,open_acc,revol_bal,revol_utl,total_acc,total_pymnt_inv,total_rec_prncp,total_rec_int,last_pymnt_d,last_paymnt_amnt,last_credit_pull_d

#----------------DERIVED METRICS------------------------


#Derived metrics for the loan Characteristics:

# loan_amnt binned into loan_amnt_cat
summary(loan1$loan_amnt) #Min=500,Max=35000

loan1$loan_amnt_cat<-cut(loan1$loan_amnt, seq(0,35000,5000), right=FALSE,dig.lab=5)

# loan funded_amnt binned into funded_amnt_cat
summary(loan1$funded_amnt) #Min=500,Max=35000

loan1$funded_amnt_cat<-cut(loan1$funded_amnt, seq(0,35000,5000), right=FALSE,dig.lab=5)

# funded_amnt_inv binned into funded_amnt_inv_cat

summary(loan1$funded_amnt_inv) #Min=0,Max=35000

loan1$funded_amnt_inv_category<-cut(loan1$funded_amnt_inv, seq(0,35000,10000), right=FALSE,dig.lab=5)

# int_rate binned into int_rate_cat

summary(loan1$int_rate) # Min=5.42, Max=24.40

loan1$int_rate_cat <- cut(loan1$int_rate, seq(0,25,5), right=FALSE)

#installment binned into installment_cat

summary(loan1$installment) #Min=15.69,Max=1305

loan1$installment_cat <- cut(loan1$installment, seq(0,1305,200), right=FALSE,dig.lab=5)


#--------------------------------------------------------------------------------------------------------------

#Derived metrics for the Borrower Characteristics:

#Adding a new variable based on home ownership
loan1$home_ownership <- tolower(loan1$home_ownership)

loan1$homeown_risk <- ifelse(loan1$home_ownership  %in% c("non","other","mortgage"),1,ifelse(loan1$home_ownership %in% "rent",2,3))


#Appending new variable dti_category
summary(loan1$dti) #min=0,max=30
loan1$dti_cat <- cut(loan1$dti,seq(0,30,5),right=FALSE)


# binning annual_inc to annual_inc_cat

summary(loan1$annual_inc) #Min=4000,Max=208000

loan1$annual_inc_cat <- cut(loan1$annual_inc, seq(0,208000,20000), right=FALSE, dig.lab=6)
                            

#-----------------------------------------------------------------------------------------------------

##Derived metrics for the Credit line characteristics

summary(loan1$inq_last_6mths) #(Min=0,Max=8)

loan1$inq_last_6mths_cat <- cut(loan1$inq_last_6mths, seq(0,8,2), right=FALSE) #inq_last_6mths_cat is derived

summary(loan1$open_acc) #(Min=2,Max=44)

loan1$open_acc_cat <- cut(loan1$open_acc, seq(0,44,4), right=FALSE) #loan1$open_acc_cat is derived

summary(loan1$revol_bal) #Min=0,Max=149600

loan1$revol_bal_cat <- cut(loan1$revol_bal, seq(0,149600,29000), right=FALSE, dig.lab=6)

summary(loan1$revol_util) #Min=0.Max=99.9

loan1$revol_util_cat <- cut(loan1$revol_util,seq(0,99.9,30),right=FALSE,dig.lab=5)

summary(loan1$total_acc) #min=2,max=90,

loan1$total_acc_cat <- cut(loan1$total_acc,seq(0,90,15),right=FALSE,dig.lab=5)

summary(loan1$total_pymnt_inv) #min=0,max=58560

loan1$total_pymnt_inv_cat <- cut(loan1$total_pymnt_inv,seq(0,58560,11000),right=F,dig.lab=5)

summary(loan1$total_rec_int) #min=0,max=23560

loan1$total_rec_int_cat <- cut(loan1$total_rec_int,seq(0,23560,4000),right=FALSE,dig.lab=5)

summary(loan1$last_pymnt_amnt) #min=0,max=36120

loan1$last_pymnt_amnt_cat <- cut(loan1$last_pymnt_amnt,breaks=seq(0,36120,7000),dig.lab=5,right=FALSE)

summary(loan1$total_rec_prncp) #min=0,max=35000

loan1$total_rec_prncp_cat <- cut(loan1$total_rec_prncp,breaks=seq(0,35000,10000),dig.lab=5,right=FALSE)


#-----------------------------------------------------------------------------------------------------------------


#Now that we have binned the variables across the 3 characteristics.We will now check the chi statistic with each
#one of those variables to check the degree of how statistically dependent on the loan status


#All the Categorical columns for the loan characteristics

loan_variables <- c("loan_amnt","funded_amnt","funded_amnt_inv","term","int_rate","installment","grade","sub_grade")

borrower_variables <- c("emp_length","homeown_risk","annual_inc_cat","verification_status","purpose","dti_cat")

creditline_variables <- c("inq_last_6mths_cat","open_acc_cat","revol_bal_cat","revol_util_cat","total_acc_cat","total_pymnt_inv_cat","total_rec_prncp_cat","total_rec_int_cat","last_pymnt_amnt_cat")


#----------------------------------------------------------------------------------------

#Function to compute the Chi-square statistic(Using the table method) and proportion table

chisq_test <- function(x,y){
  tab1 <- table(x,y)
  tab1 <- tab1[which(rowSums(tab1)!=0),]  #eliminating rows which has 0's
  tab1 <- tab1[,-2]                       #removing current column
  tab1 <- summary(tab1)   
  return  (tab1[3])                       #extracting the chi statistic number
}

#function to return the prop table
prop_func <- function(x,y){
  tab1 <- table(x,y)
  tab1 <- tab1[which(rowSums(tab1)!=0),]  #eliminating rows which has 0's
  tab1 <- tab1[,-2] 
  prop_table <- round(prop.table(tab1,1),3)  #rowwise proportion
  return(prop_table)
}
#----------------------------------------------------------------------------------------

#Performing the chi-square test for the loan characteristics

loan_variables_df <- loan1[,loan_variables]  #subsetting the loan1 dataframe 

chi_loan_variables <- sapply(loan_variables_df,function(x) chisq_test(x,loan1$loan_status)) #passing the loan_status as another column for the chi-sq test

loan_chi_dataframe <-  data.frame(chi_statistic = round(unlist(chi_loan_variables),2))

View(loan_chi_dataframe)

#Looking at the comparitive chi statistic numbers, given the lower is the number, higher is the statistical dependence

#The top 5 indicators , that would be taken for further study are
# term,loan_amnt,funded_amnt,grade,sub_grade


#----------------------------------------------------------------------------------------

#Performing the chi-square test for the borrower characteristics

borrower_variables_df <- loan1[,borrower_variables]  #subsetting the loan1 dataframe 

chi_borrower_variables <- sapply(borrower_variables_df,function(x) chisq_test(x,loan1$loan_status)) #passing the loan_status as another column for the chi-sq test

borrower_chi_dataframe <-  data.frame(chi_statistic = round(unlist(chi_borrower_variables),2))

View(borrower_chi_dataframe)

#Looking at the comparitive chi statistic numbers, given the lower is the number, higher is the statistical dependence

#The top 5 indicators , that would be taken for further study are
# home ownership,dti,emp_length,verification_status,addr_state


#----------------------------------------------------------------------------------------

#Performing the chi-square test for the credit_line characteristics

creditline_variables_df <- loan1[,creditline_variables]  #subsetting the loan1 dataframe 

chi_creditline_variables <- sapply(creditline_variables_df,function(x) chisq_test(x,loan1$loan_status)) #passing the loan_status as another column for the chi-sq test

creditline_chi_dataframe <-  data.frame(chi_statistic = round(unlist(chi_creditline_variables),2))

View(creditline_chi_dataframe)

#Looking at the comparitive chi statistic numbers, given the lower is the number, higher is the statistical dependence

#The top 5 indicators , that would be taken for further study are
# total_acc,revol_bal,total_rec_int,open_acc,inq_last_6mths

#-----------------------------------------------------------------------------------------------


#######################################################################################3
#---------------Generic test on the issue date ------------------

loan_issue_date <- loan1 %>% group_by(yearqtr=as.yearqtr(format(issue_d),"%Y-%m-%d")) %>% summarise(count=n())

ggplot(loan_issue_date, aes(x=yearqtr, y=count)) + geom_line(colour="red", linetype="solid", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="green")+labs(title="Demand over years",x="years",y="number of loans")+scale_x_yearqtr(format="%Y Q%q", n=4)

#Shows an exponential increase in the number of loans issued over by quarters by years


#-----------------earliest_cr_line--------------------

early_cr_line <- loan1 %>% group_by(yearmon=as.yearmon(earliest_cr_line),loan_status) %>% summarise(n=n())

ggplot(early_cr_line, aes(x=yearmon, y=n,col=loan_status)) + geom_line(linetype="solid", size=1) +labs(title="Y2K effect signalling increase in buying power",x="years",y="number of loans")+scale_x_yearmon(format = "%Y",n=5)

#There is a spike in the number of credit lines opened in borrower account in the Year 2K, possibly coinciding with economic growth and the dotcom bubble


#-------------------------Address state----------------------------

#  5) addr_state: The impact of borrower's address 

addr_prop <- prop_func(loan1$addr_state,loan1$loan_status) #calling the prop_func
addr_prop <- data.frame(addr_prop)
addr_prop$state_name <- state.name[match(addr_prop$x,state.abb)] #state code to state name conversion
addr_prop <- subset(addr_prop,y=="Charged Off") #subsetting by loan status as charged off only

addr_prop1 <- addr_prop %>% arrange(desc(Freq))  %>% slice(1:9) #Picking the top 9 states by default %

#Plotting address state by frequency of charged off's
ggplot(addr_prop1,aes(x=state_name,y=Freq,fill="blue"))+geom_bar(stat="identity")+labs(title="State vs loan status",x="State",y="freq")

#                Freq   state_name
# 1 NE Charged Off 0.60     Nebraska
# 2 NV Charged Off 0.23       Nevada
# 3 SD Charged Off 0.19     South Dakota

###########################################################################################################################

# Assessment of the loan Characteristics

##########################################################################################################################
#From the chi statistic on the loan variables, we have deduced , we will be drilling down on the following variables

#term,loan_amnt,funded_amnt,grade,sub_grade.Lets look into each one of them
#Since the grade and subgrade are related, also picking the int_rate which has a high statistical co-relation with status
#---------------------------------------------------------------------------------------------
#  1) term: The impact of tenure( 36 months or 60 months on the loan status)

term_prop <- prop_func(loan1$term,loan1$loan_status) #calling the prop_func
term_prop    #Shows the probability of a borrower defaulting is high(0.25) for 60 months tenure

#              Charged Off  Fully Paid
# 36 months        0.11       0.89
# 60 months        0.25       0.75

term_prop <- data.frame(term_prop)
#Column chart to show the proportion of spread among loan term and loan status
ggplot(term_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge")+labs(title="term vs loan status",x="term",y="freq")

#---------------------------------------------------------------------------------------------------

# 2) loan_amnt :The impact of the loan_amnt on loan status

summary(loan1$loan_amnt)  

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 500    5200    9600   10900   15000   35000 


#Plotting loan amount over the issue date

ggplot(loan1,aes(x=year(issue_d),y=loan_amnt,group=year(issue_d)))+geom_boxplot(col="blue")+labs(title="Loan amount over issue date")
#There is a marginal increase in the median loan amount seeked, over the years

# % of frequency distribution by loan amount category and loan status
loan_amnt_prop <-  data.frame(prop_func(loan1$loan_amnt_cat,loan1$loan_status))#calling the prop_func
loan_amnt_prop 
#Probability of borrower defaulting is relatively higher (0.218 in the bracket of loan amount $30000 USD to $35000 

# x           y  Freq
# 1       [0,5000) Charged Off 0.138
# 2   [5000,10000) Charged Off 0.131
# 3  [10000,15000) Charged Off 0.134
# 4  [15000,20000) Charged Off 0.169
# 5  [20000,25000) Charged Off 0.170
# 6  [25000,30000) Charged Off 0.197
# 7  [30000,35000) Charged Off 0.218
# 8       [0,5000)  Fully Paid 0.862
# 9   [5000,10000)  Fully Paid 0.869
# 10 [10000,15000)  Fully Paid 0.866
# 11 [15000,20000)  Fully Paid 0.831
# 12 [20000,25000)  Fully Paid 0.830
# 13 [25000,30000)  Fully Paid 0.803
# 14 [30000,35000)  Fully Paid 0.782

#Plotting frequency distribution for above table
ggplot(loan_amnt_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + labs(title="loan_amnt vs loan status",x="loan amount categories",y="freq")

#Viewing whats the loan amount for different purpose

loan1$purpose <- as.factor(loan1$purpose)
loan_bypurpose <- loan1 %>% group_by(purpose,loan_status) %>% summarise(average_loan=mean(loan_amnt)) %>% arrange(desc(average_loan))

ggplot(loan_bypurpose,aes(x=purpose,y=average_loan,fill=loan_status))+geom_bar(stat="identity",position="dodge") +labs(title="Average loan amount for different purpose",x="purpose",y="average loan")

#Small Business which has average loan amount of around $14588 has higher charge-off's

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#3.Funded amount analysis with respect to loan status

summary(loan1$funded_amnt)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 500    5020    9400   10700   15000   35000 

# % of frequency distribution by funded amount category and loan status
funded_amnt_prop <-  data.frame(prop_func(loan1$funded_amnt_cat,loan1$loan_status))#calling the prop_func
funded_amnt_prop 
#Probability  of borrower defaulting is relatively higher (0.197) in the bracket of funded amount $30000 USD to $35000 

# x           y  Freq
# 1       [0,5000) Charged Off 0.139
# 2   [5000,10000) Charged Off 0.131
# 3  [10000,15000) Charged Off 0.136
# 4  [15000,20000) Charged Off 0.174
# 5  [20000,25000) Charged Off 0.175
# 6  [25000,30000) Charged Off 0.192
# 7  [30000,35000) Charged Off 0.197
# 8       [0,5000)  Fully Paid 0.861
# 9   [5000,10000)  Fully Paid 0.869
# 10 [10000,15000)  Fully Paid 0.864
# 11 [15000,20000)  Fully Paid 0.826
# 12 [20000,25000)  Fully Paid 0.825
# 13 [25000,30000)  Fully Paid 0.808
#Plotting frequency distribution for above table
funded_amnt_prop_plot <- ggplot(funded_amnt_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + labs(title="funded_amnt vs loan status",x="funded amount categories",y="freq")
funded_amnt_prop_plot 

#--------------------------------------------------------------------------------------------------------------------------------

#4. Grade and Subgrade analysis with respect to loan status

summary(loan1$grade)
# A     B     C     D     E     F     G 
# 9956 11536  7728  5007  2599   944   287 

summary(loan1$sub_grade)
# A1   A2   A3   A4   A5   B1   B2   B3   B4   B5   C1   C2   C3   C4   C5   D1   D2   D3   D4   D5   E1   E2   E3 
# 1131 1498 1794 2850 2683 1779 1974 2780 2415 2588 2027 1910 1464 1194 1133  918 1267 1096  903  823  710  601  503 
# E4   E5   F1   F2   F3   F4   F5   G1   G2   G3   G4   G5 
# 417  368  295  229  167  146  107   92   73   43   51   28 

# % of frequency distribution by grade and loan status
grade_prop <-  data.frame(prop_func(loan1$grade,loan1$loan_status))#calling the prop_func
grade_prop
#Probability of borrower defaulting increases as the grade lowers, the grade G shows the highest probability of default

# % of frequency distribution by sub-grade and loan status
sub_grade_prop <-  data.frame(prop_func(loan1$sub_grade,loan1$loan_status))#calling the prop_func
sub_grade_prop
#Sub Grade F5 has 50% chances of a borrower defaulting

#Plotting frequency distribution for the grade_prop
grade_prop_plot <- ggplot(grade_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + labs(title="Loan grade vs loan status",x="Grade",y="freq")
grade_prop_plot   #Shows a pattern for increase in freq of charged off's for lower grades

#Plotting frequency distribution for the sub_grade_prop
subgrade_prop_plot <- ggplot(sub_grade_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") +geom_line(aes(x=as.numeric(x),y=Freq),colour="red", linetype="solid", size=1) +geom_point(colour="red", size=4, shape=21, fill="green")+labs(title="Loan Sub-grade vs loan status",x="Sub-Grade",y="freq")
subgrade_prop_plot   #Subgrade F5 shows a 50% probability of borrower defaulting


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#5.  Interest rate versus loan status

summary(loan1$int_rate)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.42    8.94   11.70   11.90   14.40   24.40 

# % of frequency distribution by interest rate and loan status
intrate_prop <-  data.frame(prop_func(loan1$int_rate_cat,loan1$loan_status))#calling the prop_func
intrate_prop # Loans with interest rate in the bracket of 20% to 25% have highest freq of default

#Plotting frequency distribution for the intrate_prop
intrate_prop_plot <- ggplot(intrate_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + geom_line(aes(x=as.numeric(x),y=Freq),colour="red", linetype="solid", size=1)+labs(title="Interest rate vs loan status",x="Interest rate",y="freq")
intrate_prop_plot

# plotting relationship between the  interest rate and loan amount
int_loan_plot <- ggplot(loan1,aes(x=loan_amnt,y=int_rate,fill=loan_status))+geom_smooth(colour="red", linetype="solid", size=1)+labs(title="Interest rate vs loan amount",x="Loan amount",y="interest rate")
int_loan_plot
#Interest rates increases as the loan amount increases, higher interest rates have higher number of charge off's


#Plotting relationship between interest rate and grade

int_grade_plot <- ggplot(loan1,aes(x=grade,y=int_rate,fill=loan_status))+geom_boxplot()+labs(title="Interest rate vs grade",x="Grade",y="interest rate")
int_grade_plot #Higher interest rates are assigned to lower grades


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Pearson correlation between all the continous variables for the loan characteristics

loan_numeric_col <- c("loan_amnt","funded_amnt","funded_amnt_inv","int_rate","installment")
loan_corr_df <- loan1[,loan_numeric_col] #subsetting loan dataframe for all the numeric variables of Loan characteristics
loan_corr <- cor(loan_corr_df,use = "pairwise.complete.obs")
loan_corrplot <- corrplot.mixed(loan_corr)
#Numbers are indicative of the correlation


#---------------------------------------------------------------------------------------------------------------------------------------------------------------


###########################################################################################################################

# Assessment of the borrower Characteristics

##########################################################################################################################
#From the chi statistic on the borrower variables, we have deduced , we will be drilling down on the following variables

## home ownership,dti,emp_length,verification_status,addr_state.Lets look into each one of them
#
#---------------------------------------------------------------------------------------------
#  1) home ownership: The impact of home ownership( 1:mortgage or 2:rent or 3:own)

#homeown_prop <- prop_func(loan1$homeown_risk,loan1$loan_status) #calling the prop_func

homeown_prop <- prop_func(loan1$home_ownership,loan1$loan_status)
homeown_prop    #


    #          Charged Off Fully Paid
# mortgage        0.14       0.86
# none            0.00       1.00
# other           0.19       0.81
# own             0.15       0.85
# rent            0.15       0.85

homeown_prop <- data.frame(homeown_prop)
#Column chart to show the proportion of spread among home ownership and loan status
  ggplot(homeown_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge")+labs(title="Home Ownership vs loan status",x="term",y="freq")

#Borrowers with  home ownership status as "other" have a higher default rate*

homeown_int_prop <- data.frame(prop_func(loan1$home_ownership,loan1$int_rate_cat))
homeown_int_prop
# y
# x          [0,5) [10,15) [15,20) [20,25)
# mortgage  0.00    0.69    0.28    0.03
# none                                  
# other     0.00    0.74    0.26    0.00
# own       0.00    0.71    0.26    0.03
# rent      0.00    0.71    0.27    0.02

#Plotting home ownership versus Interest
ggplot(homeown_int_prop,aes(x=as.factor(x),y=Freq,fill=y))+geom_bar(stat="identity",position="dodge")+labs(title="Home Ownership vs Interest Rate",x="term",y="freq")
#Borrowers with "other" as the home ownership status have the highest occurances of interest rates in the bracket of (10%,15%)

purpose_prop <- prop_func(loan1$home_ownership,loan1$grade)
purpose_prop   #People who are on mortgage have silghtly higher chances of following into grade A

# x             A    C    D    E    F    G
  # mortgage 0.43 0.26 0.16 0.10 0.04 0.01
  # none     1.00 0.00 0.00 0.00 0.00 0.00
  # other    0.38 0.25 0.23 0.11 0.03 0.00
  # own      0.42 0.28 0.17 0.09 0.03 0.01             
  # rent     0.32 0.32 0.22 0.10 0.03 0.01
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
#  2) dti_cat: The impact of dti

dticat_prop <- prop_func(loan1$dti_cat,loan1$loan_status) #calling the prop_func
dticat_prop    #Shows the probabilty of a borrower defaulting is high(0.17) for borrowers with high dti(20,25)


# x         Charged Off Fully Paid
# [0,5)         0.124      0.876
# [5,10)        0.128      0.872
# [10,15)       0.146      0.854
# [15,20)       0.158      0.842
# [20,25)       0.170      0.830                 
# [25,30)       0.140      0.860

dticat_prop <- data.frame(dticat_prop)
#Column chart to show the proportion of spread among home ownership and loan status
ggplot(dticat_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge")+labs(title="DTI vs loan status",x="term",y="freq")



#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
#  3) emp_length: The impact of employment length

emplen_prop <- prop_func(loan1$emp_length,loan1$loan_status) #calling the prop_func
emplen_prop    #Shows the probabilty of a borrower defaulting is high(0.16) if they have been employed for longer than 10 years. Note that we'll ignore the employees of length NA. 
#

#              Charged Off Fully Paid
#  < 1 year         0.14       0.86
#  1 year           0.14       0.86
# 10+ years         0.16       0.84
#  2 years          0.13       0.87
#  3 years          0.14       0.86
#  4 years          0.14       0.86
#  5 years          0.14       0.86
#  6 years          0.14       0.86
#  7 years          0.15       0.85
#  8 years          0.14       0.86
#  9 years          0.13       0.87
#  n/a              0.22       0.78

emplen_prop <- data.frame(emplen_prop)
#Column chart to show the proportion of spread among home ownership and loan status
ggplot(emplen_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge")+labs(title="Employment Length vs loan status",x="term",y="freq")
#One reason for borrowers employed longer defaulting more could be because they are taking larger loan amounts

#Further plotting the loan amount versus the employee length
emp_loan <- subset(loan1,select =c("loan_status","loan_amnt","emp_length"))
emp_avg_loan <- emp_loan %>% group_by(emp_length) %>% summarise(avg_loan = round(mean(loan_amnt),2)) %>% arrange(desc(avg_loan))
emp_avg_loan
ggplot(emp_avg_loan,aes(x = emp_length,y=avg_loan))+ geom_boxplot(col="red")+labs(title="Loan amount versus employee length")

# #   emp_length  avg_loan
       # <fctr>     <dbl>
# 1   10+ years 12656
# 2     9 years 11852
# 3     8 years 11546
# 4     7 years 11483
# 5     6 years 11319
# 6     5 years 11033
# 7     4 years 10774
# 8     3 years 10502
# 9     2 years  9974
# 10     1 year  9964
# 11   < 1 year  9495
# 12        n/a  8413

#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
#  4) verification_status: The impact of verification status

verifystat_prop <- prop_func(loan1$verification_status,loan1$loan_status) #calling the prop_func
verifystat_prop    #Shows the probabilty of a borrower defaulting is high(0.17) when the borrower's income has been verified. 


#        				Charged Off 	Fully Paid
#  Not Verified           0.13       	0.87
#  Source Verified        0.15       	0.85
#  Verified               0.17       	0.83

verifystat_prop <- data.frame(verifystat_prop)

#Column chart to show the proportion of spread among home ownership and loan status
ggplot(verifystat_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge")+labs(title="Verification Status vs loan status",x="term",y="freq")

#It's a common practice to verify income if the borrower shows a higher risk profile

chrgdoff_loans <- filter(loan1,loan_status == "Charged Off")
verif_prop <- prop_func(chrgdoff_loans$grade,chrgdoff_loans$verification_status)
verif_prop

# Grade  Not Verified Verified
# A         0.68     0.32
# B         0.58     0.42
# C         0.57     0.43
# D         0.50     0.50
# E         0.29     0.71
# F         0.20     0.80
# G         0.23     0.77

#More number of accounts of poorer grades are verified when compared to higher grades. % is higher from grade C and above

verif_prop <- prop_func(chrgdoff_loans$dti_cat,chrgdoff_loans$verification_status)
verif_prop

# DTI         Not Verified Verified
# [0,10)          0.57     0.43
# [10,20)         0.51     0.49
# [20,30)         0.45     0.55
  

#More number of accounts with higher dti tend to get verified 0.531 vs. 0.321 and 0.412

#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------

#  5) annual_inc: The impact of annual income

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4000   40000   58000   64800   80000  208000 

annualinc_prop <- data.frame(prop_func(loan1$annual_inc_cat,loan1$loan_status)) #calling the prop_func
annualinc_prop    #Shows the probabilty of a borrower defaulting is high 0.215 for lower income range of 0 to $20000

# x                 Charged Off Fully Paid
# [0,20000)             0.215      0.785
# [20000,40000)         0.176      0.824
# [40000,60000)         0.153      0.847
# [60000,80000)         0.141      0.859
# [80000,100000)        0.122      0.878
# [100000,120000)       0.102      0.898
# [120000,140000)       0.115      0.885
# [140000,160000)       0.110      0.890
# [160000,180000)       0.099      0.901
# [180000,200000)       0.136      0.864

#plotting the annual income frequency table

ggplot(annualinc_prop,aes(x=x,y=Freq,fill=y))+geom_col(position="dodge")+geom_line(aes(x=as.numeric(x),y=Freq),colour="red", linetype="solid", size=1)+labs(title="Annual income versus loan status")
# The risk of default is distributed towards the end of 2 spectrum of annual inclome, i.e for lower come bracket of 
# $0 to $20k and for higher income brackets of $180k to $200K


ggplot(loan1,aes(x=grade,y=annual_inc))+geom_col()+labs(title="Grade versus annual income")
#lower incomes are assigned lower grades

ggplot(loan1,aes(x=annual_inc,y=loan_amnt))+geom_smooth()+labs(title="Annual income versus loan amount")

#People with higher income borrow larger amount of loan.

#---------------------------------------------------------------------------------------------------

#As majority of variables under the borrower characteristics are categorical in nature, we are not 

#computing the pearson correlation for this segment.


#-----------------------------------------------------------------------------------------------------

#########################################################################################3

#Assessment of Creditline characteristics

##From the chi-statistics test on creditline characteristics we will be analysing following attributes
##1.Total account 2.revol_bal 3.total_rec_int 4.open_acc 5.inq_last_6mnths Let us start with 
##total_acc
#
#---------------------------------------------------------------------------------------------
# 1) toatal_acc
summary(loan1$total_acc)
###summary of total_acc
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00   13.00   20.00   21.94   29.00   90.00
##To ease the analysis we have categorised total_acc into three parts (0,30);(30,60);(60,90)
total_acc_prop <- data.frame(prop_func(loan1$total_acc_cat,loan1$loan_status))##calling prop_func for total_acc_cat
total_acc_prop ## to view result

# x         Charged Off Fully Paid
# [0,15)        0.161      0.839
# [15,30)       0.139      0.861
# [30,45)       0.147      0.853
# [45,60)       0.125      0.875
# [60,75)       0.154      0.846
# [75,90)       0.000      1.000
total_acc_plot <- ggplot(total_acc_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + labs(title="Total A/C vs loan status",x="Total Creditlines",y="freq")
total_acc_plot

##Insights:Default rate over total_acc_cat is marginally higher for the total number of accounts in (0,15) and (60,75) bracket


#----------------------------------------------------------------------------------------------------------------------------------
#2) revol_bal
summary(loan1$revol_bal)
##Summary is below

# Min. 1st Qu.  Median Mean  3rd Qu.    Max. 
# 0    3630    8680   13000   16700     150000 

##To ease the analysis we have divided revol_acc into five category under new attribute called
##"revol_acc_cat"
summary(loan1$revol_bal_cat)
# [0,29000)   [29000,58000)   [58000,87000)  [87000,116000) [116000,145000) NA's 
#   34441            2776             557             203              70   10 

#Freq distribution of revol balance versus the loan status
revol_bal_cat_prop <- data.frame(prop_func(loan1$revol_bal_cat,loan1$loan_status))
revol_bal_cat_prop

##plotting frequency distribution of revol_bal_cat against loan status
revol_bal_cat_plot <- ggplot(revol_bal_cat_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + labs(title="Revolving Bal vs loan status",x="Revol balance",y="freq")
revol_bal_cat_plot
##Insights:revol_bal in range of ($29000,$58000) and ($87000,$116000) shows highest default rate

#Plotting revol balance and total received interest
ggplot(loan1,aes(x=revol_bal,y=total_rec_int))+geom_smooth()+labs(title="revol balance versus total received interest")
#There is a rise in the received interest for the revol balance from 0 to $25000
##------------------------------------------------------------------------------------------------------------------------------
#3) total_rec_int
summary(loan1$total_rec_int)
##to see summary of total_rec_int

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0     640    1290    2090    2640   23600 

##To ease the analysis total_rec_int has been categorised under five categories as new attribute "total_rec_int_cat"
summary(loan1$total_rec_int_cat)
#[0,4000)   [4000,8000)  [8000,12000) [12000,16000) [16000,20000) 
#32713          4080           917           257            77 
total_rec_int_prop <- data.frame(prop_func(loan1$total_rec_int_cat,loan1$loan_status))
total_rec_int_prop

###plotting frequency distribution of total_rec_int
total_rec_int_plot <- ggplot(total_rec_int_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + labs(title="Total received interest vs loan status",x="Received interest",y="freq")
total_rec_int_plot
# Charged off ration is high for Interest received till date  in the bracket of 16000,20000$

#Plotting grade and total received interest
ggplot(loan1,aes(x=loan1$grade,y=total_rec_int,fill=loan_status))+geom_col(position="dodge")+labs(title="Grade versus loan status")
#Higher the received interest, lower is the grade assigned and higher is instances of default

#--------------------------------------------------------------------------------------
#4)open_acc
summary(loan1$open_acc)
#summary is as below
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.000   6.000   9.000   9.248  12.000  44.000
#to ease analysis open_acc has been divided into eight categoties under name open_acc_cat as
summary(loan1$open_acc_cat)
#[0,4)   [4,8)  [8,12) [12,16) [16,20) [20,24) [24,28) [28,32) [32,36) [36,40) 
#2052   13062   13010    6489    2459     733     173      57      15       4 
#[40,44)    
#2       
open_acc_cat_prop <- data.frame(prop_func(loan1$open_acc_cat,loan1$loan_status))
open_acc_cat_prop
##plotting frequency distrubution of open_acc_cat against loan status
open_acc_cat_plot <-  ggplot(open_acc_cat_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + labs(title="Open A/C Cat vs loan status",x="Open accounts",y="freq")
open_acc_cat_plot

#Chances of default is higher if the open credit lines is in the bracket of 32 to 36

#Plotting open account and int rate
ggplot(loan1,aes(x=loan1$open_acc,y=loan1$int_rate))+geom_smooth()
# The interest rate starts increasing for the open accounts from 20 to 30 and it dips down after 30


##------------------------##------------------------------------###------------------------------------------
## 5)inq_last_6mths
summary(loan1$inq_last_6mths)
##summary is as below
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  1.0000  0.8685  1.0000  8.0000 
#summary of respective categorical variable
summary(loan1$inq_last_6mths_cat)
##[0,2) [2,4) [4,6) [6,8)  NA's 
##29011  8492   447    95    12
inq_last_6mths_cat_prop <- data.frame(prop_func(loan1$inq_last_6mths_cat,loan1$loan_status))
inq_last_6mths_cat_prop
inq_last_6mnths_cat_plot <-  ggplot(inq_last_6mths_cat_prop,aes(x=x,y=Freq,fill=y))+geom_bar(stat="identity",position="dodge") + labs(title="Inquiry in last 6mnths vs loan status",x="Number of inquiries",y="freq")
inq_last_6mnths_cat_plot
##Insights: Default rate is higher in higher inquiry category.

#Plotting open accounts versus inq_last_6_months
ggplot(loan1,aes(x=loan1$open_acc,y=loan1$inq_last_6mths))+geom_smooth()
#Variation in the number of open accounts versus the inquries in last 6 months


#-------------------------------------------------------------------------------
# Pearson correlation between all the continous variables for the loan characteristics

creditline_numeric_col <- c("inq_last_6mths" ,"open_acc","revol_bal" ,"revol_util","total_acc","total_pymnt_inv" ,"total_rec_prncp" ,"total_rec_int","last_pymnt_amnt")
creditline_corr_df <- loan1[,creditline_numeric_col] #subsetting loan dataframe for all the numeric variables of credit line  characteristics
creditline_corr <- cor(creditline_corr_df,use = "pairwise.complete.obs")
creditline_corrplot <- corrplot.mixed(creditline_corr)
#Numbers are indicative of the correlation

#################################### OUR ANALYSIS ENDS HERE #####################################################
