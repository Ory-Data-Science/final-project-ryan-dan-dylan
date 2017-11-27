# Load library
library(tidyverse)
library("dplyr")
library(stringr)
library(lubridate)
library(plyr)
library(scales)
library(ggplot2)


acquisition_data <- read_csv('NADAC__National_Average_Drug_Acquisition_Cost_.csv') # Important to note that this data is from 2013 - 2017.

print(acquisition_data) # Determining the drugs that are being analyzed.



Adderall_data <- acquisition_data %>% # This data is going to separate Adderall from the other drugs to work with
  select(NDC_Description , Effective_Date, NADAC_Per_Unit, NDC)  %>%  #Select is used to choose the columns you want to work with
  filter(str_detect(NDC_Description, "ADDERALL|AMOXICILLIN|ALBUTEROL|PENICILLIN"))  # You can add several drugs in this str_detect with this | symbol



New_NDC <- str_pad(Adderall_data$NDC, 11 , pad = "0") #Makes all the strings 11 words long, not all manufaturer codes are 5 digits long

Manufacturer_Code <- substr(New_NDC, 0, 5) # Manufacturer's Code first 5 digits
Product_Code <- substr(New_NDC, 6, 9) # Product Code, middle 4 digits
Packaging_Code <- substr(New_NDC, 10, 11) # Packaging Code, each drug has an individual packaging code


Working_data <- cbind(Adderall_data , New_NDC , Manufacturer_Code, Product_Code, Packaging_Code) # This will be used for the user input process

Adderall_data <- cbind(Adderall_data , New_NDC , Manufacturer_Code, Product_Code, Packaging_Code) # Combines the two lines of code together.


print(Adderall_data)

#Albuterol_data_2.5 <- Adderall_data[Manufacturer_Code=="76204" & Product_Code=="0200" & Packaging_Code=="60"] # This line selects the exact Manufacturer_Code and the Product_Code 


Albuterol_data_2.5 <- Adderall_data %>%                                        # Defined array for one particular drug.
   filter(Manufacturer_Code == "76204", Product_Code == "0200", Packaging_Code == "60")  # Filters the columns based on this 

Adderall <- Adderall_data %>%
  filter(Manufacturer_Code == "57844", Product_Code == "0110", Packaging_Code == "01")


# Albuterol_data_2.5[order(as.Date(Albuterol_data_2.5$Effective_Date , format="%d/%m/%Y")),,drop=FALSE]
  
Albuterol_data_2.5$Effective_Date <- mdy(Albuterol_data_2.5$Effective_Date)
Albuterol_data_2.5 <- arrange(Albuterol_data_2.5 , (Effective_Date))

Adderall$Effective_Date <- mdy(Adderall$Effective_Date)
Adderall <- arrange(Adderall , (Effective_Date))

#Albuterol_data_2.5 <- Albuterol_data_2.5[order(as.Date(Albuterol_data_2.5$Effective_Date,format="%d/%m/%Y")),,drop=FALSE]
#Albuterol_data_2.5[order(as.Date(Albuterol_data_2.5$Effective_Date, format="%d/%m/%Y")),, drop=FALSE]

print(Albuterol_data_2.5) 

print(Adderall)

ggplot(Albuterol_data_2.5, aes(Effective_Date, NADAC_Per_Unit, size = 2)) + geom_line(color = "blue") +
    scale_x_date(date_labels = "%Y-%m", expand = c(0,10), breaks = date_breaks("3 months")) +
    xlab("Effective Date (Year-Month)") + ylab("NADAC Per Unit (Dollars per mL)") + labs(title = "Albuterol 2.5 mg/mL")


ggplot(Adderall, aes(Effective_Date, NADAC_Per_Unit, size = 2)) + geom_line(color = "red") +
  scale_x_date(date_labels = "%Y-%m", expand = c(0,10), breaks = date_breaks("3 months")) +
  xlab("Effective Date (Year-Month)") + ylab("NADAC Per Unit (Dollars Each)") + labs(title = "Adderall")

# User input code
Medicaidinput <- function()
{ 
  print("Hello, this program allows you to search drugs in this database using NDC numbers")
  m <- readline(prompt="Please enter the manufacturing code: ")
  toString(m)
  p <- readline(prompt="Please enter Product code: ")
  pa <- readline(prompt="Please enter Packaging code: ")
  
  m <- as.numeric(m)
  m <- sprintf('%05d', m)
  
  p <- as.numeric(p)
  p <- sprintf('%04d', p)
  
  print(paste("The NDC code that you have inputed is", m, "-", p, "-" , pa))
  
  if (m %in% Working_data$Manufacturer_Code && p %in% Working_data$Product_Code && pa %in% Working_data$Packaging_Code){
      print("Alright, we have found the drug that you were searching for.") # find out what %in% is.
    
      UserNDC <- Working_data %>%
      filter(Manufacturer_Code == m, Product_Code == p, Packaging_Code == pa)
      
      UserNDC$Effective_Date <- mdy(UserNDC$Effective_Date)
      UserNDC <- arrange(UserNDC , (Effective_Date))
      usertitle <- unique(UserNDC$NDC_Description) # Unique identifies unique variables in vector, usertitle would not work if there 
                                                   # More than one unique value.
      
      View(UserNDC)
      
      
      graph <- ggplot(UserNDC, aes(Effective_Date, NADAC_Per_Unit, size = 2)) + geom_line(color = "yellow") +
        scale_x_date(date_labels = "%Y-%m", expand = c(0,10), breaks = date_breaks("3 months")) +
        xlab("Effective Date (Year-Month)") + ylab("NADAC Per Unit (Dollars per mL)") + labs(title = usertitle)
    
      print(graph)
    
    } else {
      print("Unfortunately, we could not find what you are looking for.")
  }
  
  
  return(m)
}

print(Medicaidinput())




# ggplot(Albuterol_data_2.5, aes(Effective_Date, NADAC_Per_Unit)) + geom_line() +
#  scale_x_date(format = "%b-%Y") + xlab("") + ylab("NADAC Per Unit")



#///////////////////////////////////////////////////////////////////////THINGS TO DO//////////////////////////////////////////////////////////////////////////////////
# Now in order to plot this, we need to organize by date. The date itself needs to be parsed in order to organized.
# It may be possible to organize the  dates via a function or loop. We should be able to generate our first plot by monday.
# This is the particular drug I chose 76204-0200-60 (Albuterol SUl 2.5 MG/3ML SOLN)

#-----------------------------------------------------------------------PLOTING CODE----------------------------------------------------------------------------------
# NDC_Description == "ADDERALL 30 MG TABLET will only find exactly that string, there would about 40 results.
# THe new line in filter is used to identify words with just adderall itself. This line guarrantees that you can more than one result.

# ggplot(data = acquisition_data, aes(x = NADAC_Per_Unit)) + # says the data used is acquisition_data, and NADAC_Per_Unit is the variable we are going to use.
#  labs(x = "Year" , y = "NADAC per Unit") +
#  geom_histogram() # makes a histogram. 

#ggplot(data=acquisition_data, aes(x = Effective_Date, y = NADAC_Per_Unit)) +
#   geom_line()+
#  geom_point()


# The current problem about this histogram us that some drugs have a very small NADAC, others have a huge NADAV values. A different graph should be used.
# In addition to this, lets try plotting a line graph for this.
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------EXPERIMENTAL CODE FROM EARLIER--------------------------------------------------------------
# New_NDC <- sprintf("%012d",Adderall_data$NDC)
# Sorted_NDC <- extract(Adderall_data, New_NDC, into = c("Product.Code", "Labeler.Code", "Packaging.Code"), regex = "(.{5})(.{4})(.{2})", remove = FALSE)
# print(Sorted_NDC)


# NDC(Adderall_data) <- stringr::str_pad(NDC(Adderall_data), 11, side = "left", pad = 0)


#  separate(NDC, into = c("NDC_1") , sep = c(-0:-3) , remove = FALSE) 

# sprintf to add new numbers on it.

# Albuterol_data_2.5 %>%                                        #(Alternative way to filter - the way you are doing is in base R)
#   filter(Manufacturer_Code == 76204, Product_Code == 0200) 

#Albuterol_data_2.5 <- Adderall_data[Manufacturer_Code=="76204" & Product_Code=="0200" & Packaging_Code=="60"] # This line selects the exact Manufacturer_Code and the Product_Code 

#------------------------------------------------------------------------------------------------------------------------------------------------------------------