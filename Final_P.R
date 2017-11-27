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

Adderall_data <- cbind(Adderall_data , New_NDC , Manufacturer_Code, Product_Code, Packaging_Code) # Combines the two lines of code together.


print(Adderall_data)

#Albuterol_data_2.5 <- Adderall_data[Manufacturer_Code=="76204" & Product_Code=="0200" & Packaging_Code=="60"] # This line selects the exact Manufacturer_Code and the Product_Code 


Albuterol_data_2.5 <- Adderall_data %>%                                        # Defined array for one particular drug.
  filter(Manufacturer_Code == "76204", Product_Code == "0200", Packaging_Code == "60")  # Filters the columns based on this 

Adderall <- Adderall_data %>%
  filter(Manufacturer_Code == "57844", Product_Code == "0120")

PenVK <- Adderall_data %>%
  filter(Manufacturer_Code == "00093", Product_Code == "1174")

Amox <- Adderall_data %>%
  filter(Manufacturer_Code == "00781", Product_Code == "2613")


# Albuterol_data_2.5[order(as.Date(Albuterol_data_2.5$Effective_Date , format="%d/%m/%Y")),,drop=FALSE]

Albuterol_data_2.5$Effective_Date <- mdy(Albuterol_data_2.5$Effective_Date)
Albuterol_data_2.5 <- arrange(Albuterol_data_2.5 , (Effective_Date))

Adderall$Effective_Date <- mdy(Adderall$Effective_Date)
Adderall <- arrange(Adderall , (Effective_Date))

PenVK$Effective_Date <- mdy(PenVK$Effective_Date)
PenVK <- arrange(PenVK , (Effective_Date))

Amox$Effective_Date <- mdy(Amox$Effective_Date)
Amox <- arrange(Amox , (Effective_Date))


#Albuterol_data_2.5 <- Albuterol_data_2.5[order(as.Date(Albuterol_data_2.5$Effective_Date,format="%d/%m/%Y")),,drop=FALSE]
#Albuterol_data_2.5[order(as.Date(Albuterol_data_2.5$Effective_Date, format="%d/%m/%Y")),, drop=FALSE]

print(Albuterol_data_2.5) 

print(Adderall)

print(PenVK)

print(Amox)

#These are the graphs you're looking for!

ggplot(Albuterol_data_2.5, aes(Effective_Date, NADAC_Per_Unit, size = 2)) + geom_line(color = "blue") +
  scale_x_date(date_labels = "%Y-%m", expand = c(0,10), breaks = date_breaks("3 months")) +
  xlab("Effective Date (Year-Month)") + ylab("NADAC Per Unit (Dollars per mL)") + 
  labs(title = "Drug Unit Prices", subtitle = "Albuterol 2.5 mg/mL")

ggplot(Adderall, aes(Effective_Date, NADAC_Per_Unit, size = 2)) + geom_line(color = "red") +
  scale_x_date(date_labels = "%Y-%m", expand = c(0,50), breaks = date_breaks("3 months")) +
  xlab("Effective Date (Year-Month)") + ylab("NADAC Per Unit (Dollars Each)") + 
  labs(title = "Drug Unit Prices", subtitle = "Adderall 20mg")

ggplot(PenVK, aes(Effective_Date, NADAC_Per_Unit, size = 2)) + geom_line(color = "orange") +
  scale_x_date(date_labels = "%Y-%m", expand = c(0,50), breaks = date_breaks("3 months")) +
  xlab("Effective Date (Year-Month)") + ylab("NADAC Per Unit (Dollars Each)") + 
  labs(title = "Drug Unit Prices", subtitle = "Penicillin VK 500mg")

ggplot(Amox, aes(Effective_Date, NADAC_Per_Unit, size = 2)) + geom_line(color = "green") +
  scale_x_date(date_labels = "%Y-%m", expand = c(0,50), breaks = date_breaks("3 months")) +
  xlab("Effective Date (Year-Month)") + ylab("NADAC Per Unit (Dollars Each)") + 
  labs(title = "Drug Unit Prices", subtitle = "Amoxicillin 500mg")

# ggplot(Albuterol_data_2.5, aes(Effective_Date, NADAC_Per_Unit)) + geom_line() +
#  scale_x_date(format = "%b-%Y") + xlab("") + ylab("NADAC Per Unit")

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

