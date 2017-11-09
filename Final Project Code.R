# Load library
library(tidyverse)
library("dplyr")
library(stringr)


acquisition_data <- read_csv('NADAC__National_Average_Drug_Acquisition_Cost_.csv') # Important to note that this data is from 2013 - 2017.

print(acquisition_data) # Determining the drugs that are being analyzed.

Adderall_data <- acquisition_data %>% # This data is going to separate Adderall from the other drugs to work with
  select(NDC_Description , Effective_Date, NADAC_Per_Unit, NDC)  %>%  #Select is used to choose the columns you want to work with
  filter(str_detect(NDC_Description, "ADDERALL|AMOXICILLIN|ALBUTEROL"))  # You can add several drugs in this str_detect with this | symbol
  

New_NDC <- str_pad(Adderall_data$NDC, 11 , pad = "0") #Makes all the strings 11 words long

Manufacturer_Code <- substr(New_NDC, 0, 5)
Product_Code <- substr(New_NDC, 6, 9)
Packaging_Code <- substr(New_NDC, 10, 11)

cbind(Adderall_data , New_NDC , Manufacturer_Code, Product_Code, Packaging_Code)



print(Adderall_data)


# New_NDC <- sprintf("%012d",Adderall_data$NDC)
# Sorted_NDC <- extract(Adderall_data, New_NDC, into = c("Product.Code", "Labeler.Code", "Packaging.Code"), regex = "(.{5})(.{4})(.{2})", remove = FALSE)

print(Sorted_NDC)


# NDC(Adderall_data) <- stringr::str_pad(NDC(Adderall_data), 11, side = "left", pad = 0)


#  separate(NDC, into = c("NDC_1") , sep = c(-0:-3) , remove = FALSE) 

# sprintf to add new numbers on it.

# print(Adderall_data)
# separate(Adderall_data, NDC_1, into = c("NDC_1") , sep = c(-0 , -5) , remove = FALSE)

# NDC_Description == "ADDERALL 30 MG TABLET will only find exactly that string, there would about 40 results.
# THe new line in filter is used to identify words with just adderall itself. This line guarrantees that you can more than one result.

print(Adderall_data)

# ggplot(data = acquisition_data, aes(x = NADAC_Per_Unit)) + # says the data used is acquisition_data, and NADAC_Per_Unit is the variable we are going to use.
#  labs(x = "Year" , y = "NADAC per Unit") +
#  geom_histogram() # makes a histogram. 

#ggplot(data=acquisition_data, aes(x = Effective_Date, y = NADAC_Per_Unit)) +
#   geom_line()+
#  geom_point()


# The current problem about this histogram us that some drugs have a very small NADAC, others have a huge NADAV values. A different graph should be used.
# In addition to this, lets try plotting a line graph for this.