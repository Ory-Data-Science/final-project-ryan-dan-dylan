# Load library
library(tidyverse)
library("dplyr")
library(stringr)


acquisition_data <- read_csv('NADAC__National_Average_Drug_Acquisition_Cost_.csv') # Important to note that this data is from 2013 - 2017.

print(acquisition_data) # Determining the drugs that are being analyzed.

Adderall_data <- acquisition_data %>% # This data is going to separate Adderall from the other drugs to work with
  select(NDC_Description , Effective_Date, NADAC_Per_Unit)  %>%  #Select is used to choose the columns you want to work with
  filter(str_detect(NDC_Description, "ADDERALL|AMOXICILLIN|ALBUTEROL")) # You can add several drugs in this str_detect with this | symbol


# NDC_Description == "ADDERALL 30 MG TABLET will only find exactly that string, there would about 40 results.
# THe new line in filter is used to identify words with just adderall itself. This line guarrantees that you can more than one result.

print(Adderall_data)

ggplot(data = acquisition_data, aes(x = NADAC_Per_Unit)) + # says the data used is acquisition_data, and NADAC_Per_Unit is the variable we are going to use.
  labs(x = "Year" , y = "NADAC per Unit") +
  geom_histogram() # makes a histogram. 

ggplot(data=acquisition_data, aes(x = Effective_Date, y = NADAC_Per_Unit)) +
  geom_line()+
  geom_point()


# The current problem about this histogram us that some drugs have a very small NADAC, others have a huge NADAV values. A different graph should be used.
# In addition to this, lets try plotting a line graph for this.