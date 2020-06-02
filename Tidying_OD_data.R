#Load packages
library(readxl)
library(tidyxl)
library(tidyverse)

#Upload untidy data
Untidy_Sample_Data <- read_excel("Untidy_Sample_Data.xlsx") #Raw data output

x<- xlsx_cells('Untidy_Sample_Data.xlsx') #tidyxl::xlsx_cells used to read in data

datafile<- 'Untidy_Sample_Data.xlsx' #The data path to be used for the function below. 

#A function to read in data and turn into a tidy form of data
data.extractor<- function(datapath, #System file path for the excel sheet
                          Timepoints, #Numeric of the timepoints anaylzed
                          Wavelengths){ #Numeric vector of the tested wavelengths

  raw.data<- tidyxl::xlsx_cells(datapath, sheets = 1) #Data uploaded as one row for each cell
  Stored.Data.Rows<- list() #Empty storage list to hold values
  data.rows.fx<- for(x in length(Wavelengths)){ #loop to determine the row positions that contain the data
    rows<- seq(2+(2*x), #Starting values
               Timepoints*((2*length(Wavelengths)+3)), #Ending values
               2*length(Wavelengths)+3) #Incremental values
    Stored.Data.Rows[x]<- list(rows) #Store values in list
  }
  data.rows<- unlist(Stored.Data.Rows) #Unlist values to vector
  
  subset.data<- raw.data[raw.data$row %in% data.rows, c("numeric")] #Subset by the rows that contain data that are numeric
  df.data<- as.data.frame( #As data frame
    matrix( #As matrix
      pull(subset.data), nrow = 97 #vector of subset data 
    )
  )
  df.data<- df.data[-c(1),] #Remove the first row, Not needed.
  
  colnames(df.data)<- c(rep(c(0:(Timepoints-1)), each = length(Wavelengths))) #Rename columns by timepoint
  
  Wavelength.list<-list() #Storage list
  for (y in 1:length(Wavelengths)) { #Loop to pick the columns that are assigned to each row
    col<- df.data[,c(seq(y, Timepoints*length(Wavelengths), length(Wavelengths)))]
    Wavelength.list[y]<- list(col) #Add specific columns to storage list to break up by wavelength
  }
  names(Wavelength.list)<- Wavelengths #Rename list elements by the name of the Wavelengths tested
  
  Coordinates_letters<- LETTERS[1:8] #Coordinates to be used
  Coordinates_numbers<- 1:12
  Coordinates<- expand.grid(Coordinates_letters, Coordinates_numbers)
  Coordinates$paste<- paste(Coordinates$Var1,Coordinates$Var2, sep = "")
  Coordinates<- Coordinates[order(Coordinates$Var1),]

  sort.fx<- function(df){ #function to pass into `map` to sort data by timpoint
    df[order(df$Timepoint),]
  }
  
  Wavelength.list<- Wavelength.list%>%
    map(tidyr::pivot_longer, cols= c(1:Timepoints), names_to = "Timepoint", values_to = "Absorbance")%>% #Convert to tidy long form
    map(sort.fx)%>% #sort by timepoint using the function written
    map(dplyr::mutate, Coordinate = rep(Coordinates$paste, times = Timepoints)) #Add well coordinates

  return(Wavelength.list) #Return the list of the Wavelengths tested. 
}