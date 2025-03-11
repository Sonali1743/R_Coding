
#Reading and merging data files

personnel_data = read.table("~/Documents/GitHub/regular-expressions-r-tha-Sonali1743/data/CaliforniaHospitalData_Personnel.txt", header = TRUE, sep = "\t")
personnel_data
hospital_data = read.csv("~/Documents/GitHub/regular-expressions-r-tha-Sonali1743/data/CaliforniaHospitalData.csv")
hospital_data
merged_data = merge(personnel_data, hospital_data, by = "HospitalID")
merged_data

#1. Obtain a list of all employees who have the title Safety Inspection Member

list1 = merged_data[grepl("Safety Inspection Member", merged_data$PositionTitle),]
list1

#2. All employees with the first name Emily

list2 = merged_data[grepl("Emily", merged_data$FirstName),]
list2

#3. All personnel who started work after 2011 (inclusive) and work for a non-profit

list3 = merged_data[grepl("Non Profit", merged_data$TypeControl) & grepl("1/1/20([2-9][0-9]|1[1-9])",merged_data$StartDate),]
list3

#4. Same as #3, except exclude all individuals with the title Safety Inspection Member

list4 = merged_data[grepl("Non Profit", merged_data$TypeControl) & grepl("1/1/20([2-9][0-9]|1[1-9])",merged_data$StartDate) & !(grepl("Safety Inspection Member",merged_data$PositionTitle)),]
list4

#5. All personnel who work for an academic institution
#Answer: I have included personnel with "Teaching" (in "Teaching" column) since after looking at their respective hospital websites I inferred them to be academic institution. Also, I have not included all the personnal with .edu in their Email for this since some of their hospital didn't seem like the academic institutes.

list5 = merged_data[grepl("Teaching", merged_data$Teaching),]
list5
