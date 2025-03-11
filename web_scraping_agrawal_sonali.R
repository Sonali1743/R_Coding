#Assignment
#Create CSS selectors as well as XPath selectors for each item - Name, Title (assistant, associate, full, etc), Office address, Phone number and Email address
css_name = "div.panel-group.accordion>div>div.panel-heading>h4>a.collapsed"
css_title = "div.panel-group.accordion>div>div.panel-collapse>div>p>span:last-child"
css_phone = "div.panel-group.accordion>div>div.panel-collapse>div>address>a:nth-child(3)"
css_email = "div.panel-group.accordion>div>div.panel-collapse>div>address>a:nth-child(6)"
css_office = "div.panel-group.accordion>div>div.panel-collapse>div>address"

xml_name = '//div[@class="panel-group accordion"]/div/div[@class="panel-heading"]/h4/a[1]'
xml_title = '//div[@class="panel-group accordion"]/div/div[@class="panel-collapse collapse"]/div/p/span[2]'
xml_phone = '//div[@class="panel-group accordion"]/div/div[@class="panel-collapse collapse"]/div/address/a[1]'
xml_email = '//div[@class="panel-group accordion"]/div/div[@class="panel-collapse collapse"]/div/address/a[2]'
xml_office =  '//div[@class="panel-group accordion"]/div/div[@class="panel-collapse collapse"]/div/address'

#Scrape the data for faculty and lecturers and convert it into a data frame
directory_link = read_html("https://business.okstate.edu/directory/index.html?d=Department%20of%20Management%20Science%20%26%20Information%20Systems")
name = directory_link %>%
  html_nodes(css_name) %>%
  html_text()
title = directory_link %>%
  html_nodes(css_title) %>%
  html_text()
phone = directory_link %>%
  html_nodes(css_phone) %>%
  html_text()
email = directory_link %>%
  html_nodes(css_email) %>%
  html_text()
office = directory_link %>%
  html_nodes(css_office) %>%
  html_text()

#Extracting office address through regex
regex1 = "(\\n\\t\\t\\t\\t\\t\\t)([A-Za-z0-9 -,.-]*)(\\n\\t\\t\\t\\t\\t\\t)(Phone: [0-9-]+[a-z\\._]*@okstate.edu)(\\n\\t\\t\\t\\t\\t)"
list4 = str_extract_all(office,regex1)
list5 = str_match(list4,regex1)
list6 = as.data.frame(list5)
office2 = list6[,3]

list = list(Name = name, Title = title, Office_address = office2, Phone_number = phone, Email_address = email)
data_frame = as.data.frame(list)
data_frame

#Export the data frame as a tab-delimited file with a .txt extension
write.table(data_frame, file = 'Directory.txt', sep = '\t', col.names = TRUE)

#Bonus Points
#Using regular expressions, separate the first name and last name

regex2 = "([A-Za-z]+) ([A-Za-z]+)"
list1 = str_extract_all(name,regex2)
list2 = str_match(list1,regex2)
list3 = as.data.frame(list2)
first_last_name = list3[,c(2,3)]
first_last_name

data_frame$First_Name = first_last_name[,1]
data_frame$Last_Name = first_last_name[,2]
data_frame2 = data_frame[,c(1,6,7,2,3,4,5)]
data_frame2

