#Scrape Data
library(ggplot2)
library(RSelenium)
library(dplyr)
library(stringr)

web_url = 'https://www.videocardbenchmark.net/high_end_gpus.html'
rD = rsDriver(verbose=FALSE, browser = 'firefox', port = 4566L)
remDr = rD$client
remDr$navigate(web_url)

# Xpath for video card urls
url_list_xpath <- '//ul[@class="chartlist"]/li/a'

# Obtain HTML elements for the anchor tags
url_list = remDr$findElements(using='xpath', url_list_xpath)

# Create a vector containing all the urls
vid_urls = vector()
for(i in url_list) {
  vid_urls <- append(vid_urls, i$getElementAttribute('href')[[1]])
}

# Create empty lists for all the data.
# These will become the columns in the
# data frame below.
cardname = vector()
businterf = vector()
maxmem = vector()
coreclock = vector()
memclock = vector()
vidcat = vector()
price = vector()
avgg3dmark = vector()

# Xpath selectors for card attributes
# found on each individual webpage.
# The xpath for the card's name and the
# AVGG3D intentionally left blank for
# you to fill out.
cardname_xp <- '//div[@id="main_content"]/div[3]/div[2]/div[2]/div[1]/div[1]/div[1]/div/span'
card_attrib_xp <- '//em[@class="left-desc-cpu"]'
card_foot_xp <- '//div[@class="desc-foot"]'
avgg3d_xp <- '//div[@class="right-desc"]/span'

# Regex patterns for the attributes.
# Some patterns are blank intentionally
# for you to fill out.
businterf_patt = 'Bus Interface: '
maxmem_patt = 'Max Memory Size: '
coreclock_patt = 'Core Clock(s): '
memclock_patt = 'Memory Clock(s): '
vidcat_patt = 'Videocard Category: '
price_patt = 'Last Price Change:  '
dollar_patt = '$'

# A function used to scrape the data from
# each video card webpage.
rip_card = function(html_em, html_foot) {
  # Determine if the attribute exists on the webpage.
  # Not every video card webpage contains all the
  # attributes. Defaults to "no".
  businterf_exist = 0
  maxmem_exist = 0
  coreclock_exist = 0
  memclock_exist = 0
  vidcat_exist = 0
  price_exist = 0
  # Create a string with all the <em> attributes; separated by \n
  # This is the first "block" of attributes.
  attribs_em = unlist(sapply(html_em, function(x) {x$getElementText()}))
  # Split the string by \n and save as list
  attribs_list = str_split(attribs_em, '\n')
  # Loop over attribs_list
  for(j in attribs_list[[1]]) {
    # Detect if Business Interface exists
    if(str_detect(j, businterf_patt)) {
      businterf = str_split(j,businterf_patt)[[1]][2]
      businterf_exist = 1
    }
    # Detect if Maxmem exists
    if(str_detect(j, maxmem_patt)) {
      maxmem = str_split(j,maxmem_patt)[[1]][2]
      maxmem_exist = 1
    }
    # Detect if Coreclock exists
    if(str_detect(j, coreclock_patt)) {
      coreclock = str_split(j,coreclock_patt)[[1]][2]
      coreclock_exist = 1
    }
    # Detect if Memory Clock exists
    if(str_detect(j, memclock_patt)) {
      memclock = str_split(j,memclock_patt)[[1]][2]
      memclock_exist = 1
    }
  }
  # Create a string with all the <div> attributes, separated by \n.
  # This is the second block of attributes.
  attribs_div = unlist(sapply(html_foot, function(x) {x$getElementText()}))
  # Split the string by \n and save as list
  attribs_div_list = str_split(attribs_div, '\n')
  # Loop over attribs_div_list
  for(l in attribs_div_list[[1]]) {
    # Detect if Videocard Category exists
    if(str_detect(l, vidcat_patt)) {
      vidcat = str_split(l,vidcat_patt)[[1]][2]
      vidcat_exist = 1
    }
    # Detect if Last Price Change exists
    if(str_detect(l, price_patt)) {
      price = str_extract(l,dollar_patt)
      price_exist = 1
    }
  }
  # If values were missing, create a null value
  if(businterf_exist == 0) businterf <- NA
  if(maxmem_exist == 0) maxmem <- NA
  if(coreclock_exist == 0) coreclock <- NA
  if(memclock_exist == 0) memclock <- NA
  if(vidcat_exist == 0) vidcat <- NA
  if(price_exist == 0) price <- NA
  # Return the values back out the function    
  return (c(businterf, maxmem, coreclock, memclock, vidcat, price))
}

# Loop over the urls pulled from the landing page
for(k in vid_urls) {
  # Navigate to a video card webpage.
  remDr$navigate(k)
  # Wait for the webpage to load.
  #WebDriverWait(driver, timeout=10).until(lambda d: d.find_element_by_xpath(f))
  # Scrape the card name from the page
  #### Insert code here for card name
  cardname1 = remDr$findElements(using='xpath', cardname_xp)
  cardname2 = unlist(sapply(cardname1, function(x) {x$getElementText()}))
  cardname <- append(cardname, cardname2)
  avgg3dmark1 = remDr$findElements(using='xpath', avgg3d_xp)
  avgg3dmark2 = unlist(sapply(avgg3dmark1, function(x) {x$getElementText()}))
  avgg3dmark = append(avgg3dmark, avgg3dmark2)
  # Scrape the attributes using the function
  first_attrib = remDr$findElements(using='xpath', card_attrib_xp)
  second_attrib = remDr$findElements(using='xpath', card_foot_xp)
  results = rip_card(first_attrib, second_attrib)
  # Take the results from the function and append to the lists
  #### Insert your code here.
  businterf = append(businterf,results[1])
  maxmem = append(maxmem,results[2])
  coreclock = append(coreclock,results[3])
  memclock = append(memclock,results[4])
  vidcat = append(vidcat,results[5])
  price = append(price,results[6])
}

cardname
businterf
maxmem
coreclock
memclock
vidcat
price
avgg3dmark

remDr$close()
rD$server$stop()

#Transforming extracted vectors to dataframe
list = list(cardname, businterf, maxmem, coreclock, memclock, vidcat, price, avgg3dmark)
names = c('cardname', 'businterf', 'maxmem', 'coreclock', 'memclock', 'vidcat', 'price', 'avgg3dmark')
df = as.data.frame(do.call(cbind,list))
colnames(df) = names

#Export dataframe as a tab-delimited file
write.table(df, file = 'output_file', sep = '\t',col.names = TRUE)

df = read.table('~//Documents//GitHub//advanced-data-visualization-tha-Sonali1743//data//output_file', header = TRUE, sep = '\t')

#Converting column maxmem to numeric
df$maxmem = gsub(" MB","", df$maxmem)
df$maxmem = as.numeric(df$maxmem)

#Calculating median average g3d mark
median_g3dmark = median(df$avgg3dmark)
median_g3dmark

#Visualize the Data
scatter_plot = df %>%
  ggplot(aes(x=maxmem, y=avgg3dmark)) +
  geom_point() +
#Adjust the formatting for the title; add in a subtitle with appropriate formatting
  labs(
    x='Max Memory Size',
    y='Average G3D Mark',
    title = 'Relationship Between Memory Size and G3D Mark',
    subtitle = 'G3D Mark Increases as the Memory Size Increases') +
#Change the background, foreground
  theme(plot.background = element_rect(fill = "gray90")) +
  theme(panel.background = element_rect(fill = "gray90")) +
#Adjust the formatting for the font for the y-axis and x-axis labels; format the font for the y-axis and x-axis text
#Change the line color
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10, family ='Arial', color = 'black',face = 'bold'),
    axis.text.x = element_text(size = 10, family ='Arial', color = 'black'),
    axis.text.y = element_text(size = 10, family ='Arial', color = 'black'),
    axis.ticks = element_line(color = 'black', size = 0.4),
    axis.line = element_line(color = 'black', size = 0.4),
    plot.title = element_text(size=14,face = 'bold',lineheight = 1,family ='Arial', color = 'black'),
    plot.subtitle = element_text(size=10,face = 'bold',lineheight = 1,family ='Arial', color = 'black'))

arrows = tibble(
  x1 = 31000,
  y1 = 4980,
  x2 = 31000,
  y2 = 9000
)

scatter_plot +
#Calculate the median score and place a line in the plot
#Add in at least 2 annotations; one for the median and another of your choice
  geom_hline(aes(yintercept = median_g3dmark), color = 'black', size = 0.4) +
  annotate('text', x = 30000, y = 10000, size = 3, color = 'black',hjust = 0,
           fontface =2, label = glue::glue('Median G3D\nMark: 4975')) +
  geom_curve(data = arrows,aes(x = x1, y = y1, xend = x2, yend = y2),arrow = arrow(length = unit(0.07, 'inch')), 
             size = 0.4,color = 'black',curvature = 0.0) +
  annotate('label', x = 40000, y = 35000, size = 3, color = 'black',fill='white',
           fontface =2, label = glue::glue('Very few video cards have\naverage G3D Mark > 20000'))

min = min(df$avgg3dmark)
max = max(df$avgg3dmark)
range = max - min
bins = seq(min, max, range/5)
bins[6] = 38905
df$avgg_cat = cut(df$avgg3dmark, bins, right = FALSE, labels = c('avgg3dmarkI','avgg3dmarkII','avgg3dmarkIII','avgg3dmarkIV','avgg3dmarkV'))

#Add in one additional adjustment to improve your visualization
df %>%
  ggplot(aes(x=maxmem, y=avgg_cat)) +
  coord_flip() +
  geom_point() +
  labs(
    x='Max Memory Size',
    y='Average G3D Mark',
    title = 'Relationship Between Memory Size and G3D Mark',
    subtitle = 'Distribution of Memory Size within each G3D Mark Category') +
  theme(plot.background = element_rect(fill = "gray90")) +
  theme(panel.background = element_rect(fill = "gray90")) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10, family ='Arial', color = 'black',face = 'bold'),
    axis.text.x = element_text(size = 10, family ='Arial', color = 'black'),
    axis.text.y = element_text(size = 10, family ='Arial', color = 'black'),
    axis.ticks = element_line(color = 'black', size = 0.4),
    axis.line = element_line(color = 'black', size = 0.4),
    plot.title = element_text(size=14,face = 'bold',lineheight = 1,family ='Arial', color = 'black'),
    plot.subtitle = element_text(size=10,face = 'bold',lineheight = 1,family ='Arial', color = 'black')) +
  facet_wrap( ~avgg_cat,nrow = 1,scales = 'free') +
  theme(strip.text = element_blank())

#Generate another plot using a different explanatory variable
df %>%
  ggplot(aes(x=vidcat, y=avgg3dmark)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,25000)) +
  labs(
    x='Videocard Category',
    y='Average G3D Mark',
    title = 'Comparison for Average G3D Mark for Different Videocard Categories',
    subtitle = 'Desktop has the Highest Median Average G3D Mark') +
  theme(plot.background = element_rect(fill = "gray90")) +
  theme(panel.background = element_rect(fill = "gray90")) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10, family ='Arial', color = 'black',face = 'bold'),
    axis.text.x = element_text(size = 10, family ='Arial', color = 'black'),
    axis.text.y = element_text(size = 10, family ='Arial', color = 'black'),
    axis.ticks = element_line(color = 'black', size = 0.4),
    axis.line = element_line(color = 'black', size = 0.4),
    plot.title = element_text(size=14,face = 'bold',lineheight = 1,family ='Arial', color = 'black'),
    plot.subtitle = element_text(size=10,face = 'bold',lineheight = 1,family ='Arial', color = 'black')) +
  annotate('label', x = 3, y = 24000, size = 3, color = 'black',fill='white',
           fontface =2, label = glue::glue('Desktop has the highest\nvariation in average G3D Mark'))




