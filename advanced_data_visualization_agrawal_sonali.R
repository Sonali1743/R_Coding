#Using R, generate a line plot with the required adjustments

library('ggplot2')
library('dplyr')
library('zoo')
library('glue')
df = read.table('~//Documents//GitHub//advanced-data-visualization-ice-Sonali1743//data//maine_unemployment.txt', header = TRUE)
df$year = as.integer(df$year)
df$month = as.integer(df$month)

#Creating new dataframe with column 'date'
df1 = df %>%
  mutate(date = as.yearmon(paste(year, month), "%Y %m"))

#Creating a lineplot
lineplot = df1 %>%
  ggplot(aes(x=date, y=unemploy)) +
#The y-axis limited between 0 and 10
  scale_y_continuous(limits=c(0,10)) +
#Change the y-label to Unemployment Rate and the x-label to Monthly and adjust the font for the labels to a size 12 with the font "Georgia" and color "white"
#Adjust the title for the plot and name it 'Maine Unemployment Rate 1996 to 2006'; add in a subtitle with a more detailed description; adjust the font, size, color to whatever you choose
  labs(
    x='Monthly',
    y='Unemployment Rate',
    title = 'Maine Unemployment Rate 1996 to 2006',
    subtitle = 'Before and After Significant Dip in Aug 2000'
  ) +
#Change the background to a dark gray color
  theme(plot.background = element_rect(fill = "gray30")) +
  theme(panel.background = element_rect(fill = "gray30")) +
#Adjust the font for the y- and x-axis to size 10 with font "Arial" with color "white"
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 12, family ='Georgia', color = 'White'),
    axis.text.x = element_text(size = 10, family ='Arial', color = 'White'),
    axis.text.y = element_text(size = 10, family ='Arial', color = 'White'),
    axis.ticks = element_line(color = 'White', size = 0.4),
    axis.line = element_line(color = 'White', size = 0.4),
    plot.title = element_text(size=14,face = 'bold',lineheight = 1,family ='Georgia', color = 'White'),
    plot.subtitle = element_text(size=12,face = 'bold',lineheight = 1,family ='Georgia', color = 'White')
  ) +
#Change the color of the line to a lighter color of gray
  geom_line(color='gray80')

unemploy_max = max(df2$unemploy)
arrows = tibble(
  x1 = as.yearmon('Aug 2000'),
  y1 = 7.3,
  x2 = as.yearmon('Aug 2000'),
  y2 = 6.8
)

lineplot1 = lineplot + 
#Insert an annotation with an explanation as to why this dip occurred; adjust the font color, type, size as well as the background color
  annotate('label', x = as.yearmon('Aug 2000'), y = 2.1, size = 3, color = 'white',fill='black',
           fontface =2, label = glue::glue('Hiring spree among\ncompanies led to this dip')) +
#Add in an annotation with your first and last name; adjust the style according to your wishes
  annotate('text', x = as.yearmon('Jan 1996'), y = 10, size = 3.5, color = 'white',hjust = 0,
           fontface =2, label = glue::glue(':By Sonali Agrawal')) +
  geom_hline(aes(yintercept = unemploy_max),color = "gray80",size = 1) +
#Add in a horizontal line indicating the maximum unemployment rate; add in an annotation with an arrow pointing it out
  annotate('text', x = as.yearmon('Jan 2000'), y = 7.5, size = 3, color = 'white',hjust = 0,
           fontface =2, label = glue::glue('Maximum unemployment\nrate: 6.7%')) +
  geom_curve(data = arrows,aes(x = x1, y = y1, xend = x2, yend = y2),arrow = arrow(length = unit(0.07, 'inch')), 
             size = 0.8,color = 'gray80',curvature = 0.0) +
#Create 2 boxes on the plot to indicate the time prior to dip and the time after dip
  annotate("rect", fill = "blue", alpha=0.2, xmin = -Inf, xmax = as.yearmon('Aug 2000'), 
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha=0.2, xmin = as.yearmon('Aug 2000'), xmax = Inf, 
           ymin = -Inf, ymax = Inf)
lineplot1


    