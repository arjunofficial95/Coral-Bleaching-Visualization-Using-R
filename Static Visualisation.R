# loading the ggplot2 library for ggplot
library(ggplot2)
# reading the csv data of the corals
corals = read.csv("/Users/arjunbasu/Desktop/assignment-02-data-formated.csv")
# viewing the first five rows along with the columns
head(corals)
# removing the percentage character from the value data
# in the inner bracket we are substituting the % character by null
# in the outside bracket we are chnaging the data type to double
corals$value = as.double(gsub("%", "", corals$value))
# producing the ggplot of the coral bleaching
# inside ggplot corals is the data set supplied with 
# aesthetic mapping of year (X-Axis) and value (Y-Axis)
ggplot(corals, aes(year,value)) + 
  # point plot
  geom_point() +
  # forming matrix of discrete variables (location and coral type)
  # using reorder function to order the sites by its geographical location, hence
  # minus sign is put in front of latitude to show sites from top to bottom
  # by geographical location, sites are ordered in SITE 3, 1, 5, 7, 8, 6, 2 and 4
  facet_grid(coralType~reorder(location, -latitude)) +
  # scale_x_continuous is used to scale the x axis and show the label value of 
  # few particular years for clear view
  scale_x_continuous(breaks=c(2010,2013,2016)) +
  # theme function is used for the x axis styling
  # we made the year labels bold and rotate by 65 degrees
  theme(axis.text.x = element_text(face = "bold", angle = 65)) +
  # geom_smooth function is used to smoothen the points plot. 
  # The smoothening function used here is a polynomial function of degree 2.
  # Please note that, different degrees has also been tried and 
  # later settled with degree 2 for perfect fit (visually).
  # Also, we are not showing the CI around the smooth line by setting se to FALSE
  geom_smooth(method="lm",formula=y~poly(x,2),se=FALSE) +
  # labelling the x and y axis and the setting the title of the main plot by using labs function
  labs(y = 'Bleaching Percentage(%)',x = 'Year (2010-2017)',
       title='Coral Bleaching Percentage across different sites (2010 - 2017)')


