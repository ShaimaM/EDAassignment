Nada, [14.12.20 16:00]
#import libraries
library (tidyverse)

#view the diamonds dataset
diamonds

#--------------filter

# filter  where the carat is above 2.5 (good size)
filter (diamonds, carat > 2.5)

# filter where its price above 10,000 (expensive), carat > 1.5 and cut is very good and has the best color
filter (diamonds,
        price > 10000,
        carat > 1.05 ,
        cut == "Very Good",
        color == "D",
        clarity == "VS2")

#--------------arrange

# arrange the dataset based on the prices (higher to lower)
p <- arrange (diamonds, desc(price))

# arrange based on these 2 values (depth of the diamond , the table(top diamon width))
d <- arrange (diamonds, depth , table)

#--------------select

#view the diamond brief details
select(diamonds , cut , color, clarity)

# view the diamond brief details from the arranged dataset
c <- select (p , cut , color, clarity)

# view all with the clarity as the first column
select (diamonds, clarity , everything())

# view the details of the diamond x = length, y = width , z = depth , depth = the total depth %
select (diamonds, depth , x , y , z)

#--------------Mutate

# calculate the vat from the prices and add it to the dataset
vat <- mutate (diamonds,
               vat = price + (price * 0.10))

# add a new column that show the diamond with high prices and kinda good cut
pricey_bad <- mutate(diamonds,
                     pricey_bad_cut = price > 1000 &
                       cut == "Very Good")

#--------------transmute

# calculate a new column that has a good quality and has a premium cut
transmute(diamonds, clarity == "SI1" & cut == "Premium")


#--------------summarize & group by

# group by cuts and count each value and the mean of the carats
by_cut <- group_by(diamonds, cut)
by_cut_s <- summarise(by_cut, count = n(), avg = mean(carat))

# group by colour and count each value and the min and max price
by_color <- group_by(diamonds, color)
by_color_s <- summarise(
  by_color,
  count = n(),
  max_price = max(price),
  min_price = min(price)
)

#group by clarity and count each value and the min and max carat value
by_clarity <- group_by(diamonds, clarity)
summarise(
  by_clarity,
  count = n(),
  high_carat = max(carat),
  low_carat = min(carat)
)


#--------------visualization

# create a dataset with small carats (under 3) & create a histogram that show the carat and its count
smaller <- diamonds %>%
  filter(carat < 3)
ggplot(data = smaller , aes(x = carat)) +
  geom_histogram(binwidth = 0.1)


# use the smaller dataset and visualize the distribution of carat and color the lines based on the cut value
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)


# use the grouped by cut dataset to create a point plot that shows the cut and it total values
ggplot(data = by_cut_s ,
       mapping = aes(x = cut, y = count , color = "pink")) +
  geom_point()


# visualize the cut , color , clarity as a boxplot with the prices as the min and max values
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()


# use the rectangle plot to visualize the color and count relationship
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n), color = "white") +
  scale_fill_gradient(low = "pink", high = "red")