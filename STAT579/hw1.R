#1
#(a)
year <- seq(from = 2008, to = 1948, by = -4) # year column
winner <- c(185, 182, 182, 189, 189, 188, 185, 185, 177, 182, 182, 193, 183, 179, 179, 175) # winners' heights
opponent <- c(175, 193, 185, 187, 188, 173, 180, 177, 183, 185, 180, 180, 182, 178, 178, 173) # opponents' heights
height <- data.frame(year = year, winner = winner, opponent = opponent)
height

#(b)
difference <- winner - opponent # calculate the difference of heights
height1 <- data.frame(year = year, winner = winner, opponent = opponent, difference = difference) # create new date frame with difference of heights
height1

#(c)
taller.won = (difference > 0) # difference > 0 means the winner being taller is TRUE.
height <- data.frame(height, taller.won = taller.won) # add the colume to the data frame in (a)
height

#(d)
count <- table(height$taller.won) # table function for counts
percentage <- count / length(height$taller.won) # calculate the percentage
percentage

#(e)
barplot(height = rev(height1$difference), names.arg = rev(height$year)) # reverse and plot


#2
#(a)
student <- read.table(file = "C:/Users/fanne/Desktop/STAT579/students.txt", header = T) # read data 
MeanHeight <- mean(student$height) #calculate the mean height
MeanHeight
MeanShoesSize <- mean(student$shoesize) #calculate the mean shoesize
MeanShoesSize
SDHeight <- sd(student$height) # standard deviation of height
SDHeight
SDShoesSize <- sd(student$shoesize) # standard deviation of shoesize
SDShoesSize

#(b)
table(student$gender) #count the number of male and female students

#(c)
populationColor <- ifelse(student$population == "kuopio", c("blue"), c("red")) # recode the population variable
students_new <- student # create new dataset "students_new"
students_new$population <- populationColor # use the recoded population cariable for the new dataset
students_new

#(d)
female <- subset(student, gender == "female") # female subset
female
write.table(female, "C:/Users/fanne/Desktop/STAT579/female.txt", quote = F, row.names = F) # export to female.txt

male <- subset(student, gender == "male") #male subset
male
write.table(male, "C:/Users/fanne/Desktop/STAT579/male.txt", quote = F, row.names = F) # export to male.txt

#(e)
MedianHeight <- median(student$height) #calculate the median height
below <- subset(student, height < MedianHeight)
below
write.csv(below, "C:/Users/fanne/Desktop/STAT579/below.csv", quote = F, row.names = F) # export to female.txt

abovem <- subset(student, height > MedianHeight)
abovem
write.csv(abovem, "C:/Users/fanne/Desktop/STAT579/abovem.csv", quote = F, row.names = F) # export to female.txt


#3
#(a)
cars <- read.table(file = "http://maitra.public.iastate.edu/stat579/datasets/cars.dat", header = T) # read the file
cars

#(b)
attach(cars) #attach the dataframe

#(c)
speed_ft_per_s <- 5280 * speed / 3600 # convert speed to feet per second
speed_ft_per_s

#(d)
plot(x = dist, y = speed_ft_per_s, xlab = "Distance (feet)", ylab = "Speed (feet/second)", main = "Speed (feet/second) vs. Distance (feet)") # plot speed (feet/second) against distance (feet)

#(e)
speed_m_per_s <- speed * 1.6093 * 1000 / 3600 # convert speed to meter per second
speed_m_per_s

dist_m <- dist / 5280 * 1.6093 * 1000  # convert distance to meter
dist_m

#(f)
detach()

#(g)
plot(x = dist_m, y = speed_m_per_s, xlab = "Distance (meter)", ylab = "Speed (meter/second)", main = "Speed (meter/second) vs. Distance (meter)") # plot speed (meter/second) against distance (meter)

#(h)
pdf(file = "C:/Users/fanne/Desktop/STAT579/speed-vs-distance.pdf", onefile = T) # print to one file
plot(x = cars$dist, y = speed_ft_per_s, xlab = "Distance (feet)", ylab = "Speed (feet/second)", main = "Speed (feet/second) vs. Distance (feet)") # plot speed (feet/second) against distance (feet)
plot(x = dist_m, y = speed_m_per_s, xlab = "Distance (meter)", ylab = "Speed (meter/second)", main = "Speed (meter/second) vs. Distance (meter)") # plot speed (meter/second) against distance (meter)
dev.off()
#4
activation <- matrix(scan("C:/Users/fanne/Desktop/STAT579/activ.dat"), nrow = 83, ncol = 108, byrow = T) # read activation data
anatomic <- matrix(scan("C:/Users/fanne/Desktop/STAT579/anat.dat"), nrow = 83, ncol = 108, byrow = T) # read anatomic data
image(-activation, axes = F, col = gray.colors(n = 255, start = 0.1, end = 1)) # plot the image of activation
contour(anatomic, axes = F, drawlabels = F, add = T) # overlay contour of anatomic atop activation image
