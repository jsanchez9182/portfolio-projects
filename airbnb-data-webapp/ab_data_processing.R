library(dplyr)

ab.data <- read.csv("AB_NYC_2019.csv",header=T,stringsAsFactors = F)

#changing name encoding to UTF-8
Encoding(ab.data$name) <- "UTF-8"

#if reviews_per_month has missing value it is changed to 0
ab.data$reviews_per_month <- ifelse(is.na(ab.data$reviews_per_month),0,ab.data$reviews_per_month)

#Created new column recieved_review
ab.data$received_review <- ifelse(ab.data$reviews_per_month == 0,"No","Yes")

#creating matrix to store mean prices for different neighbourhood groups and room types
mean_prices <- matrix(rep(NA),nrow=length(unique(ab.data$room_type)),ncol=length(unique(ab.data$neighbourhood_group)))

#calculating mean prices for different neighbourhood groups and room types
for (i in 1:length(unique(ab.data$room_type))){
  for (j in 1:length(unique(ab.data$neighbourhood_group))){
    temp_subset <- filter(ab.data,room_type==sort(unique(ab.data$room_type))[i],
                          neighbourhood_group==sort(unique(ab.data$neighbourhood_group))[j])
    mean_prices[i,j] <- round(mean(temp_subset$price))
  }
}


ab.data$room_type_num <- as.numeric(factor(ab.data$room_type))
ab.data$neighbourhood_group_num <- as.numeric(factor(ab.data$neighbourhood_group))


#assiging mean price column for a given location and room type
for (i in 1:nrow(ab.data)){
  ab.data$mean_price[i] <- mean_prices[ab.data$room_type_num[i],ab.data$neighbourhood_group_num[i]]
}


#creating labels to be shown on map
ab.data$labs <- lapply(seq(nrow(ab.data)),function(i){
  paste("<p>",ab.data[i,"name"],"<br/>",
        "Location:",ab.data[i,"neighbourhood_group"],"<br/>",
        "Type:",ab.data[i,"room_type"],"<br/>",
        "Price: $",ab.data[i,"price"],"<br/>",
        "Average price for location and type: $",ab.data[i,"mean_price"],
        "</p>")
}
)

#changing labels to character data type
ab.data$labs <- as.character(ab.data$labs)

#selecting only required columns
cleaned.ab.data <- select(ab.data,neighbourhood_group,room_type,labs,longitude,latitude,received_review)

#writing dataframe to csv file
write.csv(cleaned.ab.data,"cleaned_abdata.csv")
