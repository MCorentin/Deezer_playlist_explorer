# Script to get information about a Deezer playlist
# @author: Corentin
# @date 25/11/2024

if(!require("httr")) install.packages("httr")
if(!require("ggplot2")) install.package("ggplot2")

playlist_id <- "13298262963"

playlist_request <- httr::GET(paste0("https://api.deezer.com/playlist/", playlist_id))
playlist_content <- httr::content(x = playlist_request, as = "parsed")

user <- playlist_content$creator$name

tracks_list <- playlist_content$tracks$data

# Normalising track lists
# "title_version" is absent for some songs...
for(i in c(1:length(tracks_list))) {
  tracks_list[[i]]$title_version <- NULL
}

# Putting everything in a DF:
tracks <- data.frame(do.call("rbind", tracks_list))

tracks$duration <- as.numeric(tracks$duration)
ggplot(data = tracks, aes(x = duration/60)) +
  geom_histogram(col = "black", fill = "deepskyblue4", alpha = 0.7) + 
  theme_bw() + xlab("Duration (min)") + 
  ggtitle(paste0(user, "'s Playlist (", nrow(tracks), " awesome songs)"))
