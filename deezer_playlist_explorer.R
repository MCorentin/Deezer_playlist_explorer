# Script to get information about a Deezer playlist
# @author: Corentin
# @date 25/11/2024

if(!require("httr")) install.packages("httr")
if(!require("ggplot2")) install.package("ggplot2")

#playlist_id <- "13298262963"
playlist_id <- "1183004551"

# Get some metadata directly from the "playlist" object:
playlist_request <- httr::GET(paste0("https://api.deezer.com/playlist/", playlist_id))
playlist_info <- httr::content(x = playlist_request, as = "parsed")
user <- playlist_info$creator$name

playlist_request <- httr::GET(paste0("https://api.deezer.com/playlist/", playlist_id, "/tracks/"))
playlist_content <- httr::content(x = playlist_request, as = "parsed")

tracks_list <- playlist_content$data

while(is.null(playlist_content$`next`) == FALSE){
  playlist_request <- httr::GET(playlist_content$`next`)
  playlist_content <- httr::content(x = playlist_request, as = "parsed")
  
  tracks_list <- append(tracks_list, playlist_content$data)
}

# Normalising track lists: "title_version" is absent for some songs...
for(i in c(1:length(tracks_list))) {
  tracks_list[[i]]$title_version <- NULL
}

# Putting everything in a data frame:
tracks <- data.frame(do.call("rbind", tracks_list))

tracks$duration <- as.numeric(tracks$duration)
png(paste0(user,"_playlist_duration.png"), unit = "cm", height = 12, width = 12, res = 300)
    ggplot(data = tracks, aes(x = duration/60)) +
      geom_histogram(col = "black", fill = "deepskyblue4", alpha = 0.7) + 
      theme_bw() + xlab("Duration (min)") + 
      ggtitle(paste0(user, "'s Playlist (", nrow(tracks), " awesome songs)"))
dev.off()


# Le genre correspond aux albums de la playlist pas aux chansons elles memes
genres <- c()
release_dates <- c()
explicits <- c()

for(album in unique(tracks$album)){
  album_request <- httr::GET(paste0("https://api.deezer.com/album/", album$id))
  album_content <- httr::content(album_request)
  
  if(length(album_content$genres$data) > 0){
    genres <- c(genres, album_content$genres$data[[1]]$name)
    release_dates <- c(release_dates, album_content$release_date)
    explicits <- c(explicits, album_content$explicit_lyrics)
  }
  # Test avec sleep pour respecter les quotas de l'API (50 requetes / secondes)
  # Il y a surement un meilleur moyen de controler le nombre de requetes.
  Sys.sleep(0.2)
}


if(!require("treemap", quietly = T)) install.packages("treemap")

treemap_data <- data.frame(table(genres))

png(paste0(user, "_treemap_deezer.png"), units = "cm", width = 12, height = 12, res = 300)
  treemap(treemap_data, 
          index = c("genres"), 
          vSize = "Freq", type = "index",
          fontsize.labels = c(30, 12),
          fontcolor.labels = c("white", "white"),
          fontface.labels = c(2, 1), # 2 = bold, 1 = normal
          bg.labels = c("transparent"),
          align.labels = list(c("center", "center"),
                              c("center", "center")),
          inflate.labels = FALSE,
          title = paste0("Deezer - ", user))
dev.off()


# Dates:
gg_dates <- data.frame(dates = table(gsub("-.*", "", release_dates)))

png(paste0(user,"_playlist_timeline.png"), unit = "cm", height = 12, width = 12, res = 300)
  ggplot(data = gg_dates, aes(x = dates.Var1, y = dates.Freq)) +
    geom_bar(col = "black", fill = "deepskyblue4", alpha = 0.7, stat = "identity") + 
    theme_bw() + xlab("Release dates") + 
    ggtitle(paste0(user, "'s Playlist (", nrow(tracks), " awesome songs)"))
dev.off()

table(explicits)
