library(ggmap)
stamen_key<-"your key here"
ggmap::register_stadiamaps(key=stamen_key)

# Define bounding box
bbox <- c(left = -122.5, bottom = 37.4, right = -121.65, top = 38.2)

# Retrieve Stamen map (e.g., "toner", "terrain", or "watercolor")
map <- get_stadiamap(bbox = bbox, zoom = 10, maptype = "stamen_toner_lite",crop=TRUE)  # Adjust zoom and type as needed

# Convert to ggmap object
ggmap_obj <- ggmap(map)

# Plot
save(map,file = here('data/mapnew.RData'))

