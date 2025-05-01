# #Combined----
# 
# #reading the individual files inside the Main
# mv_combined<-sqlFetch(channel,"Mv_Network_COMBINED_Shape_Index")
# 
# create_line <- function(minx, miny, maxx, maxy) {
#   # Define the two endpoints
#   coords <- matrix(c(minx*0.01 * 0.95, miny*0.01 * 0.95, 
#                      maxx*0.01 * 0.95, maxy*0.01 * 0.95), 
#                    ncol = 2, byrow = TRUE)
#   st_linestring(coords)
# }
# 
# # Create a list of lines from your mv_combined using mapply
# lines <- mapply(
#   create_line,
#   mv_combined$MinGX, mv_combined$MinGY,
#   mv_combined$MaxGX, mv_combined$MaxGY,
#   SIMPLIFY = FALSE
# )
# 
# # Convert the list of lines into a simple feature collection (sfc)
# lines_sfc <- st_sfc(lines)
# 
# # Combine your original data with the spatial geometry to create an sf object
# mv_combined_sf <- st_sf(mv_combined, geometry = lines_sfc, crs= st_crs(rwa_district))
# mv_combined_sf <- st_transform(mv_combined_sf, st_crs(rwa_district))
# 
# mv_combined_sf <- mv_combined_sf %>%
#   mutate(geometry = geometry * 0.01)
# 
# # Plot the spatial lines using ggplot2
# ggplot(mv_combined_sf) +
#   geom_sf(color = "darkblue", size = 1) +
#   # geom_sf(data = rwa_villages, color = "grey", fill = NA) +
#   ggtitle("MV Line Network") +
#   theme_minimal()



# # Now plot to see if it lines up better
# ggplot() +
#   geom_sf(data = rwa_district, fill = NA, color = "blue") +
#   geom_sf(data = planned_mv_sf, fill = NA, color = "red") 
# 
# 
# rwa_cell <- st_read(dsn = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data/rwa_cell/Cell.shp"))
# rwa_cell <- st_make_valid(rwa_cell)
# 
# mv_22 <- st_read(dsn = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/ASCENT/datawork/Existing Electrical Network_2022/Existing_MVLine.shp"))
# mv_22 <- st_transform(mv_22, crs = st_crs(rwa_cell))
# 
# ggplot() +
#   geom_sf(data = rwa_district, fill = NA, color = "black") +
#   geom_sf(data = planned_mv_sf, fill = NA, color = "red") +
#   labs(title = "Planned MV Line 2011 EARP")+
#   theme_minimal()
cell_mv_22 <- st_intersection(rwa_cell, mv_22)

cell_mv_22 <- cell_mv_22 %>% 
  distinct(Cell_ID,.keep_all = TRUE)

cell_mv_planned <- st_intersection(rwa_cell, planned_mv_sf)

cell_mv_planned <- cell_mv_planned %>% 
  distinct(Cell_ID, .keep_all = TRUE)


cell_plan_exist <- cell_mv_planned %>% 
  filter(Cell_ID %in% cell_mv_22$Cell_ID)






#Planned mv----

#reading the individual files inside the Main
planned_mv<-sqlFetch(channel,"Planned_MVLines_COMBINED_Shape_Index")

create_line <- function(minx, miny, maxx, maxy) {
  # Define the two endpoints
  coords <- matrix(c(minx, miny, 
                     maxx, maxy), 
                   ncol = 2, byrow = TRUE)
  st_linestring(coords)
}

# Create a list of lines from your planned_mv using mapply
lines <- mapply(
  create_line,
  planned_mv$MinGX, planned_mv$MinGY,
  planned_mv$MaxGX, planned_mv$MaxGY,
  SIMPLIFY = FALSE
)

# Convert the list of lines into a simple feature collection (sfc)
lines_sfc <- st_sfc(lines)

# Combine your original data with the spatial geometry to create an sf object
planned_mv_sf <- st_sf(planned_mv, geometry = lines_sfc, crs= st_crs(rwa_district))
planned_mv_sf <- st_transform(planned_mv_sf, st_crs(rwa_district))

planned_mv_sf <- planned_mv_sf %>%
  mutate(geometry = geometry * 0.01)

# Plot the spatial lines using ggplot2
ggplot(planned_mv_sf) +
  geom_sf(color = "blue", size = 1) +
  # geom_sf(data = rwa_villages, color = "grey", fill = NA) +
  ggtitle("Planned MV Line Network") +
  theme_void()


#Planned LV----

#reading the individual files inside the Main
planned_lv<-sqlFetch(channel,"Planned_LVLines_COMBINED_Shape_Index")

create_line <- function(minx, miny, maxx, maxy) {
  # Define the two endpoints
  coords <- matrix(c(minx, miny, 
                     maxx, maxy), 
                   ncol = 2, byrow = TRUE)
  st_linestring(coords)
}

# Create a list of lines from your planned_lv using mapply
lines <- mapply(
  create_line,
  planned_lv$MinGX, planned_lv$MinGY,
  planned_lv$MaxGX, planned_lv$MaxGY,
  SIMPLIFY = FALSE
)

# Convert the list of lines into a simple feature collection (sfc)
lines_sfc <- st_sfc(lines)

# Combine your original data with the spatial geometry to create an sf object
planned_lv_sf <- st_sf(planned_lv, geometry = lines_sfc, crs= st_crs(rwa_district))
planned_lv_sf <- st_transform(planned_lv_sf, st_crs(rwa_district))

planned_lv_sf <- planned_lv_sf %>%
  mutate(geometry = geometry * 0.01)

# Plot the spatial lines using ggplot2
ggplot(planned_lv_sf) +
  geom_sf(color = "darkgreen", size = 1) +
  # geom_sf(data = rwa_villages, color = "grey", fill = NA) +
  ggtitle("Planned LV Line Network") +
  theme_void()


#Ongling lv----



#reading the individual files inside the Main
ongoing_lv<-sqlFetch(channel,"Ongoing_LVLine_Gicumbi_Projects_NORTHERN_Shape_Index")

create_line <- function(minx, miny, maxx, maxy) {
  # Define the two endpoints
  coords <- matrix(c(minx, miny, 
                     maxx, maxy), 
                   ncol = 2, byrow = TRUE)
  st_linestring(coords)
}

# Create a list of lines from your ongoing_lv using mapply
lines <- mapply(
  create_line,
  ongoing_lv$MinGX, ongoing_lv$MinGY,
  ongoing_lv$MaxGX, ongoing_lv$MaxGY,
  SIMPLIFY = FALSE
)

# Convert the list of lines into a simple feature collection (sfc)
lines_sfc <- st_sfc(lines)

# Combine your original data with the spatial geometry to create an sf object
ongoing_lv_sf <- st_sf(ongoing_lv, geometry = lines_sfc, crs= st_crs(rwa_district))
ongoing_lv_sf <- st_transform(ongoing_lv_sf, st_crs(rwa_district))

ongoing_lv_sf <- ongoing_lv_sf %>%
  mutate(geometry = geometry * 0.01)

# Plot the spatial lines using ggplot2
ggplot(ongoing_lv_sf) +
  geom_sf(color = "darkgreen", size = 1) +
  # geom_sf(data = rwa_villages, color = "grey", fill = NA) +
  ggtitle("ongoing LV Line Network Gicumbi") +
  theme_minimal()




#reading the individual files inside the Main
ongoing_lv<-sqlFetch(channel,"Ongoing_LVLine_Janja_Projects_NORTHERN_Shape_Index")

create_line <- function(minx, miny, maxx, maxy) {
  # Define the two endpoints
  coords <- matrix(c(minx, miny, 
                     maxx, maxy), 
                   ncol = 2, byrow = TRUE)
  st_linestring(coords)
}

# Create a list of lines from your ongoing_lv using mapply
lines <- mapply(
  create_line,
  ongoing_lv$MinGX, ongoing_lv$MinGY,
  ongoing_lv$MaxGX, ongoing_lv$MaxGY,
  SIMPLIFY = FALSE
)

# Convert the list of lines into a simple feature collection (sfc)
lines_sfc <- st_sfc(lines)

# Combine your original data with the spatial geometry to create an sf object
ongoing_lv_sf <- st_sf(ongoing_lv, geometry = lines_sfc, crs= st_crs(rwa_district))
ongoing_lv_sf <- st_transform(ongoing_lv_sf, st_crs(rwa_district))

ongoing_lv_sf <- ongoing_lv_sf %>%
  mutate(geometry = geometry * 0.01)

# Plot the spatial lines using ggplot2
ggplot(ongoing_lv_sf) +
  geom_sf(color = "darkgreen", size = 1) +
  # geom_sf(data = rwa_villages, color = "grey", fill = NA) +
  ggtitle("ongoing LV Line Network Janja") +
  theme_minimal()







#reading the individual files inside the Main
ongoing_lv<-sqlFetch(channel,"Ongoing_LVLine_Projects_EASTERN_Shape_Index")

create_line <- function(minx, miny, maxx, maxy) {
  # Define the two endpoints
  coords <- matrix(c(minx, miny, 
                     maxx, maxy), 
                   ncol = 2, byrow = TRUE)
  st_linestring(coords)
}

# Create a list of lines from your ongoing_lv using mapply
lines <- mapply(
  create_line,
  ongoing_lv$MinGX, ongoing_lv$MinGY,
  ongoing_lv$MaxGX, ongoing_lv$MaxGY,
  SIMPLIFY = FALSE
)

# Convert the list of lines into a simple feature collection (sfc)
lines_sfc <- st_sfc(lines)

# Combine your original data with the spatial geometry to create an sf object
ongoing_lv_sf <- st_sf(ongoing_lv, geometry = lines_sfc, crs= st_crs(rwa_district))
ongoing_lv_sf <- st_transform(ongoing_lv_sf, st_crs(rwa_district))

ongoing_lv_sf <- ongoing_lv_sf %>%
  mutate(geometry = geometry * 0.01)

# Plot the spatial lines using ggplot2
ggplot(ongoing_lv_sf) +
  geom_sf(color = "darkgreen", size = 1) +
  # geom_sf(data = rwa_villages, color = "grey", fill = NA) +
  ggtitle("Ongoing LV Line Network Eastern Province") +
  theme_minimal()







#reading the individual files inside the Main
ongoing_lv<-sqlFetch(channel,"Ongoing_LVLine_Projects_SOUTHERN_Shape_Index")

create_line <- function(minx, miny, maxx, maxy) {
  # Define the two endpoints
  coords <- matrix(c(minx, miny, 
                     maxx, maxy), 
                   ncol = 2, byrow = TRUE)
  st_linestring(coords)
}

# Create a list of lines from your ongoing_lv using mapply
lines <- mapply(
  create_line,
  ongoing_lv$MinGX, ongoing_lv$MinGY,
  ongoing_lv$MaxGX, ongoing_lv$MaxGY,
  SIMPLIFY = FALSE
)

# Convert the list of lines into a simple feature collection (sfc)
lines_sfc <- st_sfc(lines)

# Combine your original data with the spatial geometry to create an sf object
ongoing_lv_sf <- st_sf(ongoing_lv, geometry = lines_sfc, crs= st_crs(rwa_district))
ongoing_lv_sf <- st_transform(ongoing_lv_sf, st_crs(rwa_district))

ongoing_lv_sf <- ongoing_lv_sf %>%
  mutate(geometry = geometry * 0.01)

# Plot the spatial lines using ggplot2
ggplot(ongoing_lv_sf) +
  geom_sf(color = "darkgreen", size = 1) +
  # geom_sf(data = rwa_villages, color = "grey", fill = NA) +
  ggtitle("Ongoing LV Line Network Southern Province") +
  theme_minimal()


#Plot maps for reference------

ggplot(data = rwa_district, color = "black", fill = NA) +
  geom_sf(color = "black", fill = NA) +
  ggtitle("Rwanda District Map") +
  theme_minimal() 


gicumbi <- rwa_villages %>% 
  filter(District == "Gicumbi")

ggplot(data = gicumbi, color = "black", fill = NA) +
  geom_sf(color = "black", fill = NA) +
  ggtitle("Gicumbi Map") +
  theme_minimal() 




janja <- rwa_villages %>% 
  filter(Sector == "Janja" )

ggplot(data = janja, color = "black", fill = NA) +
  geom_sf(color = "black", fill = NA) +
  ggtitle("Janja Map") +
  theme_minimal() 





eastern <- rwa_villages %>% 
  filter(Province == "East/Iburasirazuba" )

ggplot(data = eastern, color = "black", fill = NA) +
  geom_sf(color = "black", fill = NA) +
  ggtitle("Eastern Province Map") +
  theme_minimal() 



southern <- rwa_villages %>% 
  filter(Province == "South/Amajyepfo" )

ggplot(data = southern, color = "black", fill = NA) +
  geom_sf(color = "black", fill = NA) +
  ggtitle("Southern Province Map") +
  theme_minimal() 



# 
# #Boundaries----
# 
# query <- "SELECT TOP 1 * FROM Admin_District_Boundaries"  # For SQL Server; use LIMIT for others
# district_subset <- sqlQuery(channel, query)
# 
# #reading the individual files inside the Main
# district<-sqlFetch(channel,"Admin_District_Boundaries")
# 
# create_line <- function(minx, miny, maxx, maxy) {
#   # Define the two endpoints
#   coords <- matrix(c(minx, miny, 
#                      maxx, maxy), 
#                    ncol = 2, byrow = TRUE)
#   st_linestring(coords)
# }
# 
# # Create a list of lines from your district using mapply
# lines <- mapply(
#   create_line,
#   district$MinGX, district$MinGY,
#   district$MaxGX, district$MaxGY,
#   SIMPLIFY = FALSE
# )
# 
# # Convert the list of lines into a simple feature collection (sfc)
# lines_sfc <- st_sfc(lines)
# 
# # Combine your original data with the spatial geometry to create an sf object
# district_sf <- st_sf(district, geometry = lines_sfc, crs= st_crs(rwa_district))
# district_sf <- st_transform(district_sf, st_crs(rwa_district))
# 
# district_sf <- district_sf %>%
#   mutate(geometry = geometry * 0.01)
# 
# # Plot the spatial lines using ggplot2
# ggplot(district_sf) +
#   geom_sf(color = "darkblue", size = 1) +
#   # geom_sf(data = rwa_villages, color = "grey", fill = NA) +
#   ggtitle("MV Line Network") +
#   theme_minimal()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #District boundaries----
# 
# #reading the individual files inside the Main
# district <-sqlFetch(channel,"Admin_District_Boundaries_Shape_Index")
# 
# 
# create_polygon <- function(minx, miny, maxx, maxy) {
#   # Create a matrix of coordinates for the rectangle
#   coords <- matrix(
#     c(minx, miny,  # bottom-left
#       minx, maxy,  # top-left
#       maxx, maxy,  # top-right
#       maxx, miny,  # bottom-right
#       minx, miny), # close the polygon by repeating the first point
#     ncol = 2, byrow = TRUE
#   )
#   st_polygon(list(coords))
# }
# 
# # Create a list of lines from your mv_combined using mapply
# polygon <- mapply(
#   create_polygon,
#   district$MinGX, district$MinGY,
#   district$MaxGX, district$MaxGY,
#   SIMPLIFY = FALSE
# )
# 
# district_polygons_sfc <- st_sfc(polygon)
# 
# district_sf <- st_sf(district, geometry = district_polygons_sfc, crs = st_crs(rwa_district))
# 
# ggplot(district_sf) +
#   geom_sf(color = "darkblue", size = 1) +
#   geom_sf(mv_combined_sf, color = "orange") +
#   ggtitle("MV Line Network") +
#   theme_minimal()
# 
# 
# ggplot() +
#   geom_sf(data = district_sf, fill = NA, color = "grey") +
#   geom_sf(data = mv_combined_sf, color = "darkblue", alpha = 0.5) +
#   ggtitle("MV Networks Over District Boundaries") +
#   theme_minimal()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# st_crs(mv_combined_sf)
# 
# 
# # Step 2: Transform mv_combined_sf to the CRS of rwa_villages
# mv_combined_sf <- st_transform(mv_combined_sf, crs= st_crs(4326))
# 
# rwa_district <- st_transform(rwa_district, crs = st_crs(mv_combined_sf))
# # Step 3: Plot the data
# ggplot() +
#   geom_sf(data = rwa_district, fill = NA, color = "lightgrey") +
#   geom_sf(data = mv_combined_sf, color = "darkblue", size = 1) +
#   ggtitle("MV Networks on RWA Villages") +
#   theme_minimal()
# 
# 
# 






# 
# 
# 
# conn <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/Historical data/EDCL EARP Program/OneDrive_2024-10-07/SOFRECO 1/Deliverables Combined Report/GDB_All/EWSA_Rwanda_Combined_Zones.mdb")
# # 
# # # Open a connection to the MDB file
# # conn <- odbcConnectAccess("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/Historical data/EDCL EARP Program/OneDrive_2024-10-07/SOFRECO 1/Deliverables Combined Report/GDB_All/EWSA_Rwanda_Combined_Zones.mdb")
# 
# # List available tables
# tables <- dbListTables(conn)
# 
# write_xlsx(tables, path = "C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/Historical data/EDCL EARP Program/output/tables.xlsx")
# # Read a specific table
# mv_network <- sqlFetch(conn, "MV_Network_COMBINED")
# mv_network <- dbReadTable(conn, "MV_Network_COMBINED")
# 
# # Close the connection
# odbcClose(conn)
# 













