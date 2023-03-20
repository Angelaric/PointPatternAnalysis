## NNA function


# For manual testing purposes
# period_sites <- RP_Settl_1_sf %>% dplyr::filter(РЕ==1)
# study_area <- Box1

# r <- getNNratio(period_sites, Box1)


# result <- NULL
getNNratio <- function(period_sites, study_area){  # these two arguments should be sf objects in the same CRS
  library(sf)
  library(spatstat)
  st_crs(period_sites)==st_crs(study_area)
  # create an owin object out of the study area polygon
  window <- as.owin(study_area)
  # create ppp object out of period-sites that fall within the study area
  period_sites_in_w<- period_sites$geometry %>%
    st_intersection(study_area)
  period_sites_ppp <- period_sites_in_w %>%
    st_coordinates() %>%
    as.ppp(W = window)
  # run NNA analysis on the ppp objects
  observed_dist <- mean(nndist(period_sites_ppp))
  
  expected_dist <- 0.5*sqrt(st_area(study_area)/nrow(st_sf(period_sites_in_w)))
  r <- observed_dist/expected_dist
  # You can also run this on a linear network (e.g. Tundzha) after creating linnet , a linear network from it
  # period_sites_lpp <- period_sites$geometry %>%
  #    st_intersection(study_area) %>%
  #    st_coordinates() %>%
  #    as.lpp(L = window)
  # mean_nn_linear <- mean(nndist.lpp(period_sites_lpp))
  # rlin <- mean_nn_linear/mean_dist
  
  # result <- rbind(result, r)
  # result
  # plot points within the window with a result
  # plot(period_sites_ppp, main = paste0("NN ratio for the ",quote(period_sites), " is ", round(result,2)))
  # plot all the points (inclusive of those outside the window)
  #plot(period_sites$geometry, main = paste0("NN ratio inside the bounding polygon is ", round(r,2))); plot(study_area$geometry,border = "blue", add = T)
  
  #plot(study_area$geometry,border = "blue"); plot(period_sites$geometry, main = paste0("NN ratio inside the bounding polygon is ", round(r,2)), add=TRUE) # I (AG) just added this because I want to see what is inside the box. That's why we have to outcomes
  
  r
}

# r <- getNNratio(my_period_sites, my_Box) # How to run this function


