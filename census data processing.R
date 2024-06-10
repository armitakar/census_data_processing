library(dplyr)
library(sf)
library(ggplot2)

library(tidycensus)
library(tigris)



####### getting census data ########
census_api_key("0374af53cfe8d2728204af53395977de1b54b17d")

# variable serach
v18 <- load_variables(2022, "acs5", cache = TRUE)

### function to extract blockgroup-level ACS data for each state
acs_state = function(state_code){
  acs_data = get_acs(geography = "county",#geography = "block group",
                     state = state_code,
                     #county = "Franklin County",
                     variables = c(tot_pop = "B01003_001", #total population
                                   pop_over25 = "B15003_001", #population over age 25
                                   white = "B02001_002", #total white population
                                   black = "B02001_003", #total black population
                                   NHW = "B03002_003", #total non-hispanic white population
                                   hispanic = "B03002_012", #total hispanic population
                                   high_school = "B15003_018", #total population with a high school degree
                                   med_inc = "B19013_001", #median income
                                   poverty = "B17017_002", #total households living below poverty level
                                   no_ins_under19 = "B27010_017", #population group under 19 with no medical insurance
                                   no_ins_19_34 = "B27010_033", #population group within age 19-34 with no medical insurance
                                   no_ins_35_64 = "B27010_050", #population group within age 35-64 with no medical insurance
                                   no_ins_over64 = "B27010_066", #population group over 64 with no medical insurance
                                   HH_size = "B11001_001", #total number of households
                                   owner_no_veh = "B25044_003", #households with no vehicle (owners of the housing unit)
                                   renter_no_veh = "B25044_010", #households with no vehicle (renters of the housing unit)
                                   house_own = "B25003_002", #housing units occupied by owners
                                   women_15_50 = "B13016_001", #women 15-20years
                                   women_15_50_birth = "B13016_002", # women 15-20years who gave birth in past 12 months
                                   disability = "B18101_001", #total population with a disability status
                                   fertility = "B99132_001"), # total WOMEN 15 TO 50 YEARS with fertility
                     year = 2022,
                     survey = "acs5",
                     geometry = TRUE)
  print("acs_data_downloaded")
  acs_data <- select(acs_data, -moe)
  acs_data1 <- spread(acs_data, key = variable, value = estimate)
  acs_data1$p_n_whi = 100 - ((acs_data1$white/acs_data1$tot_pop)*100)
  acs_data1$p_his = (acs_data1$hispanic/acs_data1$tot_pop)*100
  acs_data1$p_hs = (acs_data1$high_school/acs_data1$pop_over25)*100
  acs_data1$p_pov = (acs_data1$poverty/acs_data1$HH_size)*100
  acs_data1$p_noins = ((acs_data1$no_ins_under19 + acs_data1$no_ins_19_34 +
                          acs_data1$no_ins_35_64 + acs_data1$no_ins_over64)/acs_data1$tot_pop)*100
  acs_data1$p_ow_nv = (acs_data1$owner_no_veh/acs_data1$HH_size)*100
  acs_data1$p_re_nv = (acs_data1$renter_no_veh/acs_data1$HH_size)*100
  acs_data1$p_nv = acs_data1$p_re_nv + acs_data1$p_ow_nv
  acs_data1$p_own = (acs_data1$house_own/acs_data1$HH_size)*100
  acs_data1$p_disable = (acs_data1$disability/acs_data1$tot_pop)*100
  acs_data1$p_new_mom = (acs_data1$women_15_50_birth/acs_data1$women_15_50)*100
  acs_data1_prj = st_transform(acs_data1, crs = "EPSG:5070") 
  return(acs_data1_prj)
}

ses = acs_state("OH")  %>% select(GEOID, p_pov, p_n_whi, p_noins, p_nv, p_own, p_hs)  
ses = st_transform(ses, 3857)
#ses$geometry = NULL

ggplot() + geom_sf(data = ses)
