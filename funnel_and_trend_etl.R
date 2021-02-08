#funnel_and_trend_etl.r
#load and transform some data for the funnel and trend reports

library(tidyverse)
library(lubridate)
library(DBI)

#load the data extracts
load("~/OneDrive - CBRE, Inc/data/raw_data/transform_data.RData")
#load("inc/funnel_data.RData")

ft_con <- dbConnect(odbc::odbc(), "fivetran", timeout = 10,bigint = c("numeric"))


# get web data

# ga_visits<- dbGetQuery(ft_con,"with pd_visits as ( SELECT distinct visit_id
# FROM google_analytics_360.session_hit where page_page_path similar to
# '%(dallas|park-district)%' ), pp_visits as (SELECT distinct visit_id FROM
# google_analytics_360.session_hit where page_page_path similar to
# '%(irvine|park-place|orange_county)%' ), lnd_visits as (SELECT distinct
# visit_id FROM google_analytics_360.session_hit where page_page_path similar to
# '%(london|hammersmith|st-mary|st-martin)%' )
#
# SELECT visit_start_time::date, traffic_source_medium as medium,
# traffic_source_source as source, traffic_source_campaign as campaign,
# geo_network_continent as continent, count (distinct sess.visit_id) as visits,
# count (distinct pd.visit_id) as pd_visits, count (distinct pp.visit_id) as
# pp_visits, count (distinct lnd.visit_id) as lnd_visits FROM
# google_analytics_360.ga_session sess left join pd_visits pd on
# sess.visit_id=pd.visit_id left join pp_visits pp on sess.visit_id=pp.visit_id
# left join lnd_visits lnd on sess.visit_id=lnd.visit_id group by
# visit_start_time::date, traffic_source_medium, traffic_source_source,
# traffic_source_campaign, geo_network_continent;")
#
#
# #Get scheduled tours and calls from Hubspot

hs_tours_and_calls<-dbGetQuery(ft_con,"select distinct ct.id as contact_id
,ct.property_email
,ct.property_in_which_service_are_you_interested_
,ct.property_in_which_location_are_you_interested_
,case when lower(title) like '%excited for your tour%' then 'Tour' else 'Call' end as meeting_type
,ct.property_hs_analytics_source
,max(em.start_time) as tour_time
,he.created_at as create_time
from hubspot.engagement_meeting em
join hubspot.engagement_contact ec
on em.engagement_id=ec.engagement_id
join hubspot.contact ct
on ec.contact_id=ct.id
join hubspot.engagement he
on em.engagement_id=he.id
where lower(title) similar to '%(excited for your tour|learn more about hana)%'
group by ct.id
,ct.property_email
,ct.property_in_which_service_are_you_interested_
,ct.property_in_which_location_are_you_interested_
,case when lower(title) like '%excited for your tour%' then 'Tour' else 'Call' end
,ct.property_hs_analytics_source
,he.created_at;")



hana_unit_regions_by_city<-hana_unit_geo_heirarchy %>% 
  distinct(hana_city,hana_country,hana_region) %>% 
  drop_na()

#Get the digital marketing spend
funnel_market_spend<-marketing_spend_edited %>% 
  rename(hana_product_orig=hana_product) %>% 
  pivot_longer(Team:Meet,names_to = "hana_product") %>% 
  mutate(funnel_week=floor_date(campaign_date, unit="week",week_start = 1),
         funnel_month=floor_date(campaign_date, unit="month"),
         funnel_quarter=floor_date(campaign_date,unit="quarter"),
         funnel_mtd=mday(campaign_date),
         funnel_qtd=qday(campaign_date),
         funnel_wtd=wday(campaign_date,week_start = 1),
         cohort_date=campaign_date,
         cohort_week=funnel_week,
         cohort_month=funnel_month,
         cohort_quarter=funnel_quarter,
         true_paid=1,
         measure="marketing spend",
         hana_city= case_when(
           grepl('dallas',hana_market,ignore.case = TRUE)~'Dallas',
           grepl('irvine',hana_market,ignore.case = TRUE)~'Irvine',
           grepl('london',hana_market,ignore.case = TRUE)~'London',
           TRUE~'Not Specified'),
         hana_unit='Not Specified') %>% 
  left_join(hana_unit_regions_by_city) %>% 
  select(funnel_date=campaign_date,funnel_week,funnel_month,funnel_quarter,funnel_mtd,funnel_qtd,funnel_wtd,true_paid,hana_unit,hana_city,hana_region,hana_product_orig,hana_product,value,measure,cohort_date,cohort_week,cohort_month,cohort_quarter)


#allocate visits to location if we can determine
# ga_visits_edited<-ga_visits %>% 
#   mutate(pd_visits=if_else(grepl('dallas',campaign,ignore.case = TRUE),visits,pd_visits),
#          pp_visits=if_else(grepl('irvine',campaign,ignore.case = TRUE),visits,pp_visits),
#          lnd_visits=if_else(grepl('london',campaign,ignore.case = TRUE),visits,lnd_visits),
#          visits=visits-pd_visits-pp_visits-lnd_visits) %>% 
#   pivot_longer(cols=visits:lnd_visits,names_to="hana_location",values_to="visits")



# funnel_web_traffic<-ga_visits_edited %>% #
# filter(!grepl('uk-',campaign,ignore.case=TRUE), filter(continent %in%
# c('Americas','(not set),Europe')) %>% mutate(hana_city= case_when(
# grepl('pd_visits',hana_location,ignore.case = TRUE)~'Dallas',
# grepl('pp_visits',hana_location,ignore.case = TRUE)~'Irvine',
# grepl('lnd_visits',hana_location,ignore.case = TRUE)~'London', #
# grepl('us-',campaign,ignore.case=TRUE)~'Non-Specific' TRUE~'Not Specified' ),
# hana_product= case_when( grepl('team',campaign,ignore.case = TRUE)~'Team',
# grepl('share',campaign,ignore.case = TRUE)~'Share',
# grepl('meet',campaign,ignore.case=TRUE)~'Meet', TRUE~'Not Specified' ),
# traffic_week=floor_date(visit_start_time, unit="week",week_start = 1),
# traffic_month=floor_date(visit_start_time, unit="month"),
# traffic_quarter=floor_date(visit_start_time, unit="quarter"),
# traffic_mtd=mday(visit_start_time), traffic_qtd=qday(visit_start_time),
# traffic_wtd=wday(visit_start_time,week_start = 1), Team=case_when( #allocate
# across products if it isnt explicit hana_product=='Team'~visits,
# hana_product=='Meet'~0, hana_product=='Share'~0, hana_product=='Not
# Specified'~visits*.4 ), Share=case_when( hana_product=='Team'~0,
# hana_product=='Meet'~0, hana_product=='Share'~visits, hana_product=='Not
# Specified'~visits*.4 ), Meet=case_when( hana_product=='Team'~0,
# hana_product=='Meet'~visits, hana_product=='Share'~0, hana_product=='Not
# Specified'~visits*.2 ),
# true_paid=ifelse(grepl('(cpc|ppc|paid)',medium,ignore.case = TRUE),1,0),
# hana_unit='Not Specified')%>% left_join(hana_unit_regions_by_city) %>%
# select(funnel_date=visit_start_time,funnel_week=traffic_week,funnel_month=traffic_month,funnel_quarter=traffic_quarter,funnel_mtd=traffic_mtd,funnel_qtd=traffic_qtd,funnel_wtd=traffic_wtd,true_paid,hana_unit=hana_location,hana_city,hana_region,hana_product_orig=hana_product,Team,Share,Meet)
# %>% pivot_longer(Team:Meet,names_to = "hana_product") %>%
# mutate(measure="web_traffic", cohort_date=funnel_date,
# cohort_week=funnel_week, cohort_month=funnel_month,
# cohort_quarter=funnel_quarter) %>% ungroup(.)


seg_cohort<-web_dau_all %>% 
  group_by(anonymous_id) %>% 
  summarise(cohort_date=min(dau_date)) %>% 
  mutate(cohort_week=floor_date(cohort_date, unit="week",week_start = 1),
         cohort_month=floor_date(cohort_date, unit="month"),
         cohort_quarter=floor_date(cohort_date, unit="quarter"))

funnel_web_traffic<-web_dau_all %>% 
  select(funnel_date=dau_date,hana_unit,hana_city,hana_product_orig,true_paid,anonymous_id) %>% 
  inner_join(hana_unit_regions_by_city) %>% 
  inner_join(seg_cohort,by=c("anonymous_id"="anonymous_id")) %>% 
  group_by(funnel_date,hana_unit,hana_city,hana_region,hana_product_orig,true_paid,cohort_date,cohort_week,cohort_month,cohort_quarter) %>% 
  summarise(visits=as.numeric(n_distinct(anonymous_id))) %>% 
  ungroup(.) %>% 
  replace_na(list(hana_product_orig="Not Specified",hana_unit="Not Specified")) %>% 
  mutate(funnel_week=floor_date(funnel_date, unit="week",week_start = 1),
         funnel_month=floor_date(funnel_date, unit="month"),
         funnel_quarter=floor_date(funnel_date, unit="quarter"),
         funnel_mtd=mday(funnel_date),
         funnel_qtd=qday(funnel_date),
         funnel_wtd=wday(funnel_date,week_start = 1),
         Team=case_when( #allocate across products if it isnt explicit
           hana_product_orig=='Team'~visits,
           hana_product_orig=='Meet'~0,
           hana_product_orig=='Share'~0,
           hana_product_orig=='Not Specified'~visits*.4
         ),
         Share=case_when(
           hana_product_orig=='Team'~0,
           hana_product_orig=='Meet'~0,
           hana_product_orig=='Share'~visits,
           hana_product_orig=='Not Specified'~visits*.4
         ),
         Meet=case_when(
           hana_product_orig=='Team'~0,
           hana_product_orig=='Meet'~visits,
           hana_product_orig=='Share'~0,
           hana_product_orig=='Not Specified'~visits*.2
         )) %>% 
  pivot_longer(Team:Meet,names_to = "hana_product")  %>% 
  mutate(measure="web_traffic") %>% 
  select(funnel_date,funnel_week,funnel_month,funnel_quarter,funnel_mtd,funnel_qtd,funnel_wtd,true_paid,hana_unit,hana_city,hana_region,hana_product_orig,hana_product,value,measure,cohort_date,cohort_week,cohort_month,cohort_quarter)
  


funnel_leads<-hs_contacts_edited %>%
  filter(acquisition_channel=='Marketing') %>% 
#  filter(hana_location %in%  c("UnitedStates_Texas_Dallas_PwCToweratParkDistrict_2121NorthPearlStreet","UnitedStates_California_Irvine_IrvineParkPlace_3349MichelsonDrive","UnitedKingdom_London_70SMA_70StMarysAxe","UnitedKingdom_London_245Hammersmith_245HammersmithRoad","UnitedKingdom_London_SMC_10PaternosterRow"),
#         hana_product %in% c("Hana Meet","Hana Share","Hana Team","Not Specified"),
#         !grepl('outreach',acquisition_source,ignore.case = TRUE)) %>% 
  mutate(true_paid=ifelse(digital_channel=="Paid",1,0),
         hana_product=stringr::str_replace_all(hana_product, "Hana ", ""), 
         create_mtd=mday(create_date),
         create_qtd=qday(create_date),
         create_wtd=wday(create_date,week_start = 1),
         Team=case_when(
           hana_product=='Team'~1,
           hana_product=='Meet'~0,
           hana_product=='Share'~0,
#           hana_product=='Not Specified'~.4
           TRUE~.4
         ),
         Share=case_when(
           hana_product=='Team'~0,
           hana_product=='Meet'~0,
           hana_product=='Share'~1,
#           hana_product=='Not Specified'~.4
           TRUE~.4
         ),
         Meet=case_when(
           hana_product=='Team'~0,
           hana_product=='Meet'~1,
           hana_product=='Share'~0,
#           hana_product=='Not Specified'~.2
           TRUE~.2
         )
  ) %>%
  left_join(hana_unit_geo_heirarchy,by=c("hana_location"="location_id"))  %>% 
  select(funnel_date=create_date,funnel_week=create_week,funnel_month=create_month,funnel_quarter=create_quarter,funnel_mtd=create_mtd,funnel_qtd=create_qtd,funnel_wtd=create_wtd,true_paid,hana_unit,hana_city,hana_region,hana_product_orig=hana_product,Team,Share,Meet) %>% 
  #  group_by(funnel_date=create_date,funnel_week=create_week,funnel_month=create_month,true_paid,hana_location,hana_product_orig=hana_product) %>% 
  pivot_longer(Team:Meet,names_to = "hana_product") %>% 
  mutate(measure="leads",
         cohort_date=funnel_date,
         cohort_week=funnel_week,
         cohort_month=funnel_month,
         cohort_quarter=funnel_quarter) %>% 
  ungroup(.) %>% 
  filter(!is.na(measure)) 

funnel_opptys<-sfdc_deals_edited %>% 
  filter(acquisition_channel=='Marketing') %>% 
#  filter(hana_location %in%  c("UnitedStates_Texas_Dallas_PwCToweratParkDistrict_2121NorthPearlStreet","UnitedStates_California_Irvine_IrvineParkPlace_3349MichelsonDrive","UnitedKingdom_London_70SMA_70StMarysAxe","UnitedKingdom_London_245Hammersmith_245HammersmithRoad","UnitedKingdom_London_SMC_10PaternosterRow"),
#         !grepl('outreach',acquisition_source,ignore.case = TRUE)) %>%  
  mutate(true_paid=ifelse(digital_channel=="Paid",1,0),
         hana_product=stringr::str_replace_all(hana_product, "Hana ", ""),  
         create_mtd=mday(create_date),
         create_qtd=qday(create_date),
         create_wtd=wday(create_date,week_start = 1),
         Team=case_when(#allocate across products if it isn't explicitly stated
           hana_product=='Team'~1,
           hana_product=='Meet'~0,
           hana_product=='Share'~0,
#           hana_product=='Not Specified'~.4
           TRUE~.4
         ),
         Share=case_when(
           hana_product=='Team'~0,
           hana_product=='Meet'~0,
           hana_product=='Share'~1,
#           hana_product=='Not Specified'~.4
           TRUE~.4
         ),
         Meet=case_when(
           hana_product=='Team'~0,
           hana_product=='Meet'~1,
           hana_product=='Share'~0,
#           hana_product=='Not Specified'~.2
           TRUE~.2
         )
  ) %>%
  select(funnel_date=create_date,funnel_week=create_week,funnel_month=create_month,funnel_quarter=create_quarter,funnel_mtd=create_mtd,funnel_qtd=create_qtd,funnel_wtd=create_wtd,true_paid,hana_unit,hana_city,hana_region,hana_product_orig=hana_product,contact_create_date,Team,Share,Meet) %>% 
  #  group_by(funnel_date=create_date,funnel_week=create_week,funnel_month=create_month,true_paid,hana_location,hana_product_orig=hana_product) %>% 
  pivot_longer(Team:Meet,names_to = "hana_product") %>% 
  mutate(measure="opptys",
         cohort_date=contact_create_date,
         cohort_week=floor_date(contact_create_date, unit="week",week_start = 1),
         cohort_month=floor_date(contact_create_date, unit="month"),
         cohort_quarter=floor_date(contact_create_date, unit="quarter")) %>% 
  ungroup(.) %>% 
  select(-contact_create_date) %>% 
  filter(!is.na(measure)) 


tours_long<-hs_tours_and_calls %>% 
  filter(meeting_type=='Tour') %>% 
  pivot_longer(cols=tour_time:create_time,names_to="tour_meeting_type",names_repair = "unique",values_to = "tour_time")


funnel_tours<-hs_contacts_edited %>% 
filter(acquisition_channel=="Marketing") %>% 
#  filter(hana_location %in%  c("UnitedStates_Texas_Dallas_PwCToweratParkDistrict_2121NorthPearlStreet","UnitedStates_California_Irvine_IrvineParkPlace_3349MichelsonDrive","UnitedKingdom_London_70SMA_70StMarysAxe","UnitedKingdom_London_245Hammersmith_245HammersmithRoad","UnitedKingdom_London_SMC_10PaternosterRow"),
#         hana_product %in% c("Hana Meet","Hana Share","Hana Team","Not Specified"),
#         !grepl('outreach',acquisition_source,ignore.case = TRUE)) %>% 
  mutate(true_paid=ifelse(digital_channel=="Paid",1,0)) %>% 
  inner_join(tours_long) %>% 
  mutate(tour_date= as_date(tour_time),
         tour_week=floor_date(tour_date, unit="week",week_start = 1),
         tour_month=floor_date(tour_date, unit="month"),
         tour_quarter=floor_date(tour_date,unit="quarter"),
         tour_wtd=wday(tour_date,week_start = 1),
         tour_mtd=mday(tour_date),
         tour_qtd=qday(tour_date),
         hana_product=stringr::str_replace_all(hana_product, "Hana ", ""),  
         Team=case_when(
           hana_product=='Team'~1,
           hana_product=='Meet'~0,
           hana_product=='Share'~0,
#           hana_product=='Not Specified'~.4
           TRUE~.4
         ),
         Share=case_when(
           hana_product=='Team'~0,
           hana_product=='Meet'~0,
           hana_product=='Share'~1,
#           hana_product=='Not Specified'~.4
           TRUE~.4
         ),
         Meet=case_when(
           hana_product=='Team'~0,
           hana_product=='Meet'~1,
           hana_product=='Share'~0,
#           hana_product=='Not Specified'~.2
           TRUE~.2
         ),
         measure=ifelse(tour_meeting_type=='tour_time',"tour_scheduled","tour_created")) %>%
  left_join(hana_unit_geo_heirarchy,by=c("hana_location"="location_id"))  %>% 
  select(funnel_date=tour_date,funnel_week=tour_week,funnel_month=tour_month,funnel_quarter=tour_quarter,funnel_mtd=tour_mtd,funnel_qtd=tour_qtd,funnel_wtd=tour_wtd,true_paid,hana_unit,hana_city,hana_region,hana_product_orig=hana_product,measure,create_date,Team,Share,Meet) %>% 
  pivot_longer(Team:Meet,names_to = "hana_product") %>% 
#  mutate(measure="tours") %>% 
  mutate(cohort_date=create_date,
         cohort_week=floor_date(create_date, unit="week",week_start = 1),
         cohort_month=floor_date(create_date, unit="month"),
         cohort_quarter=floor_date(create_date, unit="quarter")) %>% 
  ungroup(.) %>% 
  select(-create_date) %>% 
  filter(!is.na(measure)) 

funnel_won<-sfdc_deals_edited %>% 
  filter(pipeline_stage=='Closed Won',
         acquisition_channel=='Marketing') %>% 
#         hana_location %in%  c("UnitedStates_Texas_Dallas_PwCToweratParkDistrict_2121NorthPearlStreet","UnitedStates_California_Irvine_IrvineParkPlace_3349MichelsonDrive","UnitedKingdom_London_70SMA_70StMarysAxe","UnitedKingdom_London_245Hammersmith_245HammersmithRoad","UnitedKingdom_London_SMC_10PaternosterRow"),
#                !grepl('outreach',acquisition_source,ignore.case = TRUE)) %>% 
  mutate(true_paid=ifelse(digital_channel=="Paid",1,0),
         close_week=floor_date(close_date, unit="week",week_start = 1),
         close_month=floor_date(close_date, unit="month"),
         close_quarter=floor_date(close_date,unit="quarter"),
         close_wtd=wday(close_date,week_start = 1),
         close_mtd=mday(close_date),
         close_qtd=qday(close_date),
         hana_product=stringr::str_replace_all(hana_product, "Hana ", ""),  
         Team=case_when(
           hana_product=='Team'~1,
           hana_product=='Meet'~0,
           hana_product=='Share'~0,
#           hana_product=='Not Specified'~.4
           TRUE~.4
         ),
         Share=case_when(
           hana_product=='Team'~0,
           hana_product=='Meet'~0,
           hana_product=='Share'~1,
#           hana_product=='Not Specified'~.4
           TRUE~.4
         ),
         Meet=case_when(
           hana_product=='Team'~0,
           hana_product=='Meet'~1,
           hana_product=='Share'~0,
#           hana_product=='Not Specified'~.2
           TRUE~.2
         )
  ) %>%
  select(funnel_date=close_date,funnel_week=close_week,funnel_month=close_month,funnel_quarter=close_quarter,funnel_mtd=close_mtd,funnel_qtd=close_qtd,funnel_wtd=close_wtd,true_paid,hana_unit,hana_city,hana_region,hana_product_orig=hana_product,contact_create_date,Team,Share,Meet) %>% 
  pivot_longer(Team:Meet,names_to = "hana_product") %>% 
  mutate(measure="won",
         cohort_date=contact_create_date,
         cohort_week=floor_date(contact_create_date, unit="week",week_start = 1),
         cohort_month=floor_date(contact_create_date, unit="month"),
         cohort_quarter=floor_date(contact_create_date, unit="quarter")) %>% 
  ungroup(.) %>% 
  select(-contact_create_date) %>% 
  filter(!is.na(measure)) 

funnel_revenue<-sfdc_deals_edited %>% 
  mutate(deal_amount=case_when(
    !is.na(monthly_amount) & !is.na(lease_term)~lease_term*monthly_amount,
    !is.na(monthly_amount) & is.na(lease_term) & grepl('Team',hana_product,ignore.case = TRUE)~monthly_amount*12,
    !is.na(monthly_amount) & is.na(lease_term) & grepl('Share',hana_product,ignore.case = TRUE)~monthly_amount*3,
    TRUE~0
  )) %>% 
  filter(pipeline_stage=='Closed Won',
         acquisition_channel=='Marketing') %>% 
#         hana_location %in%  c("UnitedStates_Texas_Dallas_PwCToweratParkDistrict_2121NorthPearlStreet","UnitedStates_California_Irvine_IrvineParkPlace_3349MichelsonDrive","UnitedKingdom_London_70SMA_70StMarysAxe","UnitedKingdom_London_245Hammersmith_245HammersmithRoad","UnitedKingdom_London_SMC_10PaternosterRow"),
#         !grepl('outreach',acquisition_source,ignore.case = TRUE)) %>% 
  mutate(true_paid=ifelse(digital_channel=="Paid",1,0),
         close_week=floor_date(close_date, unit="week",week_start = 1),
         close_month=floor_date(close_date, unit="month"),
         close_quarter=floor_date(close_date,unit="quarter"),
         close_wtd=wday(close_date,week_start = 1),
         close_mtd=mday(close_date),
         close_qtd=qday(close_date),
         hana_product=stringr::str_replace_all(hana_product, "Hana ", ""),  
         Team=case_when(
           grepl('Team',hana_product,ignore.case = TRUE)~deal_amount,
           grepl('Meet',hana_product,ignore.case = TRUE)~0,
           grepl('Share',hana_product,ignore.case = TRUE)~0,
           hana_product=='Not Specified'~.4*deal_amount
         ),
         Share=case_when(
           grepl('Team',hana_product,ignore.case = TRUE)~0,
           grepl('Meet',hana_product,ignore.case = TRUE)~0,
           grepl('Share',hana_product,ignore.case = TRUE)~deal_amount,
           hana_product=='Not Specified'~.4*deal_amount
         ),
         Meet=case_when(
           grepl('Team',hana_product,ignore.case = TRUE)~0,
           grepl('Meet',hana_product,ignore.case = TRUE)~deal_amount,
           grepl('Share',hana_product,ignore.case = TRUE)~0,
           hana_product=='Not Specified'~.2*deal_amount
         )
  ) %>%
  select(funnel_date=close_date,funnel_week=close_week,funnel_month=close_month,funnel_quarter=close_quarter,funnel_mtd=close_mtd,funnel_qtd=close_qtd,funnel_wtd=close_wtd,true_paid,hana_unit,hana_city,hana_region,hana_product_orig=hana_product,contact_create_date,Team,Share,Meet) %>% 
  pivot_longer(Team:Meet,names_to = "hana_product") %>% 
  mutate(measure="deal_revenue",
         cohort_date=contact_create_date,
         cohort_week=floor_date(contact_create_date, unit="week",week_start = 1),
         cohort_month=floor_date(contact_create_date, unit="month"),
         cohort_quarter=floor_date(contact_create_date, unit="quarter")) %>% 
  ungroup(.) %>% 
  select(-contact_create_date) %>% 
  filter(!is.na(measure)) 



funnel_data<-rbind(funnel_market_spend,funnel_web_traffic,funnel_leads,funnel_opptys,funnel_tours,funnel_won,funnel_revenue) %>% 
  mutate(funnel_qtd=as.integer(funnel_qtd),
         funnel_wtd=as.integer(funnel_wtd)) %>% 
  mutate_at(vars(starts_with("hana_")), list(~ifelse(is.na(.), "Not Specified", .)))


# isr_activity_summary_edited<-
#   isr_activity_summary_edited %>% 
#   left_join(RiHana::get_hana_locations() %>% 
#               select(hs_locations,hana_city,hana_name),
#             by=c("hana_location"="hs_locations")) 
# 
# isr_activity_detail_edited<-
#   isr_activity_detail_edited %>% 
#   left_join(RiHana::get_hana_locations() %>% 
#               select(hs_locations,hana_city,hana_name),
#             by=c("hana_location"="hs_locations")) 


save(funnel_data,
     isr_activity_summary_edited,
     isr_activity_detail_edited,
#     funnel_market_spend,
     file="inc/funnel_data.RData")

