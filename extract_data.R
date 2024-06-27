library(ggplot2)
require(REDCapR); library(tidyr)

source("C:/Users/xxx/OneDrive - Northwestern Medicine/Documents/R Scripts/server_access.R")

research_view$YEAR <- format(as.Date(research_view$surgdt), "%Y")

data <- sts_data


all_state_abr <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY',
                   'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND',
                   'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
################################################################################
# Date Range
################################################################################
start = as.Date("1990-01-01")
end = as.Date("2024-05-31")

cohort_data <- sts_data
cohort_data <- cohort_data %>% mutate(
  dates = as.Date(sts_data$surgdt),
  date_range = dates %g% start & dates %<=% end
)
table(cohort_data$date_range)


colnames(cohort_data)
cohort_data <- sts_data
cohort_data <- cohort_data %>% mutate(
  dates = as.Date(sts_data$surgdt),
  date_range = dates %g% start & dates %<=% end
)
table(cohort_data$date_range)

cohort_data %>% mutate(
  country = case_when(
    patcountry %==% 237 ~ 'USA',
    patientcountry %==% 'United States Of America' ~ 'USA',
    T ~ 'Other'
  )
)



data <- cohort_data %>% select('ir_id', 'surgdt')

uri <- "xxx"
card_api <- "xxx"
req_fields <- c()
custom_api <- "xxx"

card_df <- redcap_read(
  redcap_uri = uri,
  token=card_api,
  events="enrollment_arm_1",
  fields=c("record_id", "current_study_status", "ir_id", "sa_surg_date", "sa_death_date", "sa_cause_death"),
  raw_or_label="label",
  batch_size = 250L,
  interbatch_delay = 0.1
)$data

get_records <- data %>% left_join(card_df, by=c("ir_id"))
#get_records$SurgDt <- as.Date(get_records$SurgDt)

demographics <- redcap_read(
  redcap_uri = uri,
  token=card_api,
  records = get_records$record_id,
  forms = "demographics",
  fields=c("record_id"),
  raw_or_label="label",
  batch_size = 250L,
  interbatch_delay = 0.1
)$data


sts_cohort <- cohort_data %>% select(ir_id, surgdt, pataddr, patcity, patcityold, patzip, patregion, patcountry, patientcountry, patlname, patfname)


sts_cohort <- sts_cohort %>% left_join(demographics, by=c('ir_id')); nrow(sts_cohort)

colnames(sts_cohort)

sts_cohort <- sts_cohort %>% mutate(
  State = case_when(
    tolower(patregion) %in% c('60057', '60610', 'il', 'illinois') ~ 'IL',
    grepl('illinois', tolower(patregion)) ~ 'IL',
    tolower(patregion) %in% c('ak') ~ 'AK',
    grepl('alaska', tolower(patregion)) ~ 'AK',
    tolower(patregion) %in% c('al') ~ 'AL',
    grepl('alabama', tolower(patregion)) ~ 'AL',
    tolower(patregion) %in% c('ca') ~ 'CA',
    grepl('californ*', tolower(patregion)) ~ 'CA',
    tolower(patregion) %in% c('az') ~ 'AZ',
    grepl('arizon*', tolower(patregion)) ~ 'AZ',
    tolower(patregion) %in% c('ar') ~ 'AR',
    grepl('arkans*', tolower(patregion)) ~ 'AR',
    tolower(patregion) %in% c('co') ~ 'CO',
    grepl('colora*', tolower(patregion)) ~ 'CO',
    tolower(patregion) %in% c('fl') ~ 'FL',
    grepl('florid*', tolower(patregion)) ~ 'FL',
    tolower(patregion) %in% c('ga') ~ 'GA',
    grepl('georg*', tolower(patregion)) ~ 'GA',
    tolower(patregion) %in% c('hi') ~ 'HI',
    grepl('hawai*', tolower(patregion)) ~ 'HI',
    tolower(patregion) %in% c('ia') ~ 'IA',
    grepl('iow*', tolower(patregion)) ~ 'IA',
    tolower(patregion) %in% c('in') ~ 'IN',
    grepl('india*', tolower(patregion)) ~ 'IN',
    tolower(patregion) %in% c('ks') ~ 'KS',
    grepl('kansas*', tolower(patregion)) ~ 'KS',
    tolower(patregion) %in% c('ky') ~ 'KY',
    grepl('kentuc*', tolower(patregion)) ~ 'KY',
    tolower(patregion) %in% c('la') ~ 'LA',
    grepl('louisi*', tolower(patregion)) ~ 'LA',
    tolower(patregion) %in% c('ma') ~ 'MA',
    grepl('massachu*', tolower(patregion)) ~ 'MA',
    tolower(patregion) %in% c('md') ~ 'MD',
    grepl('maryla*', tolower(patregion)) ~ 'MD',
    tolower(patregion) %in% c('mi') ~ 'MI',
    grepl('michig*', tolower(patregion)) ~ 'MI',
    tolower(patregion) %in% c('mo') ~ 'MO',
    grepl('missour*', tolower(patregion)) ~ 'MO',
    tolower(patregion) %in% c('mn') ~ 'MN',
    grepl('minnes*', tolower(patregion)) ~ 'MN',
    tolower(patregion) %in% c('nc') ~ 'NC',
    grepl('north car*', tolower(patregion)) ~ 'NC',
    tolower(patregion) %in% c('nc') ~ 'SC',
    grepl('south car*', tolower(patregion)) ~ 'SC',
    tolower(patregion) %in% c('nv') ~ 'NV',
    grepl('nevad*', tolower(patregion)) ~ 'NV',
    tolower(patregion) %in% c('nj') ~ 'NJ',
    grepl('new jer*', tolower(patregion)) ~ 'NJ',
    tolower(patregion) %in% c('nh') ~ 'NH',
    grepl('new ham*', tolower(patregion)) ~ 'NH',
    tolower(patregion) %in% c('nm') ~ 'NM',
    grepl('new mex*', tolower(patregion)) ~ 'NM',
    tolower(patregion) %in% c('ne') ~ 'NE',
    grepl('nebra*', tolower(patregion)) ~ 'NE',
    tolower(patregion) %in% c('mt') ~ 'MT',
    grepl('monta*', tolower(patregion)) ~ 'MT',
    tolower(patregion) %in% c('ny') ~ 'NY',
    grepl('new yor*', tolower(patregion)) ~ 'NY',
    tolower(patregion) %in% c('oh') ~ 'OH',
    grepl('ohio*', tolower(patregion)) ~ 'OH',
    tolower(patregion) %in% c('ok') ~ 'OK',
    grepl('oklaho*', tolower(patregion)) ~ 'OK',
    tolower(patregion) %in% c('pa') ~ 'PA',
    grepl('pennsyl*', tolower(patregion)) ~ 'PA',
    tolower(patregion) %in% c('ri') ~ 'RI',
    grepl('rhode is*', tolower(patregion)) ~ 'RI',
    tolower(patregion) %in% c('tn') ~ 'TN',
    grepl('tennes*', tolower(patregion)) ~ 'TN',
    tolower(patregion) %in% c('tx') ~ 'TX',
    grepl('texa*', tolower(patregion)) ~ 'TX',
    tolower(patregion) %in% c('ut') ~ 'UT',
    grepl('uta*', tolower(patregion)) ~ 'UT',
    tolower(patregion) %in% c('wv') ~ 'WV',
    grepl('west virg*', tolower(patregion)) ~ 'WV',
    tolower(patregion) %in% c('va') ~ 'VA',
    grepl('virgin*', tolower(patregion)) ~ 'VA',
    tolower(patregion) %in% c('wi') ~ 'WI',
    grepl('wiscon*', tolower(patregion)) ~ 'WI',
    tolower(patregion) %in% c('wy') ~ 'WY',
    grepl('wyom*', tolower(patregion)) ~ 'WY',
    tolower(patregion) %in% c('wa') ~ 'WA',
    grepl('washin*', tolower(patregion)) ~ 'WA',
    tolower(patregion) %in% c('vt') ~ 'VT',
    grepl('vermont*', tolower(patregion)) ~ 'VT',
    tolower(patregion) %in% c('id') ~ 'ID',
    grepl('idah*', tolower(patregion)) ~ 'ID',
    .default = patregion
  ),
  Country = case_when(
    patientcountry %in% c('United States Of America') ~ 'USA',
    !is.na(patientcountry) ~ patientcountry,
    patcountry %==% 237 ~ 'USA',
    state %in% all_state_abr ~ 'USA',
    .default = 'NA or No Country'
  ),
  City = case_when(
    !is.na(patcity) ~ patcity,
    .default = patcityold
  ),
  Address = case_when(
    !is.na(pataddr) ~ tolower(pataddr),
    .default = tolower(d_address_1)
  ),
  'ZIP Code' = case_when(
    !is.na(patzip) ~ tolower(patzip),
    .default = tolower(d_zip)
  )
)


sts_cohort %>% select(patfname, d_first_name, ir_id, surgdt, Address, City, State, 'ZIP Code', pataddr, patcity, patcityold, patzip, patregion, state, patcountry, patientcountry, country,
                      d_address_1, d_city, d_state, d_zip, d_country, d_no_mail_address) %>%
  select(ir_id, surgdt, Address, City, State, 'ZIP Code', country) -> output

output %>% write.csv('output.csv')
