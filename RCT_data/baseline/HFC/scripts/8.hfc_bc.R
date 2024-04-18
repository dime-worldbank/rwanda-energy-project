########################################################################################################
#                                                                                                      #
#                HIGH-FREQUENCY CHECKS AND BACKCHECK COMPARISON                                        #
#                                                                                                      #
########################################################################################################

## PURPOSE      Comparison between main HH survey and BC survey

## AUTHOR      Juliana Guerrero (adapted from Adrien Ciret & Marc-Andrea Fiorina)

## LAST UPDATE  October 5, 2023


########################################################################################################


##########################################################################
## data
##########################################################################

## HH survey raw data
df_or = fup3
## BC survey raw data
df_bc = read.csv(file.path(dropbox,
                           'Rwanda Roads Data/Primary data/HH survey/endline/data/raw/RFR_HH_Back_check_2023_WIDE.csv'),
                 stringsAsFactors = FALSE)

# remove duplicates
df_bc = df_bc[!duplicated(df_bc[c("ID_05_enter")], fromLast = TRUE), ]

# filter only hh for comparison
df_or = df_or %>% filter(ID_05_enter %in% df_bc$ID_05_enter)

#########################################################################
## first check 
#########################################################################
vars_conf = c('conf_visit_2','conf_visit','wrong_enum',
              'wrong_date','conf_consent','conf_attitude')
out_v1 = data.frame()
temp=data.frame()
for (var in vars_conf){
  temp = df_bc %>% select(ID_05_enter,var) 
  temp = temp %>%
    mutate(comment=if_else(temp[,var]!=1,'Negative or no response','OK'),
           question=var) %>% select(ID_05_enter,question,comment)
  out_v1 = rbind(out_v1,temp)
  print(var)
}
out_v1 = out_v1 %>% 
  filter(comment=='Negative or no response' | is.na(comment)) %>% 
  replace(is.na(.),'Missing response')

#########################################################################
## total number of crops
#########################################################################

# total number of crops

perm_crops= c('d_02_p_1','d_02_p_2','d_02_p_3','d_02_p_4',
              'd_02_p_5','d_02_p_6','d_02_p_7','d_02_p_8',
              'd_02_p_9','d_02_p_10','d_02_p_11','d_02_p_12',
              'd_02_p_13','d_02_p_14','d_02_p_15')
seas_crops= c('d_05_1','d_05_2','d_05_3','d_05_4',
              'd_05_5','d_05_6','d_05_7','d_05_8',
              'd_05_9','d_05_10','d_05_11','d_05_12',
              'd_05_13','d_05_14','d_05_15','d_05_16',
              'd_05_17','d_05_18','d_05_19','d_05_20',
              'd_05_21','d_05_22','d_05_23','d_05_24',
              'd_05_25','d_05_26','d_05_27','d_05_28',
              'd_05_29','d_05_30','d_05_31','d_05_32','d_05_33')
# create total number of crops 
df_bc = df_bc %>% rowwise() %>% 
  mutate(total_perm_crops=sum(c_across(perm_crops),na.rm=T),
         total_seas_crops=sum(c_across(seas_crops),na.rm=T))

df_or = df_or %>% rowwise() %>% 
  mutate(total_perm_crops=sum(c_across(perm_crops),na.rm=T),
         total_seas_crops=sum(c_across(seas_crops),na.rm=T))

table(df_bc$d_01)
table(df_bc$d_n_1)
table(df_bc$d_61_a)
table(df_bc$e_)

# comparison permanent crops
crops = c('total_perm_crops')#,'total_seas_crops'
out_v1b = data.frame()
temp11= data.frame()
# only those who answer permanent crops
df_bc_pc = df_bc %>% filter(d_01 %in% c(1,0))
for (id in unique(df_bc_pc$ID_05_enter)){ #unique(df_bc$ID_05_enter)
  for (var in crops){
    df1 = df_bc %>% filter(ID_05_enter==id) %>% 
      select(ID_05_enter,crops)
    df2 = df_or %>% filter(ID_05_enter==id) %>% 
      select(ID_05_enter,crops)
    # var='total_perm_crops'
    df1_t = df1%>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    df2_t = df2 %>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    hh = as.numeric(df2_t[,var])
    bc = as.numeric(df1_t[,var])
    if (hh!=bc){#df1_t[,var]!=df2_t[,var]
      
      temp= df1_t %>% mutate(question=var,comment='No match') %>% 
        mutate(hh_value=hh,bc_value=bc) %>% 
        select(ID_05_enter,question,comment,hh_value,bc_value) 
      temp11 = rbind(temp11,temp)
    }}
  out_v1b = rbind(out_v1b,temp11)
  names(out_v1b) = c("ID_05_enter","question","comment","hh_value","bc_value")
  print(id)
}

# comparison seasonal crops
# comparison permanent crops
crops = c('total_seas_crops')#,'total_seas_crops'
out_v1b2 = data.frame()
temp112= data.frame()
# only those who answer permanent crops
df_bc_sc = df_bc %>% filter(d_n_1 %in% c(1,0))
for (id in unique(df_bc_sc$ID_05_enter)){ #unique(df_bc$ID_05_enter)
  for (var in crops){
    df1 = df_bc %>% filter(ID_05_enter==id) %>% 
      select(ID_05_enter,crops)
    df2 = df_or %>% filter(ID_05_enter==id) %>% 
      select(ID_05_enter,crops)
    # var='total_perm_crops'
    df1_t = df1%>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    df2_t = df2 %>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    hh = as.numeric(df2_t[,var])
    bc = as.numeric(df1_t[,var])
    if (hh!=bc){#df1_t[,var]!=df2_t[,var]
      
      temp= df1_t %>% mutate(question=var,comment='No match') %>% 
        mutate(hh_value=hh,bc_value=bc) %>% 
        select(ID_05_enter,question,comment,hh_value,bc_value) 
      temp112 = rbind(temp112,temp)
    }}
  out_v1b2 = rbind(out_v1b2,temp112)
  names(out_v1b) = c("ID_05_enter","question","comment","hh_value","bc_value")
  print(id)
}
out_v1b= rbind(out_v1b,out_v1b2)

# livestock
table(df_bc$d_61_a)
livestock = df_bc %>% 
  select(d_61_1,d_61_2,d_61_3,d_61_4,d_61_5,d_61_6,d_61_7,d_61_8,# livestock
         d_62_1,d_62_2,d_62_3,d_62_4,d_62_5,d_62_6,d_62_7,d_62_8,  # Numb of livestock
  ) %>% names()

out_v2 = data.frame()
temp2 = data.frame()
df_bc_ls = df_bc %>% filter(d_61_a %in% c(1,0))
for (id in unique(df_bc_ls$ID_05_enter)){ #unique(df_bc$ID_05_enter)
  df1 = df_bc %>% filter(ID_05_enter==id) %>% 
    select(ID_05_enter,livestock)
  df2 = df_or %>% filter(ID_05_enter==id) %>% 
    select(ID_05_enter,livestock)
  for (var in livestock){
    
    df1_t = df1%>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    df2_t = df2 %>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    hh = as.numeric(df2_t[,var])
    bc = as.numeric(df1_t[,var])
    if (hh!=bc){
      temp= df1_t %>% mutate(question=var,comment='No match',
                             hh_value=hh,bc_value=bc) %>% 
        select(ID_05_enter,question,comment,hh_value,bc_value)
      temp2 = rbind(temp2,temp)
      
    }}
  out_v2 = rbind(out_v2,temp2)
  print(id)
}

# services
table(df_bc$e_accessservices)
services = df_bc %>% 
  select(e_05_1,e_05_2,e_05_3,e_05_4,e_05_5,e_05_6,e_05_7,e_05_8,
         e_05_9,e_05_10,e_05_11) %>% names()
out_v3 = data.frame()
temp3 = data.frame()
df_bc_se = df_bc %>% select(ID_05_enter,services) %>% 
  filter(across(starts_with('e_05'),~!is.na(.)))

for (id in unique(df_bc_se$ID_05_enter)){ #unique(df_bc$ID_05_enter)
  df1 = df_bc %>% filter(ID_05_enter==id) %>% 
    select(ID_05_enter,services)
  df2 = df_or %>% filter(ID_05_enter==id) %>% 
    select(ID_05_enter,services)
  for (var in services){
    
    df1_t = df1%>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    df2_t = df2 %>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    hh = as.numeric(df2_t[,var])
    bc = as.numeric(df1_t[,var])
    if (hh!=bc){
      temp= df1_t %>% mutate(question=var,comment='No match',
                             hh_value=hh,bc_value=bc) %>% 
        select(ID_05_enter,question,comment,hh_value,bc_value)
      temp3 = rbind(temp3,temp)
    }}
  out_v3 = rbind(out_v3,temp3)
  print(id)
}

# land ownership
#c_23 c_23_old

land_own = df_bc %>% 
  select(c_00_1old,c_14_00,c_00_2,c_00,c_00_1b,c_00_1b1, 
         c_41,c_41_num,c_00_1a,c_00_1a1) %>% names()
out_v4 = data.frame()
temp4 = data.frame()
df_bc_lo_na = df_bc %>% select(ID_05_enter,land_own) %>% 
  filter(across(starts_with('c_'),~is.na(.x)))
df_bc_lo = df_bc %>% filter(!(ID_05_enter %in% df_bc_lo_na$ID_05_enter)) %>% 
  select(ID_05_enter,land_own)

for (id in unique(df_bc_lo$ID_05_enter)){ #unique(df_bc$ID_05_enter)
  df1 = df_bc %>% filter(ID_05_enter==id) %>% 
    select(ID_05_enter,land_own)
  df2 = df_or %>% filter(ID_05_enter==id) %>% 
    select(ID_05_enter,land_own)
  for (var in land_own){
    
    df1_t = df1%>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    df2_t = df2 %>% 
      select(ID_05_enter,var) %>% 
      replace(is.na(.), 0)
    hh = as.numeric(df2_t[,var])
    bc = as.numeric(df1_t[,var])
    if (hh!=bc){
      temp= df1_t %>% mutate(question=var,comment='No match',
                             hh_value=hh,bc_value=bc) %>% 
        select(ID_05_enter,question,comment,hh_value,bc_value)
      temp4 = rbind(temp4,temp)
    }}
  out_v4 = rbind(out_v4,temp4)
  print(id)
}


## add enumerators names and ids and date of survey, and labels
lab_qs = read.csv(file.path(dropbox,
                            'Rwanda Roads Data/Primary data/HH survey/endline/data/raw/dictionary_bc_output.csv'))
out_v1 = out_v1 %>% 
  left_join(hfc_data %>% 
              select(ID_05_enter,ID_03,enumerator_name,SubmissionDate),
            by='ID_05_enter') %>% distinct()%>%
  filter(!(question %in% c('wrong_date','wrong_enum'))) %>% 
  left_join(df_bc %>% select(ID_05_enter,ID_backchecker),
            by='ID_05_enter') %>% 
  mutate(bc_name = case_when(ID_backchecker==1~'HABARUGIRA Jean Bosco',
                             ID_backchecker==2~'Jacques NDAYAMBAJE',
                             ID_backchecker==3~'UWINEZA Djamila ',
                             ID_backchecker==4~'MUNYANEZA Fabrice'))

out_v1 = out_v1 %>% 
  left_join(lab_qs,by='question')

out_v1b = out_v1b %>% 
  left_join(hfc_data %>% 
              select(ID_05_enter,ID_03,enumerator_name,SubmissionDate),
            by='ID_05_enter')%>% distinct() %>% 
  relocate(hh_value, .after=SubmissionDate) %>% 
  relocate(bc_value, .after=hh_value)%>% 
  left_join(df_bc %>% select(ID_05_enter,ID_backchecker),
            by='ID_05_enter') %>% 
  mutate(bc_name = case_when(ID_backchecker==1~'HABARUGIRA Jean Bosco',
                             ID_backchecker==2~'Jacques NDAYAMBAJE',
                             ID_backchecker==3~'UWINEZA Djamila ',
                             ID_backchecker==4~'MUNYANEZA Fabrice'))

out_v1b = out_v1b %>% 
  left_join(lab_qs,by='question')

out_v2 = out_v2 %>% 
  left_join(hfc_data %>% 
              select(ID_05_enter,ID_03,enumerator_name,SubmissionDate),
            by='ID_05_enter')%>% distinct()%>% 
  relocate(hh_value, .after=SubmissionDate) %>% 
  relocate(bc_value, .after=hh_value)%>% 
  left_join(df_bc %>% select(ID_05_enter,ID_backchecker),
            by='ID_05_enter') %>% 
  mutate(bc_name = case_when(ID_backchecker==1~'HABARUGIRA Jean Bosco',
                             ID_backchecker==2~'Jacques NDAYAMBAJE',
                             ID_backchecker==3~'UWINEZA Djamila ',
                             ID_backchecker==4~'MUNYANEZA Fabrice'))

out_v2 = out_v2 %>% 
  left_join(lab_qs,by='question')

out_v3 = out_v3 %>% 
  left_join(hfc_data %>% 
              select(ID_05_enter,ID_03,enumerator_name,SubmissionDate),
            by='ID_05_enter')%>% distinct()%>% 
  relocate(hh_value, .after=SubmissionDate) %>% 
  relocate(bc_value, .after=hh_value)%>% 
  left_join(df_bc %>% select(ID_05_enter,ID_backchecker),
            by='ID_05_enter') %>% 
  mutate(bc_name = case_when(ID_backchecker==1~'HABARUGIRA Jean Bosco',
                             ID_backchecker==2~'Jacques NDAYAMBAJE',
                             ID_backchecker==3~'UWINEZA Djamila ',
                             ID_backchecker==4~'MUNYANEZA Fabrice'))

out_v3 = out_v3 %>% 
  left_join(lab_qs,by='question')

out_v4 = out_v4 %>% 
  left_join(hfc_data %>% 
              select(ID_05_enter,ID_03,enumerator_name,SubmissionDate),
            by='ID_05_enter')%>% distinct()%>% 
  relocate(hh_value, .after=SubmissionDate) %>% 
  relocate(bc_value, .after=hh_value)%>% 
  left_join(df_bc %>% select(ID_05_enter,ID_backchecker),
            by='ID_05_enter') %>% 
  mutate(bc_name = case_when(ID_backchecker==1~'HABARUGIRA Jean Bosco',
                             ID_backchecker==2~'Jacques NDAYAMBAJE',
                             ID_backchecker==3~'UWINEZA Djamila ',
                             ID_backchecker==4~'MUNYANEZA Fabrice'))

out_v4 = out_v4 %>% 
  left_join(lab_qs,by='question')

hfc_bc <- googledrive::drive_get(paste0("HFC_HH_BC"))
hfc_bc %>%
  sheet_write(data = out_v1, sheet = "Survey_enum_qs")
1

hfc_bc %>%
  
  sheet_write(data = out_v1b, sheet = "crops")
1

hfc_bc %>%
  
  sheet_write(data = out_v2, sheet = "livestock")
1

hfc_bc %>%
  
  sheet_write(data = out_v3, sheet = "services")

1

hfc_bc %>%
  
  sheet_write(data = out_v4, sheet = "land_own")
1