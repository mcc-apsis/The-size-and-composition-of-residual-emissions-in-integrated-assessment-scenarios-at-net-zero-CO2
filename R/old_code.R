
```{r scenario_clusters,fig.height=7.5,fig.width=6,fig.path="../Results/",dev=c('png', 'pdf', 'svg'),dpi=300}


wd_clusters <- wd_scenarios_emissions %>%
  filter(var!="Kyoto Gases") %>%
  filter(!grepl("Carbon Sequestration",var)) %>%
  mutate(value=ifelse(is.na(value),0,value)) %>%
  mutate(include=ifelse(net_zero_CO2==year,1,0)) %>%
  filter(include==1) %>%
  group_by(id,category,var) %>%
  summarise(value=sum(value,na.rm=TRUE))

wd_clusters <- spread(wd_clusters,var,value) %>%
  ungroup()

wd_cluster_summary <- kmeans(scale(wd_clusters %>% select(-id,-category)),3,nstart=25)

print(wd_cluster_summary)

wd_clusters <- cbind(cluster=wd_cluster_summary$cluster,wd_clusters)
cc_clusters <- wd_clusters %>% 
  select(cluster,id) %>% 
  distinct()
wd_clusters <- gather(wd_clusters,var,value,-cluster,-id,-category) 
wd_clusters <- wd_clusters %>% 
  group_by(cluster,var) %>% 
  summarise(value=median(value)) %>% 
  mutate(cluster=as.factor(cluster)) %>% 
  filter(value>0)


better_labels <- cc_sectors

wd_clusters <- left_join(wd_clusters,better_labels,by="var")

p1 <- wd_clusters %>% 
  filter(sector=="AFOLU") %>% 
  mutate(var=gsub("\\|AFOLU","",var)) %>% 
  ggplot(.,aes(x=cluster,y=value,fill=reorder(var,value))) +
  geom_col(color="#636363") +
  theme_wl() +
  scale_fill_brewer(palette="Greens") +
  theme(legend.title = element_blank(),
        axis.title = element_blank()) +
  labs(title="AFOLU sector emissions at net zero by scenario cluster",
       subtitle=bquote("Gt"~CO[2]*"e/yr"))

p2 <- wd_clusters %>% 
  filter(sector=="Energy") %>% 
  mutate(var=gsub("\\|Energy","",var)) %>% 
  ggplot(.,aes(x=cluster,y=value,fill=reorder(var,value))) +
  geom_col(color="#636363") +
  theme_wl() +
  scale_fill_brewer(palette="Blues") +
  theme(legend.title = element_blank(),
        axis.title = element_blank()) +
  labs(title="Energy sector emissions at net zero by scenario cluster",
       subtitle=bquote("Gt"~CO[2]*"e/yr"))

p3 <- wd_clusters %>% 
  filter(sector!="Energy") %>% 
  filter(sector!="AFOLU") %>% 
  mutate(var=gsub("\\|Other","",var)) %>% 
  mutate(var=ifelse(var=="CO2","CO2|Other",var)) %>% 
  ggplot(.,aes(x=cluster,y=value,fill=reorder(var,value))) +
  geom_col(color="#636363") +
  theme_wl() +
  scale_fill_brewer(palette="Oranges") +
  theme(legend.title = element_blank(),
        axis.title = element_blank()) +
  labs(title="Other sector emissions at net zero by scenario cluster",
       subtitle=bquote("Gt"~CO[2]*"e/yr"))


p1 / p2 / p3 & theme(legend.justification = "left")
```



```{r clean_large_scenario_data_fgases}

## select fgas vars
# 
# fgas_vars <- data_r1 %>%
#   ungroup() %>%
#   select(var,unit) %>%
#   distinct()
# 
# ## these are the ones I could find
# 
# fgas_vars <- data.frame(var=c("Emissions|F-Gases",
#                               "Emissions|PFC|C2F6",
#                               "Emissions|C2F6",
#                               "Emissions|PFC|C6F14",
#                               "Emissions|PFC",
#                               "Emissions|PFC|CF4",
#                               "Emissions|CF4",
#                               "Emissions|HFC|HFC125",
#                               "Emissions|HFC",
#                               "Emissions|HFC|HFC134a",
#                               "Emissions|HFC|HFC143a",
#                               "Emissions|HFC|HFC227ea",
#                               "Emissions|HFC|HFC23",
#                               "Emissions|HFC|HFC245fa",
#                               "Emissions|HFC|HFC32",
#                               "Emissions|HFC|HFC43-10",
#                               "Emissions|SF6",
#                               "Population"),keep=1)
# 
# 
# data_r1_fgases <- left_join(data_r1,fgas_vars,by="var") %>%
#   filter(keep==1)
# 
# data_r5_fgases <- left_join(data_r5,fgas_vars,by="var") %>%
#   filter(keep==1)
# 
# 
# ## infill the years
# 
# data_r1 <- data_r1 %>%
#   filter(year>=2020) %>%
#   group_by(model,scenario,category,var,unit,region) %>%
#   mutate(value=zoo::na.approx(value,na.rm=FALSE))
# 
# data_r5 <- data_r5 %>%
#   filter(year>=2020) %>%
#   group_by(model,scenario,category,var,unit,region) %>%
#   mutate(value=zoo::na.approx(value,na.rm=FALSE))
# 
# 
# save(data_r1_fgases,data_r5_fgases,file="../Data/scenario_data_fgases.RData")


#Carbon Sequestration|Direct Air Capture
#Carbon Sequestration|CCS|Biomass|Energy|Supply|Gases
#Carbon Sequestration|Other

cdr_vars <- data_r1 %>%
  ungroup() %>%
  select(var,unit) %>%
  distinct()

## these are the ones I could find

cdr_vars <- data.frame(var=c("Carbon Sequestration|Direct Air Capture"),keep=1)

data_r1_cdr <- left_join(data_r1,cdr_vars,by="var") %>%
  filter(keep==1)

## infill the years

data_r1_cdr <- data_r1_cdr %>%
  filter(year>=2020) %>%
  group_by(model,scenario,category,var,unit,region) %>%
  mutate(value=zoo::na.approx(value,na.rm=FALSE))


data_r1_cdr <- data_r1_cdr %>%
  group_by(model,scenario,category) %>% 
  mutate(keep=ifelse(last(value)>0,1,0)) %>% 
  filter(keep==1)




save(data_r1_cdr,file="../Data/scenario_data_cdr.RData")

data_r1_cdr <- data_r1_cdr %>% 
  mutate(id=paste0(model,"_",scenario)) %>% 
  arrange(model,scenario,year)

data_r1_cdr_summary <- data_r1_cdr %>% 
  group_by(year,keep) %>% 
  summarise(median=median(value),
            percentile_25=quantile(value, probs = c(0.25),na.rm=TRUE),
            percentile_75=quantile(value, probs = c(0.75),na.rm=TRUE))

data_r1_cdr %>% ggplot(.,aes(x=year,y=value/1000,group=id)) +
  geom_path(color="#43a2ca",alpha=0.1) +
  geom_path(data=data_r1_cdr_summary,aes(x=year,y=median/1000,group=keep),color="#43a2ca",alpha=1,size=1.5) +
  theme_wl() +
  labs(title="DACCS in AR6 scenarios (only scenarios with >0Gt DACCS in 2100)",
       subtitle="GtCO2 per year") +
  theme(axis.title = element_blank())




wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb,"scenario_data")
openxlsx::addWorksheet(wb,"summary")

openxlsx::writeData(wb, sheet = "scenario_data", data_r1_cdr %>% select(-keep,-id), colNames = T, rowNames = F)
openxlsx::writeData(wb, sheet = "summary", data_r1_cdr_summary %>% select(-keep), colNames = T, rowNames = F)


openxlsx::saveWorkbook(wb,"scenario_daccs_data.xlsx",overwrite=T)


```





