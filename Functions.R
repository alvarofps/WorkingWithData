modifyDataframe = function(oldStockData){
  data = oldStockData %>% subset(select = -c(tprice, wprice))%>%
    rename(IcNumber = "InterchnageNumber", Year = "year",
           Model = "VehicleName",
           UniqueId = "unique", Condition = "condition",
           Miles = "miles", VehicleStockNumber = "vstockno",
           YardId = "yardid", Price = "rprice", Cost = "costprice", Status = "status",
           CurrentStatus = "currentstatus", LastPriced = "lastpriced", DeleteReason = "deletereason",
           CreditNumber = "credit_no")%>%
    # mutate(PartName = as.factor(PartName))%>%
    mutate(Model = as.factor(Model)) %>%
    mutate(PartNumber = as.factor(PartNumber))%>%
    mutate(Miles = as.double(Miles))%>%
    mutate(Condition = as.factor(Condition))%>%
    mutate(Manufacturer = as.factor(Manufacturer))%>%
    mutate(Year = as.factor(Year))%>%
    distinct(UniqueId, .keep_all = TRUE)
  
  return (data[complete.cases(data$PartNumber), ])
}

profitByPart = function(filteredData)
{
  profitByPart = filteredData %>%
    group_by(PartName) %>%
    summarise(TotalProfit = sum(Profit)) %>%
    arrange(TotalProfit) %>%
    mutate(TotalProfit = signif(TotalProfit, 3)/1000000) %>%
    mutate(PartName = fct_reorder(PartName, desc(TotalProfit)))%>%
    top_n(10, TotalProfit)
  
  return(profitByPart)
}

profitByManufacturer = function(filteredData)
{
  profitByManufacturer = filteredData %>%
    group_by(Manufacturer) %>%
    summarise(TotalProfit = sum(Profit)) %>%
    arrange(TotalProfit) %>%
    mutate(TotalProfit = signif(TotalProfit, 3)/1000000) %>%
    mutate(Manufacturer = fct_reorder(Manufacturer, desc(TotalProfit)))%>%
    top_n(10, TotalProfit)
  
  return(profitByManufacturer)
}

partsByManufacturer = function(filteredData)
{
  manufacturers = filteredData %>%
    count(Manufacturer, name = "NumberOfParts") %>%
    arrange(NumberOfParts) %>%
    mutate( NumberOfParts = signif(NumberOfParts, 3)/10000) %>%
    mutate(Manufacturer = fct_reorder(Manufacturer, desc(NumberOfParts)))%>%
    top_n(10, NumberOfParts)

  return(manufacturers) 
}

# I need to group by, the apply arrange function and then finally,
# apply fct_reorder
quantityOfProfitablePartsByManufacturer = function(filteredData){
  parts = profitByPart(filteredData) %>% top_n(5, TotalProfit)
  manufacturer = profitByManufacturer(filteredData) %>% top_n(3, TotalProfit)
  
  value = filteredData %>%
    dplyr::select(Manufacturer, PartName, Profit) %>%
    subset(Manufacturer %in% manufacturer$Manufacturer) %>%
    subset(PartName %in% parts$PartName) %>%
    group_by(Manufacturer, PartName) %>%
    summarise(TotalProfit = sum(Profit)) %>%
    mutate(TotalProfit = signif(TotalProfit, 2)/1000000) %>%
    group_by(Manufacturer) %>%
    arrange(desc(TotalProfit), .by_group = TRUE) %>%
    mutate(PartName = fct_reorder(PartName, desc(TotalProfit)))
  
  return(value)
}

getModels = function(filteredData)
{
  parts = profitByPart(filteredData) %>% top_n(4, TotalProfit) %>% subset( PartName == 'ENGINE')
  manufacturer = profitByManufacturer(filteredData) %>% top_n(3, TotalProfit)
  
  filteredData %>%
    dplyr::select(Manufacturer, Model, Year, Miles, PartName, Condition, Profit) %>%
    subset(Manufacturer %in% manufacturer$Manufacturer) %>%
    subset(PartName %in% parts$PartName) %>%
    group_by(Manufacturer, PartName, Model) %>%
    summarise(TotalProfit = sum(Profit)) %>%
    mutate(TotalProfit = signif(TotalProfit, 2)/1000000) %>%
    group_by(Manufacturer, PartName) %>%
    arrange(desc(TotalProfit), .by_group = TRUE) %>%
    mutate(Model = fct_reorder(Model, desc(TotalProfit))) %>%
    slice(1:3)
}

finalFilter = function(filteredData, filter)
{
  filtered = filteredData %>% subset(Manufacturer %in% filter$Manufacturer) %>%
    subset(Model %in% filter$Model ) %>%
    subset(PartName == filter$PartName) 
  
  # We replaces the zeroes by NA, to apply Rubins Law and imputation
  filtered$Miles[filtered$Miles == 0] = NA
  
  # Set a higher cap at 999, to standardize the top limit 
  filtered$Miles[filtered$Miles >= 999] = NA
  
  return (na.omit(filtered))
}

filterOutliers = function(firstImputation)
{
  quantiles = quantile(firstImputation$Miles, probs=c(.25, .75))
  interQuantileRange = IQR(firstImputation$Miles)
  
  upper =  quantiles[2] + 1.5*interQuantileRange  
  lower = quantiles[1] - 1.5*interQuantileRange 
  
  eliminated = subset(firstImputation, firstImputation$Miles > lower & firstImputation$Miles < upper)
  
  return(eliminated)
}
