# lib for shiny dashboard: UnitProcess
# Updated: 220727



# 01 Read tool logs -------------------------------------------------------

# Convert CurrentData ("mm/dd/yyyy ...")
ToolLog_to.Date <- function(x){
  x %>% 
    str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>% 
    mdy()
}


# read tool log data
## read smartly various file types
## add rowID and DateTime from CurrentDate and CurrentTime
ToolLog_read.log <- function(x){
  
  # get file extension
  type <- tools::file_ext(x) %>% tolower()
  
  # read file
  df <- switch (
    type,
    csv = read_csv(x),
    xls = read_excel(x),
    xlsx = read_excel(x),
    xlsm = read_excel(x),
    NULL
  )
  
  # read and prep the log data
  if(!is.null(df)){
    df <- df %>% 
      # ensure proper date and time format
      mutate(
        Date = ToolLog_to.Date(CurrentDate),
        Time = as_hms(CurrentTime),
        DateTime = paste(Date, Time) %>% ymd_hms()
      ) %>% 
      arrange(DateTime) %>% 
      mutate(
        rowID = row_number()
      ) %>% 
      select(-Date, -Time) %>% 
      # change column positions
      relocate(rowID, DateTime)
  }
  
}



# 02 Track change of column -----------------------------------------------

# Track changes of a specific column in a dataframe
## the dataframe must have rowID and DateTime columns
## drop na rows instead of imputation using fill()
ToolLog_track.change <- function(df_raw, col.var){
  
  # define new column name
  col.name <-  "First row of " %&% quo_name(enquo(col.var))
  
  df_ref <- df_raw %>% 
    select(rowID, DateTime, {{col.var}}, Batch) %>% 
    #fill({{col.var}}, .direction = "down") %>% 
    drop_na({{col.var}}) %>% 
    mutate(
      col.lag = lag({{col.var}}),
      col.change = {{col.var}} != col.lag
    ) %>% 
    # keep the 1st row and rows with col.change being TRUE
    filter(rowID == 1 | col.change == TRUE) %>% 
    mutate(rowID.last = lead(rowID) - 1) %>% 
    select(rowID, rowID.last, DateTime, {{col.var}}, Batch) %>% 
    rename(!!(col.name) := {{col.var}})
  
  #df_ref$rowID.last[nrow(df_ref)] = max(df_raw$rowID)
  df_ref[nrow(df_ref), "rowID.last"] = max(df_raw$rowID)
  
  return(df_ref)
}



# 03 Extract a run and prep data for plot ---------------------------------


# extract a run
## specify which rows to extract and how to align the process time
ToolLog_extract.align <- function(df_raw, row.start, row.end, row.align, time.align = 1000) {

  offset <- df_raw$DateTime[row.align] 
  
  row.end <- min(nrow(df_raw), row.end)

  df <- df_raw %>% 
    slice(row.start:row.end) %>% 
    drop_na(CurrentState) %>% 
    mutate(`Process Time [s]` = DateTime - offset + time.align) %>% 
    relocate(File, rowID, `Process Time [s]`, DateTime)
  
  return(df)
}



# 04 Prep data ------------------------------------------------------------

# convert data for plotting into a long form
## filter range of `Process Time [s]`
## keep File, `Process Time [s]` and variables to plot
ToolLog_prep.data <- function(df_raw, vars.y, range.x){
  
  if(missing(range.x)) range.x <- range(df_raw$`Process Time [s]`)
  
  df <- df_raw %>% 
    filter(between(`Process Time [s]`, min(range.x), max(range.x))) %>% 
    mutate(File = tools::file_path_sans_ext(basename(File))) %>% 
    # keep columns File, `Process Time [s]` and vars.y
    select(File, `Process Time [s]`, all_of(vars.y)) %>% 
    #select(File, rowID, `Process Time [s]`, DateTime, CurrentState, all_of(vars.y)) %>% 
    pivot_longer(all_of(vars.y), names_to = "Input Type", values_to = "Value")
  
  return(df)
}


# join 4 sets of data
ToolLog_prep.data.panel <- function(df_raw, vars.y1, vars.y2, vars.y3, vars.y4, range.x){
  
  if(missing(range.x)) range.x <- range(df_raw$`Process Time [s]`)
  
  df1 <- ToolLog_prep.data(df_raw, vars.y1, range.x) %>% mutate(panel = 1)
  df2 <- ToolLog_prep.data(df_raw, vars.y2, range.x) %>% mutate(panel = 2)
  df3 <- ToolLog_prep.data(df_raw, vars.y3, range.x) %>% mutate(panel = 3)
  df4 <- ToolLog_prep.data(df_raw, vars.y4, range.x) %>% mutate(panel = 4)
  
  df <- df1 %>% 
    full_join(df2) %>% 
    full_join(df3) %>% 
    full_join(df4) %>% 
    mutate(panel = factor(panel, levels = 4:1))
  
  return(df)
}



# 05 Plot data ------------------------------------------------------------

# plot log data in one panel
ToolLog_plot <- function(df_raw, vars.y, range.x, range.y, lab.y){
  
  if(missing(range.x)) range.x <- range(df_raw$`Process Time [s]`)
  if(missing(lab.y)) lab.y <- paste(vars.y, collapse = ", ")
  
  p1 <- df_raw %>% 
    ToolLog_prep.data(vars.y, range.x) %>% 
    ggplot(aes(x = `Process Time [s]`, y = Value, color = File)) +
    geom_line(aes(linetype = `Input Type`)) +
    ylab(lab.y)
  
  if(!missing(range.y)) {
    p1 <- p1 + ylim(min(range.y), max(range.y))
  }
  
  return(p1)
}


# label facets
ToolLog_label.panel <- function(vars.y1, vars.y2, vars.y3, vars.y4, lab.y1, lab.y2, lab.y3, lab.y4) {
  
  if(missing(lab.y1)) lab.y1 <- paste(vars.y1, collapse = ", ")
  if(missing(lab.y2)) lab.y2 <- paste(vars.y2, collapse = ", ")
  if(missing(lab.y3)) lab.y3 <- paste(vars.y3, collapse = ", ")
  if(missing(lab.y4)) lab.y4 <- paste(vars.y4, collapse = ", ")
  
  panel.labs <- c(lab.y1, lab.y2, lab.y3, lab.y4)
  names(panel.labs) <- 1:4
  
  return(panel.labs)
}


# plot log data in 4 panels
ToolLog_plot.panel <- function(
  df_raw, 
  vars.y1, vars.y2, vars.y3, vars.y4, 
  lab.y1, lab.y2, lab.y3, lab.y4, 
  range.x, solid.line = F){
  
  if(missing(range.x)) range.x <- range(df_raw$`Process Time [s]`)
  
  # number of panels depends on if there is vars.y4
  if(missing(vars.y4)) {
    vars.y4 <- vars.y3
  }
  
  if(identical(unique(vars.y3), unique(vars.y4))){
    panels <- 3
  } else {
    panels <- 4
  }
  
  # facet label names
  panel.labs <- ToolLog_label.panel(vars.y1, vars.y2, vars.y3, vars.y4, lab.y1, lab.y2, lab.y3, lab.y4)
  
  p1 <- df_raw %>% 
    ToolLog_prep.data.panel(vars.y1, vars.y2, vars.y3, vars.y4, range.x) %>% 
    filter(panel %in% 1:panels) %>% 
    mutate(group = File %&% `Input Type`) %>% 
    ggplot(aes(x = `Process Time [s]`, y = Value, color = File))
  
  if(solid.line == T) {
    p1 <- p1 + geom_line(aes(group = group))
  } else {
    p1 <- p1 + geom_line(aes(linetype = `Input Type`))
  }
    
  p1 <- p1 + 
    facet_grid(
      panel ~., 
      scales = "free_y", 
      labeller = labeller(panel = panel.labs),
      switch = "y"
    ) +
    coord_cartesian(xlim = range.x) +
    theme(axis.title.y = element_blank())
  
  return(p1)
}









