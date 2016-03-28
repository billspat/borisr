
read_boris <- function(boris_file) {
  b = readLines(boris_file,  warn = "F")
  dat = fromJSON(b)
  return(dat)
}

get_obslist<- function(dat){
  # check if dat is boris data
  names(dat$observations)
}

#WIP
summary.events <- function(dat){
  # this function will summarize ev counts and states
  # create an event first ev.df = get_events(dat,obs_name)
  # this is not cool; rather than taking ALL events from data, should accept just a
  # subset of events data first, using %>%
  all_events.df = get_all_events(dat)
  # all_events.df = merge(all_events.df, all_ind_vars.df, by.x = "obs_id", by.y = "obs_id", all.x = TRUE)
}

##WIP
get_all_events <- function(dat, obs.list= NULL){

  if (is.null(obs.list)) {
    obs.list = get_obslist(dat)
  }

  get_all_events = Vectorize(get_events, "obs_number")
  all_events.list = get_all_events(dat, obs.list)
  all_events.df = data.frame(ldply(all_events.list, data.frame, .id="obs_id"), strings_as_factors=FALSE)
  # make the get_ind_vars work for a a vector of obs, and get them all
  get_all_vars  = Vectorize(get_ind_vars, "obs_number")
  all_ind_vars.list = get_all_vars(dat, obs.list)
  all_ind_vars.df = data.frame(ldply(all_ind_vars.list, data.frame, .id="obs_id"), strings_as_factors=FALSE)

  return(all_events.df)

}

get_ind_vars <- function(dat, obs_number){
  obs = dat$observations[[obs_number]]
  data.frame(obs_id = obs_number, obs$independent_variables, strings_as_factors = FALSE)
}

get_events <- function(dat,obs_number) {
  # extract the events to a data frame for this one trial by trial name or index number
  # to do: add a column of the obs_number for later grouping
  # to do: add time offset to all times?
  print(obs_number)
  columns = c("obs_id", "time","ID","event","subevent","unknown")

  obs = dat$observations[[obs_number]]

  if (length(obs$events) == 0) {
    # no events here!  make a blank data.frame
   x = cbind(rep("", length(columns)))
   names(x) = columns
   return(x)
  }

  event_count = nrow(obs$events)
  column_of_obs = rep(obs_number,nrow(obs$events))
  obs.df = data.frame(column_of_obs, obs$events, stringsAsFactors = FALSE)

  names(obs.df) = c("obs_id", "time","ID","event","subevent","unknown")
  obs.df = split_subevents(obs.df)
  return(obs.df)
}

get_ethogram <- function(dat){
  ## get the ethogram entered into boris program.  For use with summaries
  ## subevents are not split up, see get_event_typs
  eth.list = dat$behaviors_conf
  eth.df = do.call(rbind.data.frame, eth.list)
  eth.df = data.frame(lapply(eth.df, as.character), stringsAsFactors=FALSE)
  return(eth.df)
}


get_event_types <- function(dat,evtype="State event"){
  # this take events and subevents and creates rows for each sub-event
  # TO DO : VECTORIZE the 'by' loop in here, which is an ugly hack

  # get subset of ethogram
  eth.df = get_ethogram(dat)
  ev.df = with(eth.df[eth.df$type==evtype,], data.frame(code, modifiers,stringsAsFactors = FALSE ))

  ## for each row, split the submods and make new rows; result is a list of df's
  x = by(ev.df, seq_len(nrow(ev.df)), function(row) data.frame(code = as.vector(row$code), "modifier" = strsplitrows(row$modifiers)))

  # combine those dfs into single df
  return(do.call(rbind.data.frame,x))
}



split_subevents <-function(evdata, seperator = "|", cnames = c("subev1", "subev2")) {
  # yikes!  spliting a column of strings is not simple
  # to do : make this more generic for any column, not just subevents
  subevents = evdata$subevent

  # first have to split the strings, but get a list of vectors back
  # have to use the "fixed" function because if seperator is a regular expression special
  #   character, like |, need to ignore that
  subev.list = str_split(subevents, fixed(seperator))

  # apply function above to entire list
  subev.list = lapply(subev.list,make_two)

  # convert list of vectors to two column matrix
  subev.matrix = do.call(rbind, lapply(subev.list, rbind))

  # add names from the function arguments to these two columns
  colnames(subev.matrix) = cnames

  # convert THAT To the data frame of two columns
  subev.df = data.frame(subev.matrix, stringsAsFactors = FALSE)

  # add these two columns to original data
  return(data.frame(evdata,subev.df))

}

##### utilities
# convert comma list to rows
strsplitrows <-function(str) {
  if (typeof(str) != "character" || nchar(str)==0) return(" ")
  return( unlist(strsplit(str, ",")) )
}

# function to make two strings out of one or two
# these data sometimes have two, and sometimes one with no seperator
make_two <- function(v){
  if (length(v) < 2) {v[2] = ""}
  return(c(v[1],v[2]))
}
