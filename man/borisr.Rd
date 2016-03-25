\name{borisr}
\alias{borisr}
\title{boris file reader}
\usage{
borisr()
}
\description{
reads BORIS video project files into R data structures
}
\examples{
boris_file = 'data/examplevideotrial.boris'
boris.data = read_boris(boris_file)
all_obs_list = get_obslist(boris.data)
# just get first one
first_obs_name = all_obs_list[1]
# get some events, and split subevents too
boris.ev1 = get_events(boris.data,first_obs_name)
}
