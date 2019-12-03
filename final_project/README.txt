This file will outline the R scripts contained in this project.

exploring_asos.R - Intended to determine the validity/integrity of the ASOS data.
interpolating_mrms.R - Intended to interpolate the traffic data down to the road segment level and write the output to csv.
prepareUniqueSegs.R - Intended to locate unique traffic segments and prepare a csv with unique lon/lat data for each segment
			in the road data set. This will be used in the interpolation of the wx data to the road segment level.
wx_traffic_analysis.R - This is the primary script used in this analysis. All conclusions and statistic about traffic and precip
			will be derived from this script.
traffic_characterization.R - This file will produce some basic information for the reports about the traffic speeds and the
				segment locations.


NOTE: The full traffic dataset cannot be provided here due to data privacy concerns.