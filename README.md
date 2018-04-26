# adsb-R-project
R package to get/evalutate/plot data from adsbexchange.com

## R-project-file:

#### Use of get_live_data:
 - point: numeric of length 2 or Name of a city
 - radius: radius around in km
 
 -> get data for the specific searchparameters
 (see http://www.virtualradarserver.co.uk/Documentation/Formats/AircraftList.aspx)

=> Possible options to implement: search for country and other variables

#### Use of n_planes and n_planes2:
- x: dataset from adsbexchange
- points/radius: see "Use of get_live_data"

-> see how many planes are in the specific area

#### Use of plot_planes:
- point/radius: see "Use of get_live_data"

-> Show the position of the planes on a map via leaflet and give additional information.
