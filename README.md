# Corridor Use Behavior - All Tracks

MoveApps

Github repository: github.com/movestore/corridors_alltracks

## Description
Identification of corridor use behavior based on movement characteristics of the animal. Tracks are plotted on a single map, settings are applied to all tracks and corridors of single tracks can be selected or unselected.

## Documentation
This App is embedded in an shiny UI, enabling the user to interactively change the parameters. The corridor identification is based on the movement of the animals, corridors are defined as those areas in the track where movement is fast and parallel (see *"LaPoint et al. (2013) Animal Behavior, Cost-based Corridor Models, and Real Corridors. Landscape Ecology.*" for more information). What is considered as fast and parallel is user defined.
The corridor use behavior is calculated on each track separately. All tracks are displayed together. For the calculation of the corridors the user can define the proportion of speeds which are considered high enough to be a valid corridor point, and the proportion of the circular variance that is low enough to be a valid corridor point. Low values of the circular variance indicate that the segments are (near) parallel. Identifying the corridors might take some playing around with these two values. These settings affect all tracks simultaneously. To give different tracks different settings see the App "Corridor Use Behavior - Per Track".

Interpretation caution: the method sometimes identifies segments as corridors that probably aren't, which mostly consist of only a few segments. With visual inspection it normally is quite straight forward to distinguish in most cases the true corridors, as they consist of a large amount of segments identified as *corridor segments*. 

To help removing wrongly identified corridors, the settings "Cluster width" and "Segment number" can be adjusted. This will define a cluster of a minimum number of corridor segments that are found within a certain diameter. The 'cluster with' can be seen as the width of the corridor.

The method is highly sensitive to the length of the segments (i.e. resolution of the data). If the data have a high fix rate, with many short segments, finding parallel segments will be rather difficult. Therefore it is recommended to thin the track to a lower fix rate to ease finding regions with parallel segments. Experience shows that for many mammal species 15min fix rate seems to work well.

After any setting has been changed, the button "update!" must be clicked to make the change effective.

It is possible to select & unselect corridor corresponding to single tracks by clicking on the menu on the top right corner of the map. Only the locations of the select corridor clusters (`potential corridors - trackID`) get recorded in the added attribute "corridorBehavior" in the output data.

**CAUTION:** Calculations can take long. The higher the number of locations, the longer it takes to calculate. Be patient and the results will be shown when the calculation is done.


### Input data
move2_loc

### Output data
move2_loc

### Artefacts
none

### Settings
`Speed`: Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds).

`Parallelism`: Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances).

`Thin track to X mins`: This is specially recommended for high resolution tracks to ease finding regions with parallel segments. Default (=0) no thinning.

`Cluster width (m)`: All identified corridor segments that fall within a circle of the given diameter will be grouped as a corridor cluster. These clusters can than be selected or unselected in the menu on the top right corner of the map. Only the locations of the select corridor clusters get recorded in the added attribute "corridorBehavior" in the output data. Default is 500m (adjust value to what best fits the data).

`Segment number`: Minimum number of segments that will define a cluster. Clusters with fewer segments will be excluded. Default is 3.

`Segment number`: options:`Per track` / `Across tracks`. Select if the minimum number of segments have to be achieved by each track, or if segments of different tracks can be counted within one cluster. If e.g. "number of segments" is set to 3, with "per track" only corridors that have at least 3 segments within a track will be shown. If "across tracks" is selected, also corridors composed of 3 segments of 3 different tracks will be shown. 

`Update!`: after changing any setting use this button to update the calculation.

### Null or error handling
**Data**: For use in further Apps the input data set is returned with the addition of the column "corridorBehavior". Empty input will give an error.
