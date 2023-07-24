This project uses machine learning techniques to study meteor lightcurves that have been recorded by the Armagh observatory.

People involved in the project: 
* Apostolos Christou
* Lucas Mandl 
* Andreas Windisch


Which facility? The AOP double station network


In July 2005, AOP (then Armagh Observatory) began operating a meteor detector system (‘Armagh Station’, see Atreya & Christou, 2008 for a desciption of the station setup).The system, which operated continuously until Dec 2019, consisted of three Watec WAT−902DM2s video cameras, hereafter referred to as “Cam−1”, “Cam−2” and “Cam−3”. Cam−1 was equipped with medium angle (50 deg) optics while Cam−2 and Cam−3 have wide angle (90 deg) optics. Cam−1 carried out simultaneous observations with a similar camera (“Cam−4”, FOV~50 deg) set up by amateur astronomer Robert Cobain in the town of Bangor, Northern Ireland (“Bangor Station”)    some ENE of Armagh, thus forming a double station network with a baseline of 73 km.

The exact locations of the two stations were 

Armagh Station:      6d 38m 59s W,    54d21m 11s N,    Alt 65 m
Bangor                                  5d 37m 28s W,    54d 39m 08s N, Alt 30 m

To summarise, cam-1 and cam-4 form a double station network between armagh and bangor, while Cams-2 and -3 are co-located with Cam-1 at armagh. Many meteors are captured simultaneously by the armagh and bangor stations; for these events it is possible to reconstruct the atmospheric trajectory. These double-station meteors can be identified as 2 or more records from different cameras obtained within a few seconds of each other, where the record filename is time-stamped to the nearest second.

The type of scientific information sought for each meteor are

LIGHTCURVE, TRAJECTORY AND ORBIT

Lightcurve refers to the brightness of a meteor as a function of time. Brightness measurements may be obtained at discrete times by carrying out photometry on the video data.

Trajectory refers to the meteor’s path in the atmosphere. It consists of the magnitude and direction of the meteor velocity vector where it it assumed that they are both constant through atmospheric flight.
 
Orbit refers to the heliocentric orbit of the meteor before it entered the Earth’s atmosphere. Ths is parameterised by a set of orbital elements, which derived from the meteor position and velocity vector at the time of atmospheric entry.    

Raw data for each meteor event is encapsulated in an Audio Video Interleave (.avi) file. The data were processed by the proprietary s/w UFOAnalyzerV2 (see https://sonotaco.com/soft/e_index.html) to extract the meteor data and time information from the avi files. This processing requires a number of ancillary files to accompany each event. Such ancillary data is available for events recorded from July 2011 onwards (note by ac: this to be checked). For data obtained prior to this data, the ancillary data needs to be reconstructed from the video, how to do this is currently under investigation.

The reduced data is arranged by camera (cam[x]), month ([january..december]) and year [20xx] in that order. For each meteor in a folder, there is one *A.xml file containing the photometric and astrometric information, as well as a *P.bmp image file. In addition, two summary files, ending in *R.xml file and M*.csv, are available in each folder. If multiple versions of these summary files are available, please use the most recent version. For details on data format, see p51-55 of UFOA2 Manual. A full dataset for the year 2015 has been uploaded on the gitlab repository, this to be followed by future uploads for years previous to this: 2014, 2013 and so on

Care must be taken when comparing lightcurves obtained from different cameras. A meteor may exit or enter the camera field, or may be observed behind cloud. In both cases the shape of the lightcurve will be affected. In addition, the detector pixels are saturated for very bright events. 
