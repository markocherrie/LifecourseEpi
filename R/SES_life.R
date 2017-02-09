#' Historical Area-level Socioeconomic Deprivation for Edinburgh
#'
#' Derives an estimate of area-level socioeconomic deprivation for any Edinburgh location from 1926-2015.
#' @param Lat has to be within Edinburgh boundary
#' @param Long has to be within Edinburgh boundary
#' @param Year has to be between 1926-2015
#' @param Polygon is the deprivation index shapefile downloaded from https://markcherrie.shinyapps.io/MMPgeodata/
#' @keywords life course, Edinburgh, historical index of deprivation
#' @export
#' @examples
#' SES_life(55.9533, -3.1883, "ME1", 1948, Polygon)
#'


# Polygon WILL BE SUPPLIED ON THE GEOPORTAL

# Function uses Latitude and Longitude of participant's address, the LBC code, and the Year (Year) they were there
SES_life= function(Lat, Long, Id, Year, Polygon){

  # make the points a spatial object
  coords = cbind(Long, Lat)
  sp = SpatialPoints(coords)
  proj4string(sp) = CRS('+proj=LongLat +datum=WGS84')
  Edinburgh_bng = spTransform(sp, CRS('+proj=tmerc +Lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'))

  # Get SES per decade
  if (Year>=1926 & Year<=1935) {
    Polygon <- spTransform(Polygon, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Polygon[41] , fn = NULL)
  } else if (Year>=1936 & Year<=1945) {
    Polygon <- spTransform(Polygon, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Polygon[42] , fn = NULL)
  } else if (Year>=1946 & Year<=1955) {
    Polygon <- spTransform(Polygon, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Polygon[43] , fn = NULL)
  } else if (Year>=1956 & Year<=1965) {
    Polygon <- spTransform(Polygon, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Polygon[44] , fn = NULL)
  } else if (Year>=1966 & Year<=1975) {
    Polygon <- spTransform(Polygon, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Polygon[45] , fn = NULL)
  } else if (Year>=1976 & Year<=1985) {
    Boundary1981 <- spTransform(Boundary1981, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Boundary1981[3] , fn = NULL)
  } else if (Year>=1986 & Year<=1995) {
    Boundary1991 <- spTransform(Boundary1991, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Boundary1991[4] , fn = NULL)
  } else if (Year>=1996 & Year<=2005) {
    Boundary2001 <- spTransform(Boundary2001, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Boundary2001[12] , fn = NULL)
  } else if (Year>=2006 & Year<=2015) {
    Boundary2011 <- spTransform(Boundary2011, proj4string(Edinburgh_bng))
    SES<-over(Edinburgh_bng , Boundary2011[20] , fn = NULL)
  } else
    SES<-NA

  # Data output and index creation
  SESexposure <-data.frame(Id, Year, SES)
  return(SESexposure)
}
