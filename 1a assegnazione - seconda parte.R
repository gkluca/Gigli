library(httr)

geocode <- function(address){
  parameters = list(q=address,addressdetails=1,format="json")
  endpointUrl = "http://nominatim.openstreetmap.org/search"
  result_g = GET(endpointUrl, query=parameters)
  addressList = content(result_g,as = "parsed")
  return(c(addressList[[1]]$lat, addressList[[1]]$lon))
}

address<-"1600 Pennsylvania Avenue, Washington, DC"
response = geocode(address)


weather_at_address = function (address) {
  response = geocode(address)
  parameters = list(lat=response[1],lon=response[2],FcstType="json")
  endpointUrl = "http://forecast.weather.gov/MapClick.php"
  result_w = GET(endpointUrl, query=parameters)
  weatherList = content(result_w,as = "parsed")
  return(data.frame(Meteo=weatherList$currentobservation$Weather,Temp=weatherList$currentobservation$Temp))
}
  
weather = weather_at_address(address)

