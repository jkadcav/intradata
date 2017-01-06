ee <- new.env()
ee_meeting_events<-''
ee_meeting<-''
ee_meeting_id<-''

ee_event=''
ee_event_id=''

#' Retrieve event IDS
#'
#' @param meetingId meeting ID to use for retrival
#' @keywords events
#' @export
#' @examples
#' retrieve_events()
retrieve_events<-function(meetingId){
  meeting <- retrieve_intradata(meetingId)
  races<-sort(names(eval(parse(text=paste('meeting$data$events')))))
  master<-data.frame(matrix(NA,length(races),2))
  colnames(master)<-c('EventID','Race')
  master$Race<-races
  master$MeetingID<-meetingId
  a<-rep('event_id',nrow(master))
  master$EventID<-mapply(retrieve_event_info,master$MeetingID,master$Race,a)
  return(master)
}

#' Retrieves event info
#'
#' @param  meetingId meetingId as per DATALAB
#' @param race race number
#' @param attributeName event attribute name
#' @keywords retrieve event information
#' @export
#' @examples
#' retrieve_event_info(110137,1,'distance')
retrieve_event_info<-function(meetingId,race,attributeName){
  if(attributeName=='includeRace') attributeName<-paste('mtx_settings$analysis_json$values$excludeRace')

  meeting<-retrieve_intradata(meetingId)
  a<-eval(parse(text=paste('meeting$data$events$`',race,'`$',attributeName,sep="")))

  if(length(a)<1 & attributeName=='mtx_settings$analysis_json$values$excludeRace') return(1)
  else if(length(a)>1) return(a)
  else if(length(a)<1) return(NA)
  else if(a==TRUE) a<-0
  else if(a==FALSE) a<-1
  return(a)
}


#' Retrieves meet info
#'
#' @param  meetingId meetingId as per DATALAB
#' @param attributeName event attribute name
#' @keywords retrieve meet information
#' @export
#' @examples
#' retrieve_meet_info(110137,'venue_name')
retrieve_meet_info<-function(meetingId,attributeName){
  meeting<-retrieve_intradata(meetingId)
  a<-eval(parse(text=paste('meeting$data$',attributeName,sep="")))
  return(a)
}

#' Retrieves Intraday API for given meeting
#'
#' @param  meetingId meetingId as per DATALAB
#' @keywords Intraday API
#' @export
#' @examples
#' retrieve_intradata(110137)
retrieve_intradata<-function(meetId) {
  ee_event <- get("ee_event", envir = ee)
  ee_event_id <- get("ee_event_id", envir = ee)

  if( meetId==ee_event_id ) {
    event<-ee_event
  } else {
    api_response <- jsonlite::fromJSON(paste('http://staging.dw.xtradeiom.com/api/intraday/',meetId,sep=''))
    event = eval(parse(text=paste('api_response',sep="")))
    assign("ee_event", event, envir = ee)
    assign("ee_event_id", meetId, envir = ee)
  }
  return(event)
}


#' Retrieves specific runner info
#'
#' @param  meetingId meetingId as per DATALAB
#' @param race race number
#' @param  competitorId competitorId as per DATALAB
#' @param attributeName event attribute name
#' @keywords retrieve competitor information
#' @export
#' @examples
#' retrive_attribute(110137,1,11047184,'mtx')
retrieve_attribute<-function(meetingId,race,competitorId,attributeName){
  if(attributeName=='mtx') mtx_dist<-retrieve_event_info(meetingId,race,'mtx_distance')

  if(attributeName=='mtx') attributeName<-paste('mtx$`',mtx_dist,'`$matrix',sep="")

  events<-retrieve_intradata( meetingId )
  a<-paste('events$data$events$`',race,'`$event_competitors$`',competitorId,'`$',attributeName,sep="")
  positions<-eval(parse(text=a))
  if (length(positions)<1) return(NA)
  else return(positions)
}


#' Retrieve amount of runners in race
#'
#' @param  meetingId meetingId as per DATALAB
#' @param race race number
#' @keywords field size
#' @export
#' @examples
#' retrieve_field( 110137,1 )
retrieve_field<-function(meetingId, race ) {
  events <- retrieve_intradata(meetingId)
  a<-paste('events$data$events$`',race,'`$event_competitors',sep="")
  runners<-names(eval(parse(text=a)))
  field<-length(runners)
  return(field)
}


#' Retrieve event competitors
#'
#' @param meetingId DM meeting ID
#' @param race Event number
#' @keywords competitors
#' @export
#' @examples
#' retrieve_runners(110137,1)
retrieve_runners<-function(meetingId,race){
  events <- retrieve_intradata(meetingId)
  #events <- jsonlite::fromJSON(paste("http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/markets?event_number=",race,"&meeting_id=",meetingId,sep=""))
  a<-paste('events$`',race,'`$event_competitors',sep="")
  runners<-names(eval(parse(text=a)))
  print(runners)
 # meetids<-names(events$data$meetings)
  master<-data.frame(matrix(NA,length(runners),6))
  master[,2]<-as.numeric(runners)
  colnames(master)<-c('Course','CompID','Matrix','Race','MeetingID','Odds')
  master$Race<-race
 # master$MeetingID<-meetingId
#  eventId<-rep(eventId,nrow(master))
 # master$Scratched<-mapply(competitor::scratched,eventId,master$CompID)
  return(master)
}


