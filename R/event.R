

add_timeout_event_to_entity<-function(entity_obj, duration){
  add_timeout_event_(entity_obj@pointer, duration)
  return(entity_obj)
}

add_seize_event_to_entity<-function(entity_obj, resource_name, amount){
  add_seize_event_(entity_obj@pointer, resource_name, amount)
  return(entity_obj)
}

add_release_event_to_entity<-function(entity_obj, resource_name, amount){
  add_release_event_(entity_obj@pointer, resource_name, amount)
  return(entity_obj)
}

add_skip_event_to_entity<-function(entity_obj, number_to_skip){
  add_skip_event_(entity_obj@pointer, number_to_skip)
  return(entity_obj)
}