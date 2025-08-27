
fix_med = function(samp) {
  return(samp/median(samp),na.rm=TRUE)
}

### One sample corrections

fix_gc = function(samp, preds) {
  return(samp/preds)
}

fix_gc_e = function(samp, preds, eps) {
  return((samp+eps)/(preds+eps))
}


### Two sample corrections


fix_2samp = function(tumor, healthy) {
  return((tumor)/(healthy))    
}

fix_2samp_e = function(tumor, healthy, eps) {
  return((tumor+eps)/(healthy+eps))
}
