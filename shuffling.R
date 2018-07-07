shuffling <- function( m.sh, seed.sh ){
  temp.sh <- LLG( seed.sh, 1 )
  I.sh <- as.integer( temp.sh$sample * (m.sh - 1) + 1 )
  seed.sh <- temp.sh$seed
  return( list(sample = I.sh, seed = seed.sh ))
}