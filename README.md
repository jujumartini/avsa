# avsa
Autographer vs activpal Posture classifications

v1.0.0 
  -added files to repository

v1.1.0
  -created process_anno function
    -tells you now if a timestamp is missing
    -tells you when the on-off doesn't work either due to a incorrect timestamp or visit log entry
  -deleted img_sbs and img_onoff functions
  
v1.2.0
  -Added to process_ap function
    -takes into account DST from file and visit now
    -has two checks within it with informative error messages
  -moved loading on off log from 2_functions.R script to 1_load.R script since both functions have it do the same thing
  
Future Goals:

  -Clean up merge function
    -don't need a 60 sec epoch file
  -Clean up results function (creats sheet that will be used in analysis script. Ref results table v6 in NACSM 2020)
  -Clean up analysis script
  