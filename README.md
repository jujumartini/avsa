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
  
v1.3.0  
  -added merge function  
  -changed function messages to warnings so it tells you which file is incorrect and tells you at the end of running the
   script  

v1.4.0  
  -added analysis function  
    -gives two tables, one of times and one of percentages  
  -cleaned up all scripts  
    -moved old code to explore and took them out of 4_analysis, 3_clean, and 1_functions  
    -moved loading the on off log to 3_clean since it isnt needed for all scripts  
    
v2.0.0  
  -cleaned analysis functions  
  -incorported vroom function into reading and writing files  
  -made reading timestamps, on_off_log, summary files into their own functions  
  
v2.0.1
 -seperated posture table into a bias and misclassifcation table
 -created sections of code for outputting figures (graphs and tables)
  
Future Goals:  
  -find a way to have analysis function take into account if either annotation and ap_posture have only 1 posture
   and if they differ (reference noldus code for this) DONE  