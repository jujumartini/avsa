# avsa
Autographer vs activpal Posture classifications

v1.0.0 
  -added files to repository

v1.1.0
  -created process_anno function
    -tells you now if a timestamp is missing
    -tells you when the on-off doesn't work either due to a incorrect timestamp or visit log entry
  -deleted img_sbs and img_onoff functions
  
Future Goals:

  -clean up process ap functions
    -merge into one function. Im thinking of having it add the correction factor if it is past a certain date
  