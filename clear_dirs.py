
import shutil
import os
import stat

def walk_dtree(top_path):
  print top_path
  for f in os.listdir(top_path):
    pathname = os.path.join(top_path,f)
    print "--"+pathname
    mode=os.stat(pathname).st_mode
    if stat.S_ISDIR(mode):
    	if (f==".svn"):
    	   print "x-"+pathname
    	   shutil.rmtree(pathname)
    	   return
    	if (f==".git"):
    	   print "x+"+pathname
    	   shutil.rmtree(pathname)
           return
        walk_dtree(pathname)
             		
path=os.getcwd()
walk_dtree(path)

