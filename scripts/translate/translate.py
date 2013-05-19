#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""This script allow users to translate a string 
from one language to another with Google translate"""

import sys
import re
import urllib
import urllib2
import json

def print_params(data):
    """print parameters from list"""
    for val in data:
        if isinstance(val, basestring):
            print "\t " + val.encode("utf-8")

def main():
    """
    Usage:
        first arg - string to translate
        second arg - source lang
        third arg - target lang    
    Example:
        translate.py 'text to translate' en ru
        translate.py 'text to translate' ru en
    """
    
    url = "http://translate.google.com/translate_a/t?%s"
    list_of_params = {'client' : 't',  
                      'hl' : 'en', 
                      'multires' : '1', }    
    
    #all arguments given
    if len(sys.argv) == 4:
        list_of_params.update({'text' : sys.argv[1].encode("utf-8"),
                               'sl' : sys.argv[2].encode("utf-8"), 
                               'tl' : sys.argv[3].encode("utf-8") })

        request = urllib2.Request(url % urllib.urlencode(list_of_params), 
           headers={ 'User-Agent': 'Mozilla/5.0', 'Accept-Charset': 'utf-8' })
        res = urllib2.urlopen(request).read()

        fixed_json = re.sub(r',{2,}', ',', res).replace(',]', ']')    
        data = json.loads(fixed_json)
        
        #simple translation
        print "%s / %s / %s" % (data[0][0][0].encode("utf-8"), data[0][0][1].encode("utf-8"), 
                                data[0][0][2].encode("utf-8") or data[0][0][3].encode("utf-8"))
        
        #abbreviation
        if not isinstance(data[1], basestring):
            print data[1][0][0].encode("utf-8")
            print_params(data[1][0][1])
            
        #interjection    
        try:
            if not isinstance(data[1][1], basestring):
                print data[1][1][0].encode("utf-8")
                print_params(data[1][1][1])
        except Exception:
            print "no interjection" 
    else:
        print main.__doc__
        
if __name__ == '__main__':
    main()
