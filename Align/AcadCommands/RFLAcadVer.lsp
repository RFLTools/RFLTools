;
;
;     Program written by Robert Livingston
;
;
(defun RFL:ACADVER (/ ACADPROD)
 (if (= nil vlax-machine-product-key)
  (setq ACADPROD (vlax-product-key))
  (setq ACADPROD (vlax-machine-product-key))
 )
 (cond ;;2008
       ((vl-string-search "\\R17.1\\" ACADPROD)
        "5.0"
       )
       
       ;;2009
       ((vl-string-search "\\R17.2\\" ACADPROD)
        "6.0"
       )
       
       ;;2010
       ((vl-string-search "\\R18.0\\" ACADPROD)
        "7.0"
       )
       
       ;;2011
       ((vl-string-search "\\R18.1\\" ACADPROD)
        "8.0"
       )
       
       ;;2012
       ((vl-string-search "\\R18.2\\" ACADPROD)
        "9.0"
       )
       
       ;;2013
       ((vl-string-search "\\R19.0\\" ACADPROD)
        "10.0"
       )
       
       ;;2014
       ((vl-string-search "\\R19.1\\" ACADPROD)
        "10.3"
       )
       
       ;;2015
       ((vl-string-search "\\R20.0\\" ACADPROD)
        "10.4"
       )
       
       ;;2016
       ((vl-string-search "\\R20.1\\" ACADPROD)
        "10.5"
       )
       
       ;;2017
       ((vl-string-search "\\R21.0\\" ACADPROD)
        "11.0"
       )
       
       ;;2018
       ((vl-string-search "\\R22.0\\" ACADPROD)
        "12.0"
       )
       
       ;;2019
       ((vl-string-search "\\R23.0\\" ACADPROD)
        "13.0"
       )
       
 )
)
