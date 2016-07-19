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
 (cond ((vl-string-search "\\R17.1\\" ACADPROD)
        "5.0"
       )
       ;;2008
       ((vl-string-search "\\R17.2\\" ACADPROD)
        "6.0"
       )
       ;;2009
       ((vl-string-search "\\R18.0\\" ACADPROD)
        "7.0"
       )
       ;;2010
       ((vl-string-search "\\R18.1\\" ACADPROD)
        "8.0"
       )
       ;;2011
       ((vl-string-search "\\R18.2\\" ACADPROD)
        "9.0"
       )
       ;;2012
       ((vl-string-search "\\R19.0\\" ACADPROD)
        "10.0"
       )
       ;;2013
       ((vl-string-search "\\R19.1\\" ACADPROD)
        "10.3"
       )
       ;;2014
       ((vl-string-search "\\R20.0\\" ACADPROD)
        "10.4"
       )
       ;;2015
       ((vl-string-search "\\R20.1\\" ACADPROD)
        "10.5"
       )
       ;;2016
       ((vl-string-search "\\R21.0\\" ACADPROD)
        "11.0"
       )
       ;;2017
 )
)
