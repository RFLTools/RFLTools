;;---------------------=={ Block Preview }==------------------;;
;;                                                            ;;
;;  Displays a vector image of a block on a DCL image tile.   ;;
;;                                                            ;;
;;  The function should be called in place of a vector_image  ;;
;;  expression between start_image and end_image expressions. ;;
;;                                                            ;;
;;  Note: the start_image and end_image functions are not     ;;
;;  called by this function. This is to enable the developer  ;;
;;  to apply a background fill (or other image operation) to  ;;
;;  the image tile before applying the block vector graphic.  ;;
;;                                                            ;;
;;  The function will approximate entities composing the      ;;
;;  block definition with a list of linear vectors (where     ;;
;;  possible), and will scale and center the vectors to fit   ;;
;;  the given image tile definition.                          ;;
;;                                                            ;;
;;  The resultant pixel coordinate list will be cached in the ;;
;;  function definition to improve performance for repeated   ;;
;;  calls to the function.                                    ;;
;;                                                            ;;
;;  Note that there may be a delay when processing complex    ;;
;;  blocks, or blocks containing non-linear entities.         ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  key    -  DCL image tile key                              ;;
;;  block  -  Name of block to be displayed on image_tile     ;;
;;  margin -  Pixel margin for block preview image            ;;
;;------------------------------------------------------------;;
;;  Returns:  T if block is displayed successfully, else nil  ;;
;;------------------------------------------------------------;;

(defun-q LM:BlockPreview ( key block margin / _getcolour _getvectors _unique bn cache dy ec el en mi mx pl r1 r2 sc vc vl xt yt )
    (setq cache '( ))

    (defun _getcolour ( l / c )
        (cond
            (   (= 0 (setq c (cdr (assoc 62 l))))
                7
            )
            (   (or (null c) (= 256 c))
                (abs (cdr (assoc 62 (tblsearch "LAYER" (cdr (assoc 8 l))))))
            )
            (   c   )
        )
    )

    (defun _getvectors ( bn / ec el en pl rg vl )
        (if (setq en (tblobjname "BLOCK" bn))
            (while (setq en (entnext en))
                (setq el (entget en))
                (cond
                    (   (= 1 (cdr (assoc 60 el))))
                    (   (= "INSERT" (cdr (assoc 0 el)))
                        (setq rg (RefGeom en))
                        (setq vl
                            (append vl
                                (mapcar
                                    (function
                                        (lambda ( x )
                                            (append
                                                (mapcar '+ (mxv (car rg) (list (car   x) (cadr   x) 0.0)) '(0 0) (cadr rg))
                                                (mapcar '+ (mxv (car rg) (list (caddr x) (cadddr x) 0.0)) '(0 0) (cadr rg))
                                                (cddddr x)
                                            )
                                        )
                                    )
                                    (_getvectors (cdr (assoc 2 el)))
                                )
                            )
                        )
                    )
                    (   (setq pl (LM:Entity->PointList en))
                        (if (or (= "POINT" (cdr (assoc 0 el))) (vlax-curve-isclosed en))
                            (setq pl (cons (last pl) pl))
                        )
                        (setq ec (_getcolour el))
                        (setq vl
                            (append vl
                                (mapcar
                                    (function
                                        (lambda ( a b )
                                            (list (car a) (cadr a) (car b) (cadr b) ec)
                                        )
                                    )
                                    pl (cdr pl)
                                )
                            )
                        )
                    )
                )
            )
        )
        vl
    )

    (defun _unique ( l / a r )
        (while (setq a (car l))
            (setq r (cons a r)
                  l (vl-remove-if (function (lambda ( b ) (equal a b))) (cdr l))
            )
        )
        (reverse r)
    )
    
    (cond
        (   (or (< margin 0)
                (<= (setq xt (dimx_tile key)) (* 2 margin))
                (<= (setq yt (dimy_tile key)) (* 2 margin))
            )
            nil
        )
        (   (setq vl (assoc (strcase block) cache))
            (foreach x (cdr vl) (apply 'vector_image x))
            t
        )            
        (   (setq vl (_getvectors block))
            (setq mi (apply 'mapcar (cons 'min vl))
                  mx (apply 'mapcar (cons 'max vl))
                  mi (list (min (car mi) (caddr mi)) (min (cadr mi) (cadddr mi)))
                  mx (list (max (car mx) (caddr mx)) (max (cadr mx) (cadddr mx)))
                  r1 (/ (- (car  mx) (car  mi)) (- xt (* 2 margin)))
                  r2 (/ (- (cadr mx) (cadr mi)) (- yt (* 2 margin)))
            )
            (cond
                (   (and (equal r1 r2 1e-8) (equal r1 0.0 1e-8))
                    (setq sc 1.0
                          vc (mapcar '- mi (list (/ xt 2.0) (/ yt 2.0)))
                    )
                )
                (   (equal r1 r2 1e-8)
                    (setq sc r1
                          vc (mapcar '(lambda ( x ) (- x (* sc margin))) mi)
                    )
                )
                (   (< r1 r2)
                    (setq sc r2)
                    (setq vc
                        (list
                            (- (car  mi) (/ (- (* sc xt) (- (car mx) (car mi))) 2.0))
                            (- (cadr mi) (* sc margin))
                        )
                    )
                )
                (   t
                    (setq sc r1)
                    (setq vc
                        (list
                            (- (car  mi) (* sc margin))
                            (- (cadr mi) (/ (- (* sc yt) (- (cadr mx) (cadr mi))) 2.0))
                        )
                    )
                )
            )
            (setq vc (append vc vc))
            (   (setq LM:BlockPreview
                    (vl-list* '( key block margin )
                         (list 'setq 'cache
                             (list 'quote
                                 (cons
                                     (cons (strcase block)
                                         (_unique
                                             (mapcar
                                                 (function
                                                     (lambda ( a / x )
                                                         (setq x (mapcar '(lambda ( a b ) (fix (/ (- a b) sc))) a vc))
                                                         (list
                                                             (car x)
                                                             (- yt (cadr x))
                                                             (caddr x)
                                                             (- yt (cadddr x))
                                                             (last a)
                                                         )
                                                     )
                                                 )
                                                 vl
                                             )
                                         )
                                     )
                                     cache
                                 )
                             )
                         )
                         (cddr LM:BlockPreview)
                    )
                )
                key block margin
            )
        )
    )
)

;; RefGeom (gile)
;; Returns a list which first item is a 3x3 transformation matrix (rotation,
;; scales, normal) and second item the object insertion point in its parent
;; (xref, block or space)
;;
;; Argument : an ename

(defun RefGeom ( ename / elst ang norm mat )
    (setq elst (entget ename)
          ang  (cdr (assoc 50 elst))
          norm (cdr (assoc 210 elst))
    )
    (list
        (setq mat
            (mxm
                (mapcar '(lambda ( v ) (trans v 0 norm t))
                   '(
                        (1.0 0.0 0.0)
                        (0.0 1.0 0.0)
                        (0.0 0.0 1.0)
                    )
                )
                (mxm
                    (list
                        (list (cos ang) (- (sin ang)) 0.0)
                        (list (sin ang) (cos ang)     0.0)
                       '(0.0 0.0 1.0)
                    )
                    (list
                        (list (cdr (assoc 41 elst)) 0.0 0.0)
                        (list 0.0 (cdr (assoc 42 elst)) 0.0)
                        (list 0.0 0.0 (cdr (assoc 43 elst)))
                    )
                )
            )
        )
        (mapcar '- (trans (cdr (assoc 10 elst)) norm 0)
            (mxv mat (cdr (assoc 10 (tblsearch "BLOCK" (cdr (assoc 2 elst))))))
        )
    )
)

;;----------------=={ Entity to Point List }==----------------;;
;;                                                            ;;
;;  Returns a list of points describing or approximating the  ;;
;;  supplied entity, else nil if the entity is not supported. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ent - Entity for which to return Point List.              ;;
;;------------------------------------------------------------;;
;;  Returns:  List of Points describing/approximating entity  ;;
;;------------------------------------------------------------;;

(defun LM:Entity->PointList ( ent / der di1 di2 di3 elst fun inc lst par rad )
    (setq elst (entget ent))
    (cond
        (   (eq "POINT" (cdr (assoc 0 elst)))
            (list (cdr (assoc 10 elst)))
        )
        (   (eq "LINE" (cdr (assoc 0 elst)))
            (list (cdr (assoc 10 elst)) (cdr (assoc 11 elst)))
        )
        (   (member (cdr (assoc 0 elst)) '("CIRCLE" "ARC"))
            (setq di1 0.0
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
                  inc (/ di2 (1+ (fix (* 35.0 (/ di2 (cdr (assoc 40 elst)) (+ pi pi))))))
                  fun (if (vlax-curve-isclosed ent) < <=)
            )
            (while (fun di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      di1 (+ di1 inc)
                )
            )
            lst
        )
        (   (or (eq (cdr (assoc 0 elst)) "LWPOLYLINE")
                (and (eq (cdr (assoc 0 elst)) "POLYLINE") (zerop (logand (cdr (assoc 70 elst)) 80)))
            )
            (setq par 0)
            (repeat (fix (1+ (vlax-curve-getendparam ent)))
                (if (setq der (vlax-curve-getsecondderiv ent par))
                    (if (equal der '(0.0 0.0 0.0) 1e-8)
                        (setq lst (cons (vlax-curve-getpointatparam ent par) lst))
                        (if (setq rad (distance '(0.0 0.0) (vlax-curve-getfirstderiv ent par))
                                  di1 (vlax-curve-getdistatparam ent par)
                                  di2 (vlax-curve-getdistatparam ent (1+ par))
                            )
                            (progn
                                (setq inc (/ (- di2 di1) (1+ (fix (* 35.0 (/ (- di2 di1) rad (+ pi pi)))))))
                                (while (< di1 di2)
                                    (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                                          di1 (+ di1 inc)
                                    )
                                )
                            )
                        )
                    )
                )
                (setq par (1+ par))
            )
            (if (or (vlax-curve-isclosed ent) (equal '(0.0 0.0 0.0) der 1e-8))
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
        (   (eq (cdr (assoc 0 elst)) "ELLIPSE")
            (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
                  di3 (* di2 (/ (+ pi pi) (abs (- (vlax-curve-getendparam ent) (vlax-curve-getstartparam ent)))))
            )
            (while (< di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      der (distance '(0.0 0.0) (vlax-curve-getsecondderiv ent (vlax-curve-getparamatdist ent di1)))
                      di1 (+ di1 (/ di3 (1+ (fix (/ 35.0 (/ di3 der (+ pi pi)))))))
                )
            )
            (if (vlax-curve-isclosed ent)
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
        (   (eq (cdr (assoc 0 elst)) "SPLINE")
            (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
                  inc (/ di2 25.0)
            )
            (while (< di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                     ;der (/ (distance '(0.0 0.0) (vlax-curve-getsecondderiv ent (vlax-curve-getparamatdist ent di1))) inc)
                      di1 (+ di1 inc) ;(+ di1 (if (equal 0.0 der 1e-10) inc (min inc (/ 1.0 der (* 10. inc)))))
                )
            )
            (if (vlax-curve-isclosed ent)
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

(vl-load-com)
(princ)