(in-package #:clim-clx)

(deftype clx-coordinate () '(signed-byte 16))
(deftype index () `(integer 0 #.array-dimension-limit))

(defconstant +clx-clip+
  (load-time-value (make-rectangle* #x-8000 #x-8000 #x7FFF #x7FFF)))

;;; FIXME it is undefined behavior when we provide (valid) coordinates that do
;;; not fall inside the drawable region. Experiments show that rendering works
;;; fine for rectangles and breaks for polygons. -- jd 2021-04-07

(defun invoke-with-clipped-line (cont tr x1 y1 x2 y2)
  (with-round-positions (tr x1 y1 x2 y2)
    (if (and (<= #x-8000 x1 #x7FFF) (<= #x-8000 y1 #x7FFF)
             (<= #x-8000 x2 #x7FFF) (<= #x-8000 y2 #x7FFF))
        (funcall cont x1 y1 x2 y2)
        (let* ((src-line (make-line* x1 y1 x2 y2))
               (dst-line (region-intersection +clx-clip+ src-line)))
          (unless (region-equal dst-line +nowhere+)
            (with-bounding-rectangle* (x1 y1 x2 y2) dst-line
              (funcall cont
                       (round-coordinate x1)
                       (round-coordinate y1)
                       (round-coordinate x2)
                       (round-coordinate y2))))))))

(defmacro with-clipped-line ((tr x1 y1 x2 y2) &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,x1 ,y1 ,x2 ,y2) ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-clipped-line (function ,cont) ,tr ,x1 ,y1 ,x2 ,y2))))

(defun invoke-with-clipped-rect (cont tr x1 y1 x2 y2)
  (with-round-positions (tr x1 y1 x2 y2)
    (let ((x1 (clamp (min x1 x2) #x-8000 #x7FFF))
          (y1 (clamp (min y1 y2) #x-8000 #x7FFF))
          (x2 (clamp (max x1 x2) #x-8000 #x7FFF))
          (y2 (clamp (max y1 y2) #x-8000 #x7FFF)))
      (unless (or (coordinate= x1 x2)
                  (coordinate= y1 y2))
        (funcall cont x1 y1 x2 y2)))))

(defmacro with-clipped-rect ((tr x1 y1 x2 y2) &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,x1 ,y1 ,x2 ,y2) ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-clipped-rect (function ,cont) ,tr ,x1 ,y1 ,x2 ,y2))))

(defun invoke-with-clipped-poly (cont tr coords closed)
  (with-round-coordinates (tr coords)
    (if (every (lambda (n) (<= #x-8000 n #x7fff)) coords)
        (if (null closed)
            (funcall cont coords)
            (funcall cont (concatenate 'vector coords
                                       (vector (elt coords 0) (elt coords 1)))))
        (let* ((src-poly (make-polygon* coords))
               (dst-poly (region-intersection +clx-clip+ src-poly)))
          (flet ((disassemble-polygon (polygon)
                   (climi::collect (result)
                     (map-over-polygon-coordinates
                      (lambda (x y)
                        (result (round-coordinate x)
                                (round-coordinate y)))
                      polygon)
                     (when closed
                       (let ((point (elt (polygon-points polygon) 0)))
                         (result (round-coordinate (point-x point))
                                 (round-coordinate (point-y point)))))
                     (result))))
            (etypecase dst-poly
              (polygon
               (funcall cont (disassemble-polygon dst-poly)))
              (standard-region-union
               (dolist (r (region-set-regions dst-poly :normalize t))
                 (funcall cont (disassemble-polygon r))))
              (climi::nowhere-region
               nil)))))))

(defmacro with-clipped-poly ((tr coords closed) &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,coords) ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-clipped-poly (function ,cont) ,tr ,coords ,closed))))
