;;; svg.el --- SVG image creation functions -*- lexical-binding: t -*-

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;         Felix E. Klee <felix.klee@inka.de>
;;         Anand Tamariya <atamariya@gmail.com> 2021
;; Keywords: image
;; Version: 1.0
;; Package-Requires: ((emacs "25"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows creating SVG images in Emacs.  SVG images are
;; vector-based XML files, really, so you could create them directly
;; as XML.  However, that's really tedious, as there are some fiddly
;; bits.

;; In addition, the `svg-insert-image' function allows inserting an
;; SVG image into a buffer that's updated "on the fly" as you
;; add/alter elements to the image, which is useful when composing the
;; images.

;; Here are some usage examples:

;; Create the base image structure, add a gradient spec, and insert it
;; into the buffer:
;;
;;     (setq svg (ii/svg-create 800 800 :stroke "orange" :stroke-width 5))
;;     (ii/svg-gradient svg "gradient" 'linear '(0 . "red") '(100 . "blue"))
;;     (save-excursion (goto-char (point-max)) (svg-insert-image svg))

;; Then add various elements to the structure:
;;
;;     (svg-rectangle svg 100 100 500 500 :gradient "gradient" :id "rec1")
;;     (svg-circle svg 500 500 100 :id "circle1")
;;     (svg-ellipse svg 100 100 50 90 :stroke "red" :id "ellipse1")
;;     (svg-line svg 100 190 50 100 :id "line1" :stroke "yellow")
;;     (svg-polyline svg '((200 . 100) (500 . 450) (80 . 100))
;;                   :stroke "green" :id "poly1")
;;     (svg-polygon svg '((100 . 100) (200 . 150) (150 . 90))
;;                  :stroke "blue" :fill "red" :id "gon1")

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'dom)
(require 'subr-x)

(defun ii/svg-create (width height &rest args)
  "Create a new, empty SVG image with dimensions WIDTH x HEIGHT.
ARGS can be used to provide `stroke' and `stroke-width' parameters to
any further elements added."
  (dom-node 'svg
            `((width . ,width)
              (height . ,height)
              (version . "1.1")
              (xmlns . "http://www.w3.org/2000/svg")
              ,@(unless (plist-get args :xmlns:xlink)
                  '((xmlns:xlink . "http://www.w3.org/1999/xlink")))
              ,@(ii/svg--arguments nil args))))

(defun ii/svg-gradient (svg id type stops)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `radial'.
STOPS is a list of percentage/color pairs."
  (ii/svg--def
   svg
   (apply
    'dom-node
    (if (eq type 'linear)
        'linearGradient
      'radialGradient)
    `((id . ,id)
      (x1 . 0)
      (x2 . 0)
      (y1 . 0)
      (y2 . 1))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
                         (stop-color . ,(cdr stop)))))
     stops))))

(defun ii/svg-rectangle (svg x y width height &rest args)
  "Create a rectangle on SVG, starting at position X/Y, of WIDTH/HEIGHT.
ARGS is a plist of modifiers.  Possible values are

:stroke-width PIXELS   The line width.
:stroke-color COLOR    The line color.
:gradient ID           The gradient ID to use."
  (ii/svg--append
   svg
   (dom-node 'rect
             `((width . ,width)
               (height . ,height)
               (x . ,x)
               (y . ,y)
               ,@(ii/svg--arguments svg args)))))

(defun ii/svg-circle (svg x y radius &rest args)
  "Create a circle of RADIUS on SVG.
X/Y denote the center of the circle."
  (ii/svg--append
   svg
   (dom-node 'circle
             `((cx . ,x)
               (cy . ,y)
               (r . ,radius)
               ,@(ii/svg--arguments svg args)))))

(defun ii/svg-ellipse (svg x y x-radius y-radius &rest args)
  "Create an ellipse of X-RADIUS/Y-RADIUS on SVG.
X/Y denote the center of the ellipse."
  (ii/svg--append
   svg
   (dom-node 'ellipse
             `((cx . ,x)
               (cy . ,y)
               (rx . ,x-radius)
               (ry . ,y-radius)
               ,@(ii/svg--arguments svg args)))))

(defun ii/svg-line (svg x1 y1 x2 y2 &rest args)
  "Create a line starting in X1/Y1, ending at X2/Y2 on SVG."
  (ii/svg--append
   svg
   (dom-node 'line
             `((x1 . ,x1)
               (x2 . ,x2)
               (y1 . ,y1)
               (y2 . ,y2)
               ,@(ii/svg--arguments svg args)))))

(defun ii/svg-polyline (svg points &rest args)
  "Create a polyline going through POINTS on SVG.
POINTS is a list of x/y pairs."
  (ii/svg--append
   svg
   (dom-node
    'polyline
    `((points . ,(mapconcat (lambda (pair)
                              (format "%s,%s" (car pair) (cdr pair)))
                            points
                            " "))
      ,@(ii/svg--arguments svg args)))))

(defun ii/svg-polygon (svg points &rest args)
  "Create a polygon going through POINTS on SVG.
POINTS is a list of x/y pairs."
  (ii/svg--append
   svg
   (dom-node
    'polygon
    `((points . ,(mapconcat (lambda (pair)
                              (format "%s,%s" (car pair) (cdr pair)))
                            points
                            " "))
      ,@(ii/svg--arguments svg args)))))

(defun ii/svg-use (svg id &rest args)
  (ii/svg--append
   svg
   (dom-node
    'use
    `((href . ,(format "#%s" id))
      ,@(ii/svg--arguments svg args)))))

(defun ii/svg-anchor (svg href &optional title text &rest args)
  "Insert ANCHOR into the SVG structure."
  (ii/svg--append
   svg
   (dom-node
    'a
    `((xlink:href . ,(xml-escape-string href))
      (xlink:title . ,(if title (xml-escape-string title)))
      (text . ,(if text (xml-escape-string text)))
      ,@(ii/svg--arguments svg args)))))

(defun ii/svg-group (svg children &rest args)
  "Create a group element with list of CHILDREN."
  (ii/svg--append
   svg
   (apply 'dom-node
          'g
          `(,@(ii/svg--arguments svg args))
          children)))

(defun ii/svg-embed (svg image image-type datap &rest args)
  "Insert IMAGE into the SVG structure.
IMAGE should be a file name if DATAP is nil, and a binary string
otherwise.  IMAGE-TYPE should be a MIME image type, like
\"image/jpeg\" or the like."
  (ii/svg--append
   svg
   (dom-node
    'image
    `((xlink:href . ,(ii/svg--image-data image image-type datap))
      ,@(ii/svg--arguments svg args)))))

(defun ii/svg-embed-href (svg image &rest args)
  "Insert IMAGE into the SVG structure.
IMAGE should be a file name."
  (ii/svg--append
   svg
   (dom-node
    'image
    `((xlink:href . ,image)
      ,@(ii/svg--arguments svg args)))))

(defun ii/svg-text (svg text &rest args)
  "Add TEXT to SVG."
  (ii/svg--append
   svg
   (dom-node
    'text
    `(,@(ii/svg--arguments svg args))
    (ii/svg--encode-text text))))

(defun ii/svg-tspan (svg text &rest args)
  "Add TEXT to SVG."
  (ii/svg--append
   svg
   (dom-node
    'tspan
    `(,@(ii/svg--arguments svg args))
    (ii/svg--encode-text text))))

(defun ii/svg-animate (svg attr dur &rest args)
  "Add ANIMATE to SVG."
  (ii/svg--append
   svg
   (dom-node
    'animate
    `((attributeName . ,attr)
      (dur . ,dur)
      ,@(ii/svg--arguments svg args)))))

(defun ii/svg--encode-text (text)
  ;; Apparently the SVG renderer needs to have all non-ASCII
  ;; characters encoded, and only certain special characters.
  (if text
      (with-temp-buffer
        (insert text)
        (dolist (substitution '(("&" . "&amp;")
                                ("<" . "&lt;")
                                (">" . "&gt;")))
          (goto-char (point-min))
          (while (search-forward (car substitution) nil t)
            (replace-match (cdr substitution) t t nil)))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((char (following-char)))
            (if (< char 128)
                (forward-char 1)
              (delete-char 1)
              (insert "&#" (format "%d" char) ";"))))
        (buffer-string))))

(defun ii/svg--decode-text (text)
  ;; Apparently the SVG renderer needs to have all non-ASCII
  ;; characters encoded, and only certain special characters.
  (if (consp text)
      ;; tspan tag
      (setq text (car (dom-children text))))
  (with-temp-buffer
    (insert text)
    (dolist (substitution '(("&amp;" . "&")
                            ("&lt;" . "<")
                            ("&gt;" . ">")))
      (goto-char (point-min))
      (while (search-forward (car substitution) nil t)
        (replace-match (cdr substitution) t t nil)))
    (replace-regexp-in-string "&#\\([0-9]+\\);"
                              (lambda (a)
                                (string (string-to-number (match-string 1 a))))
                              (buffer-string))))

(defun ii/svg--append (svg node)
  ;; id is expected to be unique.
  ;; (let ((old (and (dom-attr node 'id)
  ;;       	  (dom-by-id svg
  ;;                            (concat "\\`" (regexp-quote (dom-attr node 'id))
  ;;                                    "\\'")))))
  ;;   (if old
  ;;       ;; FIXME: This was (dom-set-attributes old (dom-attributes node))
  ;;       ;; and got changed by commit f7ea7aa11f6211b5142bbcfc41c580d75485ca56
  ;;       ;; without any explanation.
  ;;       ;; (setcdr (car old) (cdr node))
  ;;       ;; Remove old node. New node might be a different type.
  ;;       (mapc (lambda (a)
  ;;                 (dom-remove-node svg a))
  ;;               old))
  ;;   (dom-append-child svg node))
  (let ((children (dom-children svg))
        (added nil))
    (if children
        (while (and children
                    (null added))
          (when (equal (dom-attr (car children) 'id)
                       (dom-attr node 'id))
            (setcar children node)
            (setq added t))
          (unless (or added (cdr children))
            (setcdr children (list node)))
          (setq children (cdr children)))
      (dom-append-child svg node)))
  ;; (ii/svg-possibly-update-image svg)
  node)

(defun ii/svg--image-data (image image-type datap)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if datap
        (insert image)
      (insert-file-contents image))
    (base64-encode-region (point-min) (point-max) t)
    (goto-char (point-min))
    (insert "data:" image-type ";base64,")
    (buffer-string)))

(defun ii/svg--arguments (svg args)
  (let ((stroke-width (or (plist-get args :stroke-width)
                          (dom-attr svg 'stroke-width)))
        (stroke-color (or (plist-get args :stroke-color)
                          (dom-attr svg 'stroke-color)))
        (fill-color (plist-get args :fill-color))
        attr)
    (when stroke-width
      (push (cons 'stroke-width stroke-width) attr))
    (when stroke-color
      (push (cons 'stroke stroke-color) attr))
    (when fill-color
      (push (cons 'fill fill-color) attr))
    (when (plist-get args :gradient)
      (setq attr
            (append
             ;; We need a way to specify the gradient direction here...
             `((x1 . 0)
               (x2 . 0)
               (y1 . 0)
               (y2 . 1)
               (fill . ,(format "url(#%s)"
                                (plist-get args :gradient))))
             attr)))
    (cl-loop for (key value) on args by #'cddr
             unless (memq key '(:stroke-color :stroke-width :gradient
                                              :fill-color))
             ;; Drop the leading colon.
             do (push (cons (intern (substring (symbol-name key) 1) obarray)
                            value)
                      attr))
    attr))

(defun ii/svg--def (svg def)
  (dom-append-child
   (or (dom-by-tag svg 'defs)
       (let ((node (dom-node 'defs)))
         (dom-add-child-before svg node)
         node))
   def)
  svg)

(defun ii/svg-string (svg)
  "Return a string representation of SVG object."
  (with-temp-buffer
    (ii/svg-print svg)
    (buffer-string)))

(defun ii/svg-image (svg &rest props)
  "Return an image object from SVG object.
PROPS is passed on to `create-image' as its PROPS list."
  (apply
   #'create-image
   (ii/svg-string svg)
   'svg t props))

(defun ii/svg-image-from-xml (xml-file-or-string &rest props)
  "Return an image object from SVG XML.
PROPS is passed on to `create-image' as its PROPS list."
  (apply
   #'create-image
   xml-file-or-string
   'svg (not (file-exists-p xml-file-or-string)) props))

(defvar ii/svg-keymap (let ((map (make-sparse-keymap)))
                        (define-key map [mouse-1] #'ii/svg-on-click)
                        map))

(defun ii/svg-insert-image (svg)
  "Insert SVG as an image at point.
If the SVG is later changed, the image will also be updated."
  (let ((image (ii/svg-image svg))
        (image-map ii/svg-keymap)
        (marker (point-marker)))
    (insert-image image)
    (dom-set-attribute svg :image marker)))

(defun ii/svg-possibly-update-image (svg)
  (let ((marker (dom-attr svg :image))
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        image map end)
    (when (and marker
               (buffer-live-p (marker-buffer marker)))
      (with-current-buffer (marker-buffer marker)
        (setq image (ii/svg-image svg)
              end  (1+ marker)
              map (ii/svg-image-map svg))
        (if (image-get-display-property)
            (setq marker (point-min-marker)
                  end (point-max-marker)))
        (image--set-property image :map map)
        (image--set-property image :pointer 'arrow)
        (put-text-property marker end 'display image)))))

(defun ii/svg-load (file)
  "Load SVG from file."
  (let (image)
    (with-temp-buffer
      (insert-file-contents file)
      (setq image (libxml-parse-xml-region (point-min) (point-max))))
    (setq image (ii/svg--scrub-image image))
    image))

(defun ii/svg-load-from-xml (xml)
  "Load SVG from XML."
  (let (image)
    (with-temp-buffer
      (insert xml)
      (setq image (libxml-parse-xml-region (point-min) (point-max))))
    (setq image (ii/svg--scrub-image image))
    image))

(defun ii/svg-print (dom)
  "Convert DOM into a string containing the xml representation."
  (when dom
    (if (stringp dom)
        (insert dom)
      (insert (format "<%s" (car dom)))
      (dolist (attr (nth 1 dom))
        ;; Ignore attributes that start with a colon.
        (let* ((k (car attr))
               (v (cdr attr))
               n j)
          (unless (or (keywordp k)
                      (null v))
            (insert (format " %s=\"" k)) ;; k is a symbol
            (if (numberp v)
                (insert (number-to-string v))
              (insert v)
              ;; (if (< (setq n (length v)) 80)
              ;;     (insert (cdr attr))
              ;;   ;; Split long line for performance
              ;;   (dotimes (i (1+ (setq n (/ n 80))))
              ;;     (setq j (* i 80))
              ;;     (if (> i 0) (insert "\n"))
              ;;     (insert (substring v j (if (< i n) (+ j 80)))))
              ;;   )
              )
            (insert "\"")
            )))
      (insert ">")
      (unless (string= (dom-attr dom 'xml:space) "preserve")
        (newline))
      (dolist (elem (nthcdr 2 dom))
        ;; Avoid adding extra space to text node
        ;; (insert " ")
        (ii/svg-print elem))
      (insert (format "</%s>\n" (car dom))))))

(defun ii/svg-remove (svg id)
  "Remove the element identified by ID from SVG."
  (let* ((node (car (dom-by-id
                     svg
                     (concat "\\`" (regexp-quote id)
                             "\\'")))))
    (when node (dom-remove-node svg node))))

;; Function body copied from `org-plist-delete' in Emacs 26.1.
(defun ii/svg--plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun ii/svg--path-command-symbol (command-symbol command-args)
  (let ((char (symbol-name command-symbol))
        (relative (if (plist-member command-args :relative)
                      (plist-get command-args :relative)
                    (plist-get command-args :default-relative))))
    (if relative (downcase char) (upcase char))))

(defun ii/svg--elliptical-arc-coordinates
    (rx ry x y &rest args)
  (list
   rx ry
   (or (plist-get args :x-axis-rotation) 0)
   (if (plist-get args :large-arc) 1 0)
   (if (plist-get args :sweep) 1 0)
   x y))

(defun ii/svg--elliptical-arc-command (coordinates-list &rest args)
  (cons
   (ii/svg--path-command-symbol 'a args)
   (ii/svg--list-to-str coordinates-list)))
;; (apply 'append
;;        (mapcar
;;         (lambda (coordinates)
;;           (apply 'ii/svg--elliptical-arc-coordinates
;;                  coordinates))
;;         coordinates-list))))

(defun ii/svg--moveto-command (coordinate-list &rest args)
  (cons
   (ii/svg--path-command-symbol 'm args)
   (ii/svg--list-to-str (list coordinate-list))))
;; (apply 'append
;;        (mapcar
;;         (lambda (coordinates)
;;           (list (car coordinates) (cdr coordinates)))
;;         coordinates-list))))

(defun ii/svg--closepath-command (&rest args)
  (list (ii/svg--path-command-symbol 'z args)))

(defun ii/svg--lineto-command (coordinates-list &rest args)
  (cons
   (ii/svg--path-command-symbol 'l args)
   (ii/svg--list-to-str coordinates-list)))
;; (apply 'append
;;        (mapcar
;;         (lambda (coordinates)
;;           (list (car coordinates) (cdr coordinates)))
;;         coordinates-list))))

(defun ii/svg--horizontal-lineto-command (length &rest args)
  (cons
   (ii/svg--path-command-symbol 'h args)
   (list (format "%s" length))))

(defun ii/svg--vertical-lineto-command (length &rest args)
  (cons
   (ii/svg--path-command-symbol 'v args)
   (list (format "%s" length))))

(defun ii/svg--curveto-command (coordinates-list &rest args)
  (cons
   (ii/svg--path-command-symbol 'c args)
   (ii/svg--list-to-str coordinates-list)))
;; (apply 'append coordinates-list)))

(defun ii/svg--smooth-curveto-command (coordinates-list &rest args)
  (cons
   (ii/svg--path-command-symbol 's args)
   (ii/svg--list-to-str coordinates-list)))
;; (apply 'append coordinates-list)))

(defun ii/svg--quadratic-bezier-curveto-command (coordinates-list
                                                 &rest args)
  (cons
   (ii/svg--path-command-symbol 'q args)
   (ii/svg--list-to-str coordinates-list)))
;; (apply 'append coordinates-list)))

(defun ii/svg--smooth-quadratic-bezier-curveto-command (coordinates-list
                                                        &rest args)
  (cons
   (ii/svg--path-command-symbol 't args)
   (ii/svg--list-to-str coordinates-list)))

(defun ii/svg--list-to-str (coordinates)
  (cl-loop for a in coordinates
           collect (format "%s,%s" (car a) (cdr a))))

(defun ii/svg--eval-path-command (command default-relative)
  (cl-letf
      (((symbol-function 'moveto) #'ii/svg--moveto-command)
       ((symbol-function 'closepath) #'ii/svg--closepath-command)
       ((symbol-function 'lineto) #'ii/svg--lineto-command)
       ((symbol-function 'horizontal-lineto)
        #'ii/svg--horizontal-lineto-command)
       ((symbol-function 'vertical-lineto)
        #'ii/svg--vertical-lineto-command)
       ((symbol-function 'curveto) #'ii/svg--curveto-command)
       ((symbol-function 'smooth-curveto)
        #'ii/svg--smooth-curveto-command)
       ((symbol-function 'quadratic-bezier-curveto)
        #'ii/svg--quadratic-bezier-curveto-command)
       ((symbol-function 'smooth-quadratic-bezier-curveto)
        #'ii/svg--smooth-quadratic-bezier-curveto-command)
       ((symbol-function 'elliptical-arc)
        #'ii/svg--elliptical-arc-command)
       (extended-command (append command (list :default-relative
                                               default-relative))))
    ;; (mapconcat 'prin1-to-string (apply extended-command) " ")
    (apply extended-command)
    ))

(defun ii/svg-path (svg commands &rest args)
  "Add the outline of a shape to SVG according to COMMANDS.
Coordinates by default are absolute.  ARGS is a plist of
modifiers.  If :relative is t, then coordinates are relative to
the last position, or -- initially -- to the origin."
  (let* ((default-relative (plist-get args :relative))
         (stripped-args (ii/svg--plist-delete args :relative))
         (d (mapconcat 'identity
                       (apply 'append
                              (mapcar
                               (lambda (command)
                                 (ii/svg--eval-path-command command
                                                            default-relative))
                               commands)) " ")))
    (ii/svg--append
     svg
     (dom-node 'path
               `((d . ,d)
                 ,@(ii/svg--arguments svg stripped-args))))))

(defun ii/svg-clip-path (svg &rest args)
  "Add a clipping path to SVG, where ARGS is a plist of modifiers.
If applied to a shape via the :clip-path property, parts of that
shape which lie outside of the clipping path are not drawn."
  (let ((new-dom-node (dom-node 'clipPath
                                `(,@(ii/svg--arguments svg args)))))
    (ii/svg--append svg new-dom-node)
    new-dom-node))

(defun ii/svg-node (svg tag &rest args)
  "Add the custom node TAG to SVG."
  (let ((new-dom-node (dom-node tag
                                `(,@(ii/svg--arguments svg args)))))
    (ii/svg--append svg new-dom-node)
    new-dom-node))

(defun ii/svg-bbox (node &optional ref)
  "Return bounding box of node.
Return value is (TOP-LEFT-X TOP-LEFT-Y BOTTOM-RIGHT-X BOTTOM-RIGHT-Y)"
  (if (stringp node)
      nil
    (let* ((tag (dom-tag node))
           (transform (dom-attr node 'transform))
           (point nil)
           (res nil)
           (ox 0)
           (oy 0)
           x1 y1 x2 y2)
      ;; bbox without transformation
      (setq res
            (pcase tag
              ('line (setq x1 (dom-attr node 'x1)
                           y1 (dom-attr node 'y1)
                           x2 (dom-attr node 'x2)
                           y2 (dom-attr node 'y2))
                     (if (or (stringp x1) (stringp y1)
                             (stringp x2) (stringp y2))
                         ;; Ignoring percentange calculations for now
                         nil
                       (setq point (ii/svg--apply-transform 0 0 transform)
                             ox (car point)
                             oy (cdr point))
                       (list (+ ox x1)
                             (+ oy y1)
                             (+ ox x2)
                             (+ oy y2))))
              ('circle (setq x1 (dom-attr node 'cx)
                             y1 (dom-attr node 'cy)
                             point (ii/svg--apply-transform x1 y1 transform)
                             ox (car point)
                             oy (cdr point))
                       (list (- ox (dom-attr node 'r))
                             (- oy (dom-attr node 'r))
                             (+ ox (dom-attr node 'r))
                             (+ oy (dom-attr node 'r))))
              ('text (let* ((size (or (dom-attr node 'font-size) 15))
                            (font-size (if (stringp size)
                                           (string-to-number size)
                                         size))
                            (w 0)
                            (font (car (internal-char-font nil ?m)))
                            (ref-size (aref (font-info font) 2))
                            (glyph nil)
                            (asc 0)
                            (des 0)
                            (text (car (dom-children node)))
                            )
                       ;; x,y denotes bottom left corner on baseline
                       ;; size is 1em
                       (setq x1 (or (dom-attr node 'x) 0)
                             y1 (or (dom-attr node 'y) 0)
                             text (if text (ii/svg--decode-text text))
                             point (ii/svg--apply-transform x1 y1 transform)
                             ox (car point)
                             oy (cdr point))
                       (mapc (lambda (a)
                               (setq glyph (lgstring-glyph
                                            (composition-get-gstring
                                             0 1 font (string a))
                                            0)
                                     w (+ w (lglyph-width glyph))
                                     asc (max asc (lglyph-ascent glyph))
                                     des (max des (lglyph-descent glyph))
                                     )
                               ;; (message "%s" glyph)
                               )
                             text)
                       (setq w (* (/ font-size 1.0 ref-size) w)
                             asc (* (/ font-size 1.0 ref-size) asc)
                             des (* (/ font-size 1.0 ref-size) des))
                       ;; (message "%s %s" (dom-children node)
                       (list ox (- oy asc) (+ ox w) (+ oy des))))
              ((or 'rect 'image) (when (and (numberp (dom-attr node 'x))
                                            (numberp (dom-attr node 'y))
                                            (numberp (dom-attr node 'width))
                                            (numberp (dom-attr node 'height)))
                                   ;; Handle grid rectangle which doesn't have x,y
                                   ;; (let (x1 y1 cx cy)
                                   ;;   ;; If rotation is about (0,0), this will work.
                                   ;;   (setq x1 (dom-attr node 'x)
                                   ;;         y1 (dom-attr node 'y)
                                   ;;         cx (/ (dom-attr node 'width) 2.0)
                                   ;;         cy (/ (dom-attr node 'height) 2.0)
                                   ;;         point (ii/svg--apply-transform (+ x1 cx)
                                   ;;                                     (+ y1 cy)
                                   ;;                                     transform)
                                   ;;         ox (- (car point) cx)
                                   ;;         oy (- (cdr point) cy))
                                   ;;   (list ox oy
                                   ;;         (+ ox (dom-attr node 'width))
                                   ;;         (+ oy (dom-attr node 'height))))
                                   (setq ox (dom-attr node 'x)
                                         oy (dom-attr node 'y))
                                   (list ox oy
                                         (+ ox (dom-attr node 'width))
                                         (+ oy (dom-attr node 'height)))
                                   ))
              ('ellipse (setq x1 (dom-attr node 'cx)
                              y1 (dom-attr node 'cy)
                              point (ii/svg--apply-transform x1 y1 transform)
                              ox (car point)
                              oy (cdr point))
                        (list (- ox (dom-attr node 'rx))
                              (- oy (dom-attr node 'ry))
                              (+ ox (dom-attr node 'rx))
                              (+ oy (dom-attr node 'ry))))
              ((or 'polyline 'polygon)
               (let ((points (dom-attr node 'points))
                     point x y)
                 (mapc (lambda (a)
                         (setq point (split-string a ",")
                               x (string-to-number (car point))
                               y (string-to-number (cadr point))
                               x1 (if x1 (min x1 x) x)
                               y1 (if y1 (min y1 y) y)
                               x2 (if x2 (max x2 x) x)
                               y2 (if y2 (max y2 y) y)))
                       (split-string points))
                 (when x1
                   (list x1 y1 x2 y2))
                 ))
              ('path
               (let ((points (dom-attr node 'd))
                     point x y pos cmd rx ry dx dy sweep mult skip)
                 ;; (with-current-buffer "a"
                 (with-temp-buffer
                   (erase-buffer)
                   (insert points)
                   (setq points nil)
                   (goto-char (point-min))
                   (let* ((case-fold-search t)
                          (pos (point))
                          end neg)
                     (while (and (not (eobp))
                                 (re-search-forward "[MLCSQTAHVZ,]\\|\\(-?[0-9]*\\(\\.[0-9]+\\)?\\)"
                                                    nil t))
                       ;; (pp (buffer-substring pos (point)))
                       (skip-chars-forward " ")
                       (push (buffer-substring pos (point)) points)
                       ;; H384zM384 80
                       (setq pos (point))
                       ))
                   (setq points (nreverse points))
                   )
                 ;; (pp points)
                 (while points
                   (setq skip nil)
                   ;; (when (and (string-match "[[:alpha:]]" (car points))
                   ;;            (> (length (car points)) 1))
                   ;;   (setq cmd (match-string 0 (car points))
                   ;;         points (cons (substring (car points) 1) (cdr points))))
                   (if (member (upcase (car points))
                               '("M" "L" "C" "S" "Q" "T" "A" "H" "V" "Z"))
                       (setq cmd (car points)
                             points (cdr points)))

                   ;; (message "%s %s" cmd (car points))
                   (when (string= (upcase cmd) "C")
                     ;; Skip control points
                     (setq points (cddr points))
                     ;; (setq point (split-string (car points) ",")
                     ;;       point (cons (string-to-number (car point))
                     ;;                   (string-to-number (cadr point))))
                     (setq point (cons (string-to-number (pop points))
                                       (string-to-number (pop points))))
                     )

                   (when (string= (upcase cmd) "A")
                     ;; Skip control points
                     (setq rx (string-to-number (nth 0 points))
                           ry (string-to-number (nth 1 points)))
                     (cond ((> (length (nth 3 points)) 2)
                            (setq large-arc (aref (nth 3 points) 0)
                                  sweep     (aref (nth 3 points) 1)
                                  x (string-to-number (substring (nth 3 points) 2))
                                  points (nthcdr 4 points)
                                  y (string-to-number (pop points))))
                           ((= (length (nth 3 points)) 2)
                            (setq large-arc (aref (nth 3 points) 0)
                                  sweep     (aref (nth 3 points) 1)
                                  points (nthcdr 4 points)
                                  x (string-to-number (pop points))
                                  y (string-to-number (pop points))))
                           (t
                            (setq large-arc (string-to-number (nth 3 points))
                                  sweep (string-to-number (nth 4 points))
                                  points (nthcdr 5 points)
                                  x (string-to-number (pop points))
                                  y (string-to-number (pop points)))
                            ))
                     (if (string= cmd "A")
                         (setq dx (- x (car pos))
                               dy (- y (cdr pos)))
                       (setq dx x dy y
                             x (+ (car pos) dx)
                             y (+ (cdr pos) dy)))
                     (setq mult (if (= sweep 1) (signum dy) 1)
                           pos (cons x y)
                           skip t
                           x (+ (* mult rx) x)
                           y (- (* mult ry) y)))

                   (when (member (upcase cmd)
                                 '("M" "L" "C" "S" "Q" "T"))
                     ;; (setq point (split-string (car points) ","))
                     ;; (if (= (length point) 2)
                     ;;     (setq point (cons (string-to-number (car point))
                     ;;                       (string-to-number (cadr point))))
                     ;;   (setq point (cons (string-to-number (car point))
                     ;;                     (string-to-number (car points)))
                     ;;         points (cdr points)))
                     (setq point (cons (string-to-number (pop points))
                                       (string-to-number (pop points))))
                     (if (and pos
                              (member cmd '("m" "l" "c" "s" "q" "t")))
                         (progn
                           (setcar pos (+ (car pos) (car point)))
                           (setcdr pos (+ (cdr pos) (cdr point)))
                           )
                       (setq pos point)))

                   (when (member (upcase cmd) '("H" "V"))
                     (cond ((string= cmd "h")
                            (setcar pos (+ (car pos)
                                           (string-to-number (car points)))))
                           ((string= cmd "v")
                            (setcdr pos (+ (cdr pos)
                                           (string-to-number (car points)))))
                           ((string= cmd "H")
                            (setcar pos (string-to-number (car points))))
                           ((string= cmd "V")
                            (setcdr pos (string-to-number (car points)))))
                     (pop points))

                   ;; (message "%s" pos)
                   (unless skip
                     (setq x (car pos)
                           y (cdr pos)))
                   (setq
                    x1 (if x1 (min x1 x) x)
                    y1 (if y1 (min y1 y) y)
                    x2 (if x2 (max x2 x) x)
                    y2 (if y2 (max y2 y) y))
                   ;; (setq points (cdr points))
                   )
                 (when x1
                   (list x1 y1 x2 y2))
                 ))
              ('use (setq ref (or ref
                                  (if ii/canvas--svg
                                      (car (dom-by-id
                                            (car ii/canvas--svg)
                                            (concat
                                             (substring (dom-attr node 'href) 1)
                                             "$")))))
                          res (or (ii/svg-bbox ref)
                                  '(0 0 0 0))
                          ox (or (dom-attr node 'x) 0)
                          oy (or (dom-attr node 'y) 0)
                          x1 (+ ox (nth 0 res))
                          y1 (+ oy (nth 1 res))
                          x2 (+ ox (nth 2 res))
                          y2 (+ oy (nth 3 res)))
                    (list x1 y1 x2 y2))
              ((or 'g 'svg 'symbol)
               (let (x y xx yy bbox flag)
                 (when (eq tag 'svg)
                   (setq ox (or (dom-attr node 'x) 0)
                         oy (or (dom-attr node 'y) 0)
                         xx (or (dom-attr node 'width) 0)
                         yy (or (dom-attr node 'height) 0)
                         x1 ox y1 oy
                         x2 (+ ox (if (stringp xx) (string-to-number xx) xx))
                         y2 (+ oy (if (stringp yy) (string-to-number yy) yy))
                         ))
                 (mapc (lambda (a)
                         (setq bbox (ii/svg-bbox a))
                         ;; (message "%s" bbox)
                         (when bbox
                           (when (and (eq tag 'g) (null flag))
                             (setq x1 (nth 0 bbox)
                                   flag t
                                   y1 (nth 1 bbox)))
                           (setq x  (+ ox (nth 0 bbox))
                                 y  (+ oy (nth 1 bbox))
                                 xx (+ ox (nth 2 bbox))
                                 yy (+ oy (nth 3 bbox))
                                 x1 (min (or x1 ox) x xx)
                                 y1 (min (or y1 oy) y yy)
                                 x2 (max (or x2 ox) x xx)
                                 y2 (max (or y2 oy) y yy))))
                       (dom-children node))
                 (when x1
                   (list x1 y1 x2 y2))
                 ))
              ))

      ;; Apply transformation
      (unless (or (null res)
                  (null transform)
                  (memq tag '(circle line text)))
        (let (point1 point2 point3 point4)
          (setq x1 (nth 0 res)
                y1 (nth 1 res)
                x2 (nth 2 res)
                y2 (nth 3 res))

          (setq point1 (ii/svg--apply-transform x1 y1 transform)
                point2 (ii/svg--apply-transform x2 y2 transform)
                point3 (ii/svg--apply-transform x1 y2 transform)
                point4 (ii/svg--apply-transform x2 y1 transform))

          ;; (message "transform %s %s %s %s" point1 point2 point3 point4)
          (setq x1 (min (car point1) (car point2)
                        (car point3) (car point4))
                y1 (min (cdr point1) (cdr point2)
                        (cdr point3) (cdr point4))
                x2 (max (car point1) (car point2)
                        (car point3) (car point4))
                y2 (max (cdr point1) (cdr point2)
                        (cdr point3) (cdr point4)))
          (setq res (list x1 y1 x2 y2))
          ))
      res)))

(defun ii/svg--apply-transform (x y transform)
  ;; Calculate offset due to transform
  (let* ((ox x)
         (oy y)
         (num "\\([+-]?[0-9]*[.]?[0-9]+\\(e[+-]?[0-9]*\\)*\\)")
         (sep ",? *")
         (mat (concat "matrix(" num sep num sep num
                      sep num sep num sep num ")"))
         (transl (concat "translate(" num "\\(" sep num "\\)*)"))
         (scale (concat "scale(" num "\\(" sep num "\\)?)"))
         (rot (concat "rotate(" num "\\(" sep num sep num "\\)?)"))
         (start 0)
         (tlist (and transform
                     (nreverse
                      (split-string (replace-regexp-in-string ")" ")|" transform)
                                    "|"))))
         deg cx cy ca sa)
    ;; Transformation matrices are applied in order. However, matrix
    ;; multiplication ends up "working" from right to left.
    (while tlist
      (setq transform (string-trim-left (pop tlist)))
      (cond
       ((string-match transl transform start)
        (setq ox (+ x (string-to-number (match-string 1 transform))))
        (if (match-string 3 transform)
            (setq oy (+ y (string-to-number (match-string 4 transform)))))
        )
       ((string-match scale transform start)
        (setq ox (* x (string-to-number (match-string 1 transform))))
        (if (match-string 4 transform)
            (setq oy (* y (string-to-number (match-string 4 transform)))))
        )
       ((string-match rot transform start)
        (setq deg (* (/ pi 180) (string-to-number (match-string 1 transform))))
        (if (match-string 3 transform)
            (setq cx (string-to-number (match-string 4 transform))
                  cy (string-to-number (match-string 6 transform)))
          (setq cx 0 cy 0))
        (setq ca (cos deg) sa (sin deg))
        ;; rotate(45, 250, 250) =
        ;; matrix(0.707, 0.707, -0.707, 0.707, 250.000, -103.553)
        ;; (message "matrix(%.3f, %.3f, %.3f, %.3f, %.3f, %.3f)"
        ;;          (* ca ) (* sa) (* -1 sa) (* ca)
        ;;          (* (+ (* -1 ca cx) (* sa cy) cx))
        ;;          (* (+ (* -1 sa cx) (* -1 ca cy) cy)))
        (setq ox (+ (* ca x) (* -1 sa y)
                    (* (+ (* -1 ca cx) (* sa cy) cx))
                    ))
        (setq oy (+ (* sa x) (* ca y)
                    (* (+ (* -1 sa cx) (* -1 ca cy) cy))
                    ))
        )
       ((string-match mat transform start):
        ;; matrix(a,b,c,d,e,f)
        ;; x = a x (prevCoordSys) + c y (prevCoordSys) + e
        ;; y = b x (prevCoordSys) + d y (prevCoordSys) + f
        (let* ((a (string-to-number (match-string 1 transform)))
               (b (string-to-number (match-string 3 transform)))
               (c (string-to-number (match-string 5 transform)))
               (d (string-to-number (match-string 7 transform)))
               (e (string-to-number (match-string 9 transform)))
               (f (string-to-number (match-string 11 transform))))
          (setq ox (+ (* a x) (* c y) e))
          (setq oy (+ (* b x) (* d y) f))
          ))
       )
      (setq x ox y oy))
    (cons ox oy)))

(defun ii/svg-init-pos (node)
  "Return initial position of node."
  (let ((tag (dom-tag node)))
    (pcase tag
      ('line (list (dom-attr node 'x1)
                   (dom-attr node 'y1)))
      ((or 'circle 'ellipse)
       (list (dom-attr node 'cx)
             (dom-attr node 'cy)))
      ('rect (list (dom-attr node 'x)
                   (dom-attr node 'y)))
      ('polyline
       (let* ((points (split-string (dom-attr node 'points)))
              point x y)
         (setq point (split-string (car points) ",")
               x (string-to-number (car point))
               y (string-to-number (cadr point)))
         (list x y)))
      )))

(defun ii/svg--shapes-in-region (svg area)
  "Return a list of SVG nodes inside the AREA."
  (let (res bbox)
    (mapc (lambda (a)
            (setq bbox (ii/svg-bbox a))
            ;; (message "%s %s %s" bbox area a)
            (if (and bbox
                     (>= (nth 0 bbox) (nth 0 area))
                     (>= (nth 1 bbox) (nth 1 area))
                     (<= (nth 2 bbox) (nth 2 area))
                     (<= (nth 3 bbox) (nth 3 area)))
                (push a res )))
          (dom-children svg))
    res))

(defun ii/svg--point-in-region (x y area)
  "Return non NIL if (x . y) is inside the AREA."
  (and (>= x (nth 0 area))
       (>= y (nth 1 area))
       (<= x (nth 2 area))
       (<= y (nth 3 area))))

(defun ii/svg-widget (svg frag &optional width height &rest args)
  "Create widget from SVG FRAG.
If FRAG is nil, use SVG."
  (let ((e (ii/svg--extract-fragment svg frag width height nil args)))
    (if e
        (apply 'widget-create 'push-button :display (ii/svg-image e) args)
      )))

(defun ii/svg--extract-fragment (svg frag &optional width height explode &rest args)
  "Return an SVG image containing FRAG with view adjusted to
fragment's bounding box."
  ;; Skip comment tags
  (if (and frag (memq (dom-tag frag)
                      '(comment defs title metadata script style)))
      nil
    ;; svg is required for namespaces
    (let* ((width (or width 32))
           (height (or height 32))
           (stroke-width .1)
           (padding 10)
           bbox attr e ref defs
           x y bwidth bheight
           )
      (if frag
          (progn
            (when (eq (dom-tag frag) 'use)
              ;; Handle <use/> tag
              (setq defs (car (dom-by-tag svg 'defs))
                    ref (car (dom-by-id
                              svg (concat (substring (dom-attr frag 'href) 1)
                                          "$")))))
            (setq bbox (ii/svg-bbox frag ref)
                  attr (copy-tree (dom-attributes svg))
                  e (if (and explode (eq (dom-tag frag) 'g))
                        (append `(svg ,attr) (append defs (dom-children frag)))
                      `(svg ,attr ,(append defs frag))
                      )))
        (setq e svg))

      (dom-set-attribute e 'width width)
      (dom-set-attribute e 'height height)
      (when (and frag bbox)
        (setq stroke-width (/ (- (nth 2 bbox) (nth 0 bbox)) 1.0 width))
        (setq stroke-width (if (> stroke-width 0) stroke-width 0.1))
        (setq bwidth (abs (- (nth 2 bbox) (nth 0 bbox)))
              padding (or (plist-get args :padding) padding)
              bheight (abs (- (nth 3 bbox) (nth 1 bbox))))
        (if (> bwidth bheight)
            (setq bheight bwidth)
          (setq bwidth bheight))
        (setq x (- (/ (+ (nth 0 bbox) (nth 2 bbox)) 2)
                   (/ bwidth 2) padding)
              y (- (/ (+ (nth 1 bbox) (nth 3 bbox)) 2)
                   (/ bheight 2) padding)
              bwidth (+ bwidth (* 2 padding))
              bheight (+ bheight (* 2 padding)))

        ;; Border
        (when (plist-get args :border)
          (ii/svg-rectangle e x y bwidth bheight
                            :stroke (or (plist-get args :border) "red")
                            :stroke-width stroke-width
                            :fill (or (plist-get args :fill) "none")
                            :rx (* (or (plist-get args :rx) 0) stroke-width)
                            ))
        (dom-set-attribute e 'viewBox (format "%0.3f %0.3f %0.3f %0.3f"
                                              x y bwidth bheight
                                              )))
      ;; (message "%s\n%s %s" e bbox (dom-attr e 'viewBox))
      e)))

(defvar ii/canvas-plugin-map nil
  "Plugin keymap.")
(defvar ii/canvas-plugin-fn 'ii/canvas--select-mode
  "Plugin function to substitute for canvas key handling.")
(defvar ii/canvas-act-in-region-fn 'ii/canvas--act-in-region
  "Plugin function to substitute for canvas action in a region handling.")
(defvar ii/canvas--move-to-fn 'ii/canvas--move-to
  "Plugin function to substitute for move animation.")
(defvar ii/canvas-exit-fn nil
  "Plugin function to substitute for exit handling.")

(make-variable-buffer-local 'ii/canvas-plugin-map)
(make-variable-buffer-local 'ii/canvas-plugin-fn)
(make-variable-buffer-local 'ii/canvas-act-in-region-fn)
(make-variable-buffer-local 'ii/canvas--move-to-fn)
(make-variable-buffer-local 'ii/canvas-exit-fn)

(defvar ii/canvas--svg nil)
;; (make-variable-buffer-local 'ii/canvas--svg)
(defvar ii/canvas--mode nil
  "Object to draw.")
(defvar ii/canvas--id nil
  "Identifier of the object being drawn.")
(defvar ii/canvas--stroke-width nil
  "Stroke width.")
(defvar ii/canvas--stroke-color nil
  "Stroke color.")
(defvar ii/canvas--fill-color nil
  "Fill color.")
(defvar ii/canvas--font-family nil
  "Font family.")
(defvar ii/canvas--rotation nil
  "Rotation angle.")
(defvar ii/canvas--text nil
  "List of params for drawing text.")
(defvar ii/canvas--nearest-objects nil
  "Identifier of the object being selected.")
(defvar ii/canvas--undo-marker nil
  "Identifier of the object being selected for undo.")
(defvar ii/canvas--temp-points nil
  "Points while drawing connections.")
(defvar ii/canvas--dialog-object nil
  "Current object in dialog.")
(defvar ii/canvas--port-id nil
  "Current port id.")
(defvar ii/canvas--port-name nil
  "Current port name.")
(defvar ii/canvas--routing nil
  "Use 45 degree bends for routing e.g. in a PCB.")
(defvar ii/canvas--ns-prefix nil
  "Current namespace prefix used for generating names.")
(defvar ii/canvas--annotation nil
  "Status of annotation display.")
(defvar ii/canvas--layers nil
  "ID of layers in the diagram.")
(defvar ii/canvas--widget-size 100
  "Widget size.")
(defvar ii/canvas--grid-size 10
  "Grid size in pixels for snapping.
Set to NIL to turn it off.")
(defvar ii/canvas--defs nil
  "Canvas default definitions.")
(defvar ii/canvas--defs-xml
  "
<g id=\"_defs\">
  <defs>
    <!-- arrowhead marker definition -->
    <marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\"
        markerWidth=\"6\" markerHeight=\"6\"
        orient=\"auto-start-reverse\">
      <path d=\"M 0 0 L 10 5 L 0 10 z\" />
    </marker>

    <!-- simple dot marker definition -->
    <marker id=\"dot\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\"
        markerWidth=\"10\" markerHeight=\"10\" markerUnits=\"userSpaceOnUse\">
      <circle cx=\"5\" cy=\"5\" r=\"5\" fill=\"red\" />
    </marker>
    <marker id=\"dot01\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\"
        markerWidth=\"5\" markerHeight=\"5\" markerUnits=\"userSpaceOnUse\">
      <circle cx=\"5\" cy=\"5\" r=\"5\" fill=\"red\" />
    </marker>
    <marker id=\"dot001\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\"
        markerWidth=\"2\" markerHeight=\"2\" markerUnits=\"userSpaceOnUse\">
      <circle cx=\"5\" cy=\"5\" r=\"5\" fill=\"red\" />
    </marker>

    <!-- grid pattern -->
    <pattern id=\"x5Grid\" viewBox=\"0 0 10 10\" width=\"5\" height=\"5\"
        patternUnits=\"userSpaceOnUse\">
      <path d=\"M 10 0 L 0 0 0 10\" fill=\"none\" stroke=\"blue\" stroke-width=\"0.1\"/>
    </pattern>
    <pattern id=\"x10Grid\" viewBox=\"0 0 10 10\" width=\"10\" height=\"10\"
        patternUnits=\"userSpaceOnUse\">
      <path d=\"M 10 0 L 0 0 0 10\" fill=\"none\" stroke=\"red\" stroke-width=\"0.1\"/>
    </pattern>
    <pattern id=\"x20Grid\" viewBox=\"0 0 10 10\" width=\"20\" height=\"20\"
        patternUnits=\"userSpaceOnUse\">
      <path d=\"M 10 0 L 0 0 0 10\" fill=\"none\" stroke=\"gray\" stroke-width=\"0.1\"/>
    </pattern>
    <pattern id=\"grid\" width=\"100\" height=\"100\" patternUnits=\"userSpaceOnUse\">
      <rect width=\"100\" height=\"100\" fill=\"url(#x20Grid)\"/>
      <path d=\"M 100 0 L 0 0 0 100\" fill=\"none\" stroke=\"gray\" stroke-width=\"1\"/>
    </pattern>
  </defs>

  <rect id=\"_grid\" width=\"%d\" height=\"%d\" fill=\"url(#grid)\" />
</g>
"
  "XML string for definitions.")
(defvar ii/canvas--outline-style 'default
  "Use solid or default (dashes) style.")
(make-variable-buffer-local 'ii/canvas--outline-style)

(defvar ii/canvas--outline-marker "#dot"
  "Use a different outline marker for different zoom levels.")
(defvar ii/canvas--init-bbox nil
  "Restore zoom to initial content bbox.")
(defvar ii/canvas--sym-lib nil
  "Currently loaded symbol library.")
(defvar ii/canvas--color-picker nil
  "Color picker.")
(defvar ii/canvas--color-picker-xml
  "
<g id=\"color-picker\" width=\"256\" height=\"300\" >
  <defs>
    <linearGradient id=\"saturation\" gradientTransform=\"rotate(0)\" >
      <stop offset=\"0\"  stop-color=\"#000000\" />
      <stop offset=\"50%\" stop-color=\"#00ff00\" />
      <stop offset=\"100%\" stop-color=\"#ffffff\" />
    </linearGradient>
    <linearGradient id=\"hue\" gradientTransform=\"rotate(0)\" >
      <stop offset=\"0\"  stop-color=\"#f00\" />
      <stop offset=\"16.66%\"  stop-color=\"#ff0\" />
      <stop offset=\"33.33%\" stop-color=\"#0f0\" />
      <stop offset=\"50%\"  stop-color=\"#0ff\" />
      <stop offset=\"66.66%\" stop-color=\"#00f\" />
      <stop offset=\"83.33%\"  stop-color=\"#f0f\" />
      <stop offset=\"100%\" stop-color=\"#f00\" />
    </linearGradient>
  </defs>
  <rect x=\"0\" y=\"0\" width=\"256\" height=\"256\" fill=\"url('#saturation')\" />
  <rect x=\"0\" y=\"270\" width=\"256\" height=\"25\" fill=\"url('#hue')\" />
</g>
"
  "XML String for color picker.")
(defvar ii/canvas--hide-prompt nil
  "If non-nil, don't display a prompt")

(defun ii/canvas--pick-color ()
  "Draw color picker widget."
  (if ii/canvas--color-picker
      ii/canvas--color-picker
    (with-temp-buffer
      (insert ii/canvas--color-picker-xml)
      (setq ii/canvas--color-picker-xml
            (libxml-parse-xml-region (point-min) (point-max))))
    ))

(defun ii/svg--scrub-image (svg)
  "Convert some important attributes to number.
For bbox calculations, these must be numbers."
  ;; Handle XML PI
  (when (and svg (eq (dom-tag svg) 'top))
    (setq svg (car (dom-by-tag svg 'svg))))
  (dolist (a (dom-attributes svg))
    (if (and (memq (car a) '(x y width height x1 y1 x2 y2 cx cy r rx ry))
             (stringp (cdr a))
             (not (string-match-p "%" (cdr a))))
        (setcdr a (string-to-number (cdr a))))
    )
  (dolist (node (dom-children svg))
    (unless (stringp node)
      (ii/svg--scrub-image node)))
  svg)

(defun ii/canvas-mode-on-click (event)
  (interactive "e")
  (let* ((e (event-start event))
         (pos (posn-point e))
         (win (posn-window e))
         (image (posn-image e)))
    ;; (message "%s" e)
    (select-window win)
    (when image
      (save-excursion
        (goto-char pos)
        (setq pos (point-marker)))
      (ii/canvas-mode nil image pos))
    ))

;;;###autoload
(defun ii/canvas-mode (&optional prefix svg point)
  "Canvas mode.
Use PREFIX to override width and height values."
  (interactive "P")
  ;; Allow multiple images in the document.
  (let* ((image (or svg (get-text-property (point) 'display)))
         (marker (or point (point-marker)))
         (buf (or (if point (marker-buffer point)) (current-buffer)))
         (file (plist-get (cdr image) :file))
         (width 800)
         (height 800))
    (local-set-key [down-mouse-1] 'ii/canvas-mode-on-click)
    (if prefix
        (setq file (car (find-file-read-args "Find file: "
                                             (confirm-nonexistent-file-or-buffer)))
              width (read-number "Width: " (window-pixel-width))
              height (read-number "Height: " (window-pixel-height))))

    (if (and file (not (file-directory-p file)))
        (progn
          (with-current-buffer (find-file-noselect file)
            (setq image (libxml-parse-xml-region (point-min) (point-max))))
          ;; Handle XML PI
          (when (and image (eq (dom-tag image) 'top))
            (setq image (car (dom-by-tag image 'svg))))
          (when image
            (ii/svg--scrub-image image)
            (dom-set-attribute image 'width width)
            (dom-set-attribute image 'height height)
            (dom-set-attribute image :image marker)
            (with-current-buffer buf (ii/svg-insert-image image))))

      (if (eq 'svg (plist-get (cdr image) :type))
          (with-temp-buffer
            (insert (plist-get (cdr image) :data))
            (setq image (libxml-parse-xml-region (point-min) (point-max)))
            (ii/svg--scrub-image image)
            (dom-set-attribute image :image marker))
        (setq image (ii/svg-create width height :stroke-width 1))
        (with-current-buffer buf (ii/svg-insert-image image))))

    (setq ii/canvas--svg nil)
    (ii/canvas--set image marker)
    (ewp-crop-image-1 image)
    ))

(defun ii/canvas--set (svg marker)
  "Set SVG into canvas for current display.
MARKER identifies the image uniquely."
  (let ((width (dom-attr svg 'width))
        (height (dom-attr svg 'height))
        (image svg)
        )
    ;; (ii/svg--scrub-image image)
    (dom-set-attribute image 'width width)
    (dom-set-attribute image 'height height)
    (dom-set-attribute image :image marker)

    (unless nil ;canvas--defs
      ;; dimensions vary with images
      (setq ii/canvas--defs
            (with-temp-buffer
              (insert (format ii/canvas--defs-xml width height))
              (libxml-parse-xml-region (point-min) (point-max)))))
    (unless (dom-by-id image "_defs")
      ;; Keep it at the top so that undo works fine for deleting
      ;; visible elements
      (dom-add-child-before image ii/canvas--defs)
      (ii/svg-possibly-update-image image))
    ;; Add canvas namespace
    (unless (dom-attr image 'xmlns:canvas)
      (dom-set-attribute image 'xmlns:canvas
                         "http://www.gnu.org/canvas"))

    (setq ii/canvas--mode 'polyline
          ii/canvas--undo-marker nil
          ii/canvas--stroke-color (or ii/canvas--stroke-color
                                      (foreground-color-at-point))
          ii/canvas--fill-color (or ii/canvas--fill-color "none")
          ii/canvas--outline-marker "#dot"
          ii/canvas--rotation 90
          ii/canvas--init-bbox (or (ii/svg-bbox image)
                                   (list 0 0 width height)))

    (mapc (lambda (a)
            (setq ii/canvas--id (max (string-to-number (or (dom-attr a 'id) ""))
                                     (or ii/canvas--id 0))))
          (dom-children image))
    (setq ii/canvas--id (1+ ii/canvas--id))

    (push image ii/canvas--svg)
    ))

(defun ii/canvas--generate-names (xml attr &optional prefix)
  "Generate names by adding numbered suffix to type."
  (let* ((attr (if prefix (concat prefix ":" attr) attr))
         (attr-type (if attr (intern attr)))
         (attr-name (intern (if prefix (concat prefix ":name") "name")))
         name num i e)
    (dolist (node (dom-children xml))
      (setq name (dom-attr node attr-type))
      (when name
        (setq e (assoc name num)
              i (or (cdr e) 0)
              i (1+ i))
        (if e
            (setcdr e i)
          (push (cons name i) num))
        (dom-set-attribute node attr-name (format "%s%d" (upcase name) i))
        ))
    ;; (ii/svg-print xml)
    ))

;;;###autoload
(defun ii/canvas-symbol-lib (&optional file group)
  "Load symbol library."
  (interactive "fFile: \nP")
  (let ((i ii/canvas--widget-size)
        (b (get-buffer-create "*Symbols*"))
        image groups children e)
    (with-current-buffer (find-file-noselect file)
      (setq image (libxml-parse-xml-region (point-min) (point-max))))
    ;; Handle XML PI
    (when (and image (eq (dom-tag image) 'top))
      (setq image (car (dom-by-tag image 'svg))))
    (ii/svg--scrub-image image)
    (setq ii/canvas--sym-lib file)

    ;; Collect groups and allow selection via minibuffer
    (setq children (dom-children image))
    (when group ;(> (length children) 50)
      (dolist (el children)
        (setq group (substring (dom-attr el 'id) 0 3)
              e (assoc group groups)
              children (cdr e))
        (push el children)
        (if e
            (setcdr e children)
          (push (cons group children) groups)))
      (setq group (completing-read "Select group: " (mapcar #'car groups))
            children (cdr (assoc group groups))))

    (with-current-buffer b
      ;; (setq inhibit-read-only nil)
      (erase-buffer)
      (dolist (el children)
        (ii/svg-widget image el i i
                       :help-echo (dom-attr el 'id)
                       :value (dom-attr el 'id)
                       :action (lambda (widget &optional _s)
                                 (setq ii/canvas--undo-marker
                                       (ii/svg-load-from-xml
                                        (image-property
                                         (widget-get widget :display)
                                         :data)))
                                 ;; (message "%s" (widget-value widget))
                                 (if (> (recursion-depth) 0)
                                     (exit-recursive-edit))
                                 )
                       :tag (dom-attr el 'id)))
      ;; (widget-setup)
      (goto-char (point-min))
      )
    (pop-to-buffer b t t)
    image))

(defun ii/canvas--annotate (svg node)
  "Add annotation for name, type and value using namespace.
Name is usually optional since it can be generted by numbering the type."
  (let* ((attrs (dom-attributes svg))
         ns prefix name value attr)
    (mapc (lambda (a)
            (if (string-match-p "xmlns" (symbol-name (car a)))
                (push (cdr a) ns)))
          attrs)

    (setq ns (completing-read "Namespace: " ns))
    (if (setq attr (rassoc ns attrs))
        (setq prefix (car (last (split-string (symbol-name (car attr)) ":"))))
      (setq prefix (read-string "Prefix: "))
      (if (string-empty-p prefix)
          (setq prefix nil)
        (setq ii/canvas--ns-prefix prefix)
        (dom-set-attribute svg (intern (concat "xmlns:" prefix)) ns)))

    (setq name (read-string "Attribute Name: ")
          value (read-string "Attribute Value: "))
    ;; (message "%s" (rassoc ns attrs))
    (unless (string-empty-p
             (setq name (concat (if prefix (concat prefix ":")) name)))
      (dom-set-attribute node (intern name) value))
    ))

(defun ii/canvas--show-annotation (svg &optional node)
  "Show annotations for name and value."
  (let* ((all-attrs (dom-attributes node))
         x1 y1 x2 y2 w h bbox name value attrs len1 len2 font-size id)
    (when node
      (mapc (lambda (a)
              (setq name (symbol-name (car a)))
              (if (string-match "\\(name\\|value\\)" name)
                  (push (cons (match-string 1 name) (cdr a)) attrs)))
            all-attrs)
      ;; (message "%s" attrs)
      (when attrs
        (setq bbox (ii/svg-bbox node)
              font-size 8
              x1 (nth 0 bbox)
              y1 (nth 1 bbox)
              x2 (nth 2 bbox)
              y2 (nth 3 bbox)
              id (or (dom-attr node 'id) 0)
              name (cdr (assoc "name" attrs))
              value (cdr (assoc "value" attrs))
              len1 (* font-size (length name) 0.5)
              len2 (* font-size (length value) 0.5)
              w (/ (- x2 x1) 2)
              h (/ (- y2 y1) 2))
        (if (> w h)
            ;; Place on top and bottom
            (progn
              (if name
                  (ii/svg-text svg name :id (format "_anno_a_%s_%d" id 0)
                               :x (+ x1 w (- len1)) :y (- y1 font-size)))
              (if value
                  (ii/svg-text svg value :id (format "_anno_a_%s_%d" id 1)
                               :x (+ x1 w (- len2)) :y (+ y2 (* 2 font-size)))))
          ;; Place on right
          (if name
              (ii/svg-text svg name :id (format "_anno_a_%s_%d" id 0)
                           :x (+ x2 font-size) :y (+ y1 font-size)))
          (if value
              (ii/svg-text svg value :id (format "_anno_a_%s_%d" id 1)
                           :x (+ x2 font-size) :y y2))
          )
        ))

    (unless node
      (mapc (lambda (a)
              (ii/canvas--show-annotation svg a))
            (dom-children svg))
      (ii/svg-possibly-update-image svg))
    ))

(defun ii/canvas--hide-annotation (svg)
  "Set display:none to all annotation nodes."
  (mapc (lambda (a)
          (dom-set-attribute a 'display "none"))
        (dom-by-id svg "_anno"))
  (ii/svg-possibly-update-image svg))

(defun ii/canvas--select-mode (key svg)
  (let ()
    (unless (or (memq key '(?u ?m ?g ?G ?R ?\M-w ?> escape ?\C-x ?A))
                ;; Ignore mouse movement
                (listp key))
      ;; All operations which don't use ii/canvas--undo-marker should
      ;; deselect objects
      (ii/canvas--deselect))

    (setq ii/canvas--mode
          (pcase key
            ;; Objects
            (?l 'line)
            (?c 'circle)
            (?r 'rect)
            (?e 'ellipse)
            (?W (setq ii/canvas--temp-points nil)
                'conn)
            (?t (let ((str (read-string "Text: " nil))
                      node size font)
                  (when str
                    (setq size (read-number "Size: " 15)
                          ;; font (completing-read "Font family: "
                          ;; (font-family-list))
                          node (ii/svg-text svg str
                                            :font-size size
                                            :font-family
                                            (unless (string-empty-p font) font)
                                            :id (number-to-string ii/canvas--id)
                                            :stroke ii/canvas--stroke-color)
                          ii/canvas--id (1+ ii/canvas--id))
                    (ii/canvas--select (list node))
                    (ii/canvas--move-init node)
                    'move)))
            (?p 'polyline)
            (?P (setq ii/canvas--temp-points nil)
                'path)

            ;; Object properties
            (?C (setq ii/canvas--stroke-color
                      (read-string "Stroke color: " (foreground-color-at-point)))
                ii/canvas--mode)
            (?w (setq ii/canvas--stroke-width (read-number "Stroke width: " 1))
                ii/canvas--mode)
            (?f (setq ii/canvas--fill-color
                      (read-string "Fill color: " (foreground-color-at-point)))
                ii/canvas--mode)
            (?F (setq ii/canvas--font-family
                      (completing-read "Font family: " (font-family-list)))
                ii/canvas--mode)
            (?A (when ii/canvas--undo-marker
                  ;; (message "%s" ii/canvas--undo-marker)
                  (ii/canvas--annotate svg (car ii/canvas--undo-marker))
                  (setq ii/canvas--annotation t)
                  (ii/canvas--show-annotation svg (car ii/canvas--undo-marker))
                  (ii/canvas--deselect)
                  (ii/svg-possibly-update-image svg))
                ii/canvas--mode)
            (?n (setq ii/canvas--ns-prefix
                      (read-string "Prefix: " ii/canvas--ns-prefix))
                ii/canvas--mode)

            ;; Object operations
            (?u (ii/canvas--undo svg))
            ('escape (when (eq ii/canvas--mode 'move)
                       (ii/canvas--move-cancel))
                     (when (eq ii/canvas--mode 'place)
                       (ii/canvas--undo svg))
                     (when (eq ii/canvas--mode 'path)
                       (ii/canvas--path svg ii/canvas--temp-points
                                        (number-to-string ii/canvas--id))
                       (setq ii/canvas--temp-points nil
                             ii/canvas--id (1+ ii/canvas--id)))
                     (when (and (eq ii/canvas--mode 'conn)
                                ii/canvas--temp-points)
                       (ii/svg-polyline svg ii/canvas--temp-points
                                        :stroke-width ii/canvas--stroke-width
                                        :id (number-to-string ii/canvas--id)
                                        :stroke-color (foreground-color-at-point)
                                        :fill "none")
                       (setq ii/canvas--temp-points nil
                             ii/canvas--id (1+ ii/canvas--id)))
                     (ii/canvas--deselect)
                     (ii/svg-possibly-update-image svg)
                     nil)
            (?m (when ii/canvas--undo-marker
                  (mapc #'ii/canvas--move-init ii/canvas--undo-marker)
                  'move))
            (?\M-g (let* ((node (car (dom-by-id svg "_grid")))
                          (attr (dom-attr node 'display)))
                     ;; Toggle grid
                     (dom-set-attribute node 'display
                                        (if attr nil "none"))
                     (ii/svg-possibly-update-image svg))
                   ii/canvas--mode)
            (?\M-w (let ((objs (copy-tree ii/canvas--undo-marker)))
                     ;; Change ids and append to svg
                     (mapc (lambda (a)
                             (dom-set-attribute a 'id
                                                (number-to-string ii/canvas--id))
                             (setq ii/canvas--id (1+ ii/canvas--id)))
                           objs)
                     (nconc svg objs)
                     (ii/canvas--deselect)
                     (ii/svg-possibly-update-image svg)
                     (setq ii/canvas--undo-marker objs))
                   'move)
            (?d (setq ii/canvas--rotation
                      (read-number "Degrees: " 90))
                nil)
            (?> ;; Enter define-port. If currently on a group, start a
             ;; dialog with the children of the group.
             (let* ((width (dom-attr svg 'width))
                    (height (dom-attr svg 'height))
                    (marker (dom-attr svg :image))
                    (frag (car ii/canvas--undo-marker))
                    (image nil))
               (when (and frag (eq (dom-tag frag) 'g))
                 (dom-set-attribute frag 'stroke-dasharray nil)
                 (setq image (ii/svg--extract-fragment svg frag width height)
                       ii/canvas--dialog-object frag)
                 (ii/canvas--set image marker)
                 (setq ii/canvas--port-id (or (length (dom-by-id svg "_port")) 0))
                 'define-port)))
            (?D (when (eq ii/canvas--mode 'define-port)
                  (setq ii/canvas--port-id (1+ ii/canvas--port-id)
                        ii/canvas--port-id (read-number "Number: "
                                                        ii/canvas--port-id)
                        ii/canvas--port-name (read-string "Name: "))
                  )
                ii/canvas--mode)
            (?< ;; Exit define-port
             (when (eq ii/canvas--mode 'define-port)
               (let ((image (pop ii/canvas--svg))
                     str x y nodes)
                 (setq str (mapconcat (lambda (a)
                                        (setq x (dom-attr a 'cx)
                                              ;; name (dom-attr a 'name)
                                              y (dom-attr a 'cy))
                                        (format "%s,%s" x y))
                                      (dom-by-id image "_port")
                                      " ")
                       nodes (append (dom-by-id image "_port")
                                     (dom-by-id image "_name")))

                 ;; Add canvas namespace and canvas:ports attribute
                 (unless (dom-attr (car ii/canvas--svg) 'xmlns:canvas)
                   (dom-set-attribute (car ii/canvas--svg) 'xmlns:canvas
                                      "http://www.gnu.org/canvas"))
                 (dom-set-attribute ii/canvas--dialog-object 'ii/canvas:ports str)
                 (nconc ii/canvas--dialog-object nodes)
                 (ii/svg-possibly-update-image (car ii/canvas--svg))
                 ;; (message "define-port1: %s %s" nodes ii/canvas--dialog-object)
                 (setq ii/canvas--dialog-object nil)
                 ))
             nil)
            (?\M-r (ii/canvas--generate-names svg "type" ii/canvas--ns-prefix))
            (?R (when (and ii/canvas--undo-marker
                           (= (length ii/canvas--undo-marker) 1))
                  ;; In-place rotation of multiple objects is not desirable.
                  ;; For rotating multiple objects, use a group.
                  (mapc (lambda (a)
                          (let* ((bbox (ii/svg-bbox a))
                                 (angle (% (+ (or (dom-attr a :angle) 0)
                                              ii/canvas--rotation)
                                           360))
                                 (x (nth 0 bbox))
                                 (y (nth 1 bbox)))
                            (dom-set-attribute a :angle
                                               (if (/= angle 0) angle))
                            (ii/canvas--move-init a)
                            (ii/canvas--move-to a x y)
                            ))
                        ii/canvas--undo-marker)
                  (if ii/canvas--annotation
                      (ii/canvas--show-annotation svg (car ii/canvas--undo-marker)))

                  (ii/canvas--highlight-obj svg ii/canvas--undo-marker)
                  (ii/svg-possibly-update-image svg))
                ii/canvas--mode)
            (?g (ii/canvas--group svg ii/canvas--undo-marker)
                nil)
            (?G (ii/canvas--ungroup svg ii/canvas--undo-marker)
                nil)

            ;; File operations
            (?D (let (width height)
                  (setq width (read-number "Width: " width)
                        height (read-number "Height: " height))
                  (dom-set-attribute svg 'width width)
                  (dom-set-attribute svg 'height height)
                  (ii/svg-possibly-update-image svg))
                ii/canvas--mode)
            (?O (let* ((file (car (find-file-read-args
                                   "Find file: "
                                   (confirm-nonexistent-file-or-buffer))))
                       (image (create-image file))
                       (size (image-size image t)))
                  (ii/svg-embed svg file
                                (format "image/%s" (image-type file)) nil
                                :id (number-to-string ii/canvas--id)
                                :x 0 :y 0
                                :width (car size)
                                :height (cdr size)
                                )
                  (setq ii/canvas--id (1+ ii/canvas--id))
                  ii/canvas--mode))
            (?s (with-temp-buffer
                  (ii/canvas--remove-marker svg)
                  (ii/svg-print svg)
                  (write-file nil))
                ;; Save file
                ii/canvas--mode)

            ;; Tools
            (?x (setq ii/canvas--stroke-color "white"
                      ii/canvas--stroke-width 5)
                'erase)
            (?X 'crop)
            ((or ?z ?0 ?+ ?-)
             (let* ((viewBox nil)
                    factor width height x y bbox w)
               (setq x 0 y 0
                     width (dom-attr svg 'width)
                     height (dom-attr svg 'height))
               (when (eq key ?z)
                 (setq ii/canvas--mode 'zoom))
               (when (eq key ?0)
                 ;; Use the larger bbox
                 (setq bbox (ii/svg-bbox svg)
                       width (nth 2 bbox)
                       w (nth 2 ii/canvas--init-bbox))
                 (ii/canvas--zoom svg (if (or (null bbox) (> w width))
                                          ii/canvas--init-bbox
                                        bbox)))

               (when (memq key '(?+ ?-))
                 (setq factor (pcase key
                                (?+ 0.1)
                                (?- -0.1)))
                 (setq viewBox (ii/svg-parse-viewBox svg)
                       x       (nth 0 viewBox)
                       y       (nth 1 viewBox)
                       width   (nth 2 viewBox)
                       height  (nth 3 viewBox))

                 ;; Reduce the area by factor
                 (ii/canvas--zoom svg (list (+ x (* 0.5 factor width))
                                            (+ y (* 0.5 factor height))
                                            (+ x (* (- 1 (* 0.5 factor)) width))
                                            (+ y (* (- 1 (* 0.5 factor)) height)))
                                  ))
               ii/canvas--mode))
            (?o ;; Layers
             ;; (message "%s" ii/canvas--layers)
             (when ii/canvas--layers
               (let* ((layers ii/canvas--layers)
                      node)
                 ;; Use comma to separate inputs
                 (setq layers (completing-read-multiple "Select Layers to show:"
                                                        layers))
                 (mapc (lambda (a)
                         (setq node (car (dom-by-id svg a)))
                         (if (member a layers)
                             (dom-set-attribute node 'display nil)
                           (dom-set-attribute node 'display "none"))
                         ;; (message "%s" (dom-attr node 'display))
                         )
                       ii/canvas--layers)
                 (ii/svg-possibly-update-image svg)
                 ))
             nil)
            ((or 'up 'down 'right 'left)
             ;; Pan view
             (let* ((viewBox nil)
                    width height x y)
               (setq viewBox (ii/svg-parse-viewBox svg)
                     x       (nth 0 viewBox)
                     y       (nth 1 viewBox)
                     width   (nth 2 viewBox)
                     height  (nth 3 viewBox))

               (pcase key
                 ('left (setq x (- x 10)))
                 ('right (setq x (+ x 10)))
                 ('up (setq y (- y 10)))
                 ('down (setq y (+ y 10))))
               (ii/canvas--zoom svg (list x y (+ x width) (+ y height)))
               ;; (message "%s %s %s" key x y)
               ii/canvas--mode))
            (?L (if ii/canvas--sym-lib
                    (ii/canvas-symbol-lib ii/canvas--sym-lib)
                  (call-interactively 'ii/canvas-symbol-lib))
                (recursive-edit)
                (switch-to-buffer (current-buffer))

                (let* ((node ii/canvas--undo-marker)
                       (viewBox (ii/svg-bbox node))
                       (x (nth 0 viewBox))
                       (y (nth 1 viewBox))
                       (attrs (dom-attributes svg)))
                  ;; Merge namespaces in main SVG
                  (dolist (attr (dom-attributes node))
                    ;; (message "%s %s" attr (eq (car attr) 'viewBox))
                    ;; (if (eq (car attr) 'viewBox)
                    ;;     (setq viewBox (cdr attr)))
                    (if (and (string-match-p "xmlns:" (symbol-name (car attr)))
                             (not (assoc (car attr) attrs)))
                        (dom-set-attribute svg (car attr) (cdr attr))))

                  ;; Set transform, width and viewbox
                  ;; (when (string-match "\\([0-9.-]*\\) \\([0-9.-]*\\)" viewBox)
                  ;;   (setq x (- (string-to-number (match-string 1 viewBox)))
                  ;;         y (- (string-to-number (match-string 2 viewBox)))))
                  (setq viewBox (format "%s %s %s %s" x y
                                        (- (nth 2 viewBox) (nth 0 viewBox))
                                        (- (nth 3 viewBox) (nth 1 viewBox))))
                  ;; (message "%s " viewBox)
                  (setq attrs `((width . ,canvas--widget-size)
                                (viewBox . ,viewBox)
                                ;; (:transformed . t)
                                ;; (transform . ,(format "translate(%s, %s)"
                                ;;                       (- x) (- y)))
                                ))
                  (setq node (list (dom-node 'g attrs
                                             (car (dom-children node)))))

                  ;; Append to current image
                  (nconc svg node)
                  (setq ii/canvas--undo-marker nil)
                  (ii/canvas--select node)
                  (ii/canvas--move-init node))
                'place)
            (?\M-a (if ii/canvas--annotation
                       (progn
                         (setq ii/canvas--annotation nil)
                         (ii/canvas--hide-annotation svg))
                     (setq ii/canvas--annotation t)
                     (ii/canvas--show-annotation svg))
                   ii/canvas--mode)
            (?\C-x (unless (or (mouse-movement-p key)
                               (null ii/canvas-plugin-map))
                     (setq key (read-key "C-x"))
                     (let ((mode (lookup-key ii/canvas-plugin-map (vector key))))
                       (if mode
                           (funcall (nth 1 mode) svg)
                         (setq ii/canvas--mode nil))
                       (message "%s %s %s" key mode ii/canvas--mode)
                       ))
                   ii/canvas--mode)
            ;; Retain mode even if mouse moves outside the image
            (_ ii/canvas--mode)
            ))
    key))

(defun ii/canvas--remove-marker (svg)
  ;; Remove grid and markers
  (mapc (lambda (a)
          (dom-remove-node svg a))
        (dom-by-id svg "_marker"))
  )

(defun ii/canvas--move-init (node)
  "Rotation changes the left corner of bounding box. Call this
function after every rotation or before move to update the information in
`canvas--temp-points'."
  (let* ((bbox (ii/svg-bbox node))
         (str nil)
         (angle (dom-attr node :angle))
         (x (/ (+ (nth 0 bbox) (nth 2 bbox)) 2))
         (y (/ (+ (nth 1 bbox) (nth 3 bbox)) 2)))
    ;; Save original position
    (ii/canvas--apply-transform (list node) t)
    ;; (message "%s" angle)

    ;; First rotate and find bbox.
    (when angle
      (setq str (format "rotate(%s, %s, %s)"
                        angle x y
                        )))
    (dom-set-attribute node 'transform str)
    ;; Use new bbox corner for translation.
    (setq bbox (ii/svg-bbox node))

    ;; Use this offset to move the object to cursor position.
    ;; Left top corner for translation and center for rotation
    (setq ii/canvas--temp-points (cons (cons (nth 0 bbox) (nth 1 bbox))
                                       (cons x y)))
    ))

(defun ii/canvas--move-to (node x y)
  (let* ((angle (dom-attr node :angle))
         (off (car ii/canvas--temp-points))
         (center (cdr ii/canvas--temp-points))
         (str (format "translate(%s, %s)"
                      (- x (car off)) (- y (cdr off))
                      )))
    (if angle
        (setq str (concat str (format " rotate(%s, %s, %s)"
                                      angle (car center) (cdr center)
                                      ))))
    (dom-set-attribute node 'transform str)
    ))

(defun ii/canvas--move-cancel ()
  "Restore status before move."
  (mapc (lambda (a)
          (dom-set-attribute a 'transform
                             (dom-attr a :init)))
        ii/canvas--undo-marker))

(defun ii/canvas--select (nodes)
  (mapc (lambda (a)
          (unless (and (dom-attr a 'id)
                       (string-match-p "^_[ms]" (dom-attr a 'id)))
            (push a ii/canvas--undo-marker)
            (dom-set-attribute a 'stroke-dasharray "10,10")))
        nodes))

(defun ii/canvas--deselect ()
  (mapc (lambda (a)
          (dom-set-attribute a 'stroke-dasharray nil))
        ii/canvas--undo-marker)
  ;; (message "%s" ii/canvas--undo-marker)
  (setq ii/canvas--undo-marker nil)
  )

(defun ii/canvas--group (svg objects)
  (when (> (length objects) 1)
    (ii/canvas--deselect)
    (mapc (lambda (node)
            (dom-remove-node svg node))
          objects)
    (dom-append-child svg
                      (nconc (dom-node
                              'g
                              `((id . ,(number-to-string ii/canvas--id))))
                             objects))
    (setq ii/canvas--id (1+ ii/canvas--id))
    (ii/svg-possibly-update-image svg)
    ))

(defun ii/canvas--ungroup (svg objects)
  (mapc (lambda (node)
          (when (eq (dom-tag node) 'g)
            (dom-remove-node svg node)
            (let ((attr (dom-attr node 'transform)))
              (if attr
                  (dolist (a (dom-children node))
                    (dom-set-attribute a 'transform attr))))
            (nconc svg (dom-children node))
            ))
        objects)
  (ii/svg-possibly-update-image svg))

(defun ii/canvas--undo (svg)
  (unless ii/canvas--undo-marker
    (setq ii/canvas--undo-marker (last (dom-children svg))))
  (nconc ii/canvas--undo-marker (dom-by-id svg "_marker"))
  ;; (message "%s" ii/canvas--undo-marker)
  (mapc (lambda (a)
          (unless (member (dom-attr a 'id)
                          '("_defs"))
            (dom-remove-node svg a)))
        ii/canvas--undo-marker)
  (setq ii/canvas--undo-marker nil)
  ;; Undo: Delete node using current ii/canvas--undo-marker.
  ;; Unset mode
  (ii/svg-possibly-update-image svg))

(defun ii/canvas--transform (svg area)
  "Transform co-ordinates according to view box.
Returns (LIST X Y W H SCALE-X SCALE-Y)."
  (let* ((viewBox nil)
         ;; x, y, w and h represents the drawing box
         (w (- (getf area :right) (getf area :left)))
         (h (- (getf area :bottom) (getf area :top)))
         (x (getf area :left))
         (y (getf area :top))
         x1 y1 ;; ViewBox co-ordinates
         ;; Variables for scaling
         width1 height1
         width height scale-x scale-y
         )

    (setq x1 0 y1 0
          width (dom-attr svg 'width)
          height (dom-attr svg 'height))
    (setq viewBox (ii/svg-parse-viewBox svg)
          x1       (nth 0 viewBox)
          y1       (nth 1 viewBox)
          width1   (nth 2 viewBox)
          height1  (nth 3 viewBox))

    (assert (and (> height 0) (> width 0)))
    ;; Scale dimensions
    (setq scale-x (/ width1 1.0 width)
          scale-y (/ height1 1.0 height)
          x (+ x1 (* x scale-x))
          y (+ y1 (* y scale-y))
          w (* w scale-x)
          h (* h scale-y))
    (list x y w h scale-x scale-y)))

(defun ii/canvas--apply-transform (nodes &optional force)
  "Apply linear transformation to NODES."
  ;; While moving objects, we want to start at current position of the
  ;; object AFTER applying existing transformation. This function
  ;; helps in that.
  (mapc (lambda (a)
          (let ((transformed (dom-attr a :transformed)))
            (when (or transformed force)
              ;; Transformation is not necessarily linear only.
              ;; Hence, save transform values.
              (dom-set-attribute a :init (dom-attr a 'transform))
              (dom-set-attribute a :transformed nil)
              ))
          ;; (if (dom-attr a 'transform)
          ;;     (let ((bbox (ii/svg-bbox a))
          ;;           x y)
          ;;       (pcase (dom-tag a)
          ;;         ('line (dom-set-attribute a 'x1 (nth 0 bbox))
          ;;                (dom-set-attribute a 'y1 (nth 1 bbox))
          ;;                (dom-set-attribute a 'x2 (nth 2 bbox))
          ;;                (dom-set-attribute a 'y2 (nth 3 bbox)))
          ;;         ('rect (dom-set-attribute a 'x (nth 0 bbox))
          ;;                (dom-set-attribute a 'y (nth 1 bbox)))
          ;;         ((or 'circle ellipse)
          ;;          (setq x (/ (+ (nth 0 bbox) (nth 2 bbox)) 2)
          ;;                y (/ (+ (nth 1 bbox) (nth 3 bbox)) 2))
          ;;          (dom-set-attribute a 'cx x)
          ;;          (dom-set-attribute a 'cy y))
          ;;         )
          ;;       (dom-set-attribute a 'transform nil)
          ;;       ))
          )
        nodes))

(defun ii/canvas--adjacent-obj (svg x y)
  "Highlight the object closest to point (X, Y).
Returns a point on the object closer than `limit' if it exists;
else NIL."
  (let ((tags (dom-children svg))
        (flag t)
        (limit 10)
        (closest 10)
        bbox x1 y1 x2 y2 id tag dist points
        closest-x closest-y
        )
    ;; (setq x (getf area :right)
    ;;       y (getf area :bottom))

    ;; Reset markers
    ;; (ii/svg-polygon svg nil :id "_marker_o" :display "none")
    ;; (ii/svg-line svg 0 0 0 0 :id "_marker_r" :display "none")
    (mapc (lambda (a)
            (dom-set-attribute a 'display "none"))
          (dom-by-id svg "_marker"))

    (setq ii/canvas--nearest-objects nil)
    (while (and tags flag)
      (setq tag (car tags)
            bbox (ii/svg-bbox tag)
            id (dom-attr tag 'id))
      ;; (when (string-match-p "_marker" id)
      ;;   ;; Reset markers
      ;;   (dom-set-attribute tag 'r 0)
      ;;   (ii/svg-circle svg 0 0 1 :id id :visibility "none")
      ;;   )

      (when (and bbox id
                 (/= ii/canvas--id (string-to-number id))
                 (not (string-match-p "^_" id)))
        (setq x1 (nth 0 bbox)
              y1 (nth 1 bbox)
              x2 (nth 2 bbox)
              y2 (nth 3 bbox))
        (when (and (> x (- (min x1 x2) limit))
                   (> y (- (min y1 y2) limit))
                   (< x (+ (max x1 x2) limit))
                   (< y (+ (max y1 y2) limit)))
          ;; Using approximation for speed
          (setq dist (min (- x (min x1 x2))
                          (abs (- x (/ (+ x1 x2) 2)))
                          (- (max x1 x2) x)
                          (- y (min y1 y2))
                          (abs (- y (/ (+ y1 y2) 2)))
                          (- (max y1 y2) y)))
          (when (or (<= dist closest)
                    (eq ii/canvas--outline-style 'solid))
            ;; (setq closest dist)
            (setq flag nil)
            (setq points (ii/canvas--obj-outline tag
                                                 x1 y1 x2 y2 limit)
                  closest (seq-reduce (lambda (r a)
                                        (let* ((x1 (- (car a) x))
                                               (y1 (- (cdr a) y))
                                               (d (+ (* x1 x1)
                                                     (* y1 y1))))
                                          (if (> d r)
                                              r
                                            (setq closest-x (car a)
                                                  closest-y (cdr a))
                                            d)))
                                      points (* limit limit))
                  closest (sqrt closest))
            (push tag ii/canvas--nearest-objects)
            )))
      (setq tags (cdr tags)))
    (if closest-x (cons closest-x closest-y))))

(defun ii/canvas--highlight-obj (svg objects)
  "Highlight nearest object."
  (mapc (lambda (a)
          (unless (and (dom-attr a 'id)
                       ;; Don't hightlight currently drawn object
                       (or (= ii/canvas--id (string-to-number (dom-attr a 'id)))
                           (string-match-p "^_[ms]" (dom-attr a 'id))))
            (let* ((bbox (ii/svg-bbox a))
                   (x1 (nth 0 bbox))
                   (y1 (nth 1 bbox))
                   (x2 (nth 2 bbox))
                   (y2 (nth 3 bbox))
                   ;; (id "_marker_o")
                   (id (format "_marker_o_%s" (dom-attr a 'id)))
                   (marker (when ii/canvas--outline-style
                             (format "url(%s)" ii/canvas--outline-marker)))
                   (points (ii/canvas--obj-outline a
                                                   x1 y1 x2 y2 10)))
              (cond ((eq (dom-tag a) 'polyline)
                     (setq points (dom-attr a 'points))
                     (ii/svg-node svg 'polyline :id id :fill "none"
                                  :points points
                                  :stroke ii/canvas--stroke-color
                                  :stroke-width 2
                                  :stroke-dasharray "5,10"
                                  :marker-end marker
                                  :marker-start marker :marker-mid marker))
                    ((eq (dom-tag a) 'g)
                     (ii/canvas--highlight-obj svg (dom-children a)))
                    ((eq ii/canvas--outline-style 'solid)
                     (ii/svg-polygon svg points :id id :fill "none"
                                     :stroke-width 8
                                     :stroke-linejoin "round"
                                     :opacity 0.6
                                     :stroke "red" :rx 0 :ry 0))
                    (t
                     (ii/svg-polygon svg points :id id :fill "none"
                                     :stroke (if (memq (dom-tag a)
                                                       '(circle ellipse))
                                                 0 ii/canvas--stroke-color)
                                     :stroke-width 2
                                     :stroke-dasharray "5,10"
                                     :marker-start marker :marker-mid marker)))
              )))
        objects))

(defun ii/canvas--obj-outline (tag x1 y1 x2 y2 limit)
  "Return points on the outline."
  (let (points p)
    (if (and (eq ii/canvas--mode 'conn)
             (dom-attr tag 'ii/canvas:ports))
        (dolist (a (split-string (dom-attr tag 'ii/canvas:ports) "[ ,]"))
          (if (= (length p) 1)
              (push (cons (string-to-number (pop p))
                          (string-to-number a))
                    points)
            (push a p)))
      (pcase (dom-tag tag)
        ('line (if (< (abs (- x2 x1)) (/ limit 2))
                   (setq x2 x1))
               (if (< (abs (- y2 y1)) (/ limit 2))
                   (setq y2 y1))
               (push (cons x1 y1) points)
               (push (cons (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)) points)
               (push (cons x2 y2) points))
        ((or 'circle 'ellipse)
         (push (cons x1 (/ (+ y1 y2) 2)) points)
         (push (cons (/ (+ x1 x2) 2) y2) points)
         (push (cons x2 (/ (+ y1 y2) 2)) points)
         (push (cons (/ (+ x1 x2) 2) y1) points))
        ;; ('polyline nil)
        (_
         (push (cons x1 y1) points)
         (push (cons x1 (/ (+ y1 y2) 2)) points)
         (push (cons x1 y2) points)
         (push (cons (/ (+ x1 x2) 2) y2) points)
         (push (cons x2 y2) points)
         (push (cons x2 (/ (+ y1 y2) 2)) points)
         (push (cons x2 y1) points)
         (push (cons (/ (+ x1 x2) 2) y1) points))
        ))
    points))

(defun ii/canvas--draw-init (svg area)
  (let (res x y w h closest)
    (ii/canvas--apply-transform ii/canvas--undo-marker)
    (setq res (ii/canvas--transform svg area)
          x (nth 0 res)
          y (nth 1 res)
          w (nth 2 res)
          h (nth 3 res)
          ii/canvas--nearest-objects nil)
    ;; Use the end point of area
    (setq closest (ii/canvas--adjacent-obj svg (+ x w) (+ y h)))
    (ii/canvas--highlight-obj svg ii/canvas--nearest-objects)
    (ii/canvas--highlight-obj svg ii/canvas--undo-marker)

    (when (and (eq ii/canvas--mode 'conn)
               ii/canvas--temp-points)
      ;; Animate drawing of connection points
      (let* ((x (or (car closest) x))
             (y (or (cdr closest) y))
             (point (car ii/canvas--temp-points))
             (x1 (car point))
             (y1 (cdr point))
             (id (number-to-string ii/canvas--id))
             (fg-color ii/canvas--stroke-color)
             (fill ii/canvas--fill-color)
             (points nil)
             dx dy
             )
        ;; (message "%s %s %s %s" ii/canvas--nearest-objects x y ii/canvas--temp-points)
        ;; (message "%s" ii/canvas--temp-points)
        ;; points must be a list
        (setq points (append (list (cons x y)
                                   (unless (or (null point) (= x1 x) (= y1 y))
                                     (setq dx (- x1 x)
                                           dy (- y1 y))

                                     (if ii/canvas--routing
                                         (if (> (abs dx) (abs dy))
                                             (cons (- x1 (* (signum dx) (abs dy))) y)
                                           (cons x (- y1 (* (signum dy) (abs dx)))))
                                       (if (> (abs dx) (abs dy))
                                           (cons x y1)
                                         (cons x1 y)))))
                             ii/canvas--temp-points))
        (ii/svg-polyline svg points
                         :stroke-width ii/canvas--stroke-width
                         :id id :stroke-color fg-color :fill fill)
        ))

    (when (memq ii/canvas--mode '(place move))
      ;; Animate movement of selected object
      (mapc (lambda (a)
              (funcall ii/canvas--move-to-fn a x y))
            ii/canvas--undo-marker))

    (ii/svg-possibly-update-image svg)
    ))

(defun ii/canvas--draw (svg area)
  (let ((fg-color ii/canvas--stroke-color)
        (points nil)
        (fill ii/canvas--fill-color)
        (res nil)
        x1 y1 closest x y w h node
        (id (number-to-string ii/canvas--id)))

    (setq res (ii/canvas--transform svg area)
          x (nth 0 res)
          y (nth 1 res)
          w (nth 2 res)
          h (nth 3 res))

    ;; Snap to nearest object
    (when (memq ii/canvas--mode
                '(line circle rect ellipse text conn move define-port path))
      (setq closest (ii/canvas--adjacent-obj svg (+ x w) (+ y h)))
      (if closest
          (if (= w h 0)
              ;; Start point
              (progn
                (setq x (car closest)
                      y (cdr closest))
                (setf (getf area :left) x
                      (getf area :top) y))
            (setq x1 (car closest)
                  y1 (cdr closest)
                  w (- x1 x)
                  h (- y1 y)))

        ;; Snap to grid
        (when ii/canvas--grid-size
          (setq x (* (round (/ x ii/canvas--grid-size)) ii/canvas--grid-size)
                y (* (round (/ y ii/canvas--grid-size)) ii/canvas--grid-size))
          (setq w (* (round (/ w ii/canvas--grid-size)) ii/canvas--grid-size)
                h (* (round (/ h ii/canvas--grid-size)) ii/canvas--grid-size)))
        ))
    ;; (message "%s %s %s %s %s %s" res area x y w h)


    ;; Horizontal or vertical ruler
    (when (memq ii/canvas--mode
                '(line move))
      (cond ((= h 0)
             (ii/svg-line svg "0%" y "100%" y
                          :id "_marker_r" :fill "none" :stroke fg-color
                          :stroke-dasharray "5,5"))
            ((= w 0)
             (ii/svg-line svg x "0%" x "100%"
                          :id "_marker_r" :fill "none" :stroke fg-color
                          :stroke-dasharray "5,5"))))

    (pcase ii/canvas--mode
      ('line (ii/svg-line svg x y (+ x w) (+ y h)
                          :stroke-width ii/canvas--stroke-width
                          :id id :stroke-color fg-color))
      ('circle (ii/svg-circle svg x y w
                              :stroke-width ii/canvas--stroke-width
                              :id id :stroke-color fg-color :fill fill))
      ('rect (ii/svg-rectangle svg x y w h
                               :stroke-width ii/canvas--stroke-width
                               :id id :stroke-color fg-color :fill fill))
      ('ellipse (ii/svg-ellipse svg x y (abs w) (abs h)
                                :stroke-width ii/canvas--stroke-width
                                :id id :stroke-color fg-color :fill fill))
      ('text (ii/svg-text svg (nth 0 ii/canvas--text)
                          :font-size (number-to-string (nth 1 ii/canvas--text))
                          :font-family (nth 2 ii/canvas--text)
                          :x x :y y
                          :stroke-width ii/canvas--stroke-width
                          :id id :stroke-color fg-color :fill fill))
      ((or 'polyline 'erase)
       (setq node (car (dom-by-id svg (format "^%s$" id)))
             points (dom-attr node 'points))
       ;; (message "%s %s %s" id points node)
       (if points
           (progn
             ;; points is a string
             (setq points (format "%s %s,%s" points (+ x w) (+ y h)))
             (dom-set-attribute node 'points points)
             ;; (ii/svg-possibly-update-image svg)
             )

         ;; points must be a list
         (setq points (list (cons x y)))
         ;; (message "%s" area)
         (ii/svg-polyline svg points
                          :stroke-width ii/canvas--stroke-width
                          :id id :stroke-color fg-color :fill fill)))
      ('path
       (setq node (car (dom-by-id svg (format "^%s$" id)))
             ii/canvas--temp-points (append ii/canvas--temp-points `((,x . ,y)))
             points ii/canvas--temp-points)
       ;; (message "%s %s %s" id points node)
       (ii/canvas--path svg ii/canvas--temp-points id))
      ('define-port
       (when ii/canvas--port-name
         ;; (message "%s %s %s" x y ii/canvas--nearest-objects)
         ;; Place text outside bbox
         (let* ((bbox (ii/svg-bbox (car ii/canvas--nearest-objects)))
                (x1 (nth 0 bbox))
                (y1 (nth 1 bbox))
                (x2 (nth 2 bbox))
                (y2 (nth 3 bbox))
                (tw (length ii/canvas--port-name))
                (tx x)
                (ty y))
           (if (<= x x1) (setq tx (- x ii/canvas--grid-size tw)))
           (if (>= x x2) (setq tx (+ x ii/canvas--grid-size)))
           (if (<= y y1) (setq ty (- y ii/canvas--grid-size)))
           (if (>= y y2) (setq ty (+ y ii/canvas--grid-size)))
           (ii/svg-text svg ii/canvas--port-name
                        :x tx
                        :y ty
                        :stroke-width ii/canvas--stroke-width
                        :id (concat "_name" (number-to-string ii/canvas--port-id))
                        :stroke-color fg-color :fill fill)
           (ii/svg-circle svg x y 5
                          :stroke-width ii/canvas--stroke-width
                          :id (concat "_port" (number-to-string ii/canvas--port-id))
                          :name ii/canvas--port-name
                          :stroke-color fg-color :fill fill))))
      ('conn (let* ((point (car ii/canvas--temp-points))
                    (x1 (car point))
                    (y1 (cdr point))
                    dx dy
                    )
               ;; (message "%s" ii/canvas--temp-points)
               (if (null point)
                   (push (cons x y) ii/canvas--temp-points)
                 (unless (or (= x1 x) (= y1 y))
                   (setq dx (- x1 x)
                         dy (- y1 y))

                   (setq ii/canvas--temp-points
                         (append (list
                                  (cons x y)
                                  (if ii/canvas--routing
                                      (if (> (abs dx) (abs dy))
                                          (cons (- x1 (* (signum dx) (abs dy))) y)
                                        (cons x (- y1 (* (signum dy) (abs dx)))))))
                                 ii/canvas--temp-points))
                   ))))
      ((or 'move 'place)
       (when (eq ii/canvas--mode 'place)
         ;; Translate to x,y. Set id.
         (setq w x h y ii/canvas--id (1+ ii/canvas--id))
         (dom-set-attribute (car ii/canvas--undo-marker) 'id id))

       ;; (cond ((null ii/canvas--undo-marker)
       ;;        (setq ii/canvas--mode nil))
       ;;       (t
       ;;        (mapc (lambda (a)
       ;;                (let ((str (dom-attr a :init))
       ;;                      (off ii/canvas--temp-points))
       ;;                  (dom-set-attribute a :transformed t)
       ;;                  (dom-set-attribute a 'transform
       ;;                                     (format "%s translate(%s, %s)"
       ;;                                             (or str "")
       ;;                                             (- x (car off))
       ;;                                             (- y (cdr off))
       ;;                                             ))))
       ;;              ii/canvas--undo-marker)))

       ;; (ii/svg-possibly-update-image svg)
       )
      (_ (ii/svg-rectangle svg x y w h
                           :stroke-dasharray (format "%0.2f,%0.2f"
                                                     (* 10 (nth 4 res))
                                                     (* 10 (nth 5 res)))
                           :id "_selection"
                           :stroke-color (foreground-color-at-point) :fill "none"))
      )
    (ii/canvas--highlight-obj svg ii/canvas--nearest-objects)
    (ii/svg-possibly-update-image svg)
    ))

(defun ii/canvas--path (svg points id)
  "Convert points to SVG path."
  (let* ((fg-color ii/canvas--stroke-color)
         (fill ii/canvas--fill-color)
         cmds p)
    (setq p (pop points)
          cmds (append `((moveto ,p))
                       (cl-loop for (a b) on points by #'cddr while b
                                collect `(smooth-curveto (,a ,b)))))
    ;; (message "%s" cmds)
    (ii/svg-path svg cmds
                 :stroke-width ii/canvas--stroke-width
                 :id id :stroke-color fg-color :fill fill)
    ))

(defun ii/canvas--zoom (svg area)
  (let* ((x (nth 0 area))
         (y (nth 1 area))
         (width (- (nth 2 area) (nth 0 area)))
         (height (- (nth 3 area) (nth 1 area)))
         (aspect 1)
         grid node
         )
    (setq aspect (/ (dom-attr svg 'width) (dom-attr svg 'height) 1.0))
    (if (> width height)
        (setq height (/ width aspect))
      (setq width (* height aspect)))
    ;; (message "%s %s %s" width height (= height width 0))
    (assert (and (> height 0) (> width 0)))

    (setq ii/canvas--outline-marker
          (cond ((< width 20) "#dot001")
                ((< width 50) "#dot01")
                (t "#dot")))
    (dom-set-attribute svg 'viewBox
                       (format "%s %s %s %s" x y width height))
    (setq grid
          (cond ((< width 40) "x5Grid")
                ((< width 100) "x10Grid")
                (t "x20Grid"))
          node (car (dom-children (car (dom-by-id svg "^grid$")))))
    (when node
      (dom-set-attribute node 'fill (format "url(#%s)" grid)))

    (ii/svg-possibly-update-image svg)))

(defun ii/canvas--select-in-group (node x y)
  "Select the object closest to x, y in a group."
  (let (res prev transform)
    (when (eq (dom-tag node) 'g)
      (setq transform (dom-attr node 'transform))
      (mapc (lambda (a)
              (when (eq (dom-tag a) 'g)
                (setq prev (dom-attr a 'transform))
                (dom-set-attribute a 'transform transform)
                (if (ii/canvas--select-in-group a x y)
                    (push a res))
                (dom-set-attribute a 'transform prev)
                ))
            (dom-children node))
      (unless res
        (let* ((bbox (ii/svg-bbox node))
               (x1 (nth 0 bbox))
               (y1 (nth 1 bbox))
               (x2 (nth 2 bbox))
               (y2 (nth 3 bbox)))
          ;; (message "%s %s" bbox node)
          (if (and (>= x x1) (<= x x2)
                   (>= y y1) (<= y y2))
              (setq res node)
            ))
        ))
    res))

(defun ii/canvas--act-in-region (svg area)
  "Zoom or highlight shapes inside area via dashes."
  (let* ((res (ii/canvas--transform svg area))
         (x (nth 0 res))
         (y (nth 1 res))
         (w (nth 2 res))
         (h (nth 3 res)))
    (pcase ii/canvas--mode
      ('zoom ;; Remove selection box
       ;; (message "%s\n%s" area (last (dom-children svg)))
       (dom-remove-node svg (car (dom-by-id svg "_selection")))
       (ii/canvas--zoom svg (list x y (+ x w) (+ y h)))
       ;; (message "%s %s" (dom-attributes svg) area)
       (ii/svg-possibly-update-image svg))
      ('crop ;; Remove selection box
       (when (y-or-n-p "Crop region? ")
         ;; (setq x 0 y 0 w 400 h 400)
         ;; (message "%s" (list x y w h))
         (let* ((tmp (copy-tree svg))
                (node (car (dom-by-id tmp "_grid")))
                (attr (dom-attr node 'display))
                file)
           ;; Co-ordinates are absolute; remove viewbox
           ;; Disable grid
           (dom-set-attribute tmp 'viewBox nil)
           (dom-set-attribute node 'display "none")
           (dom-remove-node tmp (car (dom-by-id svg "_selection")))
           (setq file (ii/canvas-crop-image tmp (list x y (+ x w) (+ y h))))
           (dom-set-attribute node 'display attr)

           (setq ii/canvas--undo-marker (dom-children svg))
           (ii/canvas--undo svg)
           (ii/svg-embed svg file
                         (format "image/%s" (image-type file nil t)) t
                         :id (number-to-string ii/canvas--id)
                         :x x :y y
                         :width w
                         :height h
                         )
           (setq ii/canvas--id (1+ ii/canvas--id)
                 ii/canvas--mode nil)
           (ii/svg-possibly-update-image svg)))
       (dom-remove-node svg (car (dom-by-id svg "_selection")))
       )
      ((guard (eq ii/canvas--mode nil))
       ;; Remove selection box
       (dom-remove-node svg (car (dom-by-id svg "_selection")))
       (ii/canvas--select (or (ii/svg--shapes-in-region svg (list x y (+ x w) (+ y h)))
                              ii/canvas--nearest-objects))
       ;; (message "%s" ii/canvas--undo-marker)
       (ii/svg-possibly-update-image svg))
      ((or 'conn 'path) nil)
      ('place (let* ((node (copy-tree ii/canvas--undo-marker)))
                (ii/canvas--deselect)
                ;; Append to current image
                (nconc svg node)
                (setq ii/canvas--undo-marker nil)
                (ii/canvas--select node)
                (ii/canvas--move-init node)))
      ('move
       (setq ii/canvas--mode nil)
       (ii/canvas--deselect))
      (_ (setq ii/canvas--id (1+ ii/canvas--id))))))

(defun ii/canvas--split-window (file &optional svg name)
  "Show SVG FILE in a new NAME window.
Resizes the image to fit the window."
  (let* ((svg (or svg (ii/svg-load file)))
         (buf (get-buffer-create (or name "*temp*")))
         (cur (current-buffer))
         w h)
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf
      (erase-buffer)
      ;; Resize to fit new window
      (setq w (window-pixel-width)
            h (window-pixel-height)
            svg (ii/svg--extract-fragment svg nil w h))
      (insert-image (ii/svg-image svg)))

    ;; Resize to fit current window
    (pop-to-buffer cur)
    (setq w (window-pixel-width)
          h (window-pixel-height)
          svg (car ii/canvas--svg)
          svg (ii/svg--extract-fragment svg nil w h))
    (ii/svg-possibly-update-image svg)
    ))

(defun ii/canvas-crop-image (svg area)
  (let* ((w (- (nth 2 area) (nth 0 area)))
         (h (- (nth 3 area) (nth 1 area)))
         (type "png"))
    ;; (with-temp-buffer
    (with-current-buffer (get-buffer-create "b.png")
      (set-buffer-multibyte nil)
      (image-mode-as-text)
      (erase-buffer)
      (ii/svg-print svg)
      (call-process-region
       (point-min) (point-max) "convert"
       t (list (current-buffer) "*Messages*") nil
       "-crop"
       (format
        ;; width x height + left + top
        "%dx%d+%d+%d" w h (nth 0 area) (nth 1 area))
       "+repage"
       "-" (format "%s:-" type))
      ;; (save-buffer)
      (buffer-string)
      )))

(defvar ii/svg--float-left 0)
(defvar ii/svg--float-right 0)
(defun ii/svg-render-tag (svg node x y width height
                              &optional padding margin border)
  (let* ((i ii/canvas--id)
         (padding (or padding 0))
         (margin (or margin 0))
         (border (or border 0))
         (w 0)
         (h 0)
         (left x)
         (right (+ x width))
         (top y)
         (width-max 0)
         (height-max 0)
         g box bbox added break n
         (added1 nil))
    ;; (message "%s" (dom-tag node))
    (pcase (dom-tag node)
      ((or 'body 'div 'h1 'h2 'h3 'p 'a 'ul 'ol 'li 'i 'b 'span
           'code 'pre 'label 'button 'nav 'header 'main 'abbr 'small 'footer
           'sup 'sub 'strong
           'table 'td 'tr 'th 'tbody)
       (if (eq (dom-tag node) 'a)
           (setq g (dom-node 'a
                             `((:id  . ,(number-to-string (+ i 1)))
                               (text . ,(dom-attr node 'text))
                               (href . ,(dom-attr node 'href)))))
         (setq g (ii/svg-group nil nil
                               :id (number-to-string (+ i 1))
                               :data-id (dom-attr node 'id)
                               :data-class (dom-attr node 'class)
                               :class (dom-tag node))))
       ;; height (if (memq (dom-tag node) '(h2 p)) 15 height)
       (setq box (ii/svg-rectangle g x y width height
                                   :id (number-to-string (+ i 2))
                                   :fill "none"
                                   :stroke "black"
                                   ;; :stroke-width border
                                   :class (dom-attr node 'class)))
       (setq i (+ i 2)))
      ('\#text
       (when (not (string-empty-p (setq node (string-trim node))))
         (setq g (ii/svg-group svg nil
                               :id (number-to-string (+ i 1)))
               added t)
         (let* ((h (or height 15))
                (n (/ (* width) h))
                (size (length node))
                start str)
           (with-temp-buffer
             (insert node)
             (goto-char (point-min))
             (while (not (eobp))
               (setq start (point))
               (if (> (+ start n) size)
                   (goto-char (1+ size))
                 (goto-char (+ start n))
                 (forward-word 1))
               (setq str (buffer-substring-no-properties start (point))
                     y (+ y h)
                     i (1+ i))
               (ii/svg-text g str
                            :x x
                            :y y
                            :font-size h
                            :id (number-to-string (+ i 1)))
               ;; (message "%s %s %s" size n str)
               )))
         ;; (setq added t)
         ;; (ii/svg-text svg node
         ;;           :x x
         ;;           :y (+ y (or height 15))
         ;;           :font-size (or height 15)
         ;;           :id (number-to-string (+ i 1)))
         ))
      )
    (unless (memq (dom-tag node)
                  '(\#text head script noscript style comment form map))
      ;; Need to process children of unknown tag types
      (setq n (if (eq (dom-tag node) 'tr) (1- (length (dom-children node))) 1))
      (mapc (lambda (a)
              (setq break (or (memq (dom-tag a) '(h1 h2 p br ul li tr))))
              (when (setq added1
                          ;; (if (setq added1
                          (ii/svg-render-tag
                           g a 0 0
                           (if width  (- (/ width n)  (* 2 (+ border margin padding))))
                           (if height (- height (* 2 (+ border margin padding))))
                           padding margin border))
                (setq bbox (or (ii/svg-bbox (last g))
                               '(0 0 0 0))
                      w (if (memq (dom-tag node) '(th td))
                            width
                          (- (nth 2 bbox) (nth 0 bbox)))
                      h (- (nth 3 bbox) (nth 1 bbox))
                      w (+ (* 2 (+ border margin padding)) w)
                      h (+ (* 2 (+ border margin padding)) h)
                      added t
                      i (1+ i))
                ;; (message "%s %s, %s %s %s %s %s %s"
                ;;          (dom-tag node) a x y left right height-max w)
                (when (or (and (> x left) break)
                          (> (+ x w) right))
                  (setq width-max (max width-max (+ (- x left)))
                        x left
                        y (+ y height-max)
                        break nil
                        height-max 0))
                (dom-set-attribute (last g) 'transform
                                   (format "translate(%d,%d)"
                                           (+ x border margin padding)
                                           (+ y border margin padding)
                                           ))
                (setq x (+ x w)
                      height-max (max height-max h)))
              )
            (dom-children node))

      (when (and added box)
        (dom-append-child svg g)
        (setq width-max (max width-max (+ (- x left)))
              height-max (+ (max height-max h) (- y top)))
        (dom-set-attribute box 'width (if width
                                          (min width width-max)
                                        width-max))
        (dom-set-attribute box 'height (if height
                                           (min height height-max)
                                         height-max)))
      )
    (setq ii/canvas--id (+ i 1))
    added))

(defun ii/svg-render-html (&optional dom)
  "Render HTML DOM as an SVG image."
  (interactive)
  (let* ((w (window-pixel-width))
         (h (window-pixel-height))
         (svg (ii/svg-create w h :font-family "DejaVu Sans Mono"))
         (ii/canvas--id 0)
         (width w)
         (padding 8)
         (source nil)
         bbox)
    (unless dom
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    ;; (nconc svg (dom-by-tag dom 'style))

    (ii/svg-render-tag svg (dom-by-tag dom 'body) 0 0 width nil padding)

    (with-current-buffer (get-buffer "a.svg")
      (setq inhibit-modification-hooks t
            bbox (ii/svg-bbox svg))
      (dom-set-attribute svg 'width  (- (nth 2 bbox) (nth 0 bbox)))
      (dom-set-attribute svg 'height (- (nth 3 bbox) (nth 1 bbox)))
      (image-mode-as-text)
      (erase-buffer)

      (when source
        (setq cursor-type t)
        (ii/svg-print svg)
        (goto-char (point-min))
        (while (re-search-forward "background" nil t)
          (replace-match  "fill" nil nil))
        (call-process-region (point-min) (point-max) "xmllint" t t nil
                             "--format" "-")
        (image-mode)
        )

      (unless source
        (setq cursor-type nil)
        (dotimes (i (/ (- (nth 3 bbox) (nth 1 bbox)) h))
          (dom-set-attribute svg 'height h)
          (dom-set-attribute svg 'viewBox (format "0 %d %d %d" (* i h) w h))
          ;; (ii/svg-image-map svg)
          (ii/svg-insert-image svg)
          (ii/svg-possibly-update-image svg)
          (newline)
          ))
      (save-buffer))
    svg))

;; ewp functions copied from https://github.com/larsmagne/ewp/blob/master/ewp.el
(defun ewp-crop-image (&optional square)
  "Crop the image under point.
If SQUARE (the prefix), crop a square from the image."
  (interactive "P")
  (let ((image (get-text-property (point) 'display)))
    (when (or (not image)
              (not (consp image))
              (not (eq (car image) 'image)))
      (error "No image under point"))
    ;; We replace the image under point with an SVG image that looks
    ;; just like that image.  That allows us to draw lines over it.
    ;; At the end, we replace that SVG with a cropped version of the
    ;; original image.
    (let* ((data (getf (cdr image) :data))
           (undo-handle (prepare-change-group))
           (orig-data data)
           (type (cond
                  ((getf (cdr image) :format)
                   (format "%s" (getf (cdr image) :format)))
                  (data
                   (ewp-content-type data))))
           (image-scaling-factor 1)
           (size (image-size image t))
           (svg (ii/svg-create (car size) (cdr size)
                               :xmlns:xlink "http://www.w3.org/1999/xlink"
                               :stroke-width 5))
           (text (buffer-substring (line-beginning-position)
                                   (line-end-position)))
           (inhibit-read-only t))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (if (null data)
            (insert-file-contents-literally (getf (cdr image) :file))
          (insert data))
        ;; (let ((ewp-exif-rotate nil))
        ;;   (ewp-possibly-rotate-buffer image))
        (setq orig-data (buffer-string))
        (setq type (ewp-content-type orig-data))
        (call-process-region (point-min) (point-max)
                             "convert" t (current-buffer) nil
                             "-resize" "600x"
                             "-"
                             (format "%s:-" (cadr (split-string type "/"))))
        (setq data (buffer-string)))
      (ii/svg-embed svg data type t
                    :width (car size)
                    :height (cdr size))
      (delete-region (line-beginning-position)
                     (line-end-position))
      (ii/svg-insert-image svg)
      (let ((area (condition-case _
                      (save-excursion
                        (forward-line 1)
                        (ewp-crop-image-1 svg square
                                          (car size) (cdr size)))
                    (quit nil))))
        (delete-region (line-beginning-position) (line-end-position))
        (if area
            (ewp-crop-image-update area orig-data size type)
          ;; If the user didn't complete the crop, re-insert the
          ;; original image (and text).
          (insert text))
        (undo-amalgamate-change-group undo-handle)))))

(defun ewp-crop-image-update (area data size type)
  (let* ((image-scaling-factor 1)
         (osize (image-size (create-image data (ewp--image-type) t) t))
         (factor (/ (float (car osize)) (car size))))
    ;; (ewp-insert-image-data
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (call-process-region
       (point-min) (point-max) "convert"
       t (list (current-buffer) nil) nil
       "+repage" "-crop"
       (format
        ;; width x height + left + top
        "%dx%d+%d+%d"
        (abs (truncate (* factor (- (getf area :right) (getf area :left)))))
        (abs (truncate (* factor (- (getf area :bottom) (getf area :top)))))
        (truncate (* factor (min (getf area :left) (getf area :right))))
        (truncate (* factor (min (getf area :top) (getf area :bottom)))))
       "-" (format "%s:-" (cadr (split-string type "/"))))
      (buffer-string))))

(defun ewp-crop-image-1 (_svg &optional square image-width image-height)
  (track-mouse
    (cl-loop with prompt = (if square "Move square" "Set start point [%s]")
             and state = (if square 'move-unclick 'begin)
             and area = (if square
                            (list :left (- (/ image-width 2)
                                           (/ image-height 2))
                                  :top 0
                                  :right (+ (/ image-width 2)
                                            (/ image-height 2))
                                  :bottom image-height)
                          (list :left 0
                                :top 0
                                :right 0
                                :bottom 0))
             and corner = nil
             for event = (read-event (if ii/canvas--hide-prompt
                                         " "
                                       (format prompt (or ii/canvas--mode "selection"))))
             and svg = (car ii/canvas--svg)
             ;; do (message "%s" event)
             do (if (or (not (consp event))
                        (not (consp (cadr event)))
                        (not (nth 7 (cadr event)))
                        ;; Only do things if point is over the SVG being
                        ;; tracked.
                        (not (eq (getf (cdr (nth 7 (cadr event))) :type)
                                 'svg)))
                    (setq event (funcall ii/canvas-plugin-fn event svg))
                  ;; (ii/canvas--select-mode event svg)
                  ;; (message "%s %s" event state)
                  (let ((pos (nth 8 (cadr event))))
                    ;; (setf (getf area :right) (car pos)
                    ;;       (getf area :bottom) (cdr pos))
                    (cl-case state
                      ('begin
                       (cond
                        ((eq (car event) 'double-mouse-1)
                         (setq event (funcall ii/canvas-plugin-fn (car event) svg)))
                        ((eq (car event) 'down-mouse-1)
                         ;; (if ii/canvas--mode
                         ;;     ;; We need updated id since delete
                         ;;     ;; operation can set this out of order.
                         ;;     (setq ii/canvas--id (length (dom-children svg))))
                         (setq state 'stretch
                               prompt "Stretch to end point [%s]")))
                       (setf (getf area :left) (car pos)
                             (getf area :top) (cdr pos)
                             (getf area :right) (car pos)
                             (getf area :bottom) (cdr pos)))
                      ('stretch
                       (cond
                        ((eq (car event) 'mouse-movement)
                         (setf (getf area :right) (car pos)
                               (getf area :bottom) (cdr pos)))
                        ((memq (car event) '(mouse-1 drag-mouse-1))
                         ;; (if ii/canvas--mode
                         ;; Mouse release must start a new polyline
                         ;; (setq ii/canvas--id (1+ ii/canvas--id))
                         (funcall ii/canvas-act-in-region-fn svg area)
                         ;; svg (list (getf area :left)
                         ;;           (getf area :top)
                         ;;           (getf area :right)
                         ;;           (getf area :bottom)))
                         (setq state 'begin;'corner
                               prompt "Set start point [%s]"
                               ;; prompt "Choose corner to adjust (RET to crop)"
                               ))))
                      ('corner
                       (cond
                        ((eq (car event) 'down-mouse-1)
                         ;; Find out what corner we're close to.
                         (setq corner (ewp-find-corner
                                       area pos
                                       '((:left :top)
                                         (:left :bottom)
                                         (:right :top)
                                         (:right :bottom))))
                         (when corner
                           (setq state 'adjust
                                 prompt "Adjust crop")))))
                      ('adjust
                       (cond
                        ((memq (car event) '(mouse drag-mouse-1))
                         (setq state 'corner
                               prompt "Choose corner to adjust"))
                        ((eq (car event) 'mouse-movement)
                         (setf (getf area (car corner)) (car pos)
                               (getf area (cadr corner)) (cdr pos)))))
                      ('move-unclick
                       (cond
                        ((eq (car event) 'down-mouse-1)
                         (setq state 'move-click
                               prompt "Move"))))
                      ('move-click
                       (cond
                        ((eq (car event) 'mouse-movement)
                         (setf (getf area :left) (car pos)
                               (getf area :right) (+ (car pos) image-height)))
                        ((memq (car event) '(mouse-1 drag-mouse-1))
                         (setq state 'move-unclick
                               prompt "Click to move"))))))
                  (if (eq state 'stretch)
                      (ii/canvas--draw svg area)
                    (ii/canvas--draw-init svg area)))
             while (not (member event '(return ?q)))
             finally (return (or (if ii/canvas-exit-fn (funcall ii/canvas-exit-fn))
                                 (and (eq event 'return)
                                      area))))))

(defun ewp-find-corner (area pos corners)
  (cl-loop for corner in corners
           ;; We accept 10 pixels off.
           when (and (< (- (car pos) 10)
                        (getf area (car corner))
                        (+ (car pos) 10))
                     (< (- (cdr pos) 10)
                        (getf area (cadr corner))
                        (+ (cdr pos) 10)))
           return corner))

(defun ewp-content-type (image)
  ;; Get the MIME type by running "file" over it.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert image)
    (call-process-region (point-min) (point-max)
                         "file" t (current-buffer) nil
                         "--mime-type" "-")
    (cadr (split-string (buffer-string)))))

(defun ewp--image-type ()
  (if (or (and (fboundp 'image-transforms-p)
               (image-transforms-p))
          (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))


;; from="0"
;; to="500"
;; dur="5s"
;; values="0;5;0"

;; <polygon points="60,30 90,90 30,90">
;;     <animateTransform
;;       attributeName="transform"
;;       attributeType="XML"
;;       type="rotate"
;;       from="0 60 70"
;;       to="360 60 70"
;;       dur="10s"
;;       repeatCount="indefinite" />
;; </polygon>

;; <rect width="10" height="10">
;;     <animate
;;       attributeName="rx"
;;       values="0;5;0"
;;       dur="10s"
;;       repeatCount="indefinite" />
;; </rect>

;; <circle r="5" fill="red">
;;     <animateMotion
;;       dur="10s"
;;       repeatCount="indefinite"
;;       path="M20,50 C20,-50 180,150 180,50 C180-50 20,150 20,50 z" />
;;   </circle>
(defun ii/svg-animation-metadata (image)
  (let* ((dur 0)
         repeat loop)
    (dolist (node (dom-by-tag image 'animate))
      (setq dur (max dur (string-to-number (dom-attr node 'dur))) ;; 1s
            repeat (dom-attr node 'repeatCount)
            loop (or loop (string= "indefinite" repeat))))
    (cons dur loop)))

(defun ii/svg--value-to-number (v)
  (let* ((names '(("red" . #xff0000) ("green" . #x00ff00) ("blue" . #x0000ff)))
         (res (cdr (assoc v names))))
    (when (not res)
      (setq res (cond ((string-match-p "^#" v)
                       (string-to-number (substring v 1) 16))
                      ((string-match-p "^[+-]?[0-9]" v) (string-to-number v))
                      (t v))))
    res))

(defvar ii/svg-frame-rate 25)
(defun ii/svg--interpolate (n count from to &optional values freeze)
  ;; n is zero based integer value
  (let* ((range count)
         i m intervals value)
    (setq m (% n count))
    (when values
      (setq values (split-string values ";")
            intervals (+ (length values)))
      (cond ((= 1 intervals)
             (setq value (car values)
                   from value
                   to value))
            ((>= intervals count)
             (setq value (nth m values)
                   from value
                   to value))
            (t
             ;; (message "%s %s %s" count intervals m)
             (setq range (/ count intervals)
                   i     (min (/ n range) (- intervals 2))
                   m     (if (>= m intervals) (% m range) m)
                   from  (nth i values)
                   value from ;; default for non-numeric value
                   to    (nth (1+ i) values))
             ;; (message "%s %s %s" range i m)
             (if (= range 2) (setq range 1))
             )))

    (when (number-or-marker-p (ii/svg--value-to-number from))
      (setq from (ii/svg--value-to-number from)
            to   (ii/svg--value-to-number to))
      ;; m is zero based
      (setq value (+ from (* (/ (- to from) range 1.0) (1+ m)))))

    (if (>= n count)
        (setq value (if freeze to from)))
    ;; (message "%s" value)
    value))

(defun test-interpolate ()
  (interactive)
  ;; (dotimes (i 11)
  ;;   (message "%d %06x" (- 5 i)
  ;;            (ii/svg--interpolate (- 5 i) 11 nil nil "red;#000000;green")))
  (assert (= (ii/svg--interpolate 0 3 nil nil "red;#000000;green") #xff0000))
  (assert (= (ii/svg--interpolate 1 3 nil nil "red;#000000;green") #x000000))
  (assert (= (ii/svg--interpolate 2 3 nil nil "red;#000000;green") #x00ff00))

  (setq ii/svg-frame-rate 1)
  (assert (= (ii/svg--interpolate 0 2 nil nil "0") 0))
  (assert (= (ii/svg--interpolate 0 2 "25" "15" "0") 0))
  (assert (= (ii/svg--interpolate 1 2 "25" "15" "0") 0))
  (assert (= (ii/svg--interpolate 2 2 "25" "15" "0") 0))

  (assert (= (ii/svg--interpolate 0 2 "25" "15" "0;5") 0))
  (assert (= (ii/svg--interpolate 1 2 "25" "15" "0;5") 5))
  (assert (= (ii/svg--interpolate 2 2 "25" "15" "0;5") 0))
  ;; (assert (= (ii/svg--interpolate 2 2 "25" "15" "0;5" nil t) 5))

  (assert (= (ii/svg--interpolate 0 3 "25" "15" "0;5;0") 0))
  (assert (= (ii/svg--interpolate 1 3 "25" "15" "0;5;0") 5))
  (assert (= (ii/svg--interpolate 2 3 "25" "15" "0;5;0") 0))
  (assert (= (ii/svg--interpolate 3 3 "25" "15" "0;5;0") 0))

  (assert (string= (ii/svg--interpolate 0 3 "25" "15" "M 256,213;M 212,220;M 212,220")
                   "M 256,213"))
  (assert (string= (ii/svg--interpolate 1 3 "25" "15" "M 256,213;M 212,220;M 212,220")
                   "M 212,220"))
  (assert (string= (ii/svg--interpolate 2 3 "25" "15" "M 256,213;M 212,220;M 212,220")
                   "M 212,220"))
  (assert (string= (ii/svg--interpolate 3 3 "25" "15" "M 256,213;M 212,220;M 212,220")
                   "M 256,213"))
  )

(defun ii/svg-parse-viewBox (image)
  (if (dom-attr image 'viewBox)
      (mapcar 'string-to-number (split-string (dom-attr image 'viewBox)))
    (list 0 0 (dom-attr image 'width) (dom-attr image 'height))))

(defun ii/svg-image-frame (image n)
  (let* ((nodes (dom-by-tag image 'animate))
         parent attr from to repeat freeze value dur values)
    (dolist (node nodes)
      (setq dur  (dom-attr node 'dur)) ;; 1s
      (when dur
        (setq parent (dom-parent image node)
              attr (intern-soft (dom-attr node 'attributeName))
              dur (string-to-number dur)
              repeat (dom-attr node 'repeatCount)
              freeze (string= "freeze" (dom-attr node 'fill))
              values (dom-attr node 'values)
              from (dom-attr node 'from)
              to   (dom-attr node 'to))
        ;; (message "%s %s %s %s %s" n dur from to values repeat)
        (setq value (ii/svg--interpolate n (* dur ii/svg-frame-rate) from to values freeze))
        ;; (message "%s %s %s %s" n value from to)
        (dom-set-attribute parent attr value)
        image))))

(defun ii/svg-image-map (image)
  (let* ((j 0)
         (x 0)
         (y 0)
         (factor-x 1)
         (factor-y 1)
         (view (dom-attr image 'viewBox))
         title bbox map origin w h w1 h1 a1 p transform old)
    (when view
      (setq origin (ii/svg-parse-viewBox image)
            w (dom-attr image 'width)
            h (dom-attr image 'height)
            x (car origin)
            y (cadr origin)
            w1 (nth 2 origin)
            h1 (nth 3 origin)
            a1 (/ w1 h1 1.0)
            factor-x (if (< w h) (/ w w1 1.0) (* 1 a1))
            factor-y (if (> w h) (/ h h1 1.0) (/ 1 a1))
            ;; y axis is inverted
            x (+ (if (> w h) (* -.5 (- w w1)) 0) (* x factor-x))
            y (+ (if (< w h) (* -.5 (- h h1)) 0) (* y factor-y))
            ))
    (dolist (i (dom-by-tag image 'a))
      ;; Accumulate parent transformation
      (setq p i transform nil)
      (while (setq p (dom-parent image p))
        (if (dom-attr p 'transform)
            (setq transform (concat transform " " (dom-attr p 'transform)))))
      (dolist (j (dom-children i))
        (if (memq (dom-tag j) '(g circle rect ellipse polygon))
            (setq p j)))
      (when p
        (setq old (dom-attr p 'transform))
        ;; Apply accumulated transformation
        (dom-set-attribute p 'transform transform)
        (setq bbox (ii/svg-bbox p)
              j (1+ j)
              title (or (dom-attr i 'text)
                        (dom-attr i 'xlink:title)))
        (if title (setq title (xml-substitute-special title)))
        ;; Restore old transformation
        (dom-set-attribute p 'transform old))

      (when bbox
        (push `((rect . ,(read (format "((%d . %d) . (%d . %d))"
                                       (- (* (nth 0 bbox) factor-x) x)
                                       (- (* (nth 1 bbox) factor-y) y)
                                       (- (* (nth 2 bbox) factor-x) x)
                                       (- (* (nth 3 bbox) factor-y) y))))
                ,(format "%s" (dom-attr i 'id))
                (pointer hand help-echo ,(if title (format "%s" title))))
              map)))
    map))

(defvar-local ii/svg-click-function nil)
(defun ii/svg-on-click (event)
  (interactive "e")
  (let* ((e (event-start event))
         (win (posn-window e))
         (image (posn-image e))
         (id (nth 1 e))
         o href title)
    ;; (message "1 %s" e)
    (when (and image (not (numberp id)))
      (with-selected-window win
        (setq image (ii/svg--animation-init (image-property image :data))
              o (dom-by-id image id)
              href  (dom-attr o 'xlink:href)
              title (dom-attr o 'xlink:title))
        (if ii/svg-click-function (funcall ii/svg-click-function href))))
    ))

(defun ii/svg-on-click-no-map (event)
  (interactive "e")
  (let* ((e (event-start event))
         (win (posn-window e))
         (image (posn-image e))
         xy hit o href title origin x y)
    ;; (message "%s" e)
    (when image
      ;; (select-window win)
      (with-selected-window win
        (setq xy (posn-object-x-y e)
              image (ii/svg--animation-init (image-property image :data))))
      (setq origin (ii/svg-parse-viewBox image)
            x (car origin)
            y (cadr origin))
      ;; (pp xy)

      ;; Check if xy collides with any anchor object in the image
      (dolist (i (dom-by-tag image 'a))
        (setq o (car (dom-children i))
              hit (ii/svg--point-in-region (+ (car xy) x) (+ (cdr xy) y)
                                           (ii/svg-bbox o)))
        ;; (pp (ii/svg-bbox o))
        ;; (pp hit)
        (and (nth 2 o)
             (dom-set-attribute o 'r
                                (string-to-number (dom-attr (nth 2 o) 'from)))
             (setf (nth 2 o) nil))
        (when hit
          (setq href  (dom-attr i 'xlink:href)
                title (dom-attr i 'xlink:title))
          (pp href)
          ;; (pp o)
          (setq o (ii/svg-animate o "r" "1" :from "15" :to "25" :fill "freeze"))
          ;; (pp o)
          (if title (tooltip-show title))
          (with-selected-window win
            (if ii/svg-click-function (funcall ii/svg-click-function href)))
          ;; (ffap href)
          ))
      )))

(defstruct point x y r
           old-x old-y fill title href text image
           vel-x vel-y mass
           width height
           )
(defvar simulate--force-x 0)
(defvar simulate--force-y 0)
(defvar simulate--friction 0)
(defvar simulate-screen-width 100)
(defvar simulate-screen-height 100)
(defun simulate-point-update (p dt)
  (when (or (zerop simulate--friction)
            (> (abs simulate--force-x) simulate--friction)
            (> (abs simulate--force-y) simulate--friction))
    (let* ((r (point-r p))
           (acc-x (/ simulate--force-x (point-mass p) 1.0))
           (acc-y (/ simulate--force-y (point-mass p) 1.0))
           (vel-x (+ (point-vel-x p) (* acc-x dt)))
           (vel-y (+ (point-vel-y p) (* acc-y dt))))
      (setf (point-vel-x p) vel-x)
      (setf (point-vel-y p) vel-y)
      (setf (point-x p) (+ (point-x p) (* vel-x dt)))
      (setf (point-y p) (+ (point-y p) (* vel-y dt)))

      ;; Constraints
      ;; (message "1 %s" p)
      (cond ((< (point-x p) r)
             (setf (point-x p) r)
             (setf (point-vel-x p) (- vel-x)))
            ((< (point-y p) r)
             (setf (point-y p) r)
             (setf (point-vel-y p) (- vel-y)))
            ((> (point-x p) (- simulate-screen-width r))
             (setf (point-x p) (- simulate-screen-width r))
             (setf (point-vel-x p) (- vel-x)))
            ((> (point-y p) (- simulate-screen-height r))
             (setf (point-y p) (- simulate-screen-height r))
             (setf (point-vel-y p) (- vel-y))))
      ;; (message "2 %s" p)
      (setq simulate--force-x (if (> simulate--force-x 0)
                                  (- simulate--force-x simulate--friction)
                                (+ simulate--force-x simulate--friction)))
      (setq simulate--force-y (if (> simulate--force-y 0)
                                  (- simulate--force-y simulate--friction)
                                (+ simulate--force-y simulate--friction)))
      ;; (message "%s %s %s" simulate--force-x simulate--force-y simulate--friction)
      )))

(defun ii/svg-simulation-frame (image dt &optional force-x force-y friction)
  (when (zerop dt)
    ;; Initial conditions
    (setq simulate--force-x  (if force-x force-x 10)
          simulate--force-y  (if force-y force-y 10)
          simulate-screen-width  (dom-attr image 'width)
          simulate-screen-height (dom-attr image 'height)
          simulate--friction (if friction friction (/ (+ force-x force-y) 20))))

  (if (not (or (zerop simulate--friction)
               (> (abs simulate--force-x) simulate--friction)
               (> (abs simulate--force-y) simulate--friction)))
      (ii/svg-animation-cancel)
    (dolist (node (dom-children image))
      (when (memq (dom-tag node) '(circle a))
        (if (eq (dom-tag node) 'a)
            (setq node (car (dom-children node))))
        (let* ((p (make-point :x (dom-attr node 'cx)
                              :y (dom-attr node 'cy)
                              :r (dom-attr node 'r)
                              :vel-x (or (dom-attr node 'vel-x) 0)
                              :vel-y (or (dom-attr node 'vel-y) 0)
                              :mass (or (dom-attr node 'mass) 1)
                              )))
          (simulate-point-update p dt)
          (dom-set-attribute node 'cx (point-x p))
          (dom-set-attribute node 'cy (point-y p))
          (dom-set-attribute node 'vel-x (point-vel-x p))
          (dom-set-attribute node 'vel-y (point-vel-y p))
          )))))

(defvar ii/svg--bubble-nodes nil)
(defun ii/svg-bubble-frame (image &optional init)
  (if init
      (progn
        (setq ii/svg--bubble-nodes nil
              simulate-screen-width  (dom-attr image 'width)
              simulate-screen-height (dom-attr image 'height)
              )
        (dolist (node (dom-children image))
          (when (memq (dom-tag node) '(circle a))
            (if (eq (dom-tag node) 'a)
                (setq node (car (dom-children node))))
            (let* ((p (make-point :x (dom-attr node 'cx)
                                  :y (dom-attr node 'cy)
                                  :r (dom-attr node 'r)
                                  :vel-x (or (dom-attr node 'vel-x) 0)
                                  :vel-y (or (dom-attr node 'vel-y) 0)
                                  :mass (or (dom-attr node 'mass) 1)
                                  )))
              (push (cons p node) ii/svg--bubble-nodes)
              (dom-set-attribute node 'cx (point-x p))
              (dom-set-attribute node 'cy (point-y p))
              (dom-set-attribute node 'vel-x (point-vel-x p))
              (dom-set-attribute node 'vel-y (point-vel-y p))
              ))))

    (let* ((nodes ii/svg--bubble-nodes)
           node p1 p2 x1 y1 x2 y2 r1 r2 d r moved inc ang deg vx vy)
      ;; Move first point in the list
      (setq p1 (car (pop nodes)))
      (while (setq node (pop nodes))
        (setq p2 (car node)
              x1 (point-x p1)
              y1 (point-y p1)
              r1 (point-r p1)
              x2 (point-x p2)
              y2 (point-y p2)
              r2 (point-r p2)
              ang (if (= x2 x1)
                      (* 2 float-pi (cl-random 1.0))
                    (atan (/ (- y2 y1) (- x2 x1))))
              deg (* (/ 360 (* 2 float-pi)) ang)
              vx (cos ang)
              vy (sin ang)
              d (sqrt (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1))))
              r (+ r1 r2))
        (message "b1 %s %s" p1 p2)
        (message "%.1f %s %.1f %.1f %.3f %.3f" d r deg ang vx vy)
        (when (> (abs (- d r)) 1) ;; Account for Floating point error
          (setq moved t
                inc (if (< d r) r (- d r)))
          (setf (point-x p1) (+ x1 (* inc vx)))
          (setf (point-y p1) (+ y1 (* inc vy)))
          (message "b2 %s %s" p1 p2))

        (if (not moved) (ii/svg-animation-cancel))
        ;; Constraints
        (cond ((< (point-x p1) r1)
               (setf (point-x p1) (- r1 (- (point-x p1) r1))))
              ((< (point-y p1) r1)
               (setf (point-y p1) (- r1 (- (point-y p1) r1))))
              ((> (point-x p1) simulate-screen-width)
               (setf (point-x p1) (- (- simulate-screen-width r1)
                                     (- (point-x p1) (- simulate-screen-width r1)))))
              ((> (point-y p1) simulate-screen-height)
               (setf (point-y p1) (- (- simulate-screen-height r1)
                                     (- (point-y p1) (- simulate-screen-height r1))))))
        (message "b3 %s" p1)

        ;; Update DOM node
        (setq node (pop ii/svg--bubble-nodes)
              ii/svg--bubble-nodes (append ii/svg--bubble-nodes (list node)))
        (dom-set-attribute (cdr node) 'cx (point-x (car node)))
        (dom-set-attribute (cdr node) 'cy (point-y (car node)))
        ))
    ))

(defun ii/svg--animation-init (&optional xml)
  (let* ((image (ii/svg-load-from-xml (or xml (buffer-string)))))

    ;; (image-get-display-property)
    ;; (pp (ii/svg-animation-metadata image))
    ;; (pp (ii/svg-image-frame image 2))
    ;; (image-toggle-display)

    ;; (local-set-key [mouse-1] #'ii/svg-on-click)

    (setq image-animate-loop nil)
    (goto-char (1- (point-max)))
    (dom-set-attribute image :image (point-marker))
    image))

(defvar ii/svg--animation-timer nil)
;;;###autoload
(defun ii/svg-animation-cancel ()
  (interactive)
  (if ii/svg--animation-timer (cancel-timer ii/svg--animation-timer)))

;;;###autoload
(defun ii/svg-animation-run (&optional image)
  (interactive)
  (let* ((image (or image (ii/svg--animation-init)))
         (delay (/ 1 ii/svg-frame-rate 1.0))
         (i 0)
         (buf (current-buffer))
         ;; (m (image-get-display-property))
         metadata count loop)
    (setq metadata (ii/svg-animation-metadata image)
          count (1+ (* (car metadata) ii/svg-frame-rate))
          loop (cdr metadata))
    (ii/svg-animation-cancel)
    (setq ii/svg--animation-timer
          (run-with-timer 0 delay
                          (lambda ()
                            (if (with-current-buffer buf (image-get-display-property))
                                (ii/svg-possibly-update-image image)
                              (ii/svg-animation-cancel))
                            (ii/svg-image-frame image i)

                            (if (or loop (< i count))
                                (setq i (1+ i))
                              (ii/svg-animation-cancel))
                            )))
    ;; (dotimes (i 5)
    ;; (while (or (< i count)
    ;;            (if image-animate-loop (setq i 0)))
    ;;   (ii/svg-image-frame image i)
    ;;   (ii/svg-possibly-update-image image)
    ;;   (setq i (1+ i))
    ;;   (sit-for delay))
    ))

(defun ii/svg-animate1 ()
  "Circle packing"
  (interactive)
  (let* ((image nil); (ii/svg--animation-init))
         (n 7)
         (graph-draw-padding 10)
         (padding graph-draw-padding)
         (text "The quick brown fox jumps over the lazy dog")
         (points nil)
         (w (window-pixel-width))
         (h (window-pixel-height))
         (inhibit-read-only t)
         x y r word words e bbox j p)
    (setq image (ii/svg-create w h))
    (erase-buffer)
    (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 2) w h))
    (ii/svg-insert-image image)

    (setq words (split-string text))
    ;; (setq n (length words))
    ;; (dotimes (i n)
    ;;   (setq j i
    ;;         j (random (length words))
    ;;         word (nth i words)
    ;;         e (ii/svg-text nil word
    ;;                     :id 1
    ;;     	        :font-size (+ j 15))
    ;;         bbox (ii/svg-bbox e)
    ;;         ;; r (- (nth 2 bbox) (nth 0 bbox))
    ;;         ;; r (string-pixel-width word)
    ;;         r 50
    ;;         x (- w r)
    ;;         y (- h r))
    ;;   (or word (error "error"))
    ;;   (push (make-point :x x :y y :r r :fill (random-color-html)
    ;;                     :height (* 50)
    ;;                     :width  (+ 50 j)
    ;;                     :title word)
    ;;         points))

    (dotimes (i n)
      (setq j 0
            ;; j (random n)
            word (number-to-string i)
            r (+ 15 j padding)
            x (- w r)
            y (- h r))
      (push (make-point :x x :y y :r r :fill (random-color-html)
                        :href word :title word)
            points))

    (setq p (graph-enclose (graph-pack points image))
          x (point-x p)
          y (point-y p)
          r (point-r p))

    (ii/svg-circle image x y r
                   :stroke-width 2
                   :stroke "red"
                   :fill "none")

    (ii/svg-possibly-update-image image)
    ))

(defun ii/svg-animate2 ()
  "Bubble graph"
  (interactive)
  (let* ((image nil)
         (n 4)
         (graph-draw-padding 0)
         (padding graph-draw-padding)
         (text1 "Google Reddit Amazon Mail Finance Blog Web" )
         (text2 "Notes Reminder Local" )
         (text3 "Database")
         (points nil)
         (w (window-pixel-width))
         (h (window-pixel-height))
         (inhibit-read-only t)
         x y r word words j root)
    (setq image (ii/svg-create w h))
    (erase-buffer)
    (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 3) w h))
    (ii/svg-insert-image image)

    (dolist (k (list text1 text2 text3))
      (setq words (split-string k)
            points nil
            n (length words))
      (dotimes (i n)
        (setq j 0
              ;; j (random n)
              ;; word (number-to-string i)
              word (nth i words)
              r (+ 35 j padding)
              x (- w r)
              y (- h r))
        (push (make-point :x x :y y :r r :fill (random-color-html)
                          :old-x 0 :old-y 0
                          :href word :title word)
              points))
      (push points root))
    (setq graph-draw-group nil)

    ;; Small incircle
    ;; (setq p (car points))
    ;; (setf (point-r p) (* .2 (point-r p)))

    (graph-draw-tree root image)
    (ii/svg-animation-run image)
    (ii/svg-possibly-update-image image)
    ))

(defun ii/svg-animate3 (prefix)
  "Stocks heatmap"
  (interactive "P")
  (let* ((image nil)
         (mul 10)
         (graph-draw-padding 0)
         (points nil);svg--bubble-nodes)
         (w (window-pixel-width))
         (h (window-pixel-height))
         (inhibit-read-only t)
         x y r word words i j root group children child point)
    (setq image (ii/svg-create w h))
    (erase-buffer)
    (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 2) w h))
    (ii/svg-insert-image image)

    (setq group 'sector)
    (unless points
      (with-current-buffer "nifty.txt"
        (goto-char (point-min))
        (setq j (point))
        (while (re-search-forward "\n" (point-max) t)
          ;; (pp word)
          (setq words (split-string (buffer-substring-no-properties j (1- (point))) "\t"))
          (when (> (length words) 1)
            (setq word  (nth 0 words)
                  r     (* mul (string-to-number (nth 2 words)))
                  x 0 y 0
                  i (random-num 5 -5)
                  j (point))
            (push (make-point :x x :y y :r r
                              ;; :fill (random-color-html)
                              :fill (format "#%06x"
                                            (if (> i 0)
                                                (ii/svg--interpolate i 6 nil nil
                                                                     "#ffffff;#00ff00")
                                              (ii/svg--interpolate (abs i) 6 nil nil
                                                                   "#ffffff;#ff0000")))
                              :old-x 0 :old-y 0
                              :text (format "%s\n%.2f%%" (nth 1 words) i)
                              :href word :title (format "%s\n%.2f%%" word i))
                  points)
            (when (eq group 'sector)
              (if (setq child (assoc (nth 3 words) children))
                  (setcdr child (cons (car points) (cdr child)))
                (push (list (nth 3 words) (car points)) children))
              ;; (message "%s" children)
              )
            ))
        ;; (setq points (sort points (lambda (a b) (> (point-r a) (point-r b)))))
        (pcase prefix
          (1
           (setq root (mapcar 'cdr children)
                 group (mapcar 'car children)))
          ;; (setq root (list 1 2 3))
          (_ (setq root points)))
        ))
    (setq graph-draw-group t
          graph-draw-group-fn (lambda (d i)
                                (format "%s" (nth i group))))
    (special-mode)
    (use-local-map (let* ((map (make-sparse-keymap)))
                     (define-key map "i" 'ii/svg-animate-index)
                     (define-key map "s" 'ii/svg-animate-sector)
                     map))

    (setq point (graph-draw-tree root image)
          r (point-r point)
          w (* 2 r))
    ;; (setq bbox (ii/svg-bbox image)
    ;;       mul 2
    ;;       w (- (nth 2 bbox) (nth 0 bbox))
    ;;       h (- (nth 3 bbox) (nth 1 bbox)))
    ;; (pp point)
    (dom-set-attribute image 'viewBox
                       (format "%d %d %d %d"
                               (- (point-x point) r)
                               (- (point-y point) r)
                               w w))

    (ii/svg-possibly-update-image image)
    (ii/svg-animation-run image)
    ))

(defun ii/svg-animate-index ()
  (interactive)
  (setq graph-draw-group nil)
  (ii/svg-animate3 0))

(defun ii/svg-animate-sector ()
  (interactive)
  (setq graph-draw-group t)
  (ii/svg-animate3 1))

(defun ii/svg-animate4 (prefix)
  "3 body problem"
  (interactive "P")
  (let* ((image nil)
         (graph-draw-padding 0)
         (points nil);svg--bubble-nodes)
         (w (window-pixel-width))
         (h (window-pixel-height))
         (inhibit-read-only t)
         r x y word words j root group children child point attributes)
    (setq image (ii/svg-create w h))
    (erase-buffer)
    (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 2) w h))
    (ii/svg-insert-image image)

    (setq group 'sector)
    (unless points
      (with-current-buffer "3body.txt"
        (goto-char (point-min))
        (setq j (point))
        (while (re-search-forward "\n" (point-max) t)
          ;; (pp word)
          (setq words (split-string (buffer-substring-no-properties j (1- (point))) ","))
          (unless attributes
            (setq attributes words
                  words nil))
          (setq j (point))
          (when (> (length words) 1)
            (setq word  (nth 0 words)
                  r     30
                  x 0 y 0)
            (push (make-point :x x :y y :r r
                              :fill (random-color-html)
                              :old-x 0 :old-y 0
                              :text word
                              :image (nth 5 words)
                              :href word :title word)
                  points)
            (when (eq group 'sector)
              (if (setq child (assoc (nth 3 words) children))
                  (setcdr child (cons (car points) (cdr child)))
                (push (list (nth 3 words) (car points)) children))
              ;; (message "%s" children)
              )
            ;; (message "%s" points)
            ))
        ;; (setq points (sort points (lambda (a b) (> (point-r a) (point-r b)))))
        (setq points (nreverse points))
        (pcase prefix
          (1
           (setq root (mapcar 'cdr children)
                 group (mapcar 'car children)))
          (_ (setq root points)))
        ))
    (setq graph-draw-group t
          graph-draw-group-fn (lambda (d i)
                                (if (and (> d 0) (> (length (nth i root)) 1))
                                    (format "%s" (nth i group)))
                                ;; (format "%s, %s" d i)
                                ;; (message "%s, %s, %s %s" d i (nth i group) (nth i root))
                                ))
    (special-mode)
    (use-local-map (let* ((map (make-sparse-keymap)))
                     (define-key map "i" 'ii/svg-animate4-1)
                     map))

    (setq point (graph-draw-tree root image)
          r (point-r point)
          w (* 2 r))
    (dom-set-attribute image 'viewBox
                       (format "%d %d %d %d"
                               (- (point-x point) r)
                               (- (point-y point) r)
                               w w))

    (ii/svg-possibly-update-image image)
    ;; (ii/svg-animation-run image)
    ))

(defun ii/svg-animate4-1 ()
  (interactive)
  (ii/svg-animate4 1))

(provide 'implicit-svg)

;;; svg.el ends here
