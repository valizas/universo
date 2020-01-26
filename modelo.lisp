(defpackage #:universo
  (:use :common-lisp))
(in-package #:universo)

(defvar *numeracion* 0)
(defun new-name (prefijo)
  (incf *numeracion*)
  (if prefijo
      (format nil "~A-~A" prefijo *numeracion*)
      (format nil "objeto-~A" *numeracion*)))

(defclass constelacion () ((nombre :initform (new-name 'constelacion))
			   (sistemas :initform nil))) 
(defclass sistema () ((nombre :initform (new-name 'sistema))
		      (entidades :initform nil)
		      (constelacion :initform nil)))
(defclass entidad () ((nombre :initform (new-name 'entidad))
		      (componentes :initform nil)
		      (sistema :initform nil)))
(defclass componente () ((nombre :initform (new-name 'componente))
			 (tipo)
			 (atributos :initform nil)
			 (entidad :initform nil)))

(defparameter *default-constelacion* nil)
(defparameter *default-sistema* nil)
(defparameter *default-entidad* nil)
(defparameter *default-componente* nil)

(defmethod initialize-instance :after ((co constelacion ) &key)
  (setf *default-constelacion* co))

(defmethod initialize-instance :after ((s sistema ) &key (constelacion *default-constelacion*))
  (setf *default-sistema* s)
  (unless constelacion
    (setf constelacion (make-instance 'constelacion)))
  (setf *default-constelacion* constelacion)
  (setf (slot-value constelacion 'sistemas) (append (slot-value constelacion 'sistemas) (list s))))

(defmethod initialize-instance :after ((e entidad ) &key (sistema *default-sistema*))
  (setf *default-entidad* e)
  (unless sistema
    (setf sistema (make-instance 'sistema)))
  (setf *default-sistema* sistema)
  (setf (slot-value sistema 'entidades) (append (slot-value sistema 'entidades) (list e))))

(defmethod initialize-instance :after ((c componente ) &key (entidad *default-entidad*))
  (setf *default-componente* c)
  (unless entidad
    (setf entidad (make-instance 'entidad)))
  (setf *default-entidad* entidad)
  (setf (slot-value entidad 'componentes) (append (slot-value entidad 'componentes) (list c))))

(defun pretty-print-constelacion (co)
  (format t "Constelacion ~A~%" (slot-value co 'nombre))
  (dolist (s (slot-value co 'sistemas))
    (pretty-print-sistema s)))
				       
(defun pretty-print-sistema (s)
    (format t "  Sistema ~A~%" (slot-value s 'nombre))
    (dolist (e (slot-value s 'entidades))
      (pretty-print-entidad e)))

(defun pretty-print-entidad (e)
    (format t "    Entidad ~A~%" (slot-value e 'nombre))
    (dolist (c (slot-value e 'componentes))
      (pretty-print-componente c)))

(defun pretty-print-componente (c)
    (format t "      Componente ~A~%" (slot-value c 'nombre))
    (format t "             Tipo   ~A~%" (slot-value c 'nombre))
    (dolist (a (slot-value c 'atributos))
      (format t "             Atributo   ~A" a)))
