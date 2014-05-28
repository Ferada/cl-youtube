;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools.lisp
;;;; Purpose:       cl-youtube Tools
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-youtube, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cl-youtube users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-youtube)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun as-keyword (sym)
    "Translate a symbol to the corresponding keyword symbol"
    (intern (string sym) :keyword))


  (defun keyword-name (keyword)
    "Translate a keyword."
    (format nil "~A" keyword))


  (defun symbol-to-slot (slot)
    (let ((initarg (substitute #\- #\_ (string-downcase (symbol-name slot)))))
      `(,slot
        :initarg ,(as-keyword (string-upcase initarg))
        :accessor ,slot))))


;; ------
;; Tools
;; ------


(defmacro define-youtube-class (name slots)
  "Macro which creates a new object."
  `(defclass ,name ()
     ,(mapcar 'symbol-to-slot slots)))


(define-youtube-class youtube-profile
    (first-name last-name about-me age video-upload-count video-watch-count
                homepage hometown gender occupations companies city country
                books hobbies movies relationship friend-count
                favorite-video-count currently-on))


(defmethod print-object ((youtube-profile youtube-profile) stream)
  (if *print-youtube*
      (with-slots (first-name last-name about-me age video-upload-count
                              video-watch-count homepage hometown gender
                              occupations companies city country books hobbies
                              movies relationship friend-count
                              favorite-video-count currently-on) youtube-profile
        (format stream "YouTube User profile~%")
        (format stream "~A ~A ~A~%" gender first-name last-name)
        (format stream "~A - ~A~%" about-me age)
        (format stream "Informations :~%~A ~A ~A ~A ~A ~A ~A~%"
                homepage occupations companies city country books hobbies)
        (format stream "Videos ~A ~A~%" video-upload-count video-watch-count))
      (print-unreadable-object (youtube-profile stream :type t :identity t))))


(define-youtube-class youtube-video
    (author id title length-seconds rating-avg rating-count description
            view-count upload-time comment-count tags url thumbnail-url))


(defmethod print-object ((youtube-video youtube-video) stream)
  (if *print-youtube*
      (with-slots (author id title length-seconds rating-avg rating-count
                          description view-count upload-time comment-count tags
                          url thumbnail-url) youtube-video
        (format stream "YouTube Video ~A~%" id)
        (format stream "~A [~A] - ~As : ~A~%" title author length-seconds tags)
        (format stream "URL=~A - ~A~%" url thumbnail-url)
        (format stream "~A ~A~%" rating-avg rating-count))
      (print-unreadable-object (youtube-video stream :type t :identity t))))


(define-youtube-class youtube-friend
    (user video-upload-count favorite-count friend-count))


(define-youtube-class youtube-video-details
    (author title rating-avg rating-count tags description update-time view-count
            upload-time length-seconds recording-date recording-location
            recording-country thumbnail-url
            ))
;;             <comment_list>
;;             <comment>
;;             <author>steve</author>
;;             <text>asdfasdf</text>
;;             <time>1129773022</time>
;;             </comment>
;;             </comment_list>
;;             <channel_list>
;;             <channel>Humor</channel>
;;             <channel>Odd & Outrageous</channel>
;;             </channel_list>
            


(defun xml-to-initargs (xml)
  "Creates a new video from XML."
  (let (args)
    (loop for couple in (cdr xml)
       do (destructuring-bind (key value)
              `(,(as-keyword (string-upcase
                              (substitute #\- #\_
                                          (symbol-name (car couple)))))
                 ,(cadr couple))
            (push value args)
            (push key args)))
    args))


 (defun make-youtube (class xml)
   "Creates a new YouTube CLASS object with INITARGS."
   ;;(format t "--->~A~%XML : ~A~%" class xml)
   (apply 'make-instance class (xml-to-initargs xml)))

