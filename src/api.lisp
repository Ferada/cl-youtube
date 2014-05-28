;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.lisp
;;;; Purpose:       cl-youtube Common Lisp API
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



(defclass youtube-client ()
  ((id :initform nil
       :initarg :id
       :accessor youtube-id
       :documentation "YouTube developer ID."))
  (:documentation "A YouTube developper profile."))


(defun make-youtube-client (id)
  "Create a new YouTube client."
  (make-instance 'youtube-client :id id))


(defmacro with-youtube-response ((response url) &body body)
  "Make an HTTP request to URL with DRAKMA and if CODE is equal to 200,
set RESPONSE to a string which contains the XML response, and executes BODY."
  `(let (,response)
     (format t "URL : ~A~%" ,url)
     (multiple-value-bind (stream code headers uri flexy-stream must-close)
         (drakma:http-request ,url :want-stream t)
       (declare (ignore headers uri flexy-stream must-close))
       (when (= code 200)
         (with-output-to-string (os)
           (loop as line = (read-line stream nil)
              until (null line)
              do (format os "~A" line))
           (setf ,response (get-output-stream-string os))
           (close stream))))
     ,@body))


;; ------------------
;; User informations
;; ------------------

(defgeneric get-xml-user-profile (youtube-client name)
  (:documentation "XML response for retrieve the public parts of a user profile."))


(defmethod get-xml-user-profile ((youtube-client youtube-client) name)
  (with-slots (id) youtube-client
    (with-youtube-response (response (format nil *rest-user-profile* id name))
      (s-xml:parse-xml-string response :output-type :sxml))))


(defgeneric get-xml-user-favorite-videos (youtube-client name)
  (:documentation "XML response which list a user's favorite videos."))


(defmethod get-xml-user-favorite-videos ((youtube-client youtube-client) name)
  (with-slots (id) youtube-client
    (with-youtube-response (response (format nil *rest-user-favorite-videos* id name))
      (s-xml:parse-xml-string response :output-type :sxml))))
  

(defgeneric get-xml-user-friends (youtube-client name)
  (:documentation "XML response which list a user's friends."))


(defmethod get-xml-user-friends ((youtube-client youtube-client) name)
  (with-slots (id) youtube-client
    (with-youtube-response (response (format nil *rest-user-friends* id name))
      (s-xml:parse-xml-string response :output-type :sxml))))


;; ---------------
;; Videos Viewing
;; ---------------


(defgeneric get-xml-video-details (youtube-client video-id)
  (:documentation "XML response which displays the details for a video."))


(defmethod get-xml-video-details ((youtube-client youtube-client) video-id)
  (with-slots (id) youtube-client
    (with-youtube-response (response (format nil *rest-video-details* id video-id))
      (s-xml:parse-xml-string response :output-type :sxml))))


;; Pb avec comment_list et channel_list

(defmethod get-xml-video-details ((youtube-client youtube-client) video-id)
  (with-slots (id) youtube-client
    (with-youtube-response (response (format nil *rest-video-details* id video-id))
      (s-xml:parse-xml-string response :output-type :sxml))))


(defgeneric get-xml-tag-videos (youtube-client tag &key page per-page)
  (:documentation "XML response which lists all videos that have the specified
tag."))


(defmethod get-xml-tag-videos ((youtube-client youtube-client) tag &key page per-page)
  (declare (ignore page per-page))
  (with-slots (id) youtube-client
    (with-youtube-response (response (format nil *rest-tag-videos* id tag))
      (s-xml:parse-xml-string response :output-type :sxml))))


(defgeneric get-xml-user-videos (youtube-client name)
  (:documentation "XML response which lists all videos that were uploaded by the
specified user."))


(defmethod get-xml-user-videos ((youtube-client youtube-client) name)
  (with-slots (id) youtube-client
    (with-youtube-response (response (format nil *rest-user-videos* id name))
      (s-xml:parse-xml-string response :output-type :sxml))))


(defgeneric get-xml-videos-featured (youtube-client)
  (:documentation "XML response which lists the most recent 25 videos that have
been featured on the front page of the YouTube site."))


(defmethod get-xml-videos-featured ((youtube-client youtube-client))
  (with-slots (id) youtube-client
    (with-youtube-response (response (format nil *rest-videos-featured* id))
      (s-xml:parse-xml-string response :output-type :sxml))))


;; ---------
;; User API
;; ---------


(defgeneric get-user-profile (youtube-client name)
  (:documentation "Retrieves the public parts of a user profile."))


(defmethod get-user-profile ((youtube-client youtube-client) name)
  (let ((xml (get-xml-user-profile youtube-client name)))
    (when xml
      (make-youtube 'youtube-profile (third xml)))))


(defgeneric get-user-favorite-videos (youtube-client name)
  (:documentation "Lists a user's favorite videos."))


(defmethod get-user-favorite-videos ((youtube-client youtube-client) name)
  (let ((xml (get-xml-user-favorite-videos youtube-client name)))
    (when (and xml (listp (third xml)))      
      (loop for data in (cddr (third xml))
         collect (make-youtube 'youtube-video data)))))


(defgeneric get-user-friends (youtube-client name)
  (:documentation "List a user's friends."))


(defmethod get-user-friends ((youtube-client youtube-client) name)
  (let ((xml (get-xml-user-friends youtube-client name)))
    (when (and xml (listp (third xml)))
      (loop for data in (cdr (third xml))
         collect (make-youtube 'youtube-friend data)))))


(defgeneric get-video-details (youtube-client video-id)
  (:documentation "Displays the details for a video."))


(defmethod get-video-details ((youtube-client youtube-client) video-id)
  (let ((xml (get-xml-video-details youtube-client video-id)))
    (when xml
      (make-youtube 'youtube-video-details (caddr xml)))))


(defgeneric get-tag-videos (youtube-client tag &key page per-page)
  (:documentation "A list of all videos that have the specified
tag."))


(defmethod get-tag-videos ((youtube-client youtube-client) tag &key page per-page)
  (let ((xml (get-xml-tag-videos youtube-client tag :page page :per-page per-page)))
    (when (and xml (listp (third xml)))
      (loop for data in (cddr (third xml))
         collect (make-youtube 'youtube-video data)))))
    

(defgeneric get-user-videos (youtube-client name)
  (:documentation "A list of all videos that were uploaded by the
specified user."))


(defmethod get-user-videos ((youtube-client youtube-client) name)
  (let ((xml (get-xml-user-videos youtube-client name)))
    (when (and xml (listp (third xml)))
      (loop for data in (cdr (third xml))
         collect (make-youtube 'youtube-video data)))))


(defgeneric get-videos-featured (youtube-client)
  (:documentation "A list of the most recent 25 videos that have been featured
on the front page of the YouTube site."))


(defmethod get-videos-featured ((youtube-client youtube-client))
  (let ((xml (get-xml-videos-featured youtube-client)))
    (when (and xml (listp (third xml)))
      (loop for data in (cddr (third xml))
         collect (make-youtube 'youtube-video data)))))
