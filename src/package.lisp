;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for cl-youtube
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-youtube, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cl-youtube users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :cl-youtube
  (:use :cl)
  (:documentation "Youtube Common Lisp package")
  (:export #:make-youtube-client
           #:youtube-client
           #:youtube-profile
           #:youtube-video
           #:youtube-friend
           #:youtube-video-details
           
           #:get-user-profile
           #:get-user-favorite-videos
           #:get-user-friends

           #:get-video-details
           #:get-tag-videos
           #:get-user-videos
           #:get-videos-featured

           #:*print-youtube*
   ))



