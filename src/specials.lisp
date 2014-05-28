;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       cl-youtube specials informations.
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


(defparameter *youtube-server* "http://www.youtube.com/api2_rest?")


(defparameter *rest-user-profile*
  (concatenate 'string
               *youtube-server*
               "method=youtube.users.get_profile&dev_id=~A&user=~A"))


(defparameter *rest-user-favorite-videos*
  (concatenate 'string
               *youtube-server*
               "method=youtube.users.list_favorite_videos&dev_id=~A&user=~A"))


(defparameter *rest-user-friends*
  (concatenate 'string
               *youtube-server*
               "method=youtube.users.list_friends&dev_id=~A&user=~A"))


(defparameter *rest-video-details*
  (concatenate 'string
               *youtube-server*
               "method=youtube.videos.get_details&dev_id=~A&video_id=~A"))


(defparameter *rest-tag-videos*
  (concatenate 'string
               *youtube-server*
               "method=youtube.videos.list_by_tag&dev_id=~A&tag=~A"))


(defparameter *rest-user-videos*
  (concatenate 'string
               *youtube-server*
               "method=youtube.videos.list_by_user&dev_id=~A&user=~A"))


(defparameter *rest-videos-featured*
  (concatenate 'string
               *youtube-server*
               "method=youtube.videos.list_featured&dev_id=~A"))


;; Errors code

(defparameter *youTube-internal-error* 1
  "This is a potential issue with the YouTube API.")


(defparameter *bad-xml-rpc-format-parameter* 2
  "The parameter passed to the XML-RPC API call was of an incorrect type.")


(defparameter *unknown-parameter-specified* 3
  "Please double-check that the specified parameters match those in the API
reference.")


(defparameter *missing-required-parameter* 4
  "Please double-check that all required parameters for the API method you're
calling are present in your request.")


(defparameter *no-method-specified* 5 "All API calls must specify a method
name.")


(defparameter *unknown-method-specified* 6
  "Please check that you've spelled the method name correctly.")


(defparameter *missing-dev_id-parameter* 7
  "All requests must have a developer ID. If you don't have one, please create a
developer profile.")


(defparameter *bad-or-unknown-dev_id-specified* 8
  "All requests must have a valid developer ID. If you don't have one, please
create a developer profile.")


(defparameter *print-youtube* nil
    "If T, write the customized presentation of the YouTube objects.")
