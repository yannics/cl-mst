;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :ASDF)

(defsystem :cl-mst
  :name "cl-mst"
  :author "Yann Ics"
  :maintainer "<by.cmsc@gmail.com>"
  :licence "Public Domain"
  :description "Minimal Spanning Tree"
  :version "1.0"
  :serial t
  :components
  (
   (:FILE "package")
   (:FILE "src/kruskal")
   (:FILE "src/prim")
   (:FILE "cl-mst")
   (:FILE "neato")
  )
 )
