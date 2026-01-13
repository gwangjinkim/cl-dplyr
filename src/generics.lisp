(in-package #:cl-dplyr)

;;; Core Generics (Internal Functional Layer)

(defgeneric slice (data &rest args)
  (:documentation "Subset rows using their positions."))

(defgeneric %filter (data predicate)
  (:documentation "Subset rows using a predicate."))

(defgeneric %arrange (data &rest order-specs)
  (:documentation "Order rows by variables."))

(defgeneric %distinct (data &rest variables)
  (:documentation "Select distinct/unique rows."))

(defgeneric %select (data &rest selection)
  (:documentation "Select, rename, and reorder columns."))

(defgeneric %rename (data &rest renaming)
  (:documentation "Rename specific columns."))

(defgeneric %mutate (data &rest mutations)
  (:documentation "Add or modify columns."))

(defgeneric pull (data var)
  (:documentation "Extract a single column as a vector."))

(defgeneric group-by (data &rest variables)
  (:documentation "Group data by one or more variables."))

(defgeneric ungroup (data)
  (:documentation "Remove grouping metadata."))

(defgeneric %summarise (data &rest summaries)
  (:documentation "Reduce groups to single values."))

;;; Joins

(defgeneric inner-join (x y &key by))
(defgeneric left-join (x y &key by))
(defgeneric right-join (x y &key by))
(defgeneric full-join (x y &key by))
(defgeneric semi-join (x y &key by))
(defgeneric anti-join (x y &key by))
