(ns hazel.entity "Entity support interfaces and functions.")

(defmulti completions "Retrieves data elements containing 'input'. 'data-source-tags' are used to determine from which data sources the data shall be taken. 'filter' is a map from attributes to values, all of which must be fulfilled for a data element to be returned."
  (fn [db input data-source-tags filter] (set data-source-tags)))
(defmethod completions :default [db input data-source-tags filter] [])

(defn db-keys "The keys of an entity that are part of the entity db schema." [schema entity] (select-keys entity (conj (map :db/ident schema) :db/id)))