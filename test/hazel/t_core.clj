(ns hazel.t-core
  (:use midje.sweet)
  (:require [datomic.api :as d])
  (:require [hazel.core :as hazel])
  (:require [hazel.state :as db])
  (:use hazel.test))

;;Load the forms below to run all tests in the project.
#_(require 'midje.repl)
#_(midje.repl/load-facts)

(with-state-changes [(before :facts (init-db!))]
 (fact "find finds something" (hazel/find (db/db "test-db") :test/name) => #(> (count %) 0)))

(with-state-changes [(before :facts (init-db!))]
  (fact "make entid map" (hazel/entid-map (db/db "test-db") test-entity) => #(:db/id %)))

(with-state-changes [(before :facts (init-db!))]
  (fact "season map containing only :db/id"
    (let [db (db/db "test-db")
          test-entity-id-map (hazel/entid-map db test-entity)]
      (hazel/season (db/db "test-db") test-entity-id-map [:test/name])) => #(= (:test/name test-entity) (:test/name %))))

(fact "where-attrs-match works with scalars and sequences"
  (hazel/where-attrs-match {:non-seq 4 :seq [6 7]}) => (fn [result] (and (= 3 (count result))
                                                                         (some #(= ['?ent :non-seq 4] %) result)
                                                                         (some #(= ['?ent :seq 6] %) result)
                                                                         (some #(= ['?ent :seq 7] %) result))))

(fact "vectorify-trans works"
  (hazel/vectorify-trans {:db/id :my-entity :attrA 1 :attrB 2}) =>
  (fn [result] (and (some #(= % [:db/add :my-entity :attrA 1]) result)
                    (some #(= % [:db/add :my-entity :attrB 2]) result))))

(fact "trans-seq works"
  (hazel/trans-seq [:db/add :my-entity :my-attr [1 2]]) =>
  (fn [result] (and (some #(= % [:db/add :my-entity :my-attr 1]) result)
                    (some #(= % [:db/add :my-entity :my-attr 2]) result))))

(fact "normalize-transaction works for transaction with sequential value"
  (hazel/normalize-tx [{:db/id :my-entity :some-attr 2} [:db/add :my-entity :another-attr [3 4]]]) =>
  (fn [result] (and (some #(= % [:db/add :my-entity :some-attr 2]) result)
                    (some #(= % [:db/add :my-entity :another-attr 3]) result)
                    (some #(= % [:db/add :my-entity :another-attr 4]) result))))

(with-state-changes [(before :facts (init-db!))]
  (let [existing-facts (hazel/find (db/db "test-db") :test/many-longs)
        existing-values (sort (map last existing-facts))]
    (fact "get-values finds all values in case of cardinality many"
      (apply (partial hazel/get-values (db/db "test-db")) (butlast (first existing-facts))) => (sort existing-values))))

(fact "dbfn returns a proper db function"
  (hazel/dbfn [db tx] [1 2 3]) => '{:code "[1 2 3]", :lang "clojure", :params [db tx]})

(fact "dbfn-transaction returns a proper transaction for the addition of a db function"
  (hazel/dbfn-transaction :my-function (hazel/dbfn [db tx] [1 2 3])) => #(and (let [{:keys [lang code params]} (:db/fn %)]
                                                                                (and (= lang :clojure) (= code "[1 2 3]") (= params '[db tx])))
                                                                              (= (:db/ident %) :my-function)))
