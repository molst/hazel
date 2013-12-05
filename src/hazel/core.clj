(ns hazel.core "Stateless and highly independent code."
  (:refer-clojure :exclude [find]) ;;Eliminate name conflict with clojure.core
  (:require [datomic.api :as d]))

(def tags-schema
  {:db/id #db/id[:db.part/db]
   :db/ident :general/tags
   :db/valueType :db.type/keyword
   :db/cardinality :db.cardinality/many
   :db/doc "Tags that can be set on an entity to boost the search possibilities."
   :db.install/_attribute :db.part/db})

(defn attr-to-valvar "Converts an attribute, eg. :myns/myattr, to a value variable on the form ?val_myns/myattr." [attr]
  (symbol (-> (clojure.string/replace-first (str "?val" attr) ":" "_")
              (clojure.string/replace "/" "+"))))

(defn where-attr-exists "Creates a where-clause on the form [?ent :attrname ?val_attrname]." [attr]
  `[~'?ent ~attr ~(attr-to-valvar attr)])

(defn where-attrs-exist "Creates a seq of where-clauses on the form [?ent :attrname ?val_attrname]." [mandatory-attributes]
  (for [attr mandatory-attributes]
    (where-attr-exists attr)))

(defn where-attr-matches "Creates a where-clause on the form [?ent :attrname value]." [attr val]
  `[~'?ent ~attr ~val])

(defn where-attrs-match "Creates a seq of where-clauses on the form [?ent :attrname value]. Supports sequential values. nil values mean any value set for the attribute should be treated as a match."
  [attr-val-map]
  (let [{seqs true singles false} (group-by #(sequential? (val %)) (seq attr-val-map))]
    (concat (for [s singles] (if-let [val (val s)] (where-attr-matches (key s) val) (where-attr-exists (key s))))
            (for [s seqs s-val (seq (val s))]
              (where-attr-matches (key s) s-val)))))

(defn triplify "Makes a seq of triples on the form (entity attribute value)." [attr ent-vals]
  (map #(interpose attr %) ent-vals))

(defmulti find "Find all entities matching input. If input is a keyword, the keyword must be an attribute of matching entities. If input is a map, the keys and (non-nil) values must match the corresponding attributes of matching entities. Optionally supply tags that must all be set for entities to slip through the filter."
  (fn [db input & tags] (type input)))

(defmethod find clojure.lang.Keyword [db input & tags]
  (triplify input (d/q `[:find ~'?ent ~(attr-to-valvar input) :where ~@(where-attrs-exist [input])] db)))

(defmethod find clojure.lang.Associative [db input & tags]
  (when (seq input) (d/q `[:find ~'?ent :where ~@(where-attrs-match input)] db)))

;;(find (get-person-db) {:repo/name ["hej" "haj"]}) -- OR should be supported this way when needed...

(defn get-values "Returns a seq of all values for 'attr' on 'entid'."
  [db entid attr] (map first (d/q `[:find ~'?val :where [~entid ~attr ~'?val]] db)))

(defn realize-dynmap [dynmap & selected-keys]
  (apply merge
         (for [selected-key selected-keys]
           (when-let [selected-key-value (selected-key dynmap)]
             {selected-key (if (set? selected-key-value)
                             (seq selected-key-value)
                             selected-key-value)}))))

(defn entid-maps "Takes a map with any filtering attribute/value pairs and returns a seq of plain {:db/id entid} maps."
  [db id-map] (for [entid (find db id-map)] {:db/id (first entid)}))

(defn entid-map "Takes a map with any filtering attribute/value pairs and returns a plain {:db/id entid} map." [db id-map]
  (if-let [db-id (:db/id id-map)]
    {:db/id db-id}
    (first (entid-maps db id-map))))

(defn season-entid
  "Takes an entity id, or a seq in which the first element is an entity id, 'entid' and creates a map with the supplied attributes and their corresponding values."
  [db entid & attrs]
  (if-let [entid (if (coll? entid) (first entid) entid)]
    (apply (partial realize-dynmap (d/entity db entid)) attrs)
    #_(apply (partial realize-dynmap (d/entity db entid)) (conj attrs :db/id))
    #_(assoc (apply (partial realize-dynmap (d/entity db entid)) attrs)
      :db/id entid)))

(defn season-entids "Takes a seq of entity ids, or lists of which the first element are entity ids, 'entids' and returns a seq of maps, each with the supplied attributes and their corresponding values."
  [db entids & attrs]
  (for [entid entids]
    (apply (partial season-entid db entid) attrs)))

(defn season
  "Identifies an entity using the attribute/value pairs in 'map' and then fills/seasons the map with all existing data in the database corresponding to 'selected-attrs'. The first found entity will be used. Note that as soon as an entity is found, keys in 'map' that corresponds to attributes in 'selected-attrs' will be replaced if they are found in the database. Optionally use 'id-attrs' to choose which keys in 'map' that should be used for identification of the unique entity."
  [db map selected-attrs & id-attrs]
  (if-let [entid (if-let [id (:db/id map)] ;;Try to find a unique entity using a database id first
                   id
                   (first (first (find db (if id-attrs (select-keys map id-attrs) map)))))]
    (merge map (apply (partial season-entid db entid) selected-attrs))))

(defn season-dbid "Takes a seq of maps with any filtering attribute/value pairs and returns a seq with :db/id seasoned to each element."
  [db id-maps] (for [id-map id-maps] (season db id-map [:db/id])))

(defn vectorify-trans "Makes a seq of vectors out of a transaction map or vector." [trans]
  (if (map? trans)
    (for [entry (seq (dissoc trans :db/id))] [:db/add (:db/id trans) (key entry) (val entry)])
    [trans]))

(defn trans-seq "Converts a transaction with a sequential value to a seq of multiple transactions."
  [[op entid attr val :as trans]]
  (let [no-val-trans [op entid attr]]
    (if (sequential? val)
      (for [v (seq val)]
        (conj no-val-trans v))
      [trans])))

(defn normalize-tx "Converts any maps in the input seqable to a seq of :db/add and :db/retract tuples." [tx]
  (apply concat (map trans-seq (apply concat (map vectorify-trans tx)))))

(defmacro dbfn "Creates a datomic db function map."
  [params code]
  `{:lang "clojure"
    :params '[~@params]
    :code (str '~code)})

(defmacro dbfn-transaction "Creates the addition transaction of a datomic db function."
  [fn-name-keyword db-function]
  `{:db/id (d/tempid :db.part/user)
    :db/ident ~fn-name-keyword
    :db/fn (d/function ~db-function)})
