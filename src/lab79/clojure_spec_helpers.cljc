(ns lab79.clojure-spec-helpers
  (:require #?(:clj  [clojure.spec :as s]
               :cljs [cljs.spec :as s])))

(s/def ::spec-name keyword?)

(s/def ::req (s/coll-of ::spec-name :kind vector?))

(s/def ::opt (s/coll-of ::spec-name :kind vector?))

(s/def ::keys-form (s/cat :macro #{'keys}
                         :args (s/keys* :opt-un [::req ::opt])))

(s/def ::coll-form (s/cat :macro #{'every}
                          :member-type (s/alt :spec-name ::spec-name
                                              :spec-form list?
                                              :pred symbol?)
                          :rest (s/* any?)))

(s/def ::and-form (s/cat ::macro #{'and}
                         :first-form any?
                         :rest (s/* any?)))

(s/def ::merge-desc (s/cat :macro #{'merge}
                           ;rest (s/+ any?)
                           :rest (s/+ (s/alt :spec-name keyword?
                                             :spec-desc list?))))

(declare extract-spec-keys)

(s/fdef spec->spec-keys
        :args (s/cat :spec seq?)
        :ret (s/keys :opt-un [::req ::opt]))
(defn spec->spec-keys
  [spec-form]
  (condp s/valid? spec-form
    ::keys-form
    (let [out (s/conform ::keys-form spec-form)]
      (:args out))

    ::coll-form
    (let [out (s/conform ::coll-form spec-form)]
      (extract-spec-keys (-> out :member-type :spec-name)))

    ::and-form
    (let [out (s/conform ::and-form spec-form)
          first-form (:first-form out)]
      (spec->spec-keys first-form))

    ::merge-desc
    (let [out (s/conform ::merge-desc spec-form)
          rest (:rest out)]
      (->> rest
           (map (fn [[type spec-name-or-form]]
                  (condp = type
                    :spec-name (extract-spec-keys spec-name-or-form)
                    :spec-desc (spec->spec-keys spec-name-or-form))))
           (apply merge-with into)))

    ;else
    (throw (ex-info "The spec should generate a map or collection of maps."
                    {:spec spec-form}))))

(s/fdef extract-spec-keys
        :args (s/cat :spec-name ::spec-name)
        :ret (s/keys :opt-un [::req ::opt]))
(defn extract-spec-keys
  [spec-name]
  (spec->spec-keys (s/describe spec-name)))

(s/fdef is-keys-spec?
        :args (s/cat :spec-name ::spec-name)
        :ret boolean?)
(defn is-keys-spec?
  [spec-name]
  (let [spec (s/describe spec-name)]
    (or
      (and (coll? spec) (= 'keys (first spec)))
      (and (coll? spec) (= 'every (first spec)) (is-keys-spec? (second spec)))
      (and (coll? spec) (= 'and (first spec)) (is-keys-spec? (second spec))))))