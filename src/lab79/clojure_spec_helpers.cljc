(ns lab79.clojure-spec-helpers
  (:require #?(:clj  [clojure.spec :as s]
               :cljs [cljs.spec :as s])))

(s/def ::spec-name keyword?)
(s/def ::req (s/coll-of ::spec-name :kind vector?))
(s/def ::opt (s/coll-of ::spec-name :kind vector?))
(s/def ::map-form (s/cat :macro #{'keys}
                         :args (s/keys* :opt [::req ::opt])))
(s/def ::coll-form (s/cat :macro #{'every}
                          :spec-name ::spec-name
                          :rest (s/* any?)))

(s/fdef extract-spec-keys
        :args (s/cat :spec-name ::spec-name)
        :ret (s/keys :opt-un [::req ::opt]))
(defn extract-spec-keys
  [spec-name]
  (let [form (s/describe spec-name)
        out (s/conform ::map-form form)]
    (if (= ::s/invalid out)
      (let [out (s/conform ::coll-form form)]
        (if (= ::s/invalid out)
          (throw (ex-info "The spec should generate a map or collection of maps."
                          {:spec form})))
        (let [spec-name-for-possible-map (:spec-name out)]
          (extract-spec-keys spec-name-for-possible-map)))
      (:args out))))

(s/fdef is-keys-spec?
        :args (s/cat :spec-name ::spec-name)
        :ret boolean?)
(defn is-keys-spec?
  [spec-name]
  (let [spec (s/describe spec-name)]
    (or
      (and (coll? spec) (= 'keys (first spec)))
      (and (coll? spec) (= 'every (first spec)) (is-keys-spec? (second spec))))))