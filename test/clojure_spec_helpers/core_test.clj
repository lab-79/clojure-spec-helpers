(ns clojure-spec-helpers.core-test
  (:require [clojure.test :refer :all]
            [lab79.clojure-spec-helpers :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(doseq [x (stest/enumerate-namespace 'lab79.clojure-spec-helpers)]
  (stest/instrument x))

(deftest test-is-keys-spec?
  (testing "should return true for a spec-name that will generate a map."
    (s/def :x/a integer?)
    (is (false? (is-keys-spec? :x/a)))
    (is (false? (is-keys-spec? :x/a)))
    (s/def :x/b keyword?)
    (is (false? (is-keys-spec? :x/b)))
    (s/def :x/keys (s/keys :req [:x/a] :opt [:x/b]))
    (is (true? (is-keys-spec? :x/keys)))
    (s/def :x/alias-keys :x/keys)
    (is (true? (is-keys-spec? :x/alias-keys)))
    (s/def :x/merged (s/merge :x/keys (s/keys :req [:x/a])))
    (is (true? (is-keys-spec? :x/merged)))
    (s/def :x/coll-of-key-spec (s/coll-of :x/keys))
    (is (true? (is-keys-spec? :x/coll-of-key-spec)))))

(deftest test-spec->spec-keys
  (s/def :a/y any?)
  (s/def :a/z any?)
  (s/def :b/x any?)
  (s/def :b/y any?)
  (s/def :b/z any?)
  (is (= {:req [:a/y :a/z :b/x :b/y :b/z], :opt []}
         (spec->spec-keys '(or :a (keys :req [:a/y
                                              :a/z])
                               :b (keys :req [:b/x
                                              :b/y
                                              :b/z]))))))

(deftest test-extract-spec-keys
  (testing "should extract req and opt keys for a spec with both req and opt keys"
    (s/def :extract/map-req-opt (s/keys :req [:extract/a :extract/b] :opt [:extract/c :extract/d]))
    (s/def :extract/a keyword?)
    (s/def :extract/b keyword?)
    (s/def :extract/c keyword?)
    (s/def :extract/d keyword?)
    (let [out (extract-spec-keys :extract/map-req-opt)]
      (is (= [:extract/a :extract/b] (:req out)))
      (is (= [:extract/c :extract/d] (:opt out)))))
  (testing "should extract req and no opt keys for a spec with only req keys"
    (s/def :extract/map-req-only (s/keys :req [:extract/a :extract/b]))
    (let [out (extract-spec-keys :extract/map-req-only)]
      (is (= [:extract/a :extract/b] (:req out)))
      (is (empty? (:opt out)))))
  (testing "should extract no req and all opt keys for a spec with only opt keys"
    (s/def :extract/map-opt-only (s/keys :opt [:extract/c :extract/d]))
    (let [out (extract-spec-keys :extract/map-opt-only)]
      (is (= [:extract/c :extract/d] (:opt out)))
      (is (empty? (:req out)))))
  (testing "should extract req and opt keys from a 'keys form nested inside an 'and form"
    (s/def :extract/keys-inside-and (s/and (s/keys :req [:extract/a :extract/b]
                                                   :opt [:extract/c :extract/d])))
    (let [out (extract-spec-keys :extract/keys-inside-and)]
      (is (= [:extract/a :extract/b] (:req out)))
      (is (= [:extract/c :extract/d] (:opt out))))
    (testing "multiple 'keys forms"
      (s/def :extract/mult-keys-inside-and (s/and (s/keys :req [:extract/a]
                                                          :opt [:extract/b])
                                                  (s/keys :req [:extract/c]
                                                          :opt [:extract/d])))
      (let [out (extract-spec-keys :extract/mult-keys-inside-and)]
        (is (= [:extract/a :extract/c] (:req out)))
        (is (= [:extract/b :extract/d] (:opt out)))))
    (testing "heterogeneous with single 'keys (as not the first element)"
      (s/def :extract/single-buried-keys-inside-and (s/and map?
                                                           (s/keys :req [:extract/a]
                                                                   :opt [:extract/b])))
      (let [out (extract-spec-keys :extract/single-buried-keys-inside-and)]
        (is (= [:extract/a] (:req out)))
        (is (= [:extract/b] (:opt out)))))
    (testing "heterogeneous 'keys and spec-name inside an 'and"
      (s/def :extract/keys-and-spec-name-inside-and (s/and map?
                                                           :x/keys
                                                           (s/keys :req [:extract/a]
                                                                   :opt [:extract/b])))
      (let [out (extract-spec-keys :extract/keys-and-spec-name-inside-and)]
        (is (= [:x/a :extract/a] (:req out)))
        (is (= [:x/b :extract/b] (:opt out)))))
    (testing "heterogeneous 'keys and anon fn pred inside an 'and"
      (s/def :extract/keys-and-fn-inside-and (s/and (fn [x] true)
                                                    (s/keys :req [:extract/a]
                                                            :opt [:extract/b])))
      (let [out (extract-spec-keys :extract/keys-and-fn-inside-and)]
        (is (= [:extract/a] (:req out)))
        (is (= [:extract/b] (:opt out)))))

    (testing "'merge and a 'comp predicate inside an 'and"
      (s/def :extract/nested-keys-in-merge-in-and (s/and
                                                    (s/merge
                                                      :x/keys
                                                      (s/keys :req [:extract/a]
                                                              :opt [:extract/b]))
                                                    (comp #{:a} :extract/a)))
      (let [out (extract-spec-keys :extract/nested-keys-in-merge-in-and)]
        (is (= [:x/a :extract/a] (:req out)))
        (is (= [:x/b :extract/b] (:opt out))))))
  (testing "should extract req and opt keys from a 'keys form inside an 'every form"
    (s/def :extract/keys-inside-coll (s/coll-of (s/keys :req [:extract/a :extract/b]
                                                        :opt [:extract/c :extract/d])))
    (let [out (extract-spec-keys :extract/keys-inside-coll)]
      (is (= [:extract/a :extract/b] (:req out)))
      (is (= [:extract/c :extract/d] (:opt out)))))
  (testing "should extract req and opt keys from inside an 'every form"
    (s/def :extract/map-req-opt (s/keys :req [:extract/a :extract/b] :opt [:extract/c :extract/d]))
    (s/def :extract/implicit-keys-inside-coll (s/coll-of :extract/map-req-opt))
    (let [out (extract-spec-keys :extract/implicit-keys-inside-coll)]
      (is (= [:extract/a :extract/b] (:req out)))
      (is (= [:extract/c :extract/d] (:opt out)))))
  (testing "should extract req and opt keys from a 'merge form"
    (s/def ::x (s/keys :req [:x/x] :opt [:x/xx]))
    (s/def :x/x keyword?)
    (s/def :x/xx keyword?)
    (s/def ::y (s/merge ::x
                        (s/keys :req [:y/y] :opt [:y/yy])))
    (s/def :y/y keyword?)
    (s/def :y/yy keyword?)
    (let [out (extract-spec-keys ::y)]
      (is (= [:x/x :y/y] (:req out)))
      (is (= [:x/xx :y/yy] (:opt out)))))

  (testing "should extract req and opt keys from an 'or form"
    (s/def :or-x/kw keyword?)
    (s/def :or-y/kw keyword?)
    (s/def :or-x/map (s/keys :req [:or-x/kw]))
    (s/def :or-y/map (s/keys :req [:or-y/kw]))
    (s/def :or/root
      (s/or :or/x :or-x/map
            :or/y :or-y/map
            :or/form (s/keys :opt [:or-x/kw])))
    (let [out (extract-spec-keys :or/root)]
      (is (= [:or-x/kw :or-y/kw] (:req out)))
      (is (= [:or-x/kw] (:opt out))))))

(deftest test-is-collection-spec?
  (testing "should return true for coll-of"
    (s/def :x/a integer?)
    (s/def :x/some-collection (s/coll-of :x/a))
    (is (true? (is-collection-spec? :x/some-collection)))))
