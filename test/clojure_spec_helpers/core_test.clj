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
    (is (true? (is-keys-spec? :x/alias-keys)))))

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
      (is (= [:extract/c :extract/d] (:opt out)))))
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
      (is (= [:x/xx :y/yy] (:opt out))))))