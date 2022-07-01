(ns fooheads.schablon-test
  (:require
    [clojure.set :as set]
    #?(:clj  [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer-macros [deftest is testing]])
    [fooheads.schablon :refer [render]]
    #?(:clj  [fooheads.test-support :refer [thrown-ex-data]]
       :cljs [fooheads.test-support :refer-macros [thrown-ex-data]])))


(defn submap?
  "Checks whether a is a submap of b"
  [a b]
  (set/subset? (set a) (set b)))

(ex-data *e)

(deftest render-test
  (testing "simple keys"

    (is (= {:a 1} (render {:a '?foo} {:foo 1})))

    (is (= {:a {:b 1 :c 2}}
           (render {:a {:b '?foo :c '?bar}} {:foo 1 :bar 2})))

    (is (submap?
          {:msg "Variable not bound"
           :template '?foo}
          (thrown-ex-data
            (render {:a '?foo} {:fooooo 1})))))


  (testing "qualified keys"
    (is (= {:a 1} (render {:a '?foo/bar} {:foo/bar 1}))))



  (testing "seqs at root"
    (is (= {:rows [{:row-code 1} {:row-code 2}]}
           (render {:rows [{:row-code '?code}]}
                   [{:code 1} {:code 2}]))))

  (testing "contexts are vectors"
    (is (submap?
          {:msg "Context must be a vector"}
          (thrown-ex-data
            (render {:rows ^{:context :codes} [{:row-code '?code}]}
                    {:codes [{:code 1} {:code 2}]})))))


  (testing "seqs in path"
    (is (= {:rows [{:row-code 1} {:row-code 2}]}
           (render {:rows ^{:context [:codes]} [{:row-code '?code}]}
                   {:codes [{:code 1} {:code 2}]}))))


  (testing "seqs in seqs path"
    (is (= {:wo
            {:jobs
             [{:jid 1
               :ops
               [{:oid 100}
                {:oid 101}]}
              {:jid 2
               :ops
               [{:oid 200}
                {:oid 201}]}]}}

           (render {:wo
                    {:jobs

                     ^{:context [:jobs]}
                     [{:jid '?job-id
                       :ops

                       ^{:context [:operations]}
                       [{:oid '?operation-id}]}]}}

                   {:jobs [{:job-id 1
                            :operations
                            [{:operation-id 100}
                             {:operation-id 101}]}
                           {:job-id 2
                            :operations
                            [{:operation-id 200}
                             {:operation-id 201}]}]}))))


  (testing "1"
    (is (= {:wo
            {:something
             {:ops
              [{:oid 100}
               {:oid 101}]}}}

           (render {:wo
                    {:something
                     {:ops
                      ^{:context [:operations]}
                      [{:oid '?operation-id}]}}}

                   {:operations
                    [{:operation-id 100}
                     {:operation-id 101}]}))))


  (testing "list in root, default context"
    (is (= {:wo
            {:something
             {:ops
              [{:oid 100}
               {:oid 101}]}}}

           (render {:wo
                    {:something
                     {:ops
                      [{:oid '?operation-id}]}}}

                   [{:operation-id 100}
                    {:operation-id 101}]))))


  (testing "plain vector with opts"
    (is (= {:url ["https" "://" "google.com"]}

           (render {:url ['?scheme "://" '?host]}

                   {:scheme "https" :host "google.com"}

                   {[:url] {:render-as :vector}}))))


  (testing "plain vector with meta"
    (is (= {:url ["https" "://" "google.com"]}

           (render {:url ^{:render-as :vector} ['?scheme "://" '?host]}

                   {:scheme "https" :host "google.com"})))))


