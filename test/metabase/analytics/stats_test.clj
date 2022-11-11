(ns metabase.analytics.stats-test
  (:require [clojure.test :refer :all]
            [metabase.analytics.stats :as stats :refer [anonymous-usage-stats]]
            [metabase.email :as email]
            [metabase.integrations.slack :as slack]
            [metabase.models.card :refer [Card]]
            [metabase.models.pulse :refer [Pulse]]
            [metabase.models.pulse-card :refer [PulseCard]]
            [metabase.models.pulse-channel :refer [PulseChannel]]
            [metabase.models.query-execution :refer [QueryExecution]]
            [metabase.test :as mt]
            [metabase.test.fixtures :as fixtures]
            [metabase.util :as u]
            [schema.core :as s]
            [toucan.db :as db]
            [toucan.util.test :as tt]))

(use-fixtures :once (fixtures/initialize :db))

(deftest bin-small-number-test
  (are [expected n] (= expected
                       (#'stats/bin-small-number n))
    "0"     0
    "1-5"   1
    "1-5"   5
    "6-10"  6
    "6-10"  10
    "11-25" 11
    "11-25" 25
    "25+"   26
    "25+"   500))

(deftest bin-medium-number-test
  (are [expected n] (= expected
                       (#'stats/bin-medium-number n))
    "0"       0
    "1-5"     1
    "1-5"     5
    "6-10"    6
    "6-10"    10
    "11-25"   11
    "11-25"   25
    "26-50"   26
    "26-50"   50
    "51-100"  51
    "51-100"  100
    "101-250" 101
    "101-250" 250
    "250+"    251
    "250+"    5000))

(deftest bin-large-number-test
  (are [expected n] (= expected
                       (#'stats/bin-large-number n))
    "0"          0
    "1-10"       1
    "1-10"       10
    "11-50"      11
    "11-50"      50
    "51-250"     51
    "51-250"     250
    "251-1000"   251
    "251-1000"   1000
    "1001-10000" 1001
    "1001-10000" 10000
    "10000+"     10001
    "10000+"     100000))

(deftest anonymous-usage-stats-test
  (with-redefs [email/email-configured? (constantly false)
                slack/slack-configured? (constantly false)]
    (mt/with-temporary-setting-values [site-name          "Test"
                                       startup-time-millis 1234.0]
      (let [stats (anonymous-usage-stats)]
        (is (partial= {:running_on          :unknown
                       :check_for_updates   true
                       :startup_time_millis 1234.0
                       :site_name           true
                       :friendly_names      false
                       :email_configured    false
                       :slack_configured    false
                       :sso_configured      false
                       :has_sample_data     false}
                      stats))))))

(deftest conversion-test
  (is (= #{true}
         (let [system-stats (get-in (anonymous-usage-stats) [:stats :system])]
           (into #{} (map #(contains? system-stats %) [:java_version :java_runtime_name :max_memory]))))
      "Spot checking a few system stats to ensure conversion from property names and presence in the anonymous-usage-stats"))

(def ^:private large-histogram (partial #'stats/histogram #'stats/bin-large-number))

(defn- old-execution-metrics []
  (let [executions (db/select [QueryExecution :executor_id :running_time :error])]
    {:executions     (count executions)
     :by_status      (frequencies (for [{error :error} executions]
                                    (if error
                                      "failed"
                                      "completed")))
     :num_per_user   (large-histogram executions :executor_id)
     :num_by_latency (frequencies (for [{latency :running_time} executions]
                                    (#'stats/bin-large-number (/ latency 1000))))}))

(deftest new-impl-test
  (is (= (old-execution-metrics)
         (#'stats/execution-metrics))
      "the new lazy-seq version of the executions metrics works the same way the old one did"))

