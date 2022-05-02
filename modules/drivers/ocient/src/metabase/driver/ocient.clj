(ns metabase.driver.ocient
  "Metabase Ocient Driver."
  (:require [clojure
             [set :as set]]
            [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clj-http.client :as http]
            [honeysql.core :as hsql]
            [honeysql.format :as hformat]
            [metabase.util.honeysql-extensions :as hx]
            [java-time :as t]
            [medley.core :as m]
            [metabase.config :as config]
            [metabase.db.spec :as db.spec]
            [metabase.driver :as driver]
            [metabase.driver.sql.util :as sql.u]
            [metabase.driver.sql.util.unprepare :as unprepare]
            [metabase.driver.sql.query-processor :as sql.qp]
            [schema.core :as s]
            [metabase.util :as u]
            [metabase.util
             [i18n :refer [tru]]
             [date-2 :as u.date]]
            [metabase.driver.sql-jdbc.sync.common :as sync-common]
            [metabase.driver.sql-jdbc.sync.interface :as i]
            [metabase.driver.sql-jdbc
             [common :as sql-jdbc.common]
             [execute :as sql-jdbc.execute]
             [connection :as sql-jdbc.conn]
             [sync :as sql-jdbc.sync]])

  (:import [java.sql PreparedStatement Types]
           [java.time LocalDate LocalDateTime LocalTime OffsetDateTime ZonedDateTime Instant OffsetTime ZoneId]
           [java.sql DatabaseMetaData ResultSet]
           [java.util Calendar TimeZone]))


(driver/register! :ocient, :parent :sql-jdbc)

; ;;; +----------------------------------------------------------------------------------------------------------------+
; ;;; |                                         metabase.driver.sql-jdbc impls                                         |
; ;;; +----------------------------------------------------------------------------------------------------------------+

(defn- make-subname [host port db]
  (str "//" host ":" port "/" db))

(defn ocient
  "Create a Clojure JDBC database specification for the Ocient DB."
  [{:keys [host port db]
    :or   {host "localhost", port 4050, db "system"}
    :as   opts}]
  (merge
   {:classname                     "com.ocient.jdbc.JDBCDriver"
    :subprotocol                   "ocient"
    :subname                       (make-subname host port db)}
   (dissoc opts :host :port :db)))

(defmethod driver/display-name :ocient [_] "Ocient")

(defmethod sql-jdbc.conn/connection-details->spec :ocient [_ {ssl? :ssl, :as details-map}]
  (-> details-map
      (update :port (fn [port]
                      (if (string? port)
                        (Integer/parseInt port)
                        port)))
      ;; (assoc :pooling "OFF")
      ;; remove :ssl in case it's false; DB will still try (& fail) to connect if the key is there
      (dissoc :ssl)
      (merge {:sslmode "disable", :pooling "OFF"})
      (set/rename-keys {:dbname :db})
      ocient
      ;; note: seperator style is misspelled in metabase core code
      (sql-jdbc.common/handle-additional-options details-map, :seperator-style :semicolon)))


;; We'll do regex pattern matching here for determining Field types because Ocient types can have optional lengths,
;; e.g. VARCHAR(255) or NUMERIDECIMAL(16,4)
(def ^:private database-type->base-type
  (sql-jdbc.sync/pattern-based-database-type->base-type
   [[#"BIGINT"    :type/BigInteger]
    [#"INT"       :type/Integer]
    [#"SHORT"     :type/Integer]
    [#"SMALLINT"  :type/Integer]
    [#"CHAR"      :type/Text]
    [#"VARCHAR"   :type/Text]
    [#"TEXT"      :type/Text]
    [#"BLOB"      :type/*]
    [#"BINARY"    :type/*]
    [#"REAL"      :type/Float]
    [#"DOUBLE"    :type/Float]
    [#"FLOAT"     :type/Float]
    [#"LONG"      :type/BigInteger]
    [#"DECIMAL"   :type/Decimal]
    [#"BOOLEAN"   :type/Boolean]
    [#"TIMESTAMP" :type/DateTime]
    [#"DATETIME"  :type/DateTime]
    [#"DATE"      :type/Date]
    [#"TIME"      :type/Time]]))

(defmethod sql-jdbc.sync/database-type->base-type :ocient
  [_ database-type]
  (database-type->base-type database-type))


(doseq [[feature supported?] {;; Ocinet reports all temporal values in UTC
                              :set-timezone                    false
                              :native-parameters               true
                              :basic-aggregations              true
                              :standard-deviation-aggregations true
                              :expression-aggregations         true
                              :advanced-math-expressions       true
                              :left-join                       true
                              :right-join                      true
                              :inner-join                      true
                              :nested-queries                  true
                              :regex                           false
                              :binning                         true
                              :foreign-keys                    (not config/is-test?)}]
  (defmethod driver/supports? [:ocient feature] [_ _] supported?))

(defmethod sql-jdbc.execute/read-column-thunk [:ocient Types/TIMESTAMP]
  [_ rs _ i]
  (fn []
    (let [d (.getObject rs i)]
      (if (nil? d)
        nil
        (.toLocalDateTime d)))))

  ;; (fn []
  ;;   (let [instant      (.toInstant (.getObject rs i))
  ;;         zone-id      (ZoneId/of "UTC")]
  ;;     (LocalDateTime/of instant zone-id))))

(defmethod sql-jdbc.execute/read-column-thunk [:ocient Types/DATE]
  [_ rs _ i]
  (fn []
    (.toLocalDate (.getObject rs i))))

(defmethod sql-jdbc.execute/read-column-thunk [:ocient Types/TIME]
  [_ rs _ i]
  (fn []
    (let [utc-str (.toString (.getObject rs i))]
      (LocalTime/parse utc-str))))

(defmethod sql-jdbc.execute/read-column-thunk [:ocient Types/TIMESTAMP_WITH_TIMEZONE]
  [_ rs _ i]
  (fn []
    (let [local-date-time    (.toLocalDateTime (.getObject rs i))
          zone-id            (ZoneId/of "UTC")]
      (OffsetDateTime/of local-date-time zone-id))))

(defmethod sql-jdbc.execute/read-column-thunk [:ocient Types/TIME_WITH_TIMEZONE]
  [_ rs _ i]
  (fn []
    (let [utc-str (.toString (.getObject rs i))]
      (LocalTime/parse utc-str))))

(defmethod sql-jdbc.execute/set-parameter [:ocient java.time.OffsetDateTime]
  [_ ^PreparedStatement ps ^Integer i t]
  (let [cal (Calendar/getInstance (TimeZone/getTimeZone (t/zone-id t)))
        t   (t/sql-timestamp t)]
    (log/tracef "(.setTimestamp %d ^%s %s <%s Calendar>)" i (.getName (class t)) (pr-str t) (.. cal getTimeZone getID))
    (.setTimestamp ps i t cal)))

(defmethod unprepare/unprepare-value [:ocient OffsetTime]
  [_ t]
  (format "time('%s')" (t/format "HH:mm:ss.SSS ZZZZZ" t)))

(defmethod unprepare/unprepare-value [:ocient OffsetDateTime]
  [_ t]
  (format "timestamp('%s')" (t/format "yyyy-MM-dd HH:mm:ss.SSS ZZZZZ" t)))

(defmethod unprepare/unprepare-value [:ocient ZonedDateTime]
  [_ t]
  (format "timestamp('%s')" (t/format "yyyy-MM-dd HH:mm:ss.SSS VV" t)))

(defmethod unprepare/unprepare-value [:ocient Instant]
  [driver t]
  (unprepare/unprepare-value driver (t/zoned-date-time t (t/zone-id "UTC"))))

(defmethod sql.qp/->honeysql [:ocient :median]
  [driver [_ arg]]
  (sql.qp/->honeysql driver [:percentile arg 0.5]))

;;  :cause Unsupported temporal bucketing: You can't bucket a :type/Date Field by :hour.                                                                                                                                                                                      │
;;  :data {:type :invalid-query, :field [:field 11 {:temporal-unit :hour}], :base-type :type/Date, :unit :hour, :valid-units #{:quarter :day :week :default :day-of-week :month :month-of-year :day-of-month :year :day-of-year :week-of-year :quarter-of-year}}              │
;;  :via                                                                                                                                                                                                                                                                      │
;;  [{:type clojure.lang.ExceptionInfo                                                                                                                                                                                                                                        │
;;    :message Error calculating permissions for query                                                                                                                                                                                                                        │
;;    :data {:query {:database 1, :type :query, :query {:source-table 3, :aggregation [[:count]], :breakout [[:field 11 {:temporal-unit :hour}] [:field 11 {:temporal-unit :minute}]]}}}                                                                                      │
;;    :at [metabase.models.query.permissions$eval59295$mbql_permissions_path_set__59300$fn__59304 invoke permissions.clj 138]}                                                                                                                                                │
;;   {:type clojure.lang.ExceptionInfo                                                                                                                                                                                                                                        │
;;    :message Unsupported temporal bucketing: You can't bucket a :type/Date Field by :hour.                                                                                                                                                                                  │
;;    :data {:type :invalid-query, :field [:field 11 {:temporal-unit :hour}], :base-type :type/Date, :unit :hour, :valid-units #{:quarter :day :week :default :day-of-week :month :month-of-year :day-of-month :year :day-of-year :week-of-year :quarter-of-year}}            │
;;    :at [metabase.query_processor.middleware.validate_temporal_bucketing$validate_temporal_bucketing invokeStatic validate_temporal_bucketing.clj 39]}]                                                                                                                     │
;;  :trace
;; Cast time columns to timestamps
(defn- ->timestamp [honeysql-form]
  (hx/cast-unless-type-in "timestamp" #{"timestamp" "date" "time"} honeysql-form))

(defmethod driver/db-start-of-week :ocient
  [_]
  :sunday)

(defn- date-trunc [unit expr] (hsql/call :date_trunc (hx/literal unit) (hx/->timestamp expr)))

(defmethod sql.qp/date [:ocient :date]            [_ _ expr] (hsql/call :date expr))
(defmethod sql.qp/date [:ocient :minute]          [_ _ expr] (date-trunc :minute expr))
(defmethod sql.qp/date [:ocient :hour]            [_ _ expr] (date-trunc :hour expr))
(defmethod sql.qp/date [:ocient :day]             [_ _ expr] (date-trunc :day expr))
(defmethod sql.qp/date [:ocient :week]            [_ _ expr] (sql.qp/adjust-start-of-week :ocient (partial date-trunc :week) expr))
(defmethod sql.qp/date [:ocient :month]           [_ _ expr] (date-trunc :month expr))
(defmethod sql.qp/date [:ocient :quarter]         [_ _ expr] (date-trunc :quarter expr))
(defmethod sql.qp/date [:ocient :year]            [_ _ expr] (date-trunc :year expr))
(defmethod sql.qp/date [:ocient :minute-of-hour]  [_ _ expr] (hsql/call :minute expr))
(defmethod sql.qp/date [:ocient :hour-of-day]     [_ _ expr] (hsql/call :hour expr))
(defmethod sql.qp/date [:ocient :day-of-week]     [_ _ expr] (hsql/call :day_of_week expr))
(defmethod sql.qp/date [:ocient :day-of-month]    [_ _ expr] (hsql/call :day expr))
(defmethod sql.qp/date [:ocient :day-of-year]     [_ _ expr] (hsql/call :day_of_year expr))
(defmethod sql.qp/date [:ocient :week-of-year]    [_ _ expr] (hsql/call :week expr))
(defmethod sql.qp/date [:ocient :month-of-year]   [_ _ expr] (hsql/call :month expr))
(defmethod sql.qp/date [:ocient :quarter-of-year] [_ _ expr] (hsql/call :quarter expr))
(defmethod sql.qp/date [:ocient :default]         [_ _ expr] expr)

(defmethod sql.qp/current-datetime-honeysql-form :ocient [_] :%now)

(defmethod sql.qp/unix-timestamp->honeysql [:ocient :seconds]      [_ _ expr] (hsql/call :to_timestamp expr))
(defmethod sql.qp/unix-timestamp->honeysql [:ocient :milliseconds] [_ _ expr] (hsql/call :to_timestamp expr 3))
(defmethod sql.qp/unix-timestamp->honeysql [:ocient :microseconds] [_ _ expr] (hsql/call :to_timestamp expr 6))

(defmethod sql.qp/current-datetime-honeysql-form :ocient
  [_]
  :%current_timestamp)

;; TODO So this seems to work, but the query should really have a LIMIT 1 tacked onto the end of it...
(defmethod sql.qp/->honeysql [:ocient :percentile]
  [driver [_ field p]]
  (hsql/raw (format "percentile(%s, %s) over (order by %s)"
                    (hformat/to-sql (sql.qp/->honeysql driver field))
                    (hformat/to-sql (sql.qp/->honeysql driver p))
                    (hformat/to-sql (sql.qp/->honeysql driver field)))))

;; Ocient does not have a median() function, use :percentile
(defmethod sql.qp/->honeysql [:ocient :median]
  [driver [_ arg]]
  (sql.qp/->honeysql driver [:percentile arg 0.5]))

(defmethod sql.qp/->honeysql [:ocient :relative-datetime]
  [driver [_ amount unit]]
  (sql.qp/date driver unit (if (zero? amount)
                             (sql.qp/current-datetime-honeysql-form driver)
                             (sql.qp/add-interval-honeysql-form driver (sql.qp/current-datetime-honeysql-form driver) amount unit))))

(defmethod sql.qp/->honeysql [:ocient :concat]
  [driver [_ & args]]
  (->> args
       (map (partial sql.qp/->honeysql driver))
       (reduce (partial hsql/call :concat))))

(defmethod sql.qp/add-interval-honeysql-form :ocient
  [_ hsql-form amount unit]
  (hx/+
   (hx/->timestamp hsql-form)
   (case unit
     :second   (hsql/call :seconds amount)
     :minute   (hsql/call :minutes amount)
     :hour     (hsql/call :hours amount)
     :day      (hsql/call :days amount)
     :week     (hsql/call :weeks amount)
     :month    (hsql/call :months amount)
     :quarter  (hsql/call :months (hx/* amount (hsql/raw 3)))
     :quarters (hsql/call :months (hx/* amount (hsql/raw 3)))
     :year     (hsql/call :years amount))))

(defmethod sql.qp/unix-timestamp->honeysql [:ocient :seconds]
  [_ _ field-or-value]
  (hsql/call :to_timestamp field-or-value))

(defmethod sql.qp/unix-timestamp->honeysql [:ocient :milliseconds]
  [driver _ field-or-value]
  (sql.qp/unix-timestamp->honeysql driver :seconds (hx// field-or-value (hsql/raw 1000))))

(defmethod sql.qp/unix-timestamp->honeysql [:ocient :microseconds]
  [driver _ field-or-value]
  (sql.qp/unix-timestamp->honeysql driver :seconds (hx// field-or-value (hsql/raw 1000000))))
