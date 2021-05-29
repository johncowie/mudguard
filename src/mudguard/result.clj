(ns mudguard.result)

(defn validation-error
  ([id value]
   (validation-error id value {}))
  ([id value constraints]
   {::errors [{::id          id
               ::input       value
               ::constraints constraints}]}))

(defn sample-error
  ([id]
   (sample-error id {}))
  ([id constraints]
   (validation-error id ::sample-input constraints)))

(defn join-errors [errorA errorB]
  {::errors (concat (::errors errorA) (::errors errorB))})

(defn validation-errors [& errors]
  (reduce join-errors {::errors []} errors))

(defn error? [res]
  (some? (::errors res)))

(defn errors-at [key err]
  (update err ::errors (partial map (fn [e] (update e ::id (partial cons key))))))

(defn success-value [v]
  {::success v})

(defn success-value? [v]
  (and (associative? v) (contains? v ::success)))

(defn extract-success-val [v]
  (::success v))
