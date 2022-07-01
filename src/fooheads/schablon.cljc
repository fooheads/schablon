(ns fooheads.schablon
  (:require
    [clojure.string :as str]))


(defn- variable? [x]
  (and (symbol? x) (str/starts-with? (str x) "?")))


(defn- variable->keyword [v]
  (-> v (str) (str/replace #"^\?" "") (keyword)))


(defn- render-variable [template args]
  (let [var-name (variable->keyword template)]
    (when-not (contains? args var-name)
     (throw (ex-info (str "Variable not bound: " template)
                     {:msg "Variable not bound"
                      :template template
                      :args args})))
    (get args var-name)))


(declare render*)


(defn- render-sequential [template args]
  (let [render-as (or (some-> template meta :render-as) :vector-template)]
    (case render-as

      :vector-template
      (reduce
        (fn [sum arg]
          (conj sum (render* (first template) arg)))
        [] args)

      :vector
      (mapv #(render* % args) template))))


(defn- render-map [template args]
  (reduce-kv
    (fn [m k v]
      (assoc m k (render* v args)))
    {} template))


(defn- render*
  [template args]
  (let [context (or (some->> template meta :context) [])]
    (when-not (vector? context)
      (throw (ex-info "Context must be a vector" {:msg "Context must be a vector"
                                                  :template template
                                                  :args args})))
    (cond
      (sequential? template) (render-sequential template (get-in args context))
      (map? template) (render-map template (get-in args context))
      (variable? template) (render-variable template args)
      :else template)))


(defn- apply-meta [m meta-by-path]
  (reduce-kv
    (fn [m path meta-data]
      (let [new-meta (merge (meta (get-in m path)) meta-data)]
        (update-in m path with-meta new-meta)))
    m
    meta-by-path))


(defn render
  "Render a data template using the specified args.

  By default, vectors are treat as `:vector-template`, meaning the first element
  of the vector will be used as a template, mapping the corresponding path in args.

  If you need a simple vector, specify `{:render-as :vector}` as meta data on the
  vector in the template, or pass in path-opts `{[:my :path] :vector}`, e.g.

  ```
  (render {:url ['?scheme \"://\" '?host]}

          {:scheme \"https\"
           :host \"google.com\"}

          {[:url] :vector})

  ; => {:url (\"https\" \"://\" \"google.com\")}
  ```

  "
  ([template args]
   (render template args {}))

  ([template args meta-data]
   (render* (apply-meta template meta-data) args)))

