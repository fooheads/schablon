(ns fooheads.test-support)


(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:js-globals env)))


(defn ex-symbol [env]
  (if (cljs-env? env) 'js/Error 'Exception))


(defmacro thrown-ex-data [body]
  `(try
     ~body
     (throw (ex-info "Exception was not thrown" {:expected-exception :was-not-thrown}))
     (catch ~(ex-symbol &env) e#
       (ex-data e#))))

