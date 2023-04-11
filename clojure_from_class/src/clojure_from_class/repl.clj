(ns hw6.core)

(defn my-eval
  "Evaluates expression, changes environment, and returns resulting state
   with :return set to returned value.
    - state should be a map with a :environment key that stores all bindings,
      and a :return key that stores the result of evaluation
    - expression is the Clojure expression given to the REPL"
  [state expression]
  (if (not (seq? expression))
    ; THEN: Not a function call
    (if (symbol? expression)
      (assoc state :return (get (:environment state) expression)) ; fetch binding of symbol
      (assoc state :return expression)) ; return value of a literal

    ; ELSE: A function call
    (let [f (first expression)
          args (rest expression)]
      (cond
        (= f '+) (assoc state :return (apply + (map #(:return (my-eval state %))
                                                    args)))
        (= f '-) (assoc state :return (apply - (map #(:return (my-eval state %))
                                                    args)))
        (= f '*) (assoc state :return (apply * (map #(:return (my-eval state %))
                                                            args)))
        (= f '/) (assoc state :return (apply / (map #(:return (my-eval state %))
                                                            args)))
        (= f 'def) {:environment (assoc (:environment state)
                                        (first args)
                                        (:return (my-eval state (second args))))
                    :return (first args)}
        :else (assoc state :return "Error: That function isn't defined.")))))

(defn repl
  "A Clojure REPL." 
  ;; If called with no args, make a new state and call self
  ([] (repl {:environment {}
              :return nil}))
  ([state]
   (print "H]=> ")
   (flush)
   (let [line (read-line)]
     (when (not (empty? line))
       (let [new-state (my-eval state (read-string line))]
         (println "Environment:" (:environment new-state))
         (println (:return new-state))
         (recur new-state)))))) ;; recursively pass new state to repl

(defn main
  [& args]
  (repl))


(comment
  (main) 
   
  
) 
