(ns gersak.mustachos
  (:require
    [clojure.zip :as zip]
    [clojure.string :as str]))


(defrecord Section [body sections context])


(def section-zipper 
  (zip/zipper
    (comp nil? :body)
    :sections
    #(assoc %1 :sections %2)
    (->Section nil nil nil)))


(defn prepare
  [template]
  (as-> template t
    ;; Remove comments
    (str/replace t #"\{\{!.*?\}\}" "")
    ;; Remove standalone sections
    ; (str/replace t #"\n *(\{\{[/#] *[\w\d]+ *\}\}) *\n" "\n$1")
    ;; \r\n should be considered newline for standalone tags
    (str/replace t #"\r\n *(\{\{# *[\w\d]+ *\}\})" "$1")
    ;; \r\n should be considered newline for standalone tags
    (str/replace t #"(\{\{/ *[\w\d]+ *\}\}) *\r\n" "$1")
    ;;
    (str/replace t #"[\r\n]+ *(\{\{# *[\w\d]+ *\}\}) *[\r\n]" "\n$1")
    ;;
    (str/replace t #"[\r\n]+ *(\{\{/ *[\w\d]+ *\}\}) *[\r\n]+" "\n$1")
    ;;
    (str/replace t #"^ *(\{\{[/#] *[\w\d]+ *\}\}) *\n" "$1")
    ;; when template is closed with section
    (str/replace t #"\n\s*(\{\{[/#] *[\w\d]+ *\}\})\Z" "$1")))


(defn parse
  ([template]
   (letfn [(parse-text [rtext loc]
             (if (empty? rtext) [nil loc]
               (let [{:keys [context]} (zip/node loc)
                     s (str/index-of rtext "{{#")
                     e (str/index-of rtext "{{/")]
                 ;; If new section is next
                 (if (every? nil? [s e])
                   ;; If there is some context
                   ;; throw error since context wasn't properly closed!
                   (if (some? context)
                     (throw
                       (ex-info
                         "Template syntax error. Context not closed"
                         {:context context}))
                     [nil (-> loc
                              (zip/append-child
                                (->Section rtext nil context)))])
                   ;; If some section mark was found
                   (let [[m] (sort (remove nil? [s e]))
                         end (+ m (str/index-of (subs rtext m) "}}"))
                         name (keyword (str/trim (re-find #"(?<=\{\{[#/])\s*.*\s*" (subs rtext m end))))
                         end' (+ 2 end)
                         rtext' (subs rtext end')]
                     (if (= m s)
                       ;; If this is new section
                       (as-> loc l
                         ;; lookforward join section
                         (if (zero? m) l
                           (zip/append-child l (->Section (subs rtext 0 m) nil context)))
                         ;; Move down
                         (let [l' (->
                                    (zip/append-child l (->Section nil [] ((fnil conj []) context name)))
                                    zip/down
                                    zip/rightmost)]
                           (parse-text rtext' l')))
                       ;; And if section is closing, check that name matches context
                       ;; If it doesn't than throw exception
                       (if (= (last context) name)
                         ;; if it name matches go to previous context
                         (let [section (->Section (subs rtext 0 e) [] context)
                               loc' (cond->
                                      (zip/append-child loc section)
                                      (not-empty context) zip/up)]
                           (parse-text rtext' loc'))
                         (throw
                           (ex-info
                             (str "Wrong enclosing section " name)
                             {:section name
                              :rtext rtext})))))))))]
     (let [[_ loc] (parse-text (prepare template) section-zipper)]
       (zip/root loc)))))


(def ^:dynamic *context-stack* nil)

(defn- recompute-stack [section]
  (if (nil? section) *context-stack*
    (let [value (get *context-stack* section)]
      (cond
        (map? value)
        (merge *context-stack* value {:. true})
        ;;
        ((every-pred sequential? not-empty) value)
        (assoc *context-stack* :. value)
        ;;
        (sequential? value)
        (assoc *context-stack* :. nil)
        :else
        (assoc *context-stack* :. value)))))


(defn- get-context-value [context]
  (if context
    (let [context (map keyword (str/split (name context) #"\."))]
      (get-in *context-stack* context))
    *context-stack*))


(defn render
  ([template] (render template nil))
  ([template data] (render template data nil))
  ([template data _]
   (let [template (if (string? template) (parse template) template)]
     (letfn [(multiply? [value]
               (sequential? value)
               #_(or
                 (every? map? value)
                 (every? sequential? value)))
             (print-section [{:keys [context body sections] :as section}]
               (let [context (last context)
                     variables (when body
                                 (distinct
                                   (map
                                     (fn [variable]
                                       (let [[f :as content] (subs variable 2 (- (count variable) 2))]
                                         (case f
                                           \> {:escape? true
                                               :name (subs content 1)
                                               :partial? true}
                                           ;;
                                           \{ {:escape? false
                                               :name (str/trim
                                                       (subs content 1 (count content)))}
                                           ;;
                                           \& {:name (str/trim (subs content 1))}
                                           ;;
                                           {:escape? true
                                            :name (str/trim content)})))
                                     (re-seq #"(?m)\{\{.*?\}\}" body))))] 
                 (letfn [(replace-variable [body {:keys [name escape?]}]
                           (cond
                             ;;
                             (some? name)
                             (let [path (map keyword (str/split name #"\."))
                                   value (case name
                                             "." (get *context-stack* :.)
                                             (get-in *context-stack* path))
                                   value (when value
                                           (if-not escape? value
                                             (str/escape
                                               (str value)
                                               {\& "&amp;"
                                                \" "&quot;"
                                                \< "&lt;"
                                                \> "&gt;"
                                                \' "&apos;"})))]
                               (tap>
                                 {:message "Replacing variable"
                                  :path path
                                  :body body
                                  :variable name
                                  :escape? escape?
                                  :value value})
                               (str/replace
                                 body
                                 (re-pattern (str "\\{{2,3}[&]*\\s*" name "\\s*\\}{2,3}"))
                                 (str value)))
                             :else body))]
                   (str
                     (if-let [context-value (get-context-value context)]
                       (cond
                         ;; If current content is list
                         (and context (multiply? context-value))
                         (let [stack (recompute-stack context)]
                           (reduce
                             (fn [body' value]
                               (binding [*context-stack* (assoc stack context value :. value)]
                                 ;; TODO - Fix this... It is not working for (123)(abc))
                                 (tap>
                                   {:message "Expanding list"
                                    :context/stack *context-stack*
                                    :body body'
                                    :value value})
                                 (str body' (print-section section))))
                             ""
                             context-value))
                         ;; Otherwise check if there is body
                         ;; and variables are empty than return body
                         (and body (empty? variables))
                         (do
                           (tap>
                             {:message "No variables. Printing body"
                              :context context
                              :body body})
                           body) 
                         ;; If there is some body and variables aren't empty
                         ;; try to replace those variables
                         (some? body)
                         (binding [*context-stack* (recompute-stack context)]
                           (tap>
                             {:message "Replacing variables"
                              :context/stack *context-stack*
                              :context context
                              :body body
                              :variables variables})
                           (reduce replace-variable body variables))
                         ;; If there is no body try to print all sections
                         (nil? body)
                         (binding [*context-stack* (recompute-stack context)]
                           (tap>
                             {:message "Priniting children and focusing context"
                              :context/stack *context-stack*
                              :context context})
                           ; (println "Expanding sections: " (map :context sections))
                           (reduce str (map print-section sections)))
                         ;;
                         :else
                         (throw
                           (ex-info
                             "This shouldn't happen"
                             {:section section
                              :variables variables
                              :context context
                              :stack *context-stack*})))
                       (when (nil? context)
                         (if (not-empty sections)
                           (reduce str (map print-section sections))
                           (reduce replace-variable body variables))))))))]
       (binding [*context-stack* (cond
                                   (map? data)
                                   (assoc data :. data)
                                   ;;
                                   (sequential? data)
                                   {:. data}
                                   ;;
                                   :else
                                   {:. data})]
         (print-section template))))))

(comment
  (re-find #"^\s+\n+" text)
  (clojure.test/run-tests 'gersak.mustachos)
  (add-tap clojure.pprint/pprint)
  (parse "{{#list}}{{item}}{{/list}}"))
