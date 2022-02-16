(ns gersak.mustachos
  (:require
    [clojure.zip :as zip]
    [clojure.test :refer [deftest is testing]]
    [clojure.string :as str]))


(defrecord Section [body sections context])


(comment
  (def token-name "bool")
  (re-find
    (re-pattern
      (str "(?m)^\\{\\{/\\s*" token-name "\\s*\\}\\}[\\n\\r]+"))
    template)
  (def template "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |")
  (re-find
    (re-pattern
      (str
        "(?m)"
        "^\\{\\{/\\s*" token-name "\\s*\\}\\}[\\n\\r]+" "|"
        "\\^ *\\{\\{/\\s*" token-name "\\s*\\}\\}[\\n\\r]+" "|"
        "\\{\\{/\\s*" token-name "\\s*\\}\\}"))
    template)
  (def template
    "|
* first
* second
* third")
  (def template
    "|
{{#bool}}
* first
{{/bool}}
* {{two}}
{{#bool}}
* third
{{/bool}}")
  (def template
    "|
{{#a}}
{{one}}
{{#b}}
{{one}}{{two}}{{one}}
{{#c}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{#d}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{#five}}
{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
{{one}}{{two}}{{three}}{{four}}{{.}}6{{.}}{{four}}{{three}}{{two}}{{one}}
{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
{{/five}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{/d}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{/c}}
{{one}}{{two}}{{one}}
{{/b}}
{{one}}
{{/a}}")
  (def template "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"))


(def section-zipper 
  (zip/zipper
    (comp nil? :body)
    :sections
    #(assoc %1 :sections %2)
    (->Section nil nil nil)))


(defn parse
  ([template]
   (letfn [(clean [text]
             (str/replace text #"^\n" ""))
           (parse-text [rtext loc]
             ; (println "RTEXT: " rtext)
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
                              (zip/append-child (->Section (str/replace rtext #"\n$" "") nil context)))])
                   ;; If some section mark was found
                   (let [[m] (sort (remove nil? [s e]))
                         end (+ m (str/index-of (subs rtext m) "}}"))
                         name (keyword (str/trim (re-find #"(?<=\{\{[#/])\s*.*\s*" (subs rtext m end))))
                         end' (+ 2 end)
                         rtext' (subs rtext end')]
                     ; (println "MS: " [s e])
                     ; (println "NAME: " name)
                     ; (println "Context: " context)
                     (if (= m s)
                       ;; If this is new section
                       (as-> loc l
                         ;; lookforward join section
                         (if (zero? m) l
                           (zip/append-child l (->Section (clean (subs rtext 0 m)) nil context)))
                         ;; Move down
                         (let [l' (->
                                    (zip/append-child l (->Section nil [] ((fnil conj []) context name)))
                                    zip/down
                                    zip/rightmost)]
                           ; (println "NODE: " (zip/node l'))
                           (parse-text rtext' l')))
                       ;; And if section is closing, check that name matches context
                       ;; If it doesn't than throw exception
                       (if (= (last context) name)
                         ;; if it name matches go to previous context
                         (let [section (->Section (clean (subs rtext 0 e)) [] context)
                               loc' (cond->
                                      (zip/append-child loc section)
                                      (not-empty context) zip/up)]
                           ; (println "ADDDING END SECTION " (zip/node loc'))
                           (parse-text rtext' loc'))
                         (throw
                           (ex-info
                             (str "Wrong enclosing section " name)
                             {:section name
                              :rtext rtext})))))))))]
     (let [[_ loc] (parse-text template section-zipper)]
       (zip/root loc)))))

(comment
  (time (extract-section-tokens template)))

;(defn parse
;  ([template]
;   ;; TODO - handle Nested properly by pairing right
;   ;; sections and not oversimplifying with str/index-of str/last-index-of
;   (letfn [(next-section-token [template]
;             (re-find
;               #"(?m)^\{\{[#^].*?\}\} *[\n\r]+|^ *\{\{[#^].*?\}\} *[\n\r]+|\{\{[#^].*?\}\}"
;               template))
;           (section-start-tokens [template]
;             (re-seq
;               #"(?m)^\{\{[#^].*?\}\} *[\n\r]+|^ *\{\{[#^].*?\}\} *[\n\r]+|\{\{[#^].*?\}\}"
;               template))
;           (section-end-tokens [template token-name]
;             (re-seq
;               (re-pattern
;                 (str
;                   "(?m)"
;                   ;; Handle starting section without spaces
;                   "^\\{\\{/\\s*" token-name "\\s*\\}\\}[\\n\\r]+"
;                   ;; Handle starting spaces with only section declaration
;                   "|" "\\^ *\\{\\{/\\s*" token-name "\\s*\\}\\} *[\\n\\r]+"
;                   ;; Handle inline section
;                   "|" "\\{\\{/\\s*" token-name "\\s*\\}\\}"
;                   ;; Handle end section
;                   "|" "\\n *\\{\\{/\\s*" token-name "\\s*\\}\\} *\\Z"))
;               template))
;           (section-token-name [token]
;             (str/trim (re-find #"(?<=\{\{[#^]).*?(?=\}\})" token)))
;           (end-section-token [template token-name]
;             (last (section-end-tokens template)))]
;     (if (string? template)
;       (parse (->Section template [] nil))
;       (loop [{:keys [cursor body] :as result} template
;              text (:body template)]
;         ;; If there is next section than isolate and parse that section
;         ;; and recur with text leftower
;         (if-let [start (next-section-token text)]
;           ;; Get section name
;           (let [name (section-token-name start)
;                 _ (println "TEXT: " [name text])
;                 end (end-section-token text name)
;                 start-idx (str/index-of text start)
;                 end-idx (str/last-index-of text end)
;                 _ (println "INDX: " [start-idx end-idx])
;                 template' (subs text (+ (count start) start-idx) end-idx)
;                 pre-section-text (subs text 0 start-idx)
;                 post-section-text (subs text (+ (count end) end-idx))
;                 inverted? (str/starts-with? start "{{^")
;                 cursor' ((fnil conj []) cursor (keyword name))]
;             ; (println
;             ;   "PRE\n" pre-section-text
;             ;   "\n@\n" template'
;             ;   "\nPOST\n" post-section-text)
;             (recur
;               (->
;                 result
;                 (assoc :body nil)
;                 (update :sections
;                         (fn [sections]
;                           (cond-> (or sections [])
;                             (not-empty pre-section-text)
;                             (conj (->Section pre-section-text nil cursor))
;                             ;;
;                             :sections
;                             (conj 
;                               (->
;                                 (->Section template' [] cursor')
;                                 parse
;                                 (assoc :inverted? inverted?)))))))
;               post-section-text))
;           (if (or (some? body) (empty? text)) result
;             (update result :sections (fnil conj []) (->Section text nil cursor)))))))))


(comment
  (def sections (parse template))
  (def data {:a {:one 1} :b {:two 2} :c {:three 3 :d {:four 4 :five 5}}})
  (def cursor [:a :b :c :d :five])
  (def data {:foo "bar"})
  (def cursor [:foo])
  (def cursor [:tops :middles :bottoms])
  (def template
    "|
{{#bool}}
* first
{{/bool}}
* {{two}}
{{#bool}}
* third
{{/bool}}")
  (parse template)
  (println (render template {:bool true :two "second"}))
  (def data 
    {:tops
     {:tname {:upper "A" :lower "a"}
      :middles {:mname "1"
                :bottoms [{:bname "x"} {:bname "y"}]}}})
  (println (str/replace (render template data) #"(?<=[\n\r]+)\s+\n" "")))


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


(defn render
  ([template] (render template nil))
  ([template data] (render template data nil))
  ([template data _]
   (let [sections (if (string? template) (parse template)
                    template)]
     (letfn [(print-section [{:keys [context body sections] :as section}]
               (let [context (last context)] 
                 (if body
                   (let [variables (distinct
                                     (map
                                       (fn [variable]
                                         ; (println "VARIABLE: " variable)
                                         (let [[f :as content] (subs variable 2 (- (count variable) 2))]
                                           (case f
                                             ">" {:escape? true
                                                  :name (subs content 1)
                                                  :partial? true}
                                             "!" {:escape? true
                                                  :comment (subs content 1)}
                                             "{" {:escape? true
                                                  :name (str/trim
                                                          (subs content 1 (dec (count content))))}
                                             "&" {:name (str/trim (subs content 1))}
                                             {:escape? true
                                              :name (str/trim content)})))
                                       (re-seq #"(?m)\{\{.*?\}\}" body)))]
                     (letfn [(replace-variable [body {:keys [name]}]
                               (if (nil? name) body
                                 (let [path (map keyword (str/split name #"\."))
                                       value (case name
                                               "." (get *context-stack* :.)
                                               (get-in *context-stack* path))]
                                   ; (println "NAME: " name)
                                   ; (println "path: " path)
                                   ; (println "VALUE: " value)
                                   (str
                                     (when value
                                       (str/replace
                                         body
                                         (re-pattern (str "\\{\\{[&!]*\\s*" name "\\s*\\}\\}"))
                                         (str value)))))))]
                       (println "CONTEXT: " context)
                       (println "STACK: " *context-stack*)
                       (println "VARIABLES: " variables)
                       (str
                         (if (empty? variables)
                           ; body 
                           (if context (when (:. *context-stack*) body) body) 
                           (when-let [context-value (if context
                                                      (get *context-stack* context)
                                                      *context-stack*)]
                             (if (vector? context-value)
                               (let [stack (recompute-stack context)]
                                 (reduce
                                   (fn [body' value]
                                     (binding [*context-stack* (assoc stack context value :. value)]
                                       (str body' (print-section section))))
                                   ""
                                   context-value))
                               (binding [*context-stack* (recompute-stack context)]
                                 (reduce replace-variable body variables))))))))
                   (binding [*context-stack* (recompute-stack context)]
                     (reduce str (map print-section sections))))))]
       (binding [*context-stack* data]
         (print-section sections))))))

(comment
  (re-find #"^\s+\n+" text)
  (parse "{{#list}}{{item}}{{/list}}"))

(deftest section
  (testing "Truthy sections should have their contents rendered."
    (is
      (= "This should be rendered."
         (render
           "{{#boolean}}This should be rendered.{{/boolean}}"
           {:boolean true}))))
  (testing "Truthy sections should have their contents rendered."
    (is
      (= "This should be rendered."
         (render
           "{{#boolean}}This should not be rendered.{{/boolean}}"
           {:boolean false}))))
  (testing "Null is falsey."
    (is
      (= "This should be rendered."
         (render
           "{{#null}}This should not be rendered.{{/null}}"
           {:null nil}))))
  (testing "Objects and hashes should be pushed onto the context stack."
    (is
      (= "Hi Joe."
         (render
           "{{#context}}Hi {{name}}.{{/context}}"
           {:context {:name "Joe"}}))))
  (testing "Non-false sections have their value at the top of context,
           accessible as {{.}} or through the parent context. This gives
           a simple way to display content conditionally if a variable exists."
    (is
      (= "bar is bar"
         (render
           "{{#foo}}{{.}} is {{foo}}{{/foo}}"
           {:foo "bar"}))))
  (testing "All elements on the context stack should be accessible within lists."
    (is
      (= "a1.A1x.A1y."
         (render
           "{{#tops}}{{#middles}}{{tname.lower}}{{mname}}.{{#bottoms}}{{tname.upper}}{{mname}}{{bname}}.{{/bottoms}}{{/middles}}{{/tops}}"
           {:tops
            {:tname {:upper "A" :lower "a"}
             :middles {:mname "1"
                       :bottoms [{:bname "x"} {:bname "y"}]}}}))))
  (testing "Deeply Nested Contexts"
    (is
      (= "|
1
121
12321
1234321
123454321
12345654321
123454321
1234321
12321
121
1
"
         (render
           "|
{{#a}}
{{one}}
{{#b}}
{{one}}{{two}}{{one}}
{{#c}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{#d}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{#five}}
{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
{{one}}{{two}}{{three}}{{four}}{{.}}6{{.}}{{four}}{{three}}{{two}}{{one}}
{{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
{{/five}}
{{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
{{/d}}
{{one}}{{two}}{{three}}{{two}}{{one}}
{{/c}}
{{one}}{{two}}{{one}}
{{/b}}
{{one}}
{{/a}}"
           {:a {:one 1}
            :b {:two 2}
            :c {:three 3 :d {:four 4 :five 5}}}))))
  ;;
  (testing "Lists should be iterated; list items should visit the context stack."
    (is
      (= 
        "123"
        (render
          "{{#list}}{{item}}{{/list}}"
          {:list [{:item 1} {:item 2} {:item 3}]}))))
  (testing "Empty lists should behave like falsey values."
    (is
      (=
       ""
       (render
         "{{#list}}Yay lists!{{/list}}"
         {:list []}))))
  (testing "Multiple sections per template should be permitted."
    (is
      (=
"|
* first
* second
* third")
      (render
"|
{{#bool}}
* first
{{/bool}}
* {{two}}
{{#bool}}
* third
{{/bool}}"
        {:bool true :two "second"})))
  (testing "Nested truthy sections should have their contents rendered."
    (is
      (=
       "| A B C D E |"
       (render
         "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
         {:bool true}))))
  (testing "Nested falsey sections should be omitted."
    (is
      (=
       "| A  E |"
       (render
         "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
         {:bool false}))))
  (testing "Parent contexts"
    (is
      (= "foo, bar, baz"
         (render
           "{{#sec}}{{a}}, {{b}}, {{c.d}}{{/sec}}"
           {:a "foo" :b "wrong" :sec {:b "bar"} :c {:d "baz"}}))))
  (testing "Lists should be iterated; list items should visit the context stack."
    (is (= "123" (render "{{#list}}{{item}}{{/list}}" {:list [{:item 1} {:item 2} {:item 3}]})))))
