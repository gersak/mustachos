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
  (pr-str template)
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


(comment
  (re-find)
  (def template " | {{#boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n")
  (def template
    "|
| This Is
{{#boolean}}
|
{{/boolean}}
| A Line")
  (str/replace template #"(\n)(\s*)(\{\{[/#].*?\}\})(\n)" "$1$3")
  (def t " {{#boolean}}YES{{/boolean}}\n {{#boolean}}GOOD{{/boolean}}\n")
  (def t "  {{#boolean}}\n#{{/boolean}}\n/")
  (def template "#{{#boolean}}\n/\n  {{/boolean}}")
  (def template "|\r\n{{#boolean}}\r\n{{/boolean}}\r\n|")
  (pr-str template)
  (pr-str (slurp "deps.edn"))
  (re-find #"\n *(\{\{[/#][\w\d]* *\}\}) *\n" t))

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
   (letfn [(clean [text]
             (str/replace text #"^\n" ""))
           (parse-text [rtext loc]
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
                     ; (println "MS: " [s e])
                     ; (println "NAME: " name)
                     ; (println "Context: " context)
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
                           ; (println "NODE: " (zip/node l'))
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
                                         ; (println "CONTENT: " f content)
                                         (case f
                                           \> {:escape? true
                                               :name (subs content 1)
                                               :partial? true}
                                           \{ {:escape? true
                                                :name (str/trim
                                                        (subs content 1 (dec (count content))))}
                                           \& {:name (str/trim (subs content 1))}
                                           {:escape? true
                                            :name (str/trim content)})))
                                     (re-seq #"(?m)\{\{.*?\}\}" body))))] 
                 (letfn [(replace-variable [body {:keys [name] :as v}]
                           (cond
                             ; (:comment v)
                             ; (str/replace body (re-pattern (str "(?m)" (:comment v))) "")
                             ;;
                             (some? name)
                             (let [path (map keyword (str/split name #"\."))
                                   value (case name
                                           "." (get *context-stack* :.)
                                           (get-in *context-stack* path))]
                               (str
                                 (when value
                                   (str/replace
                                     body
                                     (re-pattern (str "\\{\\{[&!]*\\s*" name "\\s*\\}\\}"))
                                     (str value)))))
                             :else body))]
                   (str
                     (if-let [context-value (get-context-value context)]
                       (cond
                         ;; If current content is list
                         (and context (multiply? context-value))
                         (let [stack (recompute-stack context)]
                           ; (println "Expanding list value: " context-value)
                           (reduce
                             (fn [body' value]
                               (binding [*context-stack* (assoc stack context value :. value)]
                                 ;; TODO - Fix this... It is not working for (123)(abc))
                                 #_(reduce str body' (map print-section sections))
                                 (str body' (print-section section))))
                             ""
                             context-value))
                         ;; Otherwise check if there is body
                         ;; and variables are empty than return body
                         (and body (empty? variables))
                         (do
                           ; (println "Returning raw body: " body)
                           body)
                         ;; If there is some body and variables aren't empty
                         ;; try to replace those variables
                         (some? body)
                         (binding [*context-stack* (recompute-stack context)]
                           ; (println "VARIABLES: " variables)
                           ; (println "Replacing variables" body)
                           (reduce replace-variable body variables))
                         ;; If there is no body try to print all sections
                         (nil? body)
                         (binding [*context-stack* (recompute-stack context)]
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
                       (when (nil? context) body))))))]
       (binding [*context-stack* data]
         (print-section template))))))

(comment
  (re-find #"^\s+\n+" text)
  (clojure.test/run-tests 'gersak.mustachos)
  (parse "{{#list}}{{item}}{{/list}}"))

(deftest section
  (testing "Truthy sections should have their contents rendered."
    (is
      (= "This should be rendered."
         (render
           "{{#boolean}}This should be rendered.{{/boolean}}"
           {:boolean true}))))
  (testing "Falsey sections should have their contents omitted."
    (is
      (= ""
         (render
           (parse "{{#boolean}}This should not be rendered.{{/boolean}}")
           {:boolean false}))))
  (testing "Null is falsey."
    (is
      (= ""
         (render
           "{{#null}}This should not be rendered.{{/null}}"
           {:null nil}))))
  (testing "Parent contexts"
    (is
      (= "foo, bar, baz"
         (render
           "{{#sec}}{{a}}, {{b}}, {{c.d}}{{/sec}}"
           {:a "foo" :b "wrong" :sec {:b "bar"} :c {:d "baz"}}))))
  (testing "Lists should be iterated; list items should visit the context stack."
    (is (= "123" (render "{{#list}}{{item}}{{/list}}" {:list [{:item 1} {:item 2} {:item 3}]}))))
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
1"
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
* third"
      (render
"|
{{#bool}}
* first
{{/bool}}
* {{two}}
{{#bool}}
* third
{{/bool}}"
        {:bool true :two "second"}))))
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
  (testing "Failed context lookups should be considered falsey."
    (is
      (=
       "[]"
       (render "[{{#missing}}Found key 'missing'!{{/missing}}]" {}))))
  ;;
  (testing "Implict iterators"
    (is
      (=
       "(a)(b)(c)(d)(e)"
       (render "{{#list}}({{.}}){{/list}}" {:list ["a" "b" "c" "d" "e"]}))
      "Implicit iterators should directly interpolate strings.")
    (is
      (= "(1)(2)(3)(4)(5)"
         (render "{{#list}}({{.}}){{/list}}" {:list [1 2 3 4 5]}))
      "Implicit Iterator - Integer")
    (is
      (= "(1.1)(2.2)(3.3)(4.4)(5.5)"
         (render "{{#list}}({{.}}){{/list}}" {:list [1.10, 2.20, 3.30, 4.40, 5.50]}))
      "Implicit iterators should cast decimals to strings and interpolate.")
    ;; FIXME
    (is
      (= "(123)(abc)"
         (render
           (parse "{{#list}}({{#.}}{{.}}{{/.}}){{/list}}")
           {:list [[1 2 3] ["a" "b" "c"]]}))
      "Implicit iterators should allow iterating over nested arrays."))
  ;;
  (testing "Dotted names"
    (is
      (=
       "\"Here\" == \"Here\""
       (render (parse "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\"") {:a {:b {:c true}}}))
      "Dotted names should be valid for Section tags.")
    (is
      (=
       "\"\" == \"\""
       (render
         "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
         {:a {:b {:c false}}}))
      "Dotted names should be valid for Section tags.")
    (is
      (=
       "\"\" == \"\""
       (render "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\"" {:a {}}))
      "Dotted names that cannot be resolved should be considered falsey."))

  ;;

  (testing "Whitespace sensitivity"
    (is
      (=
       " | \t|\t | \n"
       (render " | {{#boolean}}\t|\t{{/boolean}} | \n" {:boolean true}))
      "Sections should not alter surrounding whitespace.")
    (is
      (= " |  \n  | \n"
         (render
           (parse " | {{#boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n")
           {:boolean true}))
      "Sections should not alter internal whitespace.")
    (is
      (=
       " YES\n GOOD\n"
       (render
         " {{#boolean}}YES{{/boolean}}\n {{#boolean}}GOOD{{/boolean}}\n"
         {:boolean true}))
      "Single-line sections should not alter surrounding whitespace.")
    ;;
    (is
      (=
       "|
| This Is
|
| A Line"
       (render
         "|
| This Is
{{#boolean}}
|
{{/boolean}}
| A Line"
         {:boolean true}))
      "Standalone lines should be removed from the template.")
    ;;
    (is
      (=
       "|
| This Is
|
| A Line"
       (render
         "|
| This Is
  {{#boolean}}
|
  {{/boolean}}
| A Line"
         {:boolean true}))
      "Indented standalone lines should be removed from the template.")
    ;;
    (is
      (=
       "|\r\n|"
       (render (parse "|\r\n{{#boolean}}\r\n{{/boolean}}\r\n|") {:boolean true}))
      "\"\r\n\" should be considered a newline for standalone tags.")
    ;;
    (is
      (=
       (render
         "  {{#boolean}}\n#{{/boolean}}\n/"
         {:boolean true})
       "#\n/")
      "Standalone tags should not require a newline to precede them.")
    ;; TODO - This test isn't working fix this
    ;; Problem is.... How to differentiate inline template \n from usual
    ;; standalone section in sepparate row
    ; (is
    ;   (=
    ;    (render
    ;      (parse "#{{#boolean}}\n/\n  {{/boolean}}")
    ;      {:boolean true})
    ;    "#\n/\n")
    ;   "Standalone tags should not require a newline to follow them.")
    ;;
    (is
      (=
       (render "|{{# boolean }}={{/ boolean }}|" {:boolean true})
       "|=|")
      "Superfluous in-tag whitespace should be ignored."))
  
  )
