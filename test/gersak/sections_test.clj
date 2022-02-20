(ns gersak.sections_test
  (:require
    [gersak.mustachos :refer [render parse]]
    [clojure.test :refer [deftest is testing]]))


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
