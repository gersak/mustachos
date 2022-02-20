(ns gersak.interpolation_test
  (:require
    [gersak.mustachos :refer [render parse]]
    [clojure.test :refer [deftest is testing]]))


(deftest interpolation
  (testing "Interpolation tags are used to integrate dynamic content into the template.
  The tag's content MUST be a non-whitespace character sequence NOT containing
  the current closing delimiter.
  This tag's content names the data to replace the tag.  A single period (`.`)
  indicates that the item currently sitting atop the context stack should be
  used; otherwise, name resolution is as follows:
    1) Split the name on periods; the first part is the name to resolve, any
    remaining parts should be retained.
    2) Walk the context stack from top to bottom, finding the first context
    that is a) a hash containing the name as a key OR b) an object responding
    to a method with the given name.
    3) If the context is a hash, the data is the value associated with the
    name.
    4) If the context is an object, the data is the value returned by the
    method with the given name.
    5) If any name parts were retained in step 1, each should be resolved
    against a context stack containing only the result from the former
    resolution.  If any part fails resolution, the result should be considered
    falsey, and should interpolate as the empty string.
  Data should be coerced into a string (and escaped, if appropriate) before
  interpolation.
  The Interpolation tags MUST NOT be treated as standalone."
    (is
      (= "Hello from {Mustache}!" (render "Hello from {Mustache}!"))
      "Mustache-free templates should render as-is.")
    (is
      (= "Hello, world!" (render "Hello, {{subject}}!" {:subject "world"}))
      "Unadorned tags should interpolate content into the template.")
    (is
      (=
       "These characters should be HTML escaped: &amp; &quot; &lt; &gt;"
       (render "These characters should be HTML escaped: {{forbidden}}"
               {:forbidden "& \" <  >"}))
      "Basic interpolation should be HTML escaped.")
    (is
      (=
       "These characters should not be HTML escaped: & \" < >"
       (render "These characters should not be HTML escaped: {{{forbidden}}}"
               {:forbidden "& \" < >"}))
      "Triple mustaches should interpolate without HTML escaping.")
    (is
      (=
       "These characters should not be HTML escaped: & \" < >"
       (render "These characters should not be HTML escaped: {{&forbidden}}"
               {:forbidden "& \" < >"}))
      "Ampersand should interpolate without HTML escaping.")
    (is
      (=
       "85 miles an hour!"
       (render "{{mph}} miles an hour!"
               {:mph 85}))
      "Integers should interpolate seamlessly.")
    (is
      (=
       "\"85 miles an hour!\""
       (render "\"{{{mph}}} miles an hour!\"" {:mph 85}))
      "Integers should interpolate seamlessly.")
    (is
      (=
       "\"1.21 jiggawatts!\""
       (render "\"{{power}} jiggawatts!\"" {:power 1.210}))
      "Decimals should interpolate seamlessly with proper significance.")
    (is
      (=
       "\"1.21 jiggawatts!\""
       (render "\"{{{power}}} jiggawatts!\"" {:power 1.210}))
      "Decimals should interpolate seamlessly with proper significance.")
    (is
      (=
       "\"1.21 jiggawatts!\""
       (render "\"{{&power}} jiggawatts!\"" {:power 1.210}))
      "Decimals should interpolate seamlessly with proper significance.")
    (is
      (=
       "\"I () be seen!\""
       (render "\"I ({{cannot}}) be seen!\"" {:cannot nil}))
      "Nulls should interpolate as the empty string.")
    (is
      (=
       "\"I () be seen!\""
       (render
         "\"I ({{{cannot}}}) be seen!\""
         {:cannot nil}))
      "Nulls should interpolate as the empty string.")
    (is
      (=
       "I () be seen!"
       (render
         "I ({{&cannot}}) be seen!"
         {:cannot nil}))
      "Nulls should interpolate as the empty string."))
  ;;
  (testing "Context Misses"
    (is
      (=
       "I () be seen!"
       (render "I ({{cannot}}) be seen!" {}))
      "Failed context lookups should default to empty strings.")
    (is
      (=
       "I () be seen!"
       (render "I ({{&cannot}}) be seen!" {}))
      "Failed context lookups should default to empty strings.")
    (is
      (=
       "I () be seen!"
       (render "I ({{&cannot}}) be seen!" {}))
      "Failed context lookups should default to empty strings."))
  ;;
  (testing "Dotted names"
    (is
      (=
       "\"Joe\" == \"Joe\""
       (render "\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\"" {:person {:name "Joe"}}))
      "Dotted names should be considered a form of shorthand for sections.")
    (is
      (=
       "\"Joe\" == \"Joe\""
       (render "\"{{{person.name}}}\" == \"{{#person}}{{{name}}}{{/person}}\""
               {:person {:name "Joe"}}))
      "Dotted names should be considered a form of shorthand for sections.")
    (is
      (=
       "\"Joe\" == \"Joe\""
       (render
         "\"{{&person.name}}\" == \"{{#person}}{{&name}}{{/person}}\""
         {:person {:name "Joe"}}))
      "Dotted names should be considered a form of shorthand for sections.")
    (is
      (=
       "\"Phil\" == \"Phil\""
       (render
         "\"{{a.b.c.d.e.name}}\" == \"Phil\""
         {:a {:b {:c {:d {:e {:name "Phil"}}}}}}))
      "Dotted names should be functional to any level of nesting.")
    (is
      (=
       "\"\" == \"\""
       (render "\"{{a.b.c}}\" == \"\"" {}))
      "Any falsey value prior to the last part of the name should yield ''.")
    (is
      (=
       "\"\" == \"\""
       (render "\"{{a.b.c.name}}\" == \"\"" {:a {:b {}} :c {:name "Jim"}}))
       "Each part of a dotted name should resolve only against its parent.")
    (is
      (=
       "\"Phil\" == \"Phil\""
       (render "\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\""
               {:a {:b {:c {:d {:e {:name "Phil"}}}}}
                :b {:c {:d {:e {:name "Wrong"}}}}}))
      "The first part of a dotted name should resolve as any other name.")
    (is
      (=
       ""
       (render "{{#a}}{{b.c}}{{/a}}" {:a {:b {}} :b {:c "ERROR"}}))
      "Dotted names should be resolved against former resolutions."))
    ;;
    (testing "Implicit iterators"
      (is
        (=
         "Hello, world!"
         (render "Hello, {{.}}!" "world"))
        "Unadorned tags should interpolate content into the template.")
      (is
        (=
         "These characters should be HTML escaped: &amp; &quot; &lt; &gt;"
         (render
           "These characters should be HTML escaped: {{.}}"
           "& \" < >"))
        "Basic interpolation should be HTML escaped.")
      (is
        (=
         "These characters should not be HTML escaped: & \" < >"
         (render
           "These characters should not be HTML escaped: {{{.}}}"
           "& \" < >"))
        "Triple mustaches should interpolate without HTML escaping.")
      (is
        (=
         "These characters should not be HTML escaped: & \" < >"
         (render "These characters should not be HTML escaped: {{&.}}" "& \" < >"))
        "Ampersand should interpolate without HTML escaping.")
      (is
        (=
         "\"85 miles an hour!\""
         (render "\"{{.}} miles an hour!\"" 85))
        "Integers should interpolate seamlessly."))
    ;;
    (testing "Whitespace sensitivity"
      (is
        (=
         "| --- |"
         (render "| {{string}} |" {:string "---"}))
        "Interpolation should not alter surrounding whitespace.")
      (is
        (=
         "| --- |"
         (render "| {{{string}}} |" {:string "---"}))
        "Interpolation should not alter surrounding whitespace.")
      (is
        (=
         "| --- |"
         (render "| {{&string}} |" {:string "---"}))
        "Interpolation should not alter surrounding whitespace.")
      (is
        (=
         "  ---\n"
         (render "  {{string}}\n" {:string "---"}))
        "Standalone interpolation should not alter surrounding whitespace.")
      (is
        (=
         "  ---\n"
         (render "  {{{string}}}\n" {:string "---"}))
        "Standalone interpolation should not alter surrounding whitespace.")
      (is
        (=
         "  ---\n"
         (render "  {{&string}}\n" {:string "---"}))
        "Standalone interpolation should not alter surrounding whitespace."))
    ;;
    (testing "Whitespace Insensitivity"
      (is
        (= "|---|"
           (render "|{{ string }}|" {:string "---"}))
        "Superfluous in-tag whitespace should be ignored.")
      (is
        (= "|---|" (render "|{{{ string }}}|" {:string "---"}))
        "Superfluous in-tag whitespace should be ignored.")
      (is
        (= "|---|" (render "|{{& string }}|" {:string "---"}))
        "Superfluous in-tag whitespace should be ignored.")))
