(ns whydoyouwannakillme.html-conv
  (:require [hickory.core :refer [as-hiccup parse-fragment]]
            [camel-snake-kebab.core :refer [->camelCase]]
            [clojure.string :as str]
            [clojure.set :as set]))

(declare attrs-to-str)

(defn parse-style-str [style]
  (->> (str/split style #";")
       (map #(str/split % #":"))))

(defn wrap-attr-str [attr-str]
  (if (empty? attr-str)
    "{}"
    (str "{ " attr-str " }")))

(defmulti conv-attr first)

(defmethod conv-attr :default [[attr-name value]]
  [(-> attr-name name ->camelCase) value])

(defn conv-attrs [attrs]
  (map conv-attr attrs))

(defmulti attr-to-str first)

(defmethod attr-to-str :style [[_ value]]
  (str "style: " (-> value parse-style-str attrs-to-str wrap-attr-str)))

(defmethod attr-to-str :class [[_ value]]
  (attr-to-str [:className value]))

(defmethod attr-to-str :default [[attr-name value]]
  (str (-> attr-name name ->camelCase) ": '" value "'"))

(defn attrs-to-str [attrs]
  (->> attrs
       (map attr-to-str)
       (str/join ", ")))

(defn repeat-str [s n]
  (.repeat s n))

(defn skip-el? [el]
  (and (string? el)
       (-> el str/trim empty?)))

(defmulti conv-el string?)

(defmethod conv-el true [el prefix offset]
  (let [trimmed (-> el
                    (.replace str #"^\s+(.+)$" " $1")
                    (.replace str #"^(.+)\s+$" "$1 "))]
    (str (repeat-str " " offset) "'" trimmed "'")))

(defmethod conv-el false [[tag attrs & children] prefix offset indent]
  (let [attr-str  (str "(" (-> attrs attrs-to-str wrap-attr-str))
        child-els (->> children
                       (remove skip-el?)
                       (map 
                         #(conv-el % prefix (+ offset indent) indent)))]
    (str 
      (repeat-str " " offset)
      prefix (name tag)
      (if (-> children empty? not)
        (str
          attr-str ",\n"
          (str/join ",\n" child-els)
          (repeat-str " " offset))
        (str attr-str))
      ")\n")))

(defn get-used-tags [html]
  (when-not (string? html)
    (let [[tag _ & children] html]
      (conj
        (->> children
             (map get-used-tags)
             (apply set/union)
             (set))
        tag))))

(defn html->ts [{:keys [html prefix indent]}]
  (let [html-el (first (map as-hiccup (parse-fragment html)))]
    (if (coll? html-el)
      {:tags (get-used-tags html-el)
       :result (conv-el html-el prefix 0 indent)}
      {})))