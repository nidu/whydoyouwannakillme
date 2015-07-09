(ns whydoyouwannakillme.html-conv
  (:require [hickory.core :refer [as-hiccup parse-fragment]]
            [camel-snake-kebab.core :refer [->camelCase]]
            [clojure.string :as str]
            [clojure.set :as set]
            [goog.string :as gstring]))

(def react-tags
  ["a" "abbr" "address" "area" "article" "aside" "audio" "b" "base" "bdi" "bdo" "big" "blockquote" "body" "br"
   "button" "canvas" "caption" "cite" "code" "col" "colgroup" "data" "datalist" "dd" "del" "details" "dfn"
   "dialog" "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5"
   "h6" "head" "header" "hr" "html" "i" "iframe" "img" "input" "ins" "kbd" "keygen" "label" "legend" "li" "link"
   "main" "map" "mark" "menu" "menuitem" "meta" "meter" "nav" "noscript" "object" "ol" "optgroup" "option"
   "output" "p" "param" "picture" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "section" "select"
   "small" "source" "span" "strong" "style" "sub" "summary" "sup" "table" "tbody" "td" "textarea" "tfoot" "th"
   "thead" "time" "title" "tr" "track" "u" "ul" "var" "video" "wbr" "circle" "clipPath" "defs" "ellipse" "g" "line" "linearGradient" "mask" "path" "pattern" "polygon" "polyline"
   "radialGradient" "rect" "stop" "svg" "text" "tspan"])

(def react-tags-lc
  (->> react-tags
       (map #(vector (str/lower-case %) %))
       (into {})))

(def react-attrs
  ["accept" "acceptCharset" "accessKey" "action" "allowFullScreen" "allowTransparency" "alt"
   "async" "autoComplete" "autoFocus" "autoPlay" "cellPadding" "cellSpacing" "charSet" "checked"
   "classID" "className" "colSpan" "cols" "content" "contentEditable" "contextMenu" "controls"
   "coords" "crossOrigin" "data" "dateTime" "defer" "dir" "disabled" "download" "draggable" "encType"
   "form" "formAction" "formEncType" "formMethod" "formNoValidate" "formTarget" "frameBorder"
   "headers" "height" "hidden" "high" "href" "hrefLang" "htmlFor" "httpEquiv" "icon" "id" "label" "lang"
   "list" "loop" "low" "manifest" "marginHeight" "marginWidth" "max" "maxLength" "media" "mediaGroup"
   "method" "min" "multiple" "muted" "name" "noValidate" "open" "optimum" "pattern" "placeholder"
   "poster" "preload" "radioGroup" "readOnly" "rel" "required" "role" "rowSpan" "rows" "sandbox" "scope"
   "scoped" "scrolling" "seamless" "selected" "shape" "size" "sizes" "span" "spellCheck" "src" "srcDoc"
   "srcSet" "start" "step" "style" "tabIndex" "target" "title" "type" "useMap" "value" "width" "wmode" "clipPath" "cx" "cy" "d" "dx" "dy" "fill" "fillOpacity" "fontFamily" "fontSize" "fx" "fy"
   "gradientTransform" "gradientUnits" "markerEnd" "markerMid" "markerStart" "offset" "opacity"
   "patternContentUnits" "patternUnits" "points" "preserveAspectRatio" "r" "rx" "ry"
   "spreadMethod" "stopColor" "stopOpacity" "stroke" "strokeDasharray" "strokeLinecap"
   "strokeOpacity" "strokeWidth" "textAnchor" "transform" "version" "viewBox" "x1" "x2" "x" "y1" "y2" "y"])

(def react-attrs-lc
  (->> react-attrs
       (map #(vector (str/lower-case %) %))
       (into {})))

(defn escape-quotes [s q]
  (str/replace s (if (= q "'") #"'" #"\"") (str "\\" q)))

(defn unescape-html [s]
  (gstring/unescapeEntities s))

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

(defmethod attr-to-str :style [[_ value] opts]
  (str "style: " (->> value
                      parse-style-str
                      (attrs-to-str opts)
                      wrap-attr-str)))

(defmethod attr-to-str :class [[_ value] opts]
  (attr-to-str [:className value] opts))

(defmethod attr-to-str :default [[attr-kw value] {:keys [quote-str] :as opts}]
  (let [attr-name (name attr-kw)
        react-name (or (get react-attrs-lc (str/lower-case attr-name))
                       attr-name)]
    (str react-name ": " quote-str (escape-quotes value quote-str) quote-str)))

(defn attrs-to-str [opts attrs]
  (->> attrs
       (map #(attr-to-str % opts))
       (str/join ", ")))

(defn repeat-str [s n]
  (.repeat s n))

(defn skip-el? [el]
  (and (string? el)
       (-> el str/trim empty?)))

(defmulti conv-el string?)

(defmethod conv-el true [el offset {:keys [prefix indent quote-str] :as opts}]
  (let [trimmed (-> el
                    (.replace str #"^\s+(.+)$" " $1")
                    (.replace str #"^(.+)\s+$" "$1 ")
                    (unescape-html)
                    (escape-quotes quote-str))]
    (str (repeat-str " " offset) quote-str trimmed quote-str)))

(defmethod conv-el false
  [[tag-kw attrs & children] offset {:keys [prefix indent quote-str] :as opts}]
  (let [attr-str  (str "(" (->> attrs (attrs-to-str opts) wrap-attr-str))
        child-els (->> children
                       (remove skip-el?)
                       (map
                         #(conv-el % (+ offset indent) opts)))
        tag (name tag-kw)
        react-tag (or (get react-tags-lc (str/lower-case tag))
                      tag)]
    (str 
      (repeat-str " " offset)
      prefix react-tag
      (if-not (empty? children)
        (str
          attr-str ",\n"
          (str/join ",\n" child-els)
          (repeat-str " " offset))
        (str attr-str))
      "\n" (repeat-str " " offset) ")")))

(defn get-used-tags [html]
  (when-not (string? html)
    (let [[tag _ & children] html]
      (conj
        (->> children
             (map get-used-tags)
             (apply set/union)
             (set))
        tag))))

(defn html->ts [{:keys [html] :as opts}]
  (let [html-el (first (map as-hiccup (parse-fragment html)))]
    (if (coll? html-el)
      {:tags (get-used-tags html-el)
       :result (conv-el html-el 0 opts)}
      {})))