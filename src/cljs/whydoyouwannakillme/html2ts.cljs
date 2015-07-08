(ns whydoyouwannakillme.html2ts
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require [rum :as rum]
            [cljs.core.async :as a :refer [put! chan <!]]
            [clojure.string :as str]
            [whydoyouwannakillme.html-conv :as html-conv :refer [html->ts]]))

(defonce state
  (atom {:opts {:full-names false
                :indent 2
                :html ""}}))

(defn map-chan [from to f]
  (go-loop []
    (let [msg (<! from)]
      (put! to (f msg)))))

(defmulti handle-message
  (fn [msg] (:type msg)))

(defmethod handle-message :set-option
  [{:keys [option value] :as msg}]
  (swap! state assoc-in [:opts option] value))

(defn make-declaration [tags]
  (if (empty? tags)
    ""
    (str "var {"
         (str/join ", " (map name tags))
         "} = React.DOM;\n")))

(defn gen-ts [opts]
  (let [{:keys [html full-names indent]} opts
        prefix (if full-names "React.DOM." "")
        {:keys [tags result]} (html->ts {:html (:html opts)
                                         :prefix prefix
                                         :indent indent})]
    (str
      (when-not full-names
        (str
          (make-declaration tags)
          \newline))
      result)))

(rum/defc opts-editor [opts ch]
  (let [set-opt
        (fn [opt value]
          (put! ch {:type :set-option
                    :option opt
                    :value value}))]
    [:form.pure-form.pure-form-stacked.html2ts__opts
     [:fieldset
      [:legend "Convet HTML to TypeScript React code"]
      [:div.pure-g
       [:div.pure-u-1-1
        [:label {:for "indent"} "Indent"]
        [:input {:type "text"
                 :name "indent"
                 :value (str (:indent opts))
                 :on-change
                 (fn [e]
                   (let [value (.. e -target -value)
                         number (js/parseInt value)]
                     (when-not (or (js/isNaN number) (neg? number))
                       (set-opt :indent number))))}]]
       [:div.pure-u-1-2
        [:label.pure-checkbox {:for "full-names"}
         [:input {:type "checkbox"
                  :name name
                  :checked (:full-names opts)
                  :on-change #(set-opt :full-names
                                       (-> opts :full-names not))}
          " Full name"]]]
       [:div.pure-u-1-1
        [:label {:for "html"} "HTML"]
        [:textarea.html2ts__textarea
         {:name "html"
          :rows 10
          :value (:html opts)
          :on-change #(set-opt :html
                               (.. % -target -value))}]]
       [:div.pure-u-1-1
        [:label {:for "ts"} "TypeScript"]
        [:textarea.html2ts__textarea
         {:name "ts"
          :rows 10
          :value (gen-ts opts)
          :readOnly true}]]]]]))

(rum/defc html2ts < rum/reactive []
  (let [{:keys [opts html ts]} (rum/react state)
        ch (max (chan))]
    (go-loop []
      (let [msg (<! ch)]
        (handle-message msg)
        (recur)))
    [:div.html2ts
     (opts-editor opts ch)]))