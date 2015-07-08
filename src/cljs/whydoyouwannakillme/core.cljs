(ns whydoyouwannakillme.core
  (:require [rum :as rum]
            [secretary.core :as secretary :refer-macros [defroute]]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [whydoyouwannakillme.html2ts :refer [html2ts]]
            [whydoyouwannakillme.about :refer [about]])
  (:import goog.History))

(enable-console-print!)

(defonce app-state
  (atom {:modules [{:key :html2ts :title "HTML to TypeScript React"}]
         :current-module :html2ts}))

;; Routes

(secretary/set-config! :prefix "#")

(defroute module-path "/:module" {module :module}
  (let [module-keys (set (map :key (:modules @app-state)))
        module-key (or (module-keys (keyword module)) :html2ts)]
    (swap! app-state
           assoc :current-module module-key)))

(defonce route-history
  (let [h (History.)]
    (goog.events/listen h EventType/NAVIGATE #(secretary/dispatch! (.-token %)))
    (doto h (.setEnabled true))))

;; Sidebar

(rum/defc sidebar-item < rum/static [module current?]
  [:a {:key (:key module)
       :href (module-path {:module (name (:key module))})
       :class (when current? "selected")}
   (:title module)])

(rum/defc sidebar < rum/static [modules current-module]
  [:div.sidebar
   [:div.sidebar__header "Modules"]
   [:nav
    (for [module modules
          :let [current? (= (:key module) current-module)]]
      (sidebar-item module current?))]
   [:div.sidebar__footer
    [:div.sidebar__footer__contact
     [:a {:href "https://github.com/nidu/whydoyouwannakillme"} "nidu@github"]]]])

;; Container

(def module-map
  {:html2ts html2ts})

(rum/defc content [current-module]
  (let [module-comp (current-module module-map)]
    [:div.content
     (module-comp)]))

(rum/defc app < rum/reactive []
  (let [{:keys [modules current-module]} (rum/react app-state)]
    [:div
     (sidebar modules current-module)
     (content current-module)]
    ))

(defn main []
  (rum/mount
    (app)
    (. js/document (getElementById "app")))
  )