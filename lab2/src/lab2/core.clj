(ns lab2.core
  (:gen-class)
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  (:require [clj-http.client :as client])
  (:use [slingshot.slingshot :only [try+]])
  (:refer-clojure :exclude [resolve])
  (:use clojurewerkz.urly.core)
  (:import [java.net URI URL])
  (:require [clojure.tools.cli :refer [cli]]))

(defn read_file
  [file-name]
  (with-open [rdr (io/reader file-name)]
    (doall (line-seq rdr))))

(defn create_node
  [url status depth children urls location]
  {:url url :status status :depth depth :children children :urls urls :location location})

(defn remove_nils
  [col]
  (filter #(not (nil? %)) col))

(defn ?redirect
  [status]
  (if (some #(= status %) '(301 302 303 305 307))
    true
    false))

(defn resolve_relative_links
  [url relative-links]
  (map #(resolve url %) relative-links))

(defn get_relative_links
  [url links]
  (resolve_relative_links url (filter #(and (= (.indexOf % "/") 0) (= (.indexOf % "#") -1)) links)))

(defn get_absolute_links
  [links]
  (filter #(or (= (.indexOf % "http://") 0) (= (.indexOf % "https://") 0)) links))

(defn get_valid_links
  [url links]
  (let [valid-links (remove_nils links)
        relative-links (get_relative_links url valid-links)
        absolute-links (get_absolute_links valid-links)]
    (concat relative-links absolute-links)))

(defn get_links
  [html]
  (map :href (map :attrs (html/select html #{[:a]}))))

(defn parse_content
  [url response]
  (let [content-type ((response :headers) :content-type)]
    (if (re-find #"text/html" content-type)
      (get_valid_links url (get_links (html/html-snippet (response :body))))
      '())))

(defn fetch_page
  [url]
  (try+
   (client/get url)
   (catch Object _ {:status 404})))

(defn parse_page
  [url depth node]
  (let [html (fetch_page url)
        status (html :status)
        new-node (if (not= status 404)
                   (if (?redirect status)
                     (create_node url status depth (atom '()) (parse_content url html) (:location (html :headers)))
                     (create_node url status depth (atom '()) (parse_content url html) nil))
                   (create_node url status depth (atom '()) '() nil))]
    (swap! (:children node) conj new-node)
    new-node))

(defn visit_links
  [urls depth node]
  (let [new-depth (dec depth)]
    (pmap #(parse_page % new-depth node) urls)))

(defn crawling_loop
  [node urls depth]
  (if (> depth 0)
    (let [new-depth (dec depth)]
      (doseq [new-node (visit_links urls depth node)]
        (crawling_loop new-node (:urls new-node) new-depth))))
    node)

(defn crawling
  [file-name depth]
  (let [urls (read_file file-name)
        root  (create_node "root" nil 0 (atom '()) urls nil)]
    (crawling_loop root urls depth)
    root))

(defn get_indent
  [n]
  (if (<= n 0)
    ""
    (apply str (repeat n " "))))

(defn get_message
  [status links-count location]
  (if (= status 404)
    (str " bad")
    (if (?redirect status)
      (str links-count " redirect " location)
      (str links-count))))

(defn print_node
  [node level]
  (let [indent (* 4 (dec level))
        uri (:url node)
        status (:status node)
        links-count (count (:urls node))
        location (:location node)]
    (println (str (get_indent indent) uri " " (get_message status links-count location)))))

(defn walk_tree
  [node level]
  (if (not (= level 0))
    (print_node node level))
  (doseq [child @(:children node)] (walk_tree child (inc level))))

(defn print_tree
  [root]
  (walk_tree root 0))


(defn -main
  [& args]
  (let [[opts args] (cli args ["-f" "--file"  :default "resources/urls.txt"]
                              ["-d" "--depth" :default "2"])
        tree (crawling (:file opts) (Integer/parseInt (:depth opts)))]
    (print_tree tree)))

