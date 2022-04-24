(ns sql
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [honey.sql :as sql]
   [honey.sql.helpers :as h]
   [next.jdbc :as jdbc]
   [tech.v3.dataset :refer (->dataset)]))

(def db {:dbtype "sqlite" :dbname "koans.db"})
(defn ds [] (jdbc/get-datasource db))

(defn ->sql [m] (sql/format m {:inline true :pretty true}))

(defn query [m]
  (->dataset (jdbc/execute! (ds) (->sql m))))

(defn show-table []
  (query {:select [:name]
          :from [:sqlite_schema]
          :where [:and [:= :type "table"]
                  [:not-like :name "sqlite_%"]]}))

(defn parse-file [filename container]
  (let [s (str/replace (last (str/split filename #"/")) #"\.sql$" "")
        xf (comp
            (filter #(> (count %) 0))
            (map (fn [s] (str/split s #"\n" 2)))
            (map (fn [[k v]] [k (str/trim (str/replace v #"[\n\t]" " "))])))]
    {s (into container xf (str/split (slurp filename) #"-- "))}))

(defn koans []
  (transduce
   (comp
    (filter (fn [f] (.isFile f)))
    (map (comp #(parse-file % []) str)))
   merge
   {}
   (file-seq (io/file "src/koans/"))))

(defn solutions []
  (->> "src/koans/solutions/"
       io/file
       file-seq
       (filter (fn [f] (.isFile f)))
       (mapv (comp #(parse-file % {}) str))
       (reduce merge)))

(defn check-answer [queries section]
  (println (apply str "\n" (repeat 80 "=")))
  (doseq [[k m] queries
          :let [s (get-in (solutions) [section k])]
          :when (map? m)]
    (println k)
    (println m)
    (let [user (query m)
          solution (->dataset (jdbc/execute! (ds) [s]))]
      (if (= user solution)
        (println "Equal")
        (println "Not equal" user solution)))))

(defn basics []
  [["Meditate on upper-case queries" "SELECT 1"]
   ["Meditate on lower-case queries" {:select [1]}]
   ["Meditate on selecting all columns from a table" {:select [:*] :from [:book]}]
   ["Meditate on selecting one column (title) from a table"
    {:select [:title] :from [:book]}]
   ["Meditate on selecting some columns (title, available) from a table"
    {:select [:title :available] :from [:book]}]
   #_["Meditate on adding a new book"
    {:insert []}
    "insert into book(title, genre, year_published, available) values('_____', '_____', _____, 1)"]
   #_["Meditate on adding two new books"
    "insert into book(title, genre, year_published, available) values('_____', '_____', _____, 1) ,('_____', '_____', _____, 1)"]])

(comment
  (check-answer (basics) "basics"))

(defn filtering []
  [["Meditate on filtering results with WHERE: select all novels"
    {:select [:*] :from [:book] :where [:= :genre "Novel"]}]
   ["Meditate on the != operator: select all books that are not novels"
    {:select [:*] :from [:book] :where [:!= :genre "Novel"]}
    "select * from book where _____ != '_____'"]
   ["Meditate on the <> operator: select all books that are not novels"
    {:select [:*] :from [:book] :where [:<> :genre "Novel"]}
    "select * from book where _____ _____ '_____'"]
   ["Meditate on the \"in\" operator: select all novels and satires"
    {:select [:*] :from [:book] :where [:in :genre ["Novel" "Satire"]]}
    "select * from book where _____ in ('_____', '_____')"]
   ["Meditate on the \"not in\" operator: select all books that are not novels or satires"
    {:select [:*] :from [:book] :where [:not [:in :genre ["Novel" "Satire"]]]}
    "select * from book where _____ not in ('_____', '_____')"]
   ["Meditate on the > operator: select all books published after 1900"
    {:select [:*] :from [:book] :where [:> :year_published 1900]}
    "select * from book where _____ > _____"]
   ["Meditate on the \"like\" and % operators: select all books that start with 'The'"
    {:select [:*] :from [:book] :where [:like :title "The%"]}
    "select * from _____ _____ title like '_____%'"]
   ["Meditate on the \"like\" and % operators: select all books that contain 'of'"
    {:select [:*] :from [:book] :where [:like :title "%of%"]}
    "select * from _____ where _____ _____ '%_____%'"]
   ["Meditate on the \"and\" operator: select all books published before 1900 that are available"
    {:select [:*] :from [:book] :where [:and [:< :year_published 1900] [:= :available 1]]}
    "select * from book where _____ _____ _____ and _____ _____ _____"]
   ["Meditate on the \"or\" operator: select all books that start with 'The' OR are available"
    {:select [:*] :from [:book] :where [:or [:like :title "The%"] [:= :available 1]]}
    "select * from book where _____ _____ _____ or _____ _____ _____"]])

(comment
  (check-answer (filtering) "filtering"))

(defn subqueries []
  [["Meditate on using subqueries with IN"
    "select * from event where customer_id _____ (  select customer_id from event  where type = 'Lost' ) order by datetime desc"]
   ["Meditate on WITH to create a Common Table Expression query"
    "_____ book_losers as (  select customer_id from event  where type = 'Lost' ) select e.* from book_losers bl  join event e on bl.customer_id = e.customer_id order by datetime desc"]
   ["Meditate on using subqueries for deleting data"
    "delete from _____ where _____ in (  select book_id  from event  where type = 'Lost' )"]])

(comment
  (check-answer (basics) "subqueries"))

(defn joins []
  [["Meditate on the \"INNER JOIN\" keyword to link tables"
    "select * from customer  _____ _____ event on customer.id = event.customer_id"]
   ["Meditate on the \"INNER\" being implicit"
    "select * from customer  _____ event on customer.id = event.customer_id"]
   ["Meditate on join conditions"
    "select * from customer  join event on _____ = _____"]
   ["Meditate on limiting columns in joins"
    "select _____.name, _____.datetime from customer  join event on customer.id = event.customer_id"]
   ["Meditate on aliasing tables"
    "select * from customer c  join event e on _____.id = _____.customer_id"]
   ["Meditate on multiple joins"
    "select e.id, e.type, e.datetime, c.id, c.name, b.title from event e  join customer c on _____.customer_id = _____.id  join book b on _____.book_id = _____.id"]
   ["Meditate on the \"LEFT JOIN\" keyword"
    "select b.*, e.datetime from book b  _____ join event e on b.id = e.book_id"]])

(comment
  (check-answer (joins) "joins"))


(defn presentation []
  [["Meditate on aliasing columns: present the year_published column as \"Publication Date\""
    "select title, year_published as \"_____\" from book"]
   ["Meditate on sorting with \"ORDER BY\": sort by customer name"
    "select * from customer order by _____"]
   ["Meditate on multiple sorts: sort by genre, then title"
    "select * from book order by _____, _____"]
   ["Meditate on \"DESC\" keyword"
    "select * from event order by datetime _____"]])

(defn update_delete []
  [["Meditate on changing data with \"UPDATE\""
    "_____ book set available = 1 where id = 2"]
   ["Meditate on removing data with \"DELETE\""
    "_____ from customer where id = 20001"]])



(defn relationships []
  [["Meditate on ONE-TO-MANY relationships"
    "select * from customer c  join event e on _____ = _____ where c.id = 20001"]
   ["Meditate on MANY-TO-ONE relationships"
    "select e.*, b.id, b.title from event e  join book b on _____ = _____ where b.id = 1"]
   ["Meditate on MANY-TO-MANY relationships"
    "select a.first_name, a.last_name, b.title from book b  join book_to_author_map btam on _____.id = _____.book_id  join author a on _____.author_id = _____.id where author_id in (1, 5, 6)"]])



(defn aggregates []
  [["Meditate on counting rows with COUNT" "select _____(*) from book"]
   ["Meditate on counting within groups with GROUP BY"
    "select genre, count(*) from book _____ _____ genre"]
   ["Meditate on MAX" "select _____(datetime) from event"]
   ["Meditate on MIN" "select _____(datetime) from event"]
   ["Meditate on getting the maximum within groups"
    "select genre, _____(year_published) from book _____ _____ _____"]
   ["Meditate on joining data with aggregates"
    "select name, count(*) from customer  join event on _____ = _____ group by _____"]])


(comment

  (check-answer (basics) "basics")

  (get-in (solutions) ["basics"])
  ;; (parse-file "src/koans/solutions/aggregates.sql")
  (show-table)
  (println insert)
  (-> {:select [:*]
       :from [:book]}
      query)


  )
