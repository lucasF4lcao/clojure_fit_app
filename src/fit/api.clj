(ns fit.api
  (:require
    [ring.adapter.jetty :refer [run-jetty]]
    [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
    [compojure.core :refer [defroutes GET POST]]
    [compojure.route :as route]
    [clj-time.core :as t]
    [clj-time.format :as f]
    [clojure.string :as str]))

;; Estado inicial com listas
(def state (atom {:usuarios '()
                  :alimentos '()
                  :exercicios '()}))

(def date-formatter (f/formatter "yyyy-MM-dd"))

(defn parse-date [s]
  (try (f/parse date-formatter s)
       (catch Exception _ nil)))

(defn today-str []
  (f/unparse date-formatter (t/now)))

;; Adiciona data atual se não tiver
(defn with-data [m]
  (if (contains? m :data)
    m
    (assoc m :data (today-str))))

(defn registrar-usuario [usuario]
  (let [usuario-com-data (with-data usuario)]
    (swap! state update :usuarios #(concat % (list usuario-com-data)))
    {:status 201
     :body {:msg "Usuario registrado"}}))

(defn registrar-alimento [alimento]
  (let [alimento-com-data (with-data alimento)]
    (swap! state update :alimentos #(concat % (list alimento-com-data)))
    {:status 201
     :body {:msg "Alimento registrado"}}))

(defn registrar-exercicio [exercicio]
  (let [exercicio-com-data (with-data exercicio)]
    (swap! state update :exercicios #(concat % (list exercicio-com-data)))
    {:status 201
     :body {:msg "Exercicio registrado"}}))

;; Filtra uma lista de mapas pelo intervalo [data-inicio, data-fim]
(defn filtrar-por-periodo [registros data-inicio data-fim]
  (filter
    (fn [{:keys [data]}]
      (let [d (parse-date data)
            d-inicio (parse-date data-inicio)
            d-fim (parse-date data-fim)]
        (and d
             (or (not d-inicio) (not (t/before? d d-inicio)))
             (or (not d-fim) (not (t/after? d d-fim)))))
      )
    registros))

;; Novo endpoint GET /usuario (retorna último usuário registrado)
(defn consultar-usuario []
  (let [usuarios (:usuarios @state)
        ultimo-usuario (last usuarios)]
    {:status 200
     :body (or ultimo-usuario {:msg "Nenhum usuario cadastrado"})}))

(defn extrato [data-inicio data-fim]
  (let [{:keys [alimentos exercicios]} @state
        alimentos-filtrados (filtrar-por-periodo alimentos data-inicio data-fim)
        exercicios-filtrados (filtrar-por-periodo exercicios data-inicio data-fim)]
    {:status 200
     :body {:alimentos alimentos-filtrados
            :exercicios exercicios-filtrados}}))

(defn saldo [data-inicio data-fim]
  (let [{:keys [alimentos exercicios]} @state
        alimentos-filtrados (filtrar-por-periodo alimentos data-inicio data-fim)
        exercicios-filtrados (filtrar-por-periodo exercicios data-inicio data-fim)
        calorias-consumidas (reduce + (map :calorias alimentos-filtrados))
        calorias-gastas (reduce + (map :calorias exercicios-filtrados))]
    {:status 200
     :body {:saldo (- calorias-consumidas calorias-gastas)}}))

(defroutes app-routes
           (POST "/usuario" req
             (registrar-usuario (:body req)))

           (GET "/usuario" []
             (consultar-usuario))

           (POST "/alimento" req
             (registrar-alimento (:body req)))

           (POST "/exercicio" req
             (registrar-exercicio (:body req)))

           (GET "/extrato" [data-inicio data-fim]
             (extrato data-inicio data-fim))

           (GET "/saldo" [data-inicio data-fim]
             (saldo data-inicio data-fim))

           (route/not-found {:status 404 :body {:error "Not found"}}))

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main []
  (run-jetty app {:port 3000 :join? false}))
