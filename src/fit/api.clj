(ns fit.api
  (:require
    [ring.adapter.jetty :refer [run-jetty]]
    [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
    [compojure.core :refer [defroutes GET POST]]
    [compojure.route :as route]
    [clj-time.core :as t]
    [clj-time.format :as f]
    [clojure.string :as str]))


(def state (atom {:usuarios '()
                  :alimentos '()
                  :exercicios '()}))

(def date-formatter (f/formatter "yyyy-MM-dd"))

(defn parse-date [s]
  (try (f/parse date-formatter s)
       (catch Exception _ nil)))

(defn today-str []
  (f/unparse date-formatter (t/now)))

(defn with-data [m]
  (if (contains? m :data)
    m
    (assoc m :data (today-str))))

;; ---------- USUÁRIO ----------
(defn usuario-existe? [id]
  (some #(= id (:id %)) (:usuarios @state)))

(defn registrar-usuario [usuario]
  (let [{:keys [id senha altura peso idade sexo]} usuario]
    (cond
      (or (str/blank? id) (str/blank? senha))
      {:status 400 :body {:erro "ID e senha são obrigatórios"}}

      (usuario-existe? id)
      {:status 409 :body {:erro "ID já está em uso"}}

      :else
      (let [usuario-completo (assoc (dissoc usuario :data) :id id :senha senha)]
        (swap! state update :usuarios conj usuario-completo)
        {:status 201 :body {:msg "Usuário registrado com sucesso"}}))))

(defn login [credenciais]
  (let [{:keys [id senha]} credenciais
        usuario (some #(when (= id (:id %)) %) (:usuarios @state))]
    (if (and usuario (= senha (:senha usuario)))
      {:status 200 :body {:msg "Login bem-sucedido"}}
      {:status 401 :body {:erro "ID ou senha inválidos"}})))

(defn consultar-usuario [id]
  (let [usuario (some #(when (= id (:id %)) %) (:usuarios @state))]
    (if usuario
      {:status 200 :body (dissoc usuario :senha)}
      {:status 404 :body {:erro "Usuário não encontrado"}})))

;; ---------- ALIMENTOS / EXERCÍCIOS ----------
(defn registrar-alimento [alimento]
  (let [alimento-com-data (with-data alimento)]
    (swap! state update :alimentos conj alimento-com-data)
    {:status 201 :body {:msg "Alimento registrado"}}))

(defn registrar-exercicio [exercicio]
  (let [exercicio-com-data (with-data exercicio)]
    (swap! state update :exercicios conj exercicio-com-data)
    {:status 201 :body {:msg "Exercicio registrado"}}))

(defn filtrar-por-periodo [registros data-inicio data-fim]
  (filter
    (fn [{:keys [data]}]
      (let [d (parse-date data)
            d-inicio (parse-date data-inicio)
            d-fim (parse-date data-fim)]
        (and d
             (or (not d-inicio) (not (t/before? d d-inicio)))
             (or (not d-fim) (not (t/after? d d-fim))))))
    registros))

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

;; ---------- ROTAS ----------
(defroutes app-routes
           ;; Usuário
           (POST "/usuario" req (registrar-usuario (:body req)))
           (POST "/login" req (login (:body req)))
           (GET "/usuario/:id" [id] (consultar-usuario id))

           ;; Registros
           (POST "/alimento" req (registrar-alimento (:body req)))
           (POST "/exercicio" req (registrar-exercicio (:body req)))

           ;; Consulta
           (GET "/extrato" [data-inicio data-fim] (extrato data-inicio data-fim))
           (GET "/saldo" [data-inicio data-fim] (saldo data-inicio data-fim))
           (GET "/debug/state" []
             {:status 200
              :body @state})

           (route/not-found {:status 404 :body {:error "Not found"}}))

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main []
  (run-jetty app {:port 3000 :join? false}))
