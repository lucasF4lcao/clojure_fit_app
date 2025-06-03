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

(def dateFormatter (f/formatter "yyyy-MM-dd"))

(defn parseDate [s]
  (try (f/parse dateFormatter s)
       (catch Exception _ nil)))

(defn todayStr []
  (f/unparse dateFormatter (t/now)))

(defn withData [m]
  (if (contains? m :data)
    m
    (assoc m :data (todayStr))))

(defn usuarioExiste? [id]
  (some #(= id (:id %)) (:usuarios @state)))

(defn registrarUsuario [usuario]
  (let [{:keys [id senha altura peso idade sexo]} usuario]
    (cond
      (or (str/blank? id) (str/blank? senha))
      {:status 400 :body {:erro "ID e senha sao obrigatorios"}}

      (usuarioExiste? id)
      {:status 409 :body {:erro "ID ja esta em uso"}}

      :else
      (let [usuarioCompleto (assoc (dissoc usuario :data) :id id :senha senha)]
        (swap! state update :usuarios conj usuarioCompleto)
        {:status 201 :body {:msg "Usuario registrado com sucesso"}}))))

(defn login [credenciais]
  (let [{:keys [id senha]} credenciais
        usuario (some #(when (= id (:id %)) %) (:usuarios @state))]
    (if (and usuario (= senha (:senha usuario)))
      {:status 200 :body {:msg "Login bem-sucedido"}}
      {:status 401 :body {:erro "ID ou senha invalidos"}})))

(defn consultarUsuario [id]
  (let [usuario (some #(when (= id (:id %)) %) (:usuarios @state))]
    (if usuario
      {:status 200 :body (dissoc usuario :senha)}
      {:status 404 :body {:erro "Usuario nao encontrado"}})))

(defn registrarAlimento [alimento]
  (let [alimentoComData (withData alimento)]
    (swap! state update :alimentos conj alimentoComData)
    {:status 201 :body {:msg "Alimento registrado"}}))

(defn registrarExercicio [exercicio]
  (let [exercicioComData (withData exercicio)]
    (swap! state update :exercicios conj exercicioComData)
    {:status 201 :body {:msg "Exercicio registrado"}}))

(defn filtrarPorPeriodo [registros dataInicio dataFim]
  (filter
    (fn [{:keys [data]}]
      (let [d (parseDate data)
            dInicio (parseDate dataInicio)
            dFim (parseDate dataFim)]
        (and d
             (or (not dInicio) (not (t/before? d dInicio)))
             (or (not dFim) (not (t/after? d dFim))))))
    registros))

(defn extrato [dataInicio dataFim]
  (let [{:keys [alimentos exercicios]} @state
        alimentosFiltrados (filtrarPorPeriodo alimentos dataInicio dataFim)
        exerciciosFiltrados (filtrarPorPeriodo exercicios dataInicio dataFim)]
    {:status 200
     :body {:alimentos alimentosFiltrados
            :exercicios exerciciosFiltrados}}))

(defn saldo [dataInicio dataFim]
  (let [{:keys [alimentos exercicios]} @state
        alimentosFiltrados (filtrarPorPeriodo alimentos dataInicio dataFim)
        exerciciosFiltrados (filtrarPorPeriodo exercicios dataInicio dataFim)
        caloriasConsumidas (reduce + (map :calorias alimentosFiltrados))
        caloriasGastas (reduce + (map :calorias exerciciosFiltrados))]
    {:status 200
     :body {:saldo (- caloriasConsumidas caloriasGastas)}}))

(defroutes appRoutes
           (POST "/usuario" req (registrarUsuario (:body req)))
           (POST "/login" req (login (:body req)))
           (GET "/usuario/:id" [id] (consultarUsuario id))

           (POST "/alimento" req (registrarAlimento (:body req)))
           (POST "/exercicio" req (registrarExercicio (:body req)))

           (GET "/extrato" [dataInicio dataFim] (extrato dataInicio dataFim))
           (GET "/saldo" [dataInicio dataFim] (saldo dataInicio dataFim))
           (GET "/debug/state" []
             {:status 200
              :body @state})

           (route/not-found {:status 404 :body {:error "Not found"}}))

(def app
  (-> appRoutes
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main []
  (run-jetty app {:port 3000 :join? false}))