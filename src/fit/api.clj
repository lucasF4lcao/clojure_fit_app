(ns fit.api
  (:require
    [ring.adapter.jetty :refer [run-jetty]]
    [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
    [compojure.core :refer [defroutes GET POST]]
    [compojure.route :as route]
    [clj-time.core :as t]
    [clj-time.format :as f]
    [clojure.string :as str]
    [fit.exercicios :as ex]
    [fit.alimentos :as ali]))

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


(defn registrarAlimento [dados]
  (let [{:keys [id descricao quantidade indice]} dados
        alimentos (ali/buscar-calorias descricao)
        alimento (nth alimentos indice nil)]
    (if (and alimento quantidade)
      (let [calorias (ali/calcular-calorias alimento quantidade)
            registro {:id id
                      :descricao (:descricao alimento)
                      :quantidade quantidade
                      :calorias calorias
                      :data (todayStr)}]
        (swap! state update :alimentos conj registro)
        {:status 201 :body {:msg "Alimento registrado com sucesso" :registro registro}})
      {:status 400 :body {:erro "Descricao, indice ou quantidade invalida"}})))

(defn registrarExercicio [dados]
  (let [{:keys [id atividade duracao indice]} dados
        opcoes (ex/calorias-queimadas atividade duracao)
        opcao (nth opcoes indice nil)]
    (if (and opcao (:total_calories opcao))
      (let [registro {:id id
                      :atividade (:name opcao)
                      :duracao duracao
                      :calorias (:total_calories opcao)
                      :data (todayStr)}]
        (swap! state update :exercicios conj registro)
        {:status 201 :body {:msg "Exercicio registrado com sucesso" :registro registro}})
      {:status 400 :body {:erro "Atividade invalida ou nao encontrada"}})))


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

(defn extrato [data-inicio data-fim]
  (let [{:keys [alimentos exercicios]} @state
        alimentosFiltrados (filtrarPorPeriodo alimentos data-inicio data-fim)
        exerciciosFiltrados (filtrarPorPeriodo exercicios data-inicio data-fim)]
    {:status 200
     :body {:alimentos alimentosFiltrados
            :exercicios exerciciosFiltrados}}))

(defn extrair-kcal [caloria-str]
  (try
    (if (string? caloria-str)
      (let [match (re-find #"([\d]+[.,]?[\d]*)\s*kcal" caloria-str)]
        (if-let [valor (second match)]
          (Double/parseDouble (str/replace valor "," "."))
          0.0))
      (double caloria-str)) ; já é número
    (catch Exception _ 0.0)))


(defn saldo [data-inicio data-fim]
  (let [{:keys [alimentos exercicios]} @state
        alimentosFiltrados (filtrarPorPeriodo alimentos data-inicio data-fim)
        exerciciosFiltrados (filtrarPorPeriodo exercicios data-inicio data-fim)
        caloriasConsumidas (reduce + (map #(extrair-kcal (:calorias %)) alimentosFiltrados))
        caloriasGastas (reduce + (map :calorias exerciciosFiltrados))]
    {:status 200
     :body {:saldo (format "%.2f"(- caloriasConsumidas caloriasGastas))}}))



(defroutes appRoutes
           (POST "/usuario" req (registrarUsuario (:body req)))
           (POST "/login" req (login (:body req)))
           (GET "/usuario/:id" [id] (consultarUsuario id))

           (POST "/alimento" req (registrarAlimento (:body req)))
           (POST "/exercicio" req (registrarExercicio (:body req)))

           (GET "/extrato" [data-inicio data-fim] (extrato data-inicio data-fim))
           (GET "/saldo" [data-inicio data-fim] (saldo data-inicio data-fim))
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