(ns fit.api
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.string :as str]))

;; --------------------------
;; Estado da aplicação (em memória)
;; --------------------------
(def usuario (atom nil))
(def alimentos (atom []))
(def exercicios (atom []))

;; --------------------------
;; Endpoints de usuário
;; --------------------------
(defn registrar-usuario [req]
  (let [dados (:body req)]
    (if @usuario
      {:status 400 :body {:erro "Usuário já cadastrado."}}
      (do
        (reset! usuario dados)
        {:status 201 :body {:mensagem "Usuário cadastrado com sucesso."}}))))

;; --------------------------
;; Alimentos
;; --------------------------

(defn buscar-calorias [descricao]
  (let [url (str "https://caloriasporalimentoapi.herokuapp.com/api/calorias/?descricao="
                 (java.net.URLEncoder/encode descricao "UTF-8"))
        resposta (client/get url {:headers {"Accept" "application/json"}})
        corpo (:body resposta)
        dados (json/parse-string corpo true)]
    dados))

(defn parse-kcal [kcal-str]
  (Double/parseDouble (str/replace kcal-str #"[^0-9.]" "")))

(defn extrair-gramas [quant-str]
  (let [m (re-find #"\((\d+) g\)" quant-str)]
    (if m
      (Double/parseDouble (second m))
      nil)))

(defn registrar-alimento [req]
  (let [{:keys [descricao gramas opcao]} (:body req)
        resultados (buscar-calorias descricao)]

    (cond
      ;; Se não houver resultados
      (empty? resultados)
      {:status 404 :body {:erro "Nenhum alimento encontrado com essa descrição."}}

      ;; Etapa 1: só descrição, retorna opções ao usuário
      (and descricao (nil? gramas) (nil? opcao))
      {:status 200
       :body {:opcoes (map-indexed (fn [idx item]
                                     {:id idx
                                      :descricao (:descricao item)
                                      :quantidade (:quantidade item)
                                      :calorias (:calorias item)})
                                   resultados)}}

      ;; Etapa 2: descrição, id da opção escolhida e gramas
      (and (some? gramas) (some? opcao))
      (let [idx (Integer/parseInt (str opcao))
            item (nth resultados idx nil)]
        (if item
          (let [descricao (:descricao item)
                calorias-totais (parse-kcal (:calorias item))
                gramas-totais (extrair-gramas (:quantidade item))]
            (if (and calorias-totais gramas-totais (pos? gramas-totais))
              (let [calorias-por-grama (/ calorias-totais gramas-totais)
                    calorias-ajustada (* calorias-por-grama gramas)
                    registro {:descricao descricao
                              :gramas gramas
                              :calorias calorias-ajustada}]
                (swap! alimentos conj registro)
                {:status 201 :body {:mensagem "Alimento registrado."
                                    :dados registro}})
              {:status 400 :body {:erro "Dados insuficientes para calcular calorias."}}))
          {:status 400 :body {:erro "Opção inválida."}}))

      :else
      {:status 400 :body {:erro "Requisição mal formatada."}})))


;; --------------------------
;; Exercícios
;; --------------------------


(def api-key "MU8Ke7SzVWokf/dhBmEC2Q==zKLiuakEi9dKKX6M")

(defn calorias-queimadas [atividade duracao]
  (try
    (let [url "https://api.api-ninjas.com/v1/caloriesburned"
          resposta (client/get url
                               {:headers {"X-Api-Key" api-key}
                                :query-params {"activity" atividade
                                               "duration" (str duracao)}
                                :as :json})
          opcoes (:body resposta)]
      opcoes)
    (catch Exception e
      (println "Erro ao consultar API:" (.getMessage e))
      nil)))


(defn registrar-exercicio [req]
  (let [{:keys [atividade duracao opcao]} (:body req)
        resultados (calorias-queimadas atividade duracao)]

    (cond
      ;; Se não houver resultados
      (empty? resultados)
      {:status 404 :body {:erro "Nenhum exercício encontrado com essa descrição."}}

      ;; Etapa 1: só descrição
      (and atividade (nil? duracao) (nil? opcao))
      {:status 200
       :body {:opcoes (map-indexed (fn [idx item]
                                     {:id idx
                                      :nome (:name item)
                                      :calorias (:calories_per_hour item)})
                                   resultados)}}

      ;; Etapa 2: usuário seleciona uma opção e envia duração
      (and (some? opcao) (some? duracao))
      (let [idx (Integer/parseInt (str opcao))
            item (nth resultados idx nil)]
        (if item
          (let [calorias-hora (:calories_per_hour item)
                calorias-ajustada (* (/ duracao 60.0) calorias-hora)
                registro {:atividade (:name item)
                          :duracao duracao
                          :calorias calorias-ajustada}]
            (swap! exercicios conj registro)
            {:status 201 :body {:mensagem "Exercício registrado."
                                :dados registro}})
          {:status 400 :body {:erro "Opção inválida."}}))

      :else
      {:status 400 :body {:erro "Requisição mal formatada."}})))


;; --------------------------
;; Relatórios
;; --------------------------

(defn obter-extrato [_]
  {:status 200
   :body {:alimentos @alimentos
          :exercicios @exercicios}})



;; Verifica se há usuário cadastrado
(defn usuario-existe? [_]
  (if @usuario
    {:status 200 :body {:existe true :usuario @usuario}}
    {:status 200 :body {:existe false}}))


(defn obter-estado [_]
  {:status 200
   :body {:usuario @usuario
          :alimentos @alimentos
          :exercicios @exercicios}})

;; --------------------------
;; Rotas
;; --------------------------

(defroutes app-routes
           (POST "/usuario" [] registrar-usuario)
           (GET "/usuario" [] usuario-existe?)

           ;; Alimentos
           (POST "/alimento" [] registrar-alimento)

           ;; Exercícios
           (POST "/exercicio" [] registrar-exercicio)

           ;; Relatórios
           (GET "/extrato" [] obter-extrato)
           ;(GET "/saldo" [] (fn [req] (obter-saldo req)))

           (GET "/estado" [] obter-estado)

           ;; Not found
           (route/not-found {:status 404 :body {:erro "Endpoint não encontrado"}}))

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response))

;; --------------------------
;; Inicialização
;; --------------------------

(defn -main []
  (jetty/run-jetty app {:port 3000 :join? false}))
