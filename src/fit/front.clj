(ns fit.front
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as str]))

(def base-url "http://localhost:3000")

(def sessao (atom nil)) ; guarda {:id "usuario123"}

(defn ler-string [prompt]
  (println prompt)
  (flush)
  (read-line))

(defn ler-int [prompt]
  (Integer/parseInt (ler-string prompt)))

(defn ler-float [prompt]
  (Double/parseDouble (ler-string prompt)))

;; ----------- AUTENTICAÇÃO -----------

(defn cadastrar-usuario []
  (let [id (ler-string "ID do usuário:")
        senha (ler-string "Senha:")
        altura (ler-float "Altura (m):")
        peso (ler-float "Peso (kg):")
        idade (ler-int "Idade:")
        sexo (ler-string "Sexo (M/F):")]
    (let [usuario {:id id
                   :senha senha
                   :altura altura
                   :peso peso
                   :idade idade
                   :sexo sexo}
          resposta (http/post (str base-url "/usuario")
                              {:body (json/write-str usuario)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn login []
  (let [id (ler-string "ID:")
        senha (ler-string "Senha:")
        credenciais {:id id :senha senha}
        resposta (http/post (str base-url "/login")
                            {:body (json/write-str credenciais)
                             :headers {"Content-Type" "application/json"}
                             :throw-exceptions false
                             :as :json})]
    (if (= 200 (:status resposta))
      (do (reset! sessao {:id id})
          (println "Login bem-sucedido!")
          true)
      (do (println "Falha no login:" (:body resposta))
          false))))

(defn consultar-usuario []
  (let [id (:id @sessao)
        resposta (http/get (str base-url "/usuario/" id) {:as :json})]
    (println "Usuário atual:" (:body resposta))))

;; ----------- FUNCIONALIDADES -----------

(defn registrar-alimento []
  (let [nome (ler-string "Nome do alimento:")
        calorias (ler-int "Calorias:")
        data (ler-string "Data (yyyy-MM-dd) [enter para hoje]:")]
    (let [alimento (cond-> {:nome nome :calorias calorias}
                           (not (str/blank? data)) (assoc :data data))
          resposta (http/post (str base-url "/alimento")
                              {:body (json/write-str alimento)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn registrar-exercicio []
  (let [nome (ler-string "Nome do exercício:")
        calorias (ler-int "Calorias gastas:")
        data (ler-string "Data (yyyy-MM-dd) [enter para hoje]:")]
    (let [exercicio (cond-> {:nome nome :calorias calorias}
                            (not (str/blank? data)) (assoc :data data))
          resposta (http/post (str base-url "/exercicio")
                              {:body (json/write-str exercicio)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn consultar-extrato []
  (let [data-inicio (ler-string "Data início (yyyy-MM-dd) [enter para ignorar]:")
        data-fim (ler-string "Data fim (yyyy-MM-dd) [enter para ignorar]:")
        query (->> [["data-inicio" data-inicio]
                    ["data-fim" data-fim]]
                   (filter (fn [[_ v]] (not (str/blank? v))))
                   (map (fn [[k v]] (str k "=" v)))
                   (str/join "&"))
        url (str base-url "/extrato" (when (seq query) (str "?" query)))
        resposta (http/get url {:as :json})]
    (println "Extrato:" (:body resposta))))

(defn consultar-saldo []
  (let [data-inicio (ler-string "Data início (yyyy-MM-dd) [enter para ignorar]:")
        data-fim (ler-string "Data fim (yyyy-MM-dd) [enter para ignorar]:")
        query (->> [["data-inicio" data-inicio]
                    ["data-fim" data-fim]]
                   (filter (fn [[_ v]] (not (str/blank? v))))
                   (map (fn [[k v]] (str k "=" v)))
                   (str/join "&"))
        url (str base-url "/saldo" (when (seq query) (str "?" query)))
        resposta (http/get url {:as :json})]
    (println "Saldo de calorias:" (:saldo (:body resposta)))))

;; ----------- MENU -----------

(defn menu []
  (loop []
    (println "\n--- Menu Fit CLI ---")
    (println "1 - Consultar usuário")
    (println "2 - Registrar alimento")
    (println "3 - Registrar exercício")
    (println "4 - Consultar extrato")
    (println "5 - Consultar saldo")
    (println "0 - Sair")
    (print "Escolha uma opção: ")
    (flush)
    (case (read-line)
      "1" (do (consultar-usuario) (recur))
      "2" (do (registrar-alimento) (recur))
      "3" (do (registrar-exercicio) (recur))
      "4" (do (consultar-extrato) (recur))
      "5" (do (consultar-saldo) (recur))
      "0" (println "Tchau!")
      (do (println "Opção inválida.") (recur)))))

(defn tela-inicial []
  (loop []
    (println "\n--- Bem-vindo ao Fit CLI ---")
    (println "1 - Login")
    (println "2 - Cadastrar novo usuário")
    (println "0 - Sair")
    (print "Escolha uma opção: ")
    (flush)
    (case (read-line)
      "1" (if (login)
            (menu)
            (recur))
      "2" (do (cadastrar-usuario) (recur))
      "0" (println "Tchau!")
      (do (println "Opção inválida.") (recur)))))

(defn -main []
  (tela-inicial))
