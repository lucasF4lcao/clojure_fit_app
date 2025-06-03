(ns fit.front
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as str]))

(def baseUrl "http://localhost:3000")

(def sessao (atom nil))

(defn lerString [prompt]
  (println prompt)
  (flush)
  (read-line))

(defn lerInt [prompt]
  (Integer/parseInt (lerString prompt)))

(defn lerFloat [prompt]
  (Double/parseDouble (lerString prompt)))


(defn cadastrarUsuario []
  (let [id (lerString "ID do usuario:")
        senha (lerString "Senha:")
        altura (lerFloat "Altura (m):")
        peso (lerFloat "Peso (kg):")
        idade (lerInt "Idade:")
        sexo (lerString "Sexo (m/f):")]
    (let [usuario {:id id
                   :senha senha
                   :altura altura
                   :peso peso
                   :idade idade
                   :sexo sexo}
          resposta (http/post (str baseUrl "/usuario")
                              {:body (json/write-str usuario)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn login []
  (let [id (lerString "ID:")
        senha (lerString "Senha:")
        credenciais {:id id :senha senha}
        resposta (http/post (str baseUrl "/login")
                            {:body (json/write-str credenciais)
                             :headers {"Content-Type" "application/json"}
                             :throw-exceptions false
                             :as :json})]
    (if (= 200 (:status resposta))
      (do (reset! sessao {:id id})
          (println "Login feito com sucesso")
          true)
      (do (println "Falha no login:" (:body resposta))
          false))))

(defn consultarUsuario []
  (let [id (:id @sessao)
        resposta (http/get (str baseUrl "/usuario/" id) {:as :json})]
    (println "Usuario atual:" (:body resposta))))


(defn registrarAlimento []
  (let [nome (lerString "Nome do alimento:")
        calorias (lerInt "Calorias:")
        data (lerString "Data (yyyy-MM-dd) [enter para hoje]:")]
    (let [alimento (cond-> {:nome nome :calorias calorias}
                           (not (str/blank? data)) (assoc :data data))
          resposta (http/post (str baseUrl "/alimento")
                              {:body (json/write-str alimento)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn registrarExercicio []
  (let [nome (lerString "Nome do exercicio:")
        calorias (lerInt "Calorias gastas:")
        data (lerString "Data (yyyy-MM-dd) [enter para hoje]:")]
    (let [exercicio (cond-> {:nome nome :calorias calorias}
                            (not (str/blank? data)) (assoc :data data))
          resposta (http/post (str baseUrl "/exercicio")
                              {:body (json/write-str exercicio)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn consultarExtrato []
  (let [dataInicio (lerString "Data inicio (yyyy-MM-dd) [enter para ignorar]:")
        dataFim (lerString "Data fim (yyyy-MM-dd) [enter para ignorar]:")
        query (->> [["data-inicio" dataInicio]
                    ["data-fim" dataFim]]
                   (filter (fn [[_ v]] (not (str/blank? v))))
                   (map (fn [[k v]] (str k "=" v)))
                   (str/join "&"))
        url (str baseUrl "/extrato" (when (seq query) (str "?" query)))
        resposta (http/get url {:as :json})]
    (println "Extrato:" (:body resposta))))

(defn consultarSaldo []
  (let [dataInicio (lerString "Data inicio (yyyy-MM-dd) [enter para ignorar]:")
        dataFim (lerString "Data fim (yyyy-MM-dd) [enter para ignorar]:")
        query (->> [["data-inicio" dataInicio]
                    ["data-fim" dataFim]]
                   (filter (fn [[_ v]] (not (str/blank? v))))
                   (map (fn [[k v]] (str k "=" v)))
                   (str/join "&"))
        url (str baseUrl "/saldo" (when (seq query) (str "?" query)))
        resposta (http/get url {:as :json})]
    (println "Saldo de calorias:" (:saldo (:body resposta)))))

(defn haUsuarioRegistrado? []
  (let [resposta (http/get (str baseUrl "/debug/state") {:as :json})
        usuarios (:usuarios (:body resposta))]
    (not (empty? usuarios))))


(defn menu []
  (loop []
    (println "\n--- Menu Fit CLI ---")
    (println "1 - Consultar usuario")
    (println "2 - Registrar alimento")
    (println "3 - Registrar exercicio")
    (println "4 - Consultar extrato")
    (println "5 - Consultar saldo")
    (println "0 - Sair")
    (print "Escolha uma opcao: ")
    (flush)
    (case (read-line)
      "1" (do (consultarUsuario) (recur))
      "2" (do (registrarAlimento) (recur))
      "3" (do (registrarExercicio) (recur))
      "4" (do (consultarExtrato) (recur))
      "5" (do (consultarSaldo) (recur))
      "0" (println "Tchau!")
      (do (println "Opcao invalida.") (recur)))))

(defn telaInicial []
  (if (haUsuarioRegistrado?)
    (loop []
      (println "\n--- Bem-vindo ao Fit CLI ---")
      (println "1 - Login")
      (println "0 - Sair")
      (print "Escolha uma opcao: ")
      (flush)
      (case (read-line)
        "1" (if (login)
              (menu)
              (recur))
        "0" (println "Tchau!")
        (do (println "Opcao invalida.") (recur))))
    (do
      (println "\n--- Nenhum usuario registrado ---")
      (cadastrarUsuario)
      (println "Agora faca login...")
      (telaInicial))))

(defn -main []
  (telaInicial))