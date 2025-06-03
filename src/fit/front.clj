(ns fit.front
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as str]))

(def base-url "http://localhost:3000")

(def sessao (atom nil))

(defn lerString [prompt]
  (println prompt)
  (flush)
  (read-line))

(defn lerInteiro [prompt]
  (Integer/parseInt (lerString prompt)))

(defn lerFloat [prompt]
  (Double/parseDouble (lerString prompt)))

(defn cadastrarUsuario []
  (let [id (lerString "ID do usuário:")
        senha (lerString "Senha:")
        altura (lerFloat "Altura (m):")
        peso (lerFloat "Peso (kg):")
        idade (lerInteiro "Idade:")
        sexo (lerString "Sexo (m/f):")]
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
  (let [id (lerString "ID:")
        senha (lerString "Senha:")
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

(defn consultarUsuario []
  (let [id (:id @sessao)]
    (if (nil? id)
      (println "Você precisa estar logado para consultar usuário.")
      (let [resposta (http/get (str base-url "/usuario/" id) {:as :json})]
        (println "Usuário atual:" (:body resposta))))))

(defn registrarAlimento []
  (let [usuario-id (:id @sessao)]
    (if (nil? usuario-id)
      (println "Você precisa estar logado para registrar alimentos.")
      (let [nome (lerString "Nome do alimento:")
            calorias (lerInteiro "Calorias:")
            data (lerString "Data (yyyy-MM-dd) [enter para hoje]:")
            alimento (cond-> {:nome nome :calorias calorias :usuario-id usuario-id}
                             (not (str/blank? data)) (assoc :data data))
            resposta (http/post (str base-url "/alimento")
                                {:body (json/write-str alimento)
                                 :headers {"Content-Type" "application/json"}
                                 :as :json})]
        (println "Resposta:" (:body resposta))))))

(defn registrarExercicio []
  (let [usuario-id (:id @sessao)]
    (if (nil? usuario-id)
      (println "Você precisa estar logado para registrar exercícios.")
      (let [nome (lerString "Nome do exercício:")
            calorias (lerInteiro "Calorias gastas:")
            data (lerString "Data (yyyy-MM-dd) [enter para hoje]:")
            exercicio (cond-> {:nome nome :calorias calorias :usuario-id usuario-id}
                              (not (str/blank? data)) (assoc :data data))
            resposta (http/post (str base-url "/exercicio")
                                {:body (json/write-str exercicio)
                                 :headers {"Content-Type" "application/json"}
                                 :as :json})]
        (println "Resposta:" (:body resposta))))))

(defn consultarExtrato []
  (let [usuario-id (:id @sessao)]
    (if (nil? usuario-id)
      (println "Você precisa estar logado para consultar extrato.")
      (let [data-inicio (lerString "Data início (yyyy-MM-dd) [enter para ignorar]:")
            data-fim (lerString "Data fim (yyyy-MM-dd) [enter para ignorar]:")
            query (->> [["usuario-id" usuario-id]
                        ["dataInicio" data-inicio]
                        ["dataFim" data-fim]]
                       (filter (fn [[_ v]] (not (str/blank? v))))
                       (map (fn [[k v]] (str k "=" v)))
                       (str/join "&"))
            url (str base-url "/extrato" (when (seq query) (str "?" query)))
            resposta (http/get url {:as :json})]
        (println "Extrato:" (:body resposta))))))

(defn consultarSaldo []
  (let [usuario-id (:id @sessao)]
    (if (nil? usuario-id)
      (println "Você precisa estar logado para consultar saldo.")
      (let [data-inicio (lerString "Data início (yyyy-MM-dd) [enter para ignorar]:")
            data-fim (lerString "Data fim (yyyy-MM-dd) [enter para ignorar]:")
            query (->> [["usuario-id" usuario-id]
                        ["dataInicio" data-inicio]
                        ["dataFim" data-fim]]
                       (filter (fn [[_ v]] (not (str/blank? v))))
                       (map (fn [[k v]] (str k "=" v)))
                       (str/join "&"))
            url (str base-url "/saldo" (when (seq query) (str "?" query)))
            resposta (http/get url {:as :json})]
        (println "Saldo de calorias:" (:saldo (:body resposta)))))))

(defn menu []
  (loop []
    (println "\n--- Menu ---")
    (println "1 - Consultar usuário")
    (println "2 - Registrar alimento")
    (println "3 - Registrar exercício")
    (println "4 - Consultar extrato")
    (println "5 - Consultar saldo")
    (println "0 - Sair")
    (print "Escolha uma opção: ")
    (flush)
    (case (read-line)
      "1" (do (consultarUsuario) (recur))
      "2" (do (registrarAlimento) (recur))
      "3" (do (registrarExercicio) (recur))
      "4" (do (consultarExtrato) (recur))
      "5" (do (consultarSaldo) (recur))
      "0" (println "Tchau!")
      (do (println "Opção inválida.") (recur)))))

(defn telaInicial []
  (loop []
    (println "1 - Login")
    (println "2 - Cadastrar novo usuário")
    (println "0 - Sair")
    (print "Escolha uma opção: ")
    (flush)
    (case (read-line)
      "1" (if (login)
            (menu)
            (recur))
      "2" (do (cadastrarUsuario) (recur))
      "0" (println "Tchau!")
      (do (println "Opção inválida.") (recur)))))

(defn -main []
  (telaInicial))
