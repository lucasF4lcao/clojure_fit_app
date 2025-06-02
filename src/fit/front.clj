(ns fit.front
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]))

(def base-url "http://localhost:3000")

(defn ler-string [prompt]
  (println prompt)
  (flush)
  (read-line))

(defn ler-int [prompt]
  (Integer/parseInt (ler-string prompt)))

(defn ler-float [prompt]
  (Double/parseDouble (ler-string prompt)))

(defn cadastrar-usuario []
  (let [altura (ler-float "Altura (m):")
        peso (ler-float "Peso (kg):")
        idade (ler-int "Idade:")
        sexo (ler-string "Sexo (M/F):")]
    (let [usuario {:altura altura
                   :peso peso
                   :idade idade
                   :sexo sexo}
          resposta (http/post (str base-url "/usuario")
                              {:body (json/write-str usuario)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn registrar-alimento []
  (let [nome (ler-string "Nome do alimento:")
        calorias (ler-int "Calorias:")
        data (ler-string "Data (yyyy-MM-dd) [enter para hoje]:")]
    (let [alimento (cond-> {:nome nome :calorias calorias}
                           (not (clojure.string/blank? data)) (assoc :data data))
          resposta (http/post (str base-url "/alimento")
                              {:body (json/write-str alimento)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn registrar-exercicio []
  (let [nome (ler-string "Nome do exercicio:")
        calorias (ler-int "Calorias gastas:")
        data (ler-string "Data (yyyy-MM-dd) [enter para hoje]:")]
    (let [exercicio (cond-> {:nome nome :calorias calorias}
                            (not (clojure.string/blank? data)) (assoc :data data))
          resposta (http/post (str base-url "/exercicio")
                              {:body (json/write-str exercicio)
                               :headers {"Content-Type" "application/json"}
                               :as :json})]
      (println "Resposta:" (:body resposta)))))

(defn consultar-usuario []
  (let [resposta (http/get (str base-url "/usuario") {:as :json})]
    (println "Usuario:" (:body resposta))))

(defn consultar-extrato []
  (let [data-inicio (ler-string "Data inicio (yyyy-MM-dd) [enter para ignorar]:")
        data-fim (ler-string "Data fim (yyyy-MM-dd) [enter para ignorar]:")
        query (->> [["data-inicio" data-inicio]
                    ["data-fim" data-fim]]
                   (filter (fn [[_ v]] (not (clojure.string/blank? v))))
                   (map (fn [[k v]] (str k "=" v)))
                   (clojure.string/join "&"))
        url (str base-url "/extrato" (when (seq query) (str "?" query)))
        resposta (http/get url {:as :json})]
    (println "Extrato:" (:body resposta))))

(defn consultar-saldo []
  (let [data-inicio (ler-string "Data início (yyyy-MM-dd) [enter para ignorar]:")
        data-fim (ler-string "Data fim (yyyy-MM-dd) [enter para ignorar]:")
        query (->> [["data-inicio" data-inicio]
                    ["data-fim" data-fim]]
                   (filter (fn [[_ v]] (not (clojure.string/blank? v))))
                   (map (fn [[k v]] (str k "=" v)))
                   (clojure.string/join "&"))
        url (str base-url "/saldo" (when (seq query) (str "?" query)))
        resposta (http/get url {:as :json})]
    (println "Saldo de calorias:" (:saldo (:body resposta)))))

(defn menu []
  (println "\n--- Menu Fit CLI ---")
  (println "1 - Cadastrar/Consultar usuario")
  (println "2 - Registrar consumo de alimento")
  (println "3 - Registrar atividade fisica")
  (println "4 - Consultar extrato por periodo")
  (println "5 - Consultar saldo por periodo")
  (println "0 - Sair")
  (print "Escolha uma opcao: ")
  (flush)
  (let [opcao (read-line)]
    (case opcao
      "1" (do (consultar-usuario)
              (cadastrar-usuario)
              (recur))
      "2" (do (registrar-alimento)
              (recur))
      "3" (do (registrar-exercicio)
              (recur))
      "4" (do (consultar-extrato)
              (recur))
      "5" (do (consultar-saldo)
              (recur))
      "0" (println "Tchau!")
      (do (println "Opção invalida")
          (recur)))))

(defn -main []
  (menu))
