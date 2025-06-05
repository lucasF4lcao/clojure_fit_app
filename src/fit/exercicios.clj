(ns fit.exercicios
  (:require [clj-http.client :as client]))

(def api-key "MU8Ke7SzVWokf/dhBmEC2Q==zKLiuakEi9dKKX6M")

(defn mostrar-opcoes [opcoes]
  (doseq [[idx op] (map-indexed vector opcoes)]
    (println (str idx ": " (:name op)))))

(defn ler-escolha [max-index]
  (println "Digite o numero da opcao desejada:")
  (let [input (read-line)
        escolha (try
                  (Integer/parseInt input)
                  (catch Exception _
                    -1))]
    (if (and (<= 0 escolha) (< escolha max-index))
      escolha
      (do
        (println "Escolha invalida. Tente novamente.")
        (recur max-index)))))

(defn calorias-queimadas [atividade duracao]
  (try
    (let [url "https://api.api-ninjas.com/v1/caloriesburned"
          resposta (client/get url
                               {:headers {"X-Api-Key" api-key}
                                :query-params {"activity" atividade
                                               "duration" (str duracao)}
                                :as :json})
          opcoes (:body resposta)]
      ;; Sempre retorna as opções, mesmo que só haja uma
      opcoes)
    (catch Exception e
      (println "Erro ao consultar API:" (.getMessage e))
      nil)))


(defn -main []
  (println (calorias-queimadas "soccer" 60)))
