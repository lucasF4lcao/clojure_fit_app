(ns fit.exercicios
  (:require [clj-http.client :as client]))

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


(defn -main []
  (println (calorias-queimadas "soccer" 60)))
