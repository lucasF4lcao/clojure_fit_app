(ns fit.exercicios
  (:require [clj-http.client :as client]
            [clojure.edn :as edn]))

(def api-key "MU8Ke7SzVWokf/dhBmEC2Q==zKLiuakEi9dKKX6M")


(defn caloriasQueimadas [atividade duracao]
  (let [url "https://api.api-ninjas.com/v1/caloriesburned"
        resposta (client/get url
                             {:headers {"X-Api-Key" api-key}
                              :query-params {"activity" atividade
                                             "duration" duracao}
                              :as :json})
        total (-> resposta :body first :total_calories)]
    total))

(defn -main []
  (println (caloriasQueimadas "soccer" 40)))