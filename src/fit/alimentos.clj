(ns fit.alimentos
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]))

(defn buscar-alimentos [descricao]
  (let [url (str "https://caloriasporalimentoapi.herokuapp.com/api/calorias/?descricao="
                 (java.net.URLEncoder/encode descricao "UTF-8"))
        response (http/get url {:as :json})]
    (:body response)))

(defn calcular-calorias [descricao gramas]
  (let [alimentos (buscar-alimentos descricao)
        alimento (first alimentos)]
    (if alimento
      (let [kcal (Double/parseDouble (get alimento "calorias_por_100g"))]
        {:nome (get alimento "descricao")
         :gramas gramas
         :calorias (/ (* kcal gramas) 100)})
      {:erro "Alimento não encontrado"})))
