(ns fit.alimentos
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:gen-class))

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

(defn calcular-calorias [item quantidade-usuario]
  (let [descricao (:descricao item)
        calorias-totais (parse-kcal (:calorias item))
        gramas-totais (extrair-gramas (:quantidade item))]
    (if (and calorias-totais gramas-totais (pos? gramas-totais))
      (let [calorias-por-grama (/ calorias-totais gramas-totais)
            calorias-ajustada (* calorias-por-grama quantidade-usuario)]
        (format "%s - %.0fg: %.2f kcal"
                descricao
                quantidade-usuario
                calorias-ajustada))
      (format "%s - Dados insuficientes para calculo (calorias: %s, gramas: %s)"
              descricao calorias-totais gramas-totais))))


(defn -main []
  (println "Digite o nome do alimento:")
  (let [alimento (read-line)
        dados (buscar-calorias alimento)]
    (if (empty? dados)
      (println "Alimento não encontrado.")
      (do
        ;; Mostrar opções numeradas:
        (println "Escolha o alimento:")
        (doseq [[idx item] (map-indexed vector dados)]
          (println (format "%d - %s (%s)" (inc idx) (:descricao item) (:calorias item))))

        ;; Ler escolha do usuário:
        (println "Digite o numero do alimento escolhido:")
        (let [opcao-str (read-line)
              opcao (try
                      (Integer/parseInt opcao-str)
                      (catch Exception _
                        0))]
          (if (or (< opcao 1) (> opcao (count dados)))
            (println "Opção invalida.")
            (do
              ;; Pede a gramatura
              (println "Digite a quantidade (em gramas):")
              (let [quantidade (try
                                 (Double/parseDouble (read-line))
                                 (catch Exception _
                                   (println "Quantidade invalida!") 0))
                    item-escolhido (nth dados (dec opcao))
                    resultado (calcular-calorias item-escolhido quantidade)]
                (println resultado)))))))))