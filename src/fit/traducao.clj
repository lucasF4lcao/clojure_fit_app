(ns fit.traducao
  (:require [cheshire.core :as json]
            [clj-http.client :as http]))

(def api-url "https://ftapi.pythonanywhere.com/")

(defn retorna-primeiro-elemento [conteudo]
  (let [possiveis (get-in conteudo [:translations :possible-translations])]
    (if (and (vector? possiveis) (seq possiveis))
      (first possiveis)
      "")))

(defn portugues-ingles [frase]
  (try
    (let [url-requisicao (str api-url "translate?sl=pt&dl=en&text=" frase)
          resposta (http/get url-requisicao {:accept :json})
          corpo (json/parse-string (:body resposta) true)]
      (or (retorna-primeiro-elemento corpo) frase))
    (catch Exception e
      (println "Erro ao traduzir pt→en:" frase "-" (.getMessage e))
      frase)))

(defn ingles-portugues [frase]
  (try
    (let [url-requisicao (str api-url "translate?sl=en&dl=pt&text=" frase)
          resposta (http/get url-requisicao {:accept :json})
          corpo (json/parse-string (:body resposta) true)]
      (or (retorna-primeiro-elemento corpo) frase))
    (catch Exception e
      (println "Erro ao traduzir en→pt:" frase "-" (.getMessage e))
      frase)))