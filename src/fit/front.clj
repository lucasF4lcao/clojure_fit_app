(ns fit.front
  (:require [clj-http.client :as client]
            [clojure.edn :as edn]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-time.format :as f]))

(def api-url "http://localhost:3000")

(defn ler-numero []
  (Integer/parseInt (read-line)))

(defn obter-data-hoje []
  (let [formatter (f/formatter "yyyy-MM-dd")]
    (f/unparse formatter (t/now))))

(defn usuario-existe? []
  (let [resp (client/get (str api-url "/usuario") {:as :json})]
    (get-in resp [:body :existe])))

(defn cadastrar-usuario []
  (println "Nenhum usuario cadastrado. Faca seu cadastro.")
  (print "Nome: ") (flush)
  (let [nome (read-line)]
    (print "Idade: ") (flush)
    (let [idade (Integer/parseInt (read-line))]
      (print "Sexo (M/F): ") (flush)
      (let [sexo (read-line)]
        (print "Altura (cm): ") (flush)
        (let [altura (Double/parseDouble (read-line))]
          (print "Peso (kg): ") (flush)
          (let [peso (Double/parseDouble (read-line))]
            (let [usuario {:nome nome :idade idade :sexo sexo :altura altura :peso peso}
                  resp (client/post (str api-url "/usuario")
                                    {:body (json/generate-string usuario)
                                     :headers {"Content-Type" "application/json"}
                                     :throw-exceptions false
                                     :as :json})]
              (if (= 201 (:status resp))
                (println "Cadastro realizado com sucesso!")
                (println "Erro no cadastro:" (get-in resp [:body :erro]))))))))))

(defn mostrar-usuario []
  (let [resp (client/get (str api-url "/usuario") {:as :json})]
    (if (:existe (:body resp))
      (println "Usuario cadastrado:" (pr-str (:usuario (:body resp))))
      (println "Nenhum usuario cadastrado."))))


(defn registrar-alimento []
  (println "Digite o nome do alimento:")
  (let [descricao (read-line)
        resp (client/post "http://localhost:3000/alimento"
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string {:descricao descricao})
                           :as :json})
        opcoes (get-in resp [:body :opcoes])]

    (if (empty? opcoes)
      (println "Nenhuma opção encontrada.")
      (do
        ;; Mostrar opções
        (println "Escolha uma opção:")
        (doseq [{:keys [id descricao quantidade calorias]} opcoes]
          (println (str id ") " descricao " - " quantidade " - " calorias)))

        (let [opcao (ler-numero)]
          (println "Digite a gramatura (em gramas):")
          (let [gramas (Double/parseDouble (read-line))
                resp2 (client/post "http://localhost:3000/alimento"
                                   {:headers {"Content-Type" "application/json"}
                                    :body (json/generate-string {:descricao descricao
                                                                 :opcao opcao
                                                                 :gramas gramas})
                                    :as :json})
                body2 (:body resp2)]
            (println "Alimento registrado:")
            (println body2)))))))



(defn registrar-exercicio []
  (println "Digite o nome do exercício:")
  (let [atividade (read-line)
        resp (client/post "http://localhost:3000/exercicio"
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string {:atividade atividade})
                           :as :json})
        opcoes (get-in resp [:body :opcoes])]

    (if (empty? opcoes)
      (println "Nenhuma opção encontrada.")
      (do
        ;; Mostrar opções
        (println "Escolha uma opção:")
        (doseq [{:keys [id nome calorias]} opcoes]
          (println (str id ") " nome " - " calorias " kcal/h")))

        (let [opcao (ler-numero)]
          (println "Digite a duração (em minutos):")
          (let [duracao (Double/parseDouble (read-line))
                resp2 (client/post "http://localhost:3000/exercicio"
                                   {:headers {"Content-Type" "application/json"}
                                    :body (json/generate-string {:atividade atividade
                                                           :opcao opcao
                                                           :duracao duracao})
                                    :as :json})
                body2 (:body resp2)]
            (println "Exercício registrado:")
            (println body2)))))))


(defn ver-extrato []
  (let [resp (client/get (str api-url "/extrato") {:as :json})
        {:keys [alimentos exercicios]} (:body resp)]
    (println "\n--- Alimentos ---")
    (doseq [{:keys [descricao gramas calorias]} alimentos]
      (println (str "- " descricao ", " gramas "g, " (format "%.2f" calorias) " kcal")))
    (println "\n--- Exercícios ---")
    (doseq [{:keys [atividade duracao calorias]} exercicios]
      (println (str "- " atividade ", " duracao " min, " (format "%.2f" calorias) " kcal")))))


(defn menu []
  (loop []
    (println "\n--- Menu Principal ---")
    (println "1 - Ver informacoes do usuario")
    (println "2 - Registrar alimento consumido")
    (println "3 - Registrar exercicio realizado")
    (println "4 - Ver extrato")
    (println "5 - Ver saldo de calorias")
    (println "0 - Sair")
    (print "Escolha uma opcao: ") (flush)
    (case (read-line)
      "1" (do (mostrar-usuario) (recur))
      "2" (do (registrar-alimento) (recur))
      "3" (do (registrar-exercicio) (recur))
      "4" (do (ver-extrato) (recur))
      ;"5" (do (ver-saldo) (recur))
      "0" (println "Saindo...")
      (do (println "Opcao invalida!") (recur)))))

(defn -main []
  (if (usuario-existe?)
    (do
      (println "Bem Vindo!")
      (menu))
    (do
      (cadastrar-usuario)
      (menu))))
