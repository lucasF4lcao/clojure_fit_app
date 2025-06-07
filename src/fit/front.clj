(ns fit.front
  (:require [clj-http.client :as client]
            [clojure.edn :as edn]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.string :as str]))

(def api-url "http://localhost:3000")

(defn ler-numero []
  (Integer/parseInt (read-line)))


(defn obter-data-hoje []
  (let [formatter (f/formatter "yyyy-MM-dd")]
    (f/unparse formatter (t/now))))

(defn ler-data []
  (println "Digite a data (yyyy-MM-dd) ou pressione Enter para usar a data de hoje:")
  (let [entrada (read-line)]
    (if (clojure.string/blank? entrada)
      (obter-data-hoje)
      entrada)))


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


(defn imprimir-opcoes [opcoes imprimir-fn]
  (mapv imprimir-fn opcoes))

(defn registrar-alimento []
  (println "Digite o nome do alimento:")
  (let [descricao (read-line)
        resp (client/post (str api-url "/alimento")
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string {:descricao descricao})
                           :as :json})
        opcoes (get-in resp [:body :opcoes])]

    (if (empty? opcoes)
      (println "Nenhuma opcao encontrada.")
      (do
        (println "Escolha uma opcao:")
        (imprimir-opcoes opcoes
                         (fn [{:keys [id descricao quantidade calorias]}]
                           (println (str id ") " descricao " - " quantidade " - " calorias))))

        (let [opcao (ler-numero)]
          (println "Digite a gramatura (em gramas):")
          (let [gramas (Double/parseDouble (read-line))
                data (ler-data)
                resp2 (client/post (str api-url "/alimento")
                                   {:headers {"Content-Type" "application/json"}
                                    :body (json/generate-string {:descricao descricao
                                                                 :opcao opcao
                                                                 :gramas gramas
                                                                 :data data})
                                    :as :json})
                body2 (:body resp2)]
            (println "Alimento registrado:")
            (println body2)))))))

(defn registrar-exercicio []
  (println "Digite o nome do exercicio:")
  (let [atividade (read-line)
        resp (client/post (str api-url "/exercicio")
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string {:atividade atividade})
                           :as :json})
        opcoes (get-in resp [:body :opcoes])]

    (if (empty? opcoes)
      (println "Nenhuma opcao encontrada.")
      (do
        (println "Escolha uma opcao:")
        (imprimir-opcoes opcoes
                         (fn [{:keys [id nome calorias]}]
                           (println (str id ") " nome " - " calorias " kcal/h"))))

        (let [opcao (ler-numero)]
          (println "Digite a duracao (em minutos):")
          (let [duracao (Double/parseDouble (read-line))
                data (ler-data)
                resp2 (client/post (str api-url "/exercicio")
                                   {:headers {"Content-Type" "application/json"}
                                    :body (json/generate-string {:atividade atividade
                                                                 :opcao opcao
                                                                 :duracao duracao
                                                                 :data data})
                                    :as :json})
                body2 (:body resp2)]
            (println "Exercicio registrado:")
            (println body2)))))))


(defn ver-extrato []
  (print "Inicio do periodo: ")
  (let [inicio (ler-data)]
    (print "Fim do periodo: ")
    (let [fim (ler-data)
          resp (client/get (str api-url "/extrato")
                           {:as :json
                            :query-params {"inicio" inicio "fim" fim}})
          {:keys [alimentos exercicios]} (:body resp)]
      (println "\n--- Alimentos ---")
      (if (empty? alimentos)
        (println "Nenhum alimento registrado no periodo.")
        (doseq [{:keys [descricao gramas calorias data]} alimentos]
          (println (str "- " descricao ", " gramas "g, " (format "%.2f" calorias) " kcal | [" data "]"))))

      (println "\n--- Exercicios ---")
      (if (empty? exercicios)
        (println "Nenhum exercicio registrado no periodo.")
        (doseq [{:keys [atividade duracao calorias data]} exercicios]
          (println (str "- " atividade ", " duracao " min, " (format "%.2f" calorias) " kcal | [" data "]")))))))


(defn ver-saldo []
  (print "Inicio do periodo: ")
  (let [inicio (ler-data)]
    (print "Fim do periodo: ")
    (let [fim (ler-data)
          resp (client/get (str api-url "/saldo")
                           {:as :json
                            :query-params {"inicio" inicio "fim" fim}})
          {:keys [consumido queimado saldo]} (:body resp)]
      (println "\n--- Saldo de Calorias ---")
      (println (str "Consumido: " consumido " kcal"))
      (println (str "Queimado:  " queimado " kcal"))
      (println (str "Saldo:     " saldo " kcal")))))


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
      "5" (do (ver-saldo) (recur))
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
