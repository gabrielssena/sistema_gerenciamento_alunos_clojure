(ns integrador.core
  (:require [clojure.string :as str])
  (:gen-class))

(def limite-aprovacao 6.0)

(defn limpar-numero
  [s]
  (when (and s (not (str/blank? s)))
    (let [norm (str/replace s #"," ".")]
      (try
        (Double/parseDouble norm)
        (catch Exception _
          nil)))))

(defn ler-nota!
  []
  (println "Informe a nota de 0 a 10(ou deixe em branco para cancelar):")
  (let [entrada (read-line)]
    (if (str/blank? entrada)
      nil
      (let [v (limpar-numero entrada)]
        (if (and(number? v) (>= v 0) (<= v 10))
          v
          (do
            (println "Digite um numero de o a 10, exemplo: 8.5")
            (recur)))))))

(defn cadastrar-alunos
  [alunos]
  (loop [acc alunos]
    (println "Digite o nome do aluno (ou deixe em branco para encerrar o cadastro):")
    (let [nome (read-line)]
      (if (str/blank? nome)
        (do
          (println "Cadastro finalizado.")
          acc)
        (let [nota (ler-nota!)]
          (if (nil? nota)
            (do
              (println "Nota não informada. Cadastro deste aluno cancelado.")
              (recur acc))
            (let [aluno {:nome nome :nota nota}]
              (println "Aluno cadastrado:" aluno)
              (recur (conj acc aluno)))))))))

(defn aprovado?
  [aluno]
  (>= (:nota aluno) limite-aprovacao))

(defn com-status
  [aluno]
  (assoc aluno :status (if (aprovado? aluno) "Aprovado" "Reprovado")))

(defn media
  [alunos]
  (when (seq alunos)
    (let [soma (reduce + (map :nota alunos))
          qtd  (count alunos)]
      (/ soma qtd))))

(defn fmt-1c
  [x]
  (format "%.1f" (double x)))

(defn imprimir-alunos
  [alunos]
  (if (seq alunos)
    (doseq [a alunos]
      (let [nome (:nome a)
            nota (:nota a)
            status (:status a)]
        (println (str "- " nome " | Nota: " (fmt-1c nota)
                      (when status (str " | Status: " status))))))
    (println "(Nenhum aluno cadastrado)")))

(defn relatorio-notas
  [alunos]
  (println "=== RELATORIO DE NOTAS ===")
  (println "Alunos cadastrados:")
  (imprimir-alunos alunos)
  (println)
  (let [alunos-status (map com-status alunos)]
    (println "Alunos com status:")
    (imprimir-alunos alunos-status)
    (println)
    (let [aprovados (filter (comp #(= % "Aprovado") :status) alunos-status)]
      (println "Apenas aprovados:")
      (imprimir-alunos aprovados)))
  (println)
  (let [m (media alunos)]
    (if m
      (println "Media geral da turma:" (fmt-1c m))
      (println "Media geral da turma: (indisponivel nao ha alunos)")))
  (println "===========================")
  alunos)

(defn estatisticas-gerais
  [alunos]
  (println "=== ESTATISTICAS GERAIS ===")
  (let [total (count alunos)
        aprovados (count (filter aprovado? alunos))
        reprovados (- total aprovados)
        maior (when (seq alunos) (apply max (map :nota alunos)))
        menor (when (seq alunos) (apply min (map :nota alunos)))
        m (media alunos)]
    (println "Total de alunos:" total)
    (println "Aprovados:" aprovados)
    (println "Reprovados:" reprovados)
    (println "Maior nota:" (if maior (fmt-1c maior) "(indisponivel)"))
    (println "Menor nota:" (if menor (fmt-1c menor) "(indisponivel)"))
    (println "Media geral:" (if m (fmt-1c m) "(indisponivel)")))
  (println "============================")
  alunos)

(defn buscar-aluno
  "Desafio extra: busca aluno por nome (case-insensitive) e exibe nota e status."
  [alunos]
  (println "Digite o nome a buscar:")
  (let [q (read-line)
        ql (str/lower-case (or q ""))]
    (if (str/blank? ql)
      (println "Busca cancelada (nome em branco).")
      (let [match (some #(when (= (str/lower-case (:nome %)) ql) %) alunos)]
        (if match
          (let [a (com-status match)]
            (println "Encontrado:")
            (println (str "- " (:nome a) " | Nota: " (fmt-1c (:nota a)) " | Status: " (:status a))))
          (println "Nenhum aluno encontrado com esse nome.")))))
  alunos)

(defn mostrar-menu!
  []
  (println "=== MENU PRINCIPAL ===")
  (println "1 - Cadastrar Alunos")
  (println "2 - Relatorio de Notas")
  (println "3 - Estatisticas Gerais")
  (println "4 - Buscar aluno pelo nome")
  (println "0 - Sair")
  (println "Escolha uma opcao:"))

(defn -main
  [& _args]
  (loop [alunos []]
    (mostrar-menu!)
    (let [op (read-line)]
      (cond
        (= op "1") (recur (cadastrar-alunos alunos))
        (= op "2") (recur (relatorio-notas alunos))
        (= op "3") (recur (estatisticas-gerais alunos))
        (= op "4") (recur (buscar-aluno alunos))
        (= op "0") (do
                      (println "O sistema vai ser fechado...")
                      nil)
        :else (do
                (println "Opcao invalida. Tente novamente.")
                (recur alunos))))))
