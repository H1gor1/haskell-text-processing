import Data.Char (toLower, isAlpha, isSpace, isUpper)
import qualified Data.Map as Map
import Data.List (nub, sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.IO

-- ===================== QUESTÃO 1 =====================
-- 1. Escreva uma função que receba uma string contendo uma frase e retorne uma lista de palavras.

-- Recebe uma string e divide em palavras usando espaços como delimitador
-- 1. Se string vazia, retorna lista vazia
-- 2. Se começa com espaço, remove espaço e continua recursivamente  
-- 3. Caso contrário, constrói primeira palavra e processa resto da string
separaPalavras :: String -> [String]
separaPalavras [] = []
separaPalavras str 
  | head str == ' ' = separaPalavras (tail str)
  | otherwise = palavra : separaPalavras restoStr
  where
    palavra = construirPalavra str
    restoStr = pularPalavraEEspacos str

-- Constrói uma palavra coletando caracteres até encontrar um espaço
-- 1. String vazia retorna string vazia
-- 2. Se encontra espaço, para e retorna string vazia
-- 3. Caso contrário, adiciona caractere atual e continua recursivamente
construirPalavra :: String -> String
construirPalavra [] = []
construirPalavra (x:xs)
  | x == ' ' = []
  | otherwise = x : construirPalavra xs

-- Pula uma palavra completa e todos os espaços que vêm depois dela
-- 1. String vazia retorna vazia
-- 2. Se encontra espaço, chama função para pular espaços restantes
-- 3. Caso contrário, continua pulando caracteres da palavra atual
pularPalavraEEspacos :: String -> String
pularPalavraEEspacos [] = []
pularPalavraEEspacos (x:xs)
  | x == ' ' = pularEspacos xs
  | otherwise = pularPalavraEEspacos xs

-- Remove todos os espaços consecutivos do início de uma string
-- 1. String vazia retorna vazia
-- 2. Se encontra espaço, continua pulando recursivamente
-- 3. Quando encontra não-espaço, retorna resto da string
pularEspacos :: String -> String
pularEspacos [] = []
pularEspacos (x:xs)
  | x == ' ' = pularEspacos xs
  | otherwise = x:xs

-- ===================== QUESTÃO 2 =====================
-- 2. Crie uma função que receba uma palavra e retorne sua versão em minúsculas e sem acentos.

-- Normaliza texto convertendo para minúsculas e removendo acentos
-- 1. Aplica composição de funções: primeiro remove acentos, depois converte para minúscula
-- 2. Mapeia essa composição sobre cada caractere da string
normaliza :: String -> String
normaliza = map (toLower . removerAcentuacao)

-- Remove acentos de um caractere específico usando pattern matching
-- 1. Verifica se caractere corresponde a algum caractere acentuado
-- 2. Se corresponder, retorna versão sem acento
-- 3. Caso contrário, retorna caractere original
removerAcentuacao :: Char -> Char
removerAcentuacao c = case c of
  'á' -> 'a'; 'à' -> 'a'; 'ã' -> 'a'; 'â' -> 'a'; 'ä' -> 'a'
  'Á' -> 'a'; 'À' -> 'a'; 'Ã' -> 'a'; 'Â' -> 'a'; 'Ä' -> 'a'
  'é' -> 'e'; 'è' -> 'e'; 'ê' -> 'e'; 'ë' -> 'e'
  'É' -> 'e'; 'È' -> 'e'; 'Ê' -> 'e'; 'Ë' -> 'e'
  'í' -> 'i'; 'ì' -> 'i'; 'î' -> 'i'; 'ï' -> 'i'
  'Í' -> 'i'; 'Ì' -> 'i'; 'Î' -> 'i'; 'Ï' -> 'i'
  'ó' -> 'o'; 'ò' -> 'o'; 'õ' -> 'o'; 'ô' -> 'o'; 'ö' -> 'o'
  'Ó' -> 'o'; 'Ò' -> 'o'; 'Õ' -> 'o'; 'Ô' -> 'o'; 'Ö' -> 'o'
  'ú' -> 'u'; 'ù' -> 'u'; 'û' -> 'u'; 'ü' -> 'u'
  'Ú' -> 'u'; 'Ù' -> 'u'; 'Û' -> 'u'; 'Ü' -> 'u'
  'ç' -> 'c'; 'Ç' -> 'c'
  'ñ' -> 'n'; 'Ñ' -> 'n'
  _   -> c

-- ===================== QUESTÃO 3 =====================
-- 3. Escreva uma função que remova todos os caracteres que não são letras nem espaços de uma string.

-- Filtra string mantendo apenas letras e espaços
-- 1. String vazia retorna vazia
-- 2. Verifica se caractere atual é letra ou espaço
-- 3. Se for, inclui na saída e continua recursivamente
-- 4. Se não for, pula caractere e continua recursivamente
removeNonAlpha :: String -> String
removeNonAlpha [] = []
removeNonAlpha (x:xs)
  | isAlpha x || isSpace x = x : removeNonAlpha xs
  | otherwise = removeNonAlpha xs

-- ===================== QUESTÃO 4 =====================
-- 4. Crie uma função que receba uma string com várias linhas e retorne uma lista de parágrafos.

-- Divide string em linhas usando '\n' como separador
-- 1. String vazia retorna lista vazia
-- 2. Pega caracteres até encontrar '\n' (primeira linha)
-- 3. Pula o '\n' e processa recursivamente o resto
-- 4. Se não há mais '\n', adiciona linha final
linhas :: String -> [String]
linhas [] = []
linhas str = primeiraLinha : linhas resto
  where
    primeiraLinha = takeWhile (/= '\n') str
    resto = if null restoComQuebra then [] else tail restoComQuebra
    restoComQuebra = dropWhile (/= '\n') str

-- Agrupa linhas consecutivas não-vazias em parágrafos
-- 1. Lista vazia retorna lista vazia
-- 2. Identifica sequência de linhas não-vazias como um parágrafo
-- 3. Pula linhas vazias entre parágrafos
-- 4. Processa recursivamente o resto das linhas
agruparLinhasEmParagrafos :: [String] -> [[String]]
agruparLinhasEmParagrafos [] = []
agruparLinhasEmParagrafos linhasList =
  let linhasNaoVazias = [l | l <- linhasList, not (all isSpace l) && not (null l)]
      (paragrafo, resto) = span (\l -> not (all isSpace l) && not (null l)) linhasList
      restoLimpo = dropWhile (\l -> all isSpace l || null l) resto
  in if null paragrafo
     then agruparLinhasEmParagrafos restoLimpo  
     else paragrafo : agruparLinhasEmParagrafos restoLimpo

-- ===================== QUESTÃO 5 =====================
-- 5. Escreva uma função que receba uma lista de strings e retorne a contagem de palavras.

-- Divide string em palavras usando espaços como separador
-- 1. String vazia retorna lista vazia
-- 2. Pega caracteres até encontrar espaço (primeira palavra)
-- 3. Remove espaços consecutivos e processa recursivamente o resto
palavras :: String -> [String]
palavras [] = []
palavras str = primeiraPalavra : palavras resto
  where
    primeiraPalavra = takeWhile (/= ' ') str
    resto = dropWhile (== ' ') (dropWhile (/= ' ') str)

-- Conta total de palavras não-vazias em uma lista de frases
-- 1. Para cada frase na lista, divide em palavras
-- 2. Filtra apenas palavras não-vazias
-- 3. Conta quantas palavras restaram no total
contarPalavras :: [String] -> Int
contarPalavras lista = length [p | frase <- lista, p <- palavras frase, not (null p)]

-- ===================== QUESTÃO 6 =====================
-- 6. Escreva uma função que conte a frequência de cada palavra em uma lista de palavras.

-- Calcula frequência de cada palavra usando list comprehension
-- 1. Gera lista de palavras únicas com 'nub'
-- 2. Para cada palavra única, conta quantas vezes aparece na lista original
-- 3. Retorna lista de tuplas (palavra, frequência)
frequenciaPalavras :: [String] -> [(String, Int)]
frequenciaPalavras str = 
  [(palavra, length [p | p <- str, p == palavra]) | palavra <- nub str]

-- ===================== QUESTÃO 7 =====================
-- 7. Reescreva o exercício anterior usando Data.Map para fazer a contagem de forma eficiente.

-- Calcula frequência usando Map para eficiência
-- 1. Inicia com Map vazio
-- 2. Para cada palavra, incrementa contador no Map (ou inicia com 1)
-- 3. Converte Map final para lista de tuplas
frequenciaMap :: [String] -> [(String, Int)]
frequenciaMap palavras = Map.toList $ foldr contarPalavra Map.empty palavras
  where
    contarPalavra palavra mapa = Map.insertWith (+) palavra 1 mapa

-- ===================== QUESTÃO 8 =====================
-- 8. Crie uma função que ordene a lista de pares (palavra, contagem) pela contagem em ordem decrescente.

-- Ordena lista de (palavra, frequência) por frequência usando quicksort
-- 1. Lista vazia retorna vazia
-- 2. Pega primeiro elemento como pivot
-- 3. Separa elementos com frequência >= pivot (maiores)
-- 4. Separa elementos com frequência < pivot (menores)
-- 5. Recursivamente ordena ambas as partes e concatena: maiores ++ pivot ++ menores
ordenarPorFrequencia :: [(String, Int)] -> [(String, Int)]
ordenarPorFrequencia [] = []
ordenarPorFrequencia ((p, n):xs) =
  let maiores  = ordenarPorFrequencia [(p', n') | (p', n') <- xs, n' >= n]
      menores  = ordenarPorFrequencia [(p', n') | (p', n') <- xs, n' <  n]
  in maiores ++ [(p, n)] ++ menores

-- ===================== QUESTÃO 9 =====================
-- 9. Dada uma lista de palavras e uma lista de stopwords, escreva uma função que filtre as palavras.

-- Lista predefinida de palavras irrelevantes (artigos, preposições, etc.)
-- Contém palavras comuns do português que geralmente não são significativas
-- para análise de conteúdo
stopwords :: [String]
stopwords =
  [ "a", "à", "ao", "aos", "as", "às"
  , "com", "como", "da", "das", "de", "dela", "dele", "deles", "delas"
  , "do", "dos", "e", "é", "em", "entre", "era", "eram"
  , "essa", "esse", "isso", "isto", "há", "lhe", "lhes", "mais"
  , "mas", "me", "mesmo", "meu", "minha", "muito", "na", "nas", "não"
  , "nem", "no", "nos", "nós", "nossa", "o", "os", "ou", "para"
  , "pela", "pelas", "pelo", "pelos", "por", "qual", "que", "quem"
  , "se", "sem", "seu", "sua", "são", "só", "também", "te", "tem"
  , "tendo", "tenho", "tinha", "tinham", "todo", "tudo", "um", "uma"
  , "você", "vocês", "àquela", "àquele", "aquilo", "aquele", "aquela"
  ]

-- Remove stopwords de uma lista de palavras
-- 1. Recebe lista de stopwords e lista de palavras
-- 2. Usa list comprehension para manter apenas palavras que não estão na lista de stopwords
-- 3. Usa 'notElem' para verificar se palavra não está presente nas stopwords
filtrarStopwords :: [String] -> [String] -> [String]
filtrarStopwords stopwords palavras = [p | p <- palavras, p notElem stopwords]

-- ===================== QUESTÃO 10 =====================
-- 10. Escreva uma função que processe um parágrafo, retornando a lista de palavras ordenadas por frequência.

-- Pipeline completo de processamento de um parágrafo
-- 1. Remove caracteres não-alfabéticos mantendo espaços
-- 2. Normaliza texto (minúsculas e sem acentos)
-- 3. Divide em palavras individuais
-- 4. Remove stopwords da lista de palavras
-- 5. Calcula frequência usando Map
-- 6. Ordena por frequência decrescente
processarParagrafo :: String -> [(String, Int)]
processarParagrafo texto =
  let textoLimpo     = removeNonAlpha texto
      textoNormal    = normaliza textoLimpo
      palavrasList   = palavras textoNormal
      palavrasFiltradas = filtrarStopwords stopwords palavrasList
      frequencias    = frequenciaMap palavrasFiltradas
  in ordenarPorFrequencia frequencias

-- ===================== QUESTÃO 11 =====================
-- 11. Crie uma função que leia o conteúdo de um arquivo texto e o retorne como uma única string.

-- Lê conteúdo completo de um arquivo como uma única string
-- Usa função readFile do Haskell que retorna IO String
lerArquivo :: String -> IO String
lerArquivo nomeArquivo = readFile nomeArquivo

-- ===================== QUESTÃO 12 =====================
-- 12. Faça uma função que leia um arquivo e divida seu conteúdo em parágrafos.

-- Divide conteúdo do arquivo em parágrafos, retornando lista de palavras por parágrafo
-- 1. Divide conteúdo em linhas
-- 2. Agrupa linhas em parágrafos (separados por linhas vazias)
-- 3. Para cada parágrafo, concatena as palavras de todas as linhas
-- 4. Filtra parágrafos vazios
dividirEmParagrafos :: String -> [[String]]
dividirEmParagrafos conteudo = 
  [concatMap palavras linhasParagrafo | linhasParagrafo <- gruposLinhas, not (null linhasParagrafo)]
  where
    todasLinhas = linhas conteudo
    gruposLinhas = agruparLinhasEmParagrafos todasLinhas

-- ===================== QUESTÃO 13 =====================
-- 13. Para cada parágrafo, aplique a função do exercício 10 e imprima o resultado.

-- Processa cada parágrafo individualmente e imprime resultados numerados
-- 1. Converte grupos de linhas de volta para strings de parágrafo
-- 2. Aplica processarParagrafo em cada parágrafo
-- 3. Enumera parágrafos começando de 1
-- 4. Imprime cabeçalho, lista de (palavra, frequência) e linha em branco
processarCadaParagrafo :: String -> IO ()
processarCadaParagrafo conteudo = 
  let paragrafosTexto = [unlines grupo | grupo <- agruparLinhasEmParagrafos (linhas conteudo)]
      resultados = [processarParagrafo par | par <- paragrafosTexto]
  in mapM_ (\(i, pares) -> do
       putStrLn $ "=== PARÁGRAFO " ++ show i ++ " ==="
       mapM_ (\(palavra, freq) -> putStrLn $ show (palavra, freq)) pares
       putStrLn ""
     ) (zip [1..] resultados)

-- ===================== QUESTÃO 14 =====================
-- 14. Implemente um programa principal (main) que leia o nome de um arquivo como argumento.

-- Função main que lê argumentos da linha de comando
-- 1. Obtém argumentos da linha de comando
-- 2. Se exatamente um argumento, trata como nome do arquivo
-- 3. Lê arquivo e processa cada parágrafo
-- 4. Mostra mensagem de uso para 0 ou mais argumentos
mainArquivo :: IO ()
mainArquivo = do
  args <- getArgs
  case args of
    [nomeArquivo] -> do
      conteudo <- lerArquivo nomeArquivo
      processarCadaParagrafo conteudo
    [] -> putStrLn "Uso: programa <nome_do_arquivo>"
    _ -> putStrLn "Muitos argumentos. Uso: programa <nome_do_arquivo>"

-- ===================== QUESTÃO 15 =====================
-- 15. Escreva uma função que, dado um parágrafo, diga quantas palavras únicas ele contém.

-- Conta quantidade de palavras únicas em um parágrafo
-- 1. Remove caracteres não-alfabéticos do parágrafo
-- 2. Normaliza texto (minúsculas, sem acentos)
-- 3. Divide em palavras e filtra palavras vazias
-- 4. Remove duplicatas com 'nub' e conta o tamanho da lista
palavrasUnicas :: String -> Int
palavrasUnicas paragrafo = 
  length $ nub [palavra | palavra <- palavras (normaliza (removeNonAlpha paragrafo)), not (null palavra)]

-- ===================== QUESTÃO 16 =====================
-- 16. Escreva uma função que transforme uma string como "ação! interação. ação?" em ["acao", "interacao", "acao"].

-- Transforma texto com pontuação em lista de palavras normalizadas
-- 1. Remove caracteres não-alfabéticos mantendo espaços
-- 2. Normaliza texto (minúsculas, sem acentos)
-- 3. Divide em palavras individuais
-- 4. Filtra palavras vazias
transformarTexto :: String -> [String]
transformarTexto texto = 
  [palavra | palavra <- palavras (normaliza (removeNonAlpha texto)), not (null palavra)]

-- ===================== QUESTÃO 17 =====================
-- 17. Escreva uma função que simule o carregamento de uma lista de stopwords a partir de uma constante.

-- Retorna lista de stopwords predefinida
-- Simplesmente retorna a constante stopwords definida anteriormente
carregarStopwords :: [String]
carregarStopwords = stopwords

-- ===================== QUESTÃO 18 =====================
-- 18. Escreva uma função que conte quantas palavras começam com letra maiúscula em um parágrafo.

-- Conta palavras que começam com letra maiúscula (antes da normalização)
-- 1. Remove apenas pontuação, mantendo letras e espaços
-- 2. Divide em palavras sem normalizar (preserva maiúsculas)
-- 3. Filtra palavras não-vazias que começam com maiúscula
-- 4. Conta quantas palavras restaram
contarMaiusculas :: String -> Int
contarMaiusculas paragrafo = 
  length [palavra | palavra <- palavras (removeNonAlpha paragrafo), 
          not (null palavra), isUpper (head palavra)]

-- ===================== QUESTÃO 19 =====================
-- 19. Escreva uma função que receba o conteúdo completo de um arquivo e retorne o número de parágrafos.

-- Conta número total de parágrafos no texto
-- 1. Divide conteúdo em linhas
-- 2. Agrupa linhas em parágrafos
-- 3. Conta quantos grupos de parágrafos foram formados
contarParagrafos :: String -> Int
contarParagrafos conteudo = length [grupo | grupo <- agruparLinhasEmParagrafos (linhas conteudo)]

-- ===================== QUESTÃO 20 =====================
-- 20. Modele uma função que receba um texto de entrada e produza um relatório.

-- Gera relatório com estatísticas gerais do texto
-- 1. Conta número de parágrafos
-- 2. Processa todas as linhas para extrair e normalizar palavras
-- 3. Filtra palavras vazias e conta total
-- 4. Remove duplicatas para contar palavras únicas
-- Retorna tupla com (parágrafos, total_palavras, palavras_únicas)
relatorioCompleto :: String -> (Int, Int, Int)
relatorioCompleto conteudo = (numParagrafos, totalPalavras, totalUnicas)
  where
    numParagrafos = contarParagrafos conteudo
    todasPalavras = concatMap (palavras . normaliza . removeNonAlpha) (linhas conteudo)
    palavrasLimpas = [p | p <- todasPalavras, not (null p)]
    totalPalavras = length palavrasLimpas
    totalUnicas = length (nub palavrasLimpas)

-- ===================== QUESTÃO 21 =====================
-- 21. Dado um parágrafo com pontuação e palavras repetidas, escreva uma função que retorne a lista das 3 palavras mais frequentes.

-- Retorna as 3 palavras mais frequentes de um parágrafo
-- 1. Aplica processamento completo do parágrafo (que já ordena por frequência)
-- 2. Pega apenas os 3 primeiros elementos da lista ordenada
top3Palavras :: String -> [(String, Int)]
top3Palavras paragrafo = take 3 (processarParagrafo paragrafo)

-- ===================== QUESTÃO 22 =====================
-- 22. Crie uma função que, dada uma palavra, diga se ela é composta apenas por letras.

-- Verifica se string contém apenas caracteres alfabéticos
-- 1. Aplica isAlpha em cada caractere da palavra
-- 2. Usa 'and' para verificar se todos os resultados são True
apenasLetras :: String -> Bool
apenasLetras palavra = and [isAlpha c | c <- palavra]

-- ===================== QUESTÃO 23 =====================
-- 23. Implemente uma função que remove pontuação e transforma texto em minúsculas usando map e filter.

-- Limpa texto removendo pontuação e convertendo para minúsculas
-- 1. Filtra caracteres mantendo apenas letras e espaços
-- 2. Mapeia toLower sobre todos os caracteres restantes
limparTexto :: String -> String
limparTexto texto = 
  map toLower $ filter (\c -> isAlpha c || isSpace c) texto

-- ===================== QUESTÃO 24 =====================
-- 24. Faça um pequeno texto de teste (3 parágrafos) e use suas funções para gerar a saída do trabalho prático.

-- Texto de exemplo para demonstrações e testes
-- Contém 3 parágrafos sobre programação funcional e Haskell
textoTeste :: String
textoTeste = "Programação funcional é incrível! A programação em Haskell permite escrever código elegante e expressivo.\n\nHaskell usa conceitos como funções puras e imutabilidade. As funções são cidadãos de primeira classe no paradigma funcional.\n\nA comunidade Haskell é muito acolhedora e disposta a ajudar. Programação funcional está ganhando popularidade no mundo todo da tecnologia."

-- Demonstra funcionalidades usando texto de teste
-- 1. Imprime texto original
-- 2. Gera e exibe relatório completo
-- 3. Processa cada parágrafo individualmente mostrando análise detalhada
demonstracao :: IO ()
demonstracao = do
  putStrLn "=== DEMONSTRAÇÃO COM TEXTO TESTE ==="
  putStrLn "Texto original:"
  putStrLn textoTeste
  putStrLn ("\n" ++ replicate 50 '=')
  
  let (pars, total, unicas) = relatorioCompleto textoTeste
  putStrLn $ "Relatório: " ++ show pars ++ " parágrafos, " ++ 
             show total ++ " palavras totais, " ++ show unicas ++ " únicas"
  
  putStrLn "\nProcessamento por parágrafo:"
  processarCadaParagrafo textoTeste

-- ===================== QUESTÃO 25 =====================
-- 25. Escreva uma função que substitui todos os caracteres acentuados em uma string usando mapeamento manual.

-- Aplica removerAcentuacao em todos os caracteres de uma string
-- Mapeia a função removerAcentuacao sobre cada caractere da string
substituirAcentos :: String -> String
substituirAcentos texto = [removerAcentuacao c | c <- texto]

-- ===================== QUESTÃO 26 =====================
-- 26. Implemente uma função que leia a entrada padrão e conte as palavras digitadas até encontrar uma linha em branco.

-- Lê linhas da entrada padrão até encontrar linha vazia
-- 1. Solicita entrada do usuário
-- 2. Lê linhas recursivamente até linha vazia
-- 3. Conta palavras totais em todas as linhas lidas
-- 4. Retorna número total de palavras
lerEntradaPadrao :: IO Int
lerEntradaPadrao = do
  putStrLn "Digite texto (linha em branco para parar):"
  conteudo <- lerLinhasAteVazia []
  return $ length $ concatMap palavras conteudo

-- Função auxiliar que lê linhas recursivamente até linha vazia
-- 1. Lê uma linha da entrada
-- 2. Se linha vazia, retorna acumulador invertido
-- 3. Caso contrário, adiciona linha ao acumulador e continua
lerLinhasAteVazia :: [String] -> IO [String]
lerLinhasAteVazia acc = do
  linha <- getLine
  if null linha
    then return (reverse acc)
    else lerLinhasAteVazia (linha : acc)

-- ===================== QUESTÃO 27 =====================
-- 27. Modele uma função que, usando Map, retorne a palavra mais frequente em um texto.

-- Encontra a palavra mais frequente em um texto
-- 1. Processa parágrafo para obter lista ordenada por frequência
-- 2. Se lista vazia, retorna Nothing
-- 3. Caso contrário, retorna Just com primeiro elemento (mais frequente)
palavraMaisFrequente :: String -> Maybe (String, Int)
palavraMaisFrequente texto = 
  if null frequencias then Nothing
  else Just (head frequencias)
  where frequencias = processarParagrafo texto

-- ===================== QUESTÃO 28 =====================
-- 28. Escreva uma função que exiba uma palavra e sua frequência apenas se ela aparecer mais de uma vez.

-- Filtra palavras que aparecem mais de uma vez
-- 1. Processa parágrafo para obter frequências
-- 2. Filtra apenas tuplas onde frequência > 1
palavrasRepetidas :: String -> [(String, Int)]
palavrasRepetidas texto = 
  [(palavra, freq) | (palavra, freq) <- processarParagrafo texto, freq > 1]

-- ===================== QUESTÃO 29 =====================
-- 29. Implemente uma função que calcule a proporção entre palavras relevantes e o total de palavras no texto.

-- Calcula proporção de palavras relevantes (não-stopwords) no texto
-- 1. Normaliza e divide texto em palavras limpas
-- 2. Conta total de palavras não-vazias
-- 3. Filtra stopwords e conta palavras relevantes
-- 4. Calcula e retorna proporção (palavras_relevantes / total_palavras)
proporcaoRelevantes :: String -> Double
proporcaoRelevantes texto = 
  if totalPalavras == 0 then 0.0
  else fromIntegral palavrasRelevantes / fromIntegral totalPalavras
  where
    todasPalavras = palavras (normaliza (removeNonAlpha texto))
    palavrasLimpas = [p | p <- todasPalavras, not (null p)]
    totalPalavras = length palavrasLimpas
    palavrasRelevantes = length (filtrarStopwords stopwords palavrasLimpas)

-- ===================== QUESTÃO 30 =====================
-- 30. Modele um pequeno pipeline com as funções criadas para processar uma string completa (de leitura até exibição final formatada).

-- Executa pipeline completo de análise e formata resultado como string
-- 1. Gera relatório geral (parágrafos, palavras totais, palavras únicas, proporção relevantes)
-- 2. Para cada parágrafo individualmente, calcula estatísticas específicas
-- 3. Formata todas as informações em string estruturada com seções numeradas
-- 4. Retorna análise completa como string multilinha pronta para impressão
pipelineCompleto :: String -> String
pipelineCompleto conteudo = unlines $
  [ "=== PIPELINE DE PROCESSAMENTO COMPLETO ==="
  , ""
  , "1. RELATÓRIO GERAL:"
  , let (pars, total, unicas) = relatorioCompleto conteudo
    in "   Parágrafos: " ++ show pars ++ ", Palavras totais: " ++ show total ++
       ", Palavras únicas: " ++ show unicas
  , "   Proporção relevantes: " ++ show (proporcaoRelevantes conteudo)
  , ""
  , "2. ANÁLISE POR PARÁGRAFO:"
  ] ++
  concatMap (\(i, par) ->
    [ "   Parágrafo " ++ show i ++ ":"
    , "     Palavras únicas: " ++ show (palavrasUnicas par)
    , "     Palavras maiúsculas: " ++ show (contarMaiusculas par)
    , "     Top 3: " ++ show (top3Palavras par)
    , "     Repetidas: " ++ show (palavrasRepetidas par)
    , ""
    ]) (zip [1..] paragrafosTexto)
  where
    paragrafosTexto = [unlines grupo | grupo <- agruparLinhasEmParagrafos (linhas conteudo)]

-- ===================== QUESTÃO 31 =====================
-- 31. Teste sua implementação com o conteúdo de um arquivo '.txt' real e descreva os resultados em termos de contagem e filtragem.

-- Executa análise completa em arquivo especificado com relatório detalhado
-- 1. Lê conteúdo completo do arquivo especificado
-- 2. Executa e imprime pipeline completo formatado
-- 3. Executa análise detalhada processando cada parágrafo individualmente
-- 4. Calcula e imprime estatísticas finais resumidas para avaliação
testeComArquivo :: String -> IO ()
testeComArquivo nomeArquivo = do
  putStrLn $ "=== TESTE COM ARQUIVO: " ++ nomeArquivo ++ " ==="
  conteudo <- lerArquivo nomeArquivo
  putStrLn (pipelineCompleto conteudo)
  putStrLn "\n=== ANÁLISE DETALHADA ==="
  processarCadaParagrafo conteudo
  putStrLn "\n=== ESTATÍSTICAS FINAIS ==="
  let (pars, total, unicas) = relatorioCompleto conteudo
  putStrLn $ "Total de parágrafos: " ++ show pars
  putStrLn $ "Total de palavras: " ++ show total  
  putStrLn $ "Palavras únicas: " ++ show unicas
  putStrLn $ "Proporção de palavras relevantes: " ++ show (proporcaoRelevantes conteudo)

-- ===================== MAIN PRINCIPAL =====================
-- Função main interativa que oferece menu de opções ao usuário
-- 1. Apresenta menu com 4 opções diferentes de processamento
-- 2. Lê escolha do usuário da entrada padrão
-- 3. Executa ação correspondente baseada na escolha numérica
-- 4. Trata opção inválida com mensagem de erro apropriada
main :: IO ()
main = do
  putStrLn "Escolha uma opção:"
  putStrLn "1 - Demonstração com texto teste"
  putStrLn "2 - Processar arquivo (digite o nome)"
  putStrLn "3 - Ler entrada padrão"
  putStrLn "4 - Usar argumentos da linha de comando"
  opcao <- getLine
  case opcao of
    "1" -> demonstracao
    "2" -> do
      putStrLn "Digite o nome do arquivo:"
      arquivo <- getLine
      testeComArquivo arquivo
    "3" -> do
      numPalavras <- lerEntradaPadrao
      putStrLn $ "Total de palavras digitadas: " ++ show numPalavras
    "4" -> mainArquivo
    _   -> putStrLn "Opção inválida!"
