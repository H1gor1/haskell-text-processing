# Programa de Análise de Frequência de Palavras em Haskell

![Haskell Logo](https://www.haskell.org/img/haskell-logo.svg)

Este programa em Haskell realiza análise de frequência de palavras em textos, processando arquivos de entrada e gerando relatórios por parágrafo com palavras ordenadas por frequência. Desenvolvido como trabalho acadêmico para prática de programação funcional.

---

## ✨ Funcionalidades Principais

- 📝 Processamento de arquivos texto com parágrafos separados por linhas em branco  
- 🔍 Filtragem automática de *stopwords* (palavras irrelevantes)  
- 🔠 Normalização de texto (minúsculas e sem acentos)  
- 📊 Contagem eficiente de frequência usando `Data.Map`  
- 🔽 Ordenação de palavras por frequência decrescente  
- 📈 Geração de relatórios por parágrafo  
- 📋 Estatísticas gerais do texto (total de palavras, palavras únicas, etc.)  

---

## 🖥️ Requisitos do Sistema

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) versão **8.0** ou superior  
- Stack (opcional, para gerenciamento de dependências)  

---

## ⚙️ Instalação

### Windows

1. Baixe o instalador do [Haskell Platform](https://www.haskell.org/platform/)
2. Siga as instruções do instalador

### Linux (Debian/Ubuntu)

```bash
sudo apt-get update
sudo apt-get install ghc
```

### macOS (via Homebrew)

```bash
brew install ghc
```

---

## 🛠️ Compilação

Navegue até o diretório do projeto e execute:

```bash
ghc -o analisador Analisador.hs
```

Isso gerará um executável chamado `analisador`.

---

## ▶️ Uso

### Modo com argumento de linha de comando

```bash
./analisador arquivo.txt
```

**Exemplo:**

```bash
./analisador texto.txt
```

### Modo interativo

Execute sem argumentos:

```bash
./analisador
```

**Menu de opções:**

```
1 - Demonstração com texto teste  
2 - Processar arquivo (digite o nome)  
3 - Ler entrada padrão  
4 - Usar argumentos da linha de comando  
```

---

## 📄 Formato de Entrada

- Arquivos `.txt` com codificação **UTF-8**
- Parágrafos separados por uma ou mais linhas em branco
- Suporte a acentuação e caracteres especiais do português

**Exemplo (`texto.txt`):**

```
A ação é essencial. Ação, reação, e interação!

Outra linha. Outra ação, outra reação.
```

---

## 📤 Formato de Saída

Para cada parágrafo, o programa imprime:

```
=== PARÁGRAFO N ===
("palavra1", frequência)
("palavra2", frequência)
...
```

**Exemplo:**

```
=== PARÁGRAFO 1 ===
("acao",3)
("essencial",1)
("reacao",1)
("interacao",1)

=== PARÁGRAFO 2 ===
("outra",3)
("linha",1)
("acao",1)
("reacao",1)
```

---

## 🧠 Funcionalidades Avançadas

### Estatísticas do texto:

- Número de parágrafos  
- Total de palavras  
- Palavras únicas  
- Proporção de palavras relevantes  

### Análise por parágrafo:

- Palavras únicas  
- Palavras em maiúsculo  
- Top 3 palavras mais frequentes  
- Palavras repetidas  

---

## 🧩 Personalização

Para editar a lista de *stopwords*, modifique a constante `stopwords` no arquivo `Analisador.hs` (por volta da linha 170):

```haskell
stopwords :: [String]
stopwords =
  [ "a", "à", "ao", "aos", "as", "às"
  , "com", "como", "da", "das", "de", "dela", "dele", "deles", "delas"
  -- ... adicione ou remova palavras conforme necessário
  ]
```

---

## 🧪 Testes

O programa inclui um texto de demonstração interno.

Para testar:

```bash
./analisador
```

Escolha a opção `1` (Demonstração com texto teste).

---

## 📁 Estrutura de Arquivos

```
analisador-frequencia/
├── Analisador.hs       # Código-fonte principal
├── README.md           # Este arquivo
└── texto.txt           # Exemplo de arquivo de entrada
```

---

## ✍️ Autoria

**Higor Gabriel Lino Silva**
