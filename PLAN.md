# criteriobrasil — plano

Pacote em R que entrega as tabelas do Critério de Classificação Econômica
Brasil (CCEB) da ABEP em formato tidy, com a pontuação unificada, uma função
de classificação e deflação das estimativas de renda.

## Problema

Usar o Critério Brasil em consultoria hoje custa três gargalos. Achar o PDF
certo no site da ABEP, achar a tabela certa dentro dele e copiar para o Excel.
Explicar a classificação ao cliente, com a pontuação espalhada por três ou
quatro tabelas separadas. Descobrir qual edição se aplica a uma base de renda
de um ano específico, já que a data de vigência do documento e o ano da
pesquisa que gerou as estimativas nunca coincidem.

## O que o CCEB é, e o que não é

O Critério Brasil é benchmark de mercado, não índice analítico. Ele vale
porque a indústria de pesquisa inteira usa os mesmos cortes, não porque seja
o melhor estratificador possível. Isso define a postura do pacote: fidelidade
ao publicado vem antes de sofisticação estatística, e o pacote não inventa
precisão que a fonte não tem.

Três confusões merecem seção própria na vignette.

**Os cortes são pontos, não renda.** A classificação soma pontos do
inventário domiciliar, da escolaridade do chefe e dos serviços públicos, de 0
a 100 no regime atual. Renda não entra no cálculo. O documento de 2026 diz
que "a pergunta de renda não é um estimador eficiente de nível socioeconômico
e não substitui ou complementa o questionário".

**A tabela de renda é saída, não entrada.** Ela traz a renda domiciliar
mensal média condicional à classe, estimada sobre a PNADC depois de
classificar. A ABEP avisa no mesmo parágrafo que a variância é alta e que as
classes se sobrepõem, o que significa que essa média caracteriza um estrato
mas não o identifica.

**A unidade de análise é o domicílio**, não a família nem a pessoa.

## Decisões

| Tema | Decisão |
|---|---|
| Nome | `criteriobrasil`, funções com prefixo `cceb_` |
| Distribuição | GitHub e r-universe. Sem CRAN |
| Dados | Assets de release, no padrão de `cache_github.R` do `realestatebr` |
| PDFs | Nunca versionados. `data-raw/pdf/` no `.gitignore` |
| Extração | `pdftools::pdf_text(..., layout = TRUE)`. Sem Java, sem `tabulapdf` |
| Idioma da API | Inglês, como o resto do portfólio. Rótulos de valor em português com coluna de tradução |
| Deflator | Série de IPCA embutida em `sysdata.rda`, atualizada a cada release |
| Questionário | Fora de escopo. As definições de coleta não viram dado |

A opção pelo GitHub em vez do CRAN vem dos Termos de Uso da ABEP de
02/09/2024, que exigem autorização prévia por escrito para reproduzir ou
distribuir conteúdo do site. A tabela de pontos é método, e método não tem
proteção autoral pela Lei 9.610 art. 8º, mas as distribuições de classe e as
estimativas de renda são compilação da ABEP. Ficar fora do CRAN evita a
política de dados do CRAN e mantém o controle sobre o que é publicado.

## O acervo

A página `abep.org/criterio-brasil` lista documentos em português e em
inglês. A maior parte das tabelas tem camada de texto e pode ser extraída com
poppler. As tabelas de renda e distribuição de 2013 são imagens e entram por
transcrição verificada. Não existem edições de 2017, 2023 nem 2025.

| edition_id | Arquivo | Vigência | Regime | Base renda | EN |
|---|---|---|---|---|---|
| 2026 | `CCEB_2026.pdf` | 05/02/2026 | 2026 | PNADC 2025 | sim |
| 2024 | `01_cceb_2024.pdf` | 27/06/2024 | 2015 | PNADC 2023 | sim |
| 2022 | `01_cceb_2022.pdf` | 01/06/2022 | 2015 | PNADC 2021 | sim |
| 2021 | `01_cceb_2021.pdf` | 01/06/2021 | 2015 | PNADC 2020 | não |
| 2020 | `01_cceb_2020.pdf` | 01/09/2020 | 2015 | PNADC 2019 | não |
| 2019 | `01_cceb_2019.pdf` | 01/06/2019 | 2015 | PNADC 2018 | não |
| 2018 | `01_cceb_2018.pdf` | 16/04/2018 | 2015 | PNADC 2017 | não |
| 2016 | `01_cceb_2016_11_04_16_final.pdf` | inferir (11/04/2016) | 2015 | PNAD 2014 | sim |
| 2015 | `01_cceb_2015.pdf` | 01/01/2015 | 2015 | sem tabela | sim |
| 2014 | `09_cceb_2014.pdf` | 01/01/2014 | 2003 | LSE 2012 | não |
| 2013 | `02_cceb_2013.pdf` | 01/01/2013 | 2003 | sem tabela | não |
| 2012 | `03_cceb_2012_base_lse_2010.pdf` | 01/01/2012 | 2003 | LSE 2010 | não |
| 2011 | `04_cceb_base_lse_2009.pdf` | inferir, confirmar | 2003 | LSE 2009 | não |
| 2010 | `05_cceb_2008_em_vigor_em_2010_base_lse_2008.pdf` | 2010 | 2003 | LSE 2008 | não |
| 2009 | `06_cceb_2009_base_2006_2007_...pdf` | 2009 | 2003 | LSE 2006/2007 | não |
| 2008 | `07_cceb_2008_em_vigor_em_2008_base_lse_2005.pdf` | 2008 | 2003 | LSE 2005 | não |
| 2003 | `08_cceb_2003_em_vigor_em_2003_base_lse_2000.pdf` | 2003 | 2003 | LSE 2000 | não |

`cceb_abep_04122014.pdf` é a apresentação do comitê em 22 páginas, com
Kamakura e Mazzon e a descrição do modelo TRI. Serve como referência de
metodologia em `cceb_editions$method_note`, não como fonte de tabela.

### Três regimes

**2003.** Dez itens de contagem, incluindo aspirador de pó, escolaridade em
5 níveis, classes A1, A2, B1, B2, C, D, E e teto de 34 pontos.

**2008 a 2014.** Nove itens de contagem, escolaridade em 5 níveis, classes
A1, A2, B1, B2, C1, C2, D, E e teto de 46 pontos. Em 2014, a distribuição
publicada combina D/E e a renda combina D/E e A1/A2.

**2015 a 2024.** Doze itens de contagem, escolaridade em 5 níveis, serviços
públicos com água encanada e rua pavimentada. Classes A, B1, B2, C1, C2, DE.
Teto de 100 pontos.

**2026.** Quatro variáveis de contagem (automóveis, geladeiras,
microcomputadores, banheiros) e cinco binárias (lavadora de louças, lavadora
de roupas, micro-ondas, água encanada, serviço doméstico). Escolaridade em 7
níveis. Rua pavimentada sai, água encanada vira binária de 10 pontos. Teto de
100 pontos.

### Quebras a documentar

A tabela de renda existe em 2003 e de 2008 a 2014 como "renda média
familiar". Em 2013 e 2014, a fonte agrega A1/A2 como A e D/E como DE. A
tabela some em 2015 e volta em 2016 como "renda média domiciliar".
São conceitos diferentes e não formam uma série contínua. A edição 2009
publica duas colunas de referência, 2006 e 2007.

O arquivo `01_cceb_2015.pdf` tem rodapé "2014". Ano do arquivo, ano do rodapé,
data de vigência e ano da base são quatro campos distintos.

Em 2026 a tabela de distribuição mistura duas referências. Brasil e
macrorregiões vêm de estudos Datafolha e Ipsos-Ipec de 2024; as 9 RMs vêm do
LSE Kantar IBOPE Media de 2023. O ano de referência precisa ser coluna por
linha, não atributo da tabela.

As duas tabelas do mesmo documento usam pesos diferentes. Ponderando as
médias por classe de 2026 pela distribuição do Brasil publicada no mesmo PDF
chega-se a R$ 4.900,17, contra os R$ 4.628,83 que a ABEP reporta como total.
Quase 6% de diferença é demais para arredondamento, e indica que a tabela de
renda usa o universo da PNADC, que inclui rural, enquanto a de distribuição
vem de estudos de mercado urbanos. Entra como coluna de diagnóstico, não como
erro, e como aviso para quem for multiplicar as duas.

## Modelo de dados

Seis tabelas, todas chaveadas por `edition_id`.

**`cceb_editions`** — catálogo, uma linha por documento. Colunas:
`edition_id`, `effective_date`, `effective_date_source` (`stated` ou
`inferred`), `regime`, `income_source`, `income_ref_year`, `income_concept`,
`distribution_source`, `lang`, `url_pt`, `url_en`, `method_note`.

**`cceb_points`** — a pontuação inteira numa tabela só. Colunas: `edition_id`,
`block` (`count_item`, `binary_item`, `education`, `public_service`),
`variable` (slug estável entre regimes), `label_pt`, `label_en`, `level`,
`level_order`, `points`.

**`cceb_cutoffs`** — `edition_id`, `class`, `class_order`, `points_min`,
`points_max`.

**`cceb_income`** — `edition_id`, `class`, `income_mean`, `income_ref_date`,
`concept` (`household` ou `family`).

**`cceb_distribution`** — `edition_id`, `geo_level` (`country`, `region`,
`metro_total`, `metro`), `geo_code`, `geo_name`, `class`, `share`, `ref_year`,
`ref_source`.

**`cceb_ipca`** — série mensal em `sysdata.rda`, para deflação offline.

## API

Nenhuma tabela vira função de exportação própria. A API de dados é uma
função só, genérica, mais duas de utilidade:

```r
cceb_list_tables()                        # catálogo das tabelas disponíveis
cceb_get(table = NULL, edition = NULL)    # tabela do CCEB como tibble
cceb_edition_for(date)                    # qual edição valia nesta data
cceb_classify(data, mapping, edition)     # score e classe a partir de microdados
cceb_score_table(edition)                 # formatada para gt/kable
deflate_brl(x, from, to)
```

`cceb_list_tables()` devolve as cinco tabelas do acervo (`editions`,
`points`, `cutoffs`, `income`, `distribution`) com descrição e as edições
disponíveis de cada uma. Vale lembrar que os documentos fontes não nomeiam
as tabelas; esses cinco nomes são uma convenção do pacote.

`cceb_get(table, edition)` é a única porta de entrada dos dados.

- `table = NULL` devolve `distribution`, a tabela mais usada.
- `edition = NULL` devolve a edição mais recente disponível da tabela.
  Logo, `cceb_get()` sem argumentos devolve a distribuição da edição 2026.
- `edition` aceita um único valor (`2024`) e `"all"`. `"all"` empilha as
  edições com `bind_rows()` quando o esquema é idêntico entre edições —
  é o caso de `points`, `cutoffs`, `income` e `distribution`, que já vêm
  chaveadas por `edition_id`. É impossível em `editions`, que é uma única
  linha por edição; aí `"all"` devolve o catálogo inteiro.
- `table = "all"` devolve uma lista de tibbles nomeada pelas cinco
  tabelas. Se `edition = "all"` também, cada elemento da lista traz todas
  as edições empilhadas.

`edition` é valor numérico, não texto, e significa `edition_id`. O
argumento não se chama `year` de propósito: `edition_id` é rótulo,
não ano — esconde data de vigência, ano do rodapé e ano da base de
renda, que não coincidem entre si nem com o rótulo. O exemplo clássico
é a edição 2010, com pontuação de 2008 e base LSE 2008. A documentação
de `cceb_get()` abre com esta tabela de referência, derivada de
`cceb_editions`, para orientar a escolha:

| edition_id | Vigência | Rodapé | Base renda | Dataset | Nota |
|---|---|---|---|---|---|
| 2026 | 05/02/2026 | 2026 | PNADC 2025 | `CCEB_2026.pdf` | |
| 2010 | 2010 | 2008 | LSE 2008 | `05_cceb_2008_em_vigor_em_2010...` | id = ano de vigência |
| 2012 | inferida | — | LSE 2010 | `03_cceb_2012_base_lse_2010...` | id = ano de vigência inferida |
| 2015 | 01/01/2015 | 2014 | sem renda | `01_cceb_2015.pdf` | rodapé ≠ id |

`cceb_get()` nunca devolve objeto formatado nem aplica deflator; composição
com `deflate_brl()` fica com o usuário.

### Implementação do núcleo de acesso

O primeiro incremento da API implementa somente `cceb_list_tables()` e
`cceb_get()`. Não haverá uma função exportada por tabela: nomes como
`cceb_get_income()` repetiriam a mesma lógica e ampliariam uma API para um
acervo de apenas cinco tabelas.

Cada tabela será publicada como um asset `.rds` separado e com o mesmo nome
do objeto (`cceb_editions.rds`, `cceb_points.rds`, `cceb_cutoffs.rds`,
`cceb_income.rds` e `cceb_distribution.rds`). O pacote aponta para uma tag de
release imutável, registrada em uma constante interna. Uma correção ou nova
edição gera outra tag e uma nova versão do pacote passa a apontar para ela.
Isso preserva a reprodutibilidade; uma URL mutável como `data-latest` poderia
entregar dados diferentes sem mudança no código instalado.

O registro das cinco tabelas fica no pacote e contém nome, descrição e asset.
`cceb_list_tables()` combina esse registro com a disponibilidade por edição
derivada dos dados de build. Assim, listar o acervo é instantâneo e não exige
rede. Um teste no build garante que o registro local e os assets publicados
tenham os mesmos nomes e edições.

`cceb_get()` valida `table` e `edition`, baixa apenas os assets necessários por
uma URL pública do GitHub, lê o `.rds` em arquivo temporário e valida nome e
esquema antes de devolver um tibble. Uma memória interna por asset evita um
segundo download na mesma sessão. Não haverá cache persistente em disco: o
acervo comprimido atual ocupa cerca de 24 KB e muda pouco, portanto a
complexidade de expiração, permissões e limpeza não se justifica.

Também não haverá fallback para extração direta dos PDFs da ABEP. O pipeline
em `data-raw/` é ferramenta de manutenção, depende de revisão de fidelidade e
não é uma fonte segura para uso automático. Falha de rede, asset ausente ou
esquema incompatível produz erro com a URL tentada e uma orientação objetiva.

O contrato inicial será:

```r
cceb_list_tables()
cceb_get(table = "distribution", edition = NULL)
cceb_get(table = "income", edition = 2024L)
cceb_get(table = "all", edition = "all")
```

- `table` aceita um dos cinco nomes sem o prefixo `cceb_` ou `"all"`.
- `edition = NULL` seleciona a maior `edition_id` disponível naquela tabela;
  para `table = "all"`, seleciona separadamente a edição mais recente de cada
  tabela, porque a edição 2015, por exemplo, não possui renda.
- `edition` aceita um inteiro escalar ou `"all"`. Outros textos, vetores,
  valores ausentes e edições indisponíveis falham com a lista de opções
  válidas.
- `table = "all"` sempre devolve uma lista nomeada de tibbles. Uma edição sem
  determinada tabela gera um erro, em vez de omitir silenciosamente o
  elemento.
- As linhas preservam a ordem canônica do asset; a função só seleciona a
  edição e não renomeia, reordena nem transforma colunas.

Os testes cobrem os valores padrão, todas as combinações de `"all"`, seleção
de edição, classes e tipos de retorno, ordem das linhas, erros de argumento,
falhas de download e memoização. Os testes de unidade simulam o download e
rodam offline; um teste separado do pipeline confere os `.rds` gerados contra
os objetos validados. A documentação inclui exemplos executáveis sem rede com
os dados de teste e exemplos de download sob `\dontrun{}`.

`cceb_score_table()` devolve pontuação, cortes e distribuição num objeto
formatado, com fonte e data de vigência no rodapé, pronto para slide. Internamente
consome `cceb_get()`.

`cceb_edition_for(date)` devolve a última edição cuja `effective_date` seja
anterior ou igual à data. Uma base de 2010 aponta para
`05_cceb_2008_em_vigor_em_2010`. Uso típico:
`cceb_get("income", cceb_edition_for(as.Date("2010-06-15")))`.

Deflação é função própria, não argumento: `deflate_brl()` parte de
`income_ref_date`, não da data de publicação. A diferença entre
as duas é de um a três anos e é a origem da confusão: o documento de
27/06/2024 publica renda da PNADC 2023.

## Pipeline

```
data-raw/
  01_download.R      baixa os PDFs ausentes sem alterar hashes existentes
  01_refresh_sources.R atualiza PDFs e hashes após revisão manual
  manifest.csv       url, sha256, data de acesso — versionado
  fidelity.csv       fingerprint exato de cada tabela por edição
  manual/            tabelas de 2013 transcritas das imagens
  02_extract_2003.R  parser do regime antigo
  02_extract_2015.R  parser do regime intermediário (8 documentos)
  02_extract_2026.R  parser do regime novo
  03_validate.R      testes de integridade
  04_build.R         escreve os arquivos e publica no release
```

O build confere os PDFs contra o manifest antes da extração. Uma troca de
arquivo interrompe o pipeline. O script de refresh separa a atualização
intencional da execução comum. Os uploads já foram remanejados para
`/2024/02/` e `/2026/03/`.

## Validação

Os validadores conferem esquema, chaves únicas, intervalos, cortes contíguos,
classes completas, somas das distribuições e integridade entre tabelas. Essas
regras medem coerência interna e não codificam valores de uma edição
específica.

O arquivo `fidelity.csv` registra um fingerprint exato de cada tabela por
edição. Cada fingerprint está ligado ao hash do PDF. Assim, uma mudança de
uma célula pode passar pelos testes de coerência, mas falha na conferência de
fidelidade.

## Riscos

A ABEP pode mover ou substituir os PDFs. O pipeline rejeita a mudança até que
alguém revise a nova fonte e execute o refresh explícito.

Sem CRAN, a descoberta depende do README e do r-universe.

As versões em inglês podem divergir da portuguesa em números ou ano de
referência. A paridade entre idiomas ainda não faz parte do pipeline.

## Ordem de execução

1. Scaffold do pacote, `manifest.csv` e `01_download.R`
2. Parser do regime 2015, que cobre 8 documentos
3. Parsers dos regimes 2026 e 2003
4. `03_validate.R` e a suíte de testthat
5. Assets `.rds`, `cceb_list_tables()` e `cceb_get()`
6. `cceb_edition_for()`
7. `cceb_classify()` e `cceb_score_table()`
8. Série de IPCA e `deflate_brl()`
9. Vignette sobre qual edição usar, pkgdown e r-universe

## Fora de escopo agora

**`cceb_households()`**, que multiplicaria a participação publicada por um
total de domicílios do IBGE para responder "quantos domicílios B1 e B2 há na
RMSP". O caminho funciona. A coluna SP de 2026 dá 7,6% em B1 e 22,3% em B2, e
os 39 municípios da RM de São Paulo somam 7.604.313 domicílios particulares
permanentes ocupados no Censo 2022, o que leva a 2,27 milhões de domicílios.
Fica adiado porque o intervalo dependeria de supor o tamanho amostral do LSE
da Kantar por praça, que não é público e dificilmente será obtido. Sob
suposição de n igual a 2.000 e efeito de desenho 1,5, a amplitude do
intervalo de 95% seria de 375 mil domicílios, vinte e cinco vezes a do
arredondamento da participação. Uma função que devolvesse 2.273.690 sem esse
intervalo entregaria precisão inexistente.

Faltam também duas definições antes de a função existir. A "SP" da ABEP vem
do LSE da Kantar IBOPE Media, cuja Grande São Paulo é área de medição de
audiência e pode não coincidir com os 39 municípios do IBGE. E o Censo é de
julho de 2022, então a atualização até a data corrente é escolha de método.

**Reestimar o critério sobre a PNADC.** Aplicar a regra publicada é
impossível: o bloco S01 da PNADC Anual dá binárias onde o critério pede
contagens, define banheiro de forma mais estreita, exigindo chuveiro e vaso,
e não coleta lavadora de louças, micro-ondas nem serviço doméstico. O teto
observável é de 60 pontos dos 100, e a classe A começa em 73, de modo que
nenhum domicílio poderia ser classificado como A. Reestimar do zero produziria
outro índice, sem a comparabilidade com a base instalada de pesquisa de
mercado, que é a razão de usar o CCEB.

**Validação sobre a POF 2017-2018**, onde o inventário completo existe e o
escore é calculável. É a base que a própria ABEP usa. Vale como vignette de
conferência, não como dataset da série.

**Hipótese dos quantis.** Talvez a renda por classe seja a média domiciliar
dos quantis correspondentes às participações publicadas. Se confirmar, a
tabela de renda passa a ser atualizável em qualquer trimestre da PNADC, sem
esperar o PDF. Investigação aberta, e o total de R$ 4.628,83 é o primeiro
alvo.

## Decisões em aberto

- Mês âncora dentro do ano de referência da renda. Dezembro ou média anual.
  A escolha vai numa coluna `income_ref_date`, não implícita no código.
- Vigência dos arquivos sem "válidas a partir de", em especial
  `04_cceb_base_lse_2009.pdf`, que só pode ser inferida.
- Se o modelo TRI descrito na apresentação de 04/12/2014 merece uma vignette
  própria ou só uma referência bibliográfica.
- Se vale abrir um pedido de autorização à ABEP mais adiante, o que
  destravaria o CRAN.
