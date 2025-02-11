Análise de notícias 
Projeto de ciência de dados desenvolvido em R para analisar textos de notícias. Inclui módulos para web scraping de sites jornalísticos, transformação de texto em palavras (tokens) e análise do corpus usando técnicas estatísticas.

Aplicações web
Análise semanal da imprensa
Aplicação web em R que mostra gráficos quantificando o conteúdo das notícias. Permite identificar as palavras mais usadas ao longo do tempo, mostrando tendências.

Estrutura do código
O script prensa_procesar.R orquestra todas as etapas de obtenção, processamento e análise. Os processos são otimizados para eficiência, baixo consumo de memória e processamento paralelo.

Scraping

prensa_scraping.R: executa múltiplos processos simultâneos usando pacotes rvest e polite para scraping ético

Processamento

Carregamento e limpeza de dados
Tokenização de textos
Cálculo de frequências
Análise de correlação entre termos

Análise

Correlação entre palavras
Identificação automática de tópicos via machine learning
Detecção de temas por termos específicos


Licença
GNU General Public License. Código aberto, livre para uso e modificação, desde que compartilhado sob a mesma licença. Autor: Bastián Olea Herrera.
