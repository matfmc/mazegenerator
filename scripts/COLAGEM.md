# Colagem de labirintos

Na pasta do projeto, execute no PowerShell:

```powershell
.\scripts\collage.ps1
```

O padrao organiza um metodo de geracao por linha, na ordem do `mazes/index.csv`.
As colunas alinham os valores de `open_walls` em ordem crescente (0, 0.25, 0.5, 0.75, 0.9).
Para usar a grade livre anterior e mudar a quantidade de blocos por linha:

```powershell
.\scripts\collage.ps1 -Layout grid -Columns 4 -Hold 4
```

O launcher usa o Python do ambiente Codex quando disponivel. Em outro computador,
instale Python e Pillow (`python -m pip install Pillow`) e execute
`python scripts/collage.py --input mazes --output collages`.

Saidas em `collages/` (substituidas a cada execucao):

- `colagem-final.png`: estados finais com os pixels originais, sem reducao.
- `colagem-animada.gif`: animacao na mesma resolucao, paleta GIF de 256 cores.
- `previa-final.png` e `previa-animada.gif`: versoes menores, com largura de 1800 px.
- `manifest.json`: arquivos incluidos, dimensoes, duracao e arquivos ignorados.

Cada bloco mostra PNG, geracao, A*, BFS e Dijkstra. Ausencias recebem um aviso.
O script copia os arquivos para uma pasta temporaria antes da composicao. Arquivos
que mudam durante a copia ou nao podem ser decodificados sao ignorados e registrados.
Novos arquivos entram na proxima execucao. A pasta temporaria e removida ao terminar.

Os GIFs iniciam juntos e usam as duracoes originais de seus quadros, incluindo
pausas ja existentes. Cada um e reproduzido uma vez e mantem seu ultimo quadro.
Ao fim do GIF mais longo, todos ficam congelados por mais 4 segundos, e o loop reinicia.
O GIF grande pode exigir bastante memoria do visualizador; use a previa para abrir
rapidamente. A gravacao do script e sequencial e nao guarda todos os quadros na RAM.
Areas repetidas sao omitidas entre quadros para reduzir o tamanho dos GIFs.

Para otimizar um GIF antigo gerado pelo script sem alterar seus pixels ou tempos:

```powershell
python scripts/optimize_gif.py collages/colagem-animada.gif collages/colagem-animada-otimizada.gif
```

O otimizador exige a paleta global compartilhada usada por este gerador. O tamanho
resultante depende das imagens; confira o arquivo antes de enviar para um limite de upload.

Testes: `python scripts/test_collage.py`.
