"""Remove pixels repetidos entre quadros sem reduzir resolucao ou cores."""
import argparse
from pathlib import Path
from PIL import Image, GifImagePlugin
from collage import GifWriter


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    parser.add_argument('output', type=Path)
    args = parser.parse_args()
    if args.source.resolve() == args.output.resolve():
        parser.error('Escolha uma saida diferente do original')
    GifImagePlugin.LOADING_STRATEGY = GifImagePlugin.LoadingStrategy.RGB_AFTER_DIFFERENT_PALETTE_ONLY
    with Image.open(args.source) as source:
        palette = source.copy()
        writer = GifWriter(args.output, palette)
        try:
            count = source.n_frames
            for i in range(count):
                source.seek(i)
                if source.mode != 'P' or source.getpalette() != palette.getpalette():
                    raise ValueError('Otimizacao sem perdas exige uma paleta global compartilhada')
                writer.append(source, source.info.get('duration', 100))
                if i % 10 == 0 or i == count-1:
                    print(f'Quadro {i+1}/{count}', flush=True)
        finally:
            writer.close()
    print(f'{args.output}: {args.output.stat().st_size/1_000_000:.2f} MB', flush=True)


if __name__ == '__main__':
    main()
