"""Colagens de mazes. Dependencia: pip install Pillow.

Uso: python scripts/collage.py --input mazes --output collages
GIFs usam os tempos originais, uma unica passagem e pausa final de 4 s.
Os frames sao escritos sequencialmente, sem acumular a animacao na RAM.
"""
from pathlib import Path
from bisect import bisect_right
import argparse
import csv
import json
import math
import shutil
import tempfile
import time

from PIL import Image, ImageChops, ImageDraw, ImageFont, GifImagePlugin

KINDS = [('maze.png', 'Labirinto'), ('generation.gif', 'Geracao'),
         ('solve_astar.gif', 'A*'), ('solve_bfs.gif', 'BFS'),
         ('solve_dijkstra.gif', 'Dijkstra')]
BG = '#101827'


def method_rows(groups, index_path):
    """Ordena metodos pelo indice e alinha open_walls nas mesmas colunas."""
    methods, entries = [], {}
    if index_path.exists():
        with index_path.open(encoding='utf-8-sig', newline='') as handle:
            for row in csv.DictReader(handle):
                method, value = row['algorithm'], float(row['open_walls'])
                if method not in methods:
                    methods.append(method)
                entries[f'{method}_p{round(value*1000):03d}'] = (method, value)
    for group in groups:
        if group not in entries:
            method, value = group.rsplit('_p', 1)
            entries[group] = (method, int(value)/1000)
            if method not in methods:
                methods.append(method)
    values = sorted({value for method, value in entries.values()})
    lookup = {(method, value): group for group, (method, value) in entries.items()}
    return methods, values, [[lookup.get((method, value)) for value in values] for method in methods]


def font(size):
    for name in ['C:/Windows/Fonts/arial.ttf', 'DejaVuSans.ttf']:
        try:
            return ImageFont.truetype(name, size)
        except OSError:
            pass
    return ImageFont.load_default(size=size)


class Source:
    def __init__(self, path, original):
        self.original = str(original.resolve())
        self.im = Image.open(path)
        self.size = self.im.size
        self.ends = []
        total = 0
        for i in range(self.im.n_frames):
            self.im.seek(i)
            total += max(10, self.im.info.get('duration', 100))
            self.ends.append(total)
        self.duration = total if self.im.n_frames > 1 else 0
        self.im.seek(0)

    def frame(self, ms):
        index = min(bisect_right(self.ends, ms), len(self.ends) - 1)
        self.im.seek(index)
        rgba = self.im.convert('RGBA')
        result = Image.new('RGB', self.size, 'white')
        result.paste(rgba, mask=rgba.getchannel('A'))
        return result


class GifWriter:
    def __init__(self, path, palette):
        self.file = open(path, 'wb')
        self.palette = palette
        self.started = False
        self.previous = None

    def append(self, rgb, duration):
        indexed = (rgb.copy() if rgb.mode == 'P' and rgb.getpalette() == self.palette.getpalette()
                   else rgb.quantize(palette=self.palette, dither=Image.Dither.NONE))
        if not self.started:
            header, _ = GifImagePlugin.getheader(indexed, info={'loop': 0})
            for block in header:
                self.file.write(block)
            self.started = True
        frame, offset, options = indexed, (0, 0), {}
        if self.previous is not None:
            changed = ImageChops.difference(indexed, self.previous).point(lambda v: 255 if v else 0, mode='L')
            bounds = changed.getbbox()
            if bounds:
                frame = indexed.crop(bounds)
                mask = changed.crop(bounds)
                used = frame.histogram(mask=mask)
                transparent = next((i for i, count in enumerate(used) if not count), None)
                if transparent is not None:
                    frame.paste(transparent, mask=ImageChops.invert(mask))
                    options['transparency'] = transparent
                offset = bounds[:2]
            else:
                frame = indexed.crop((0, 0, 1, 1))
        for block in GifImagePlugin.getdata(frame, offset=offset, duration=duration, disposal=1, **options):
            self.file.write(block)
        self.previous = indexed

    def close(self):
        self.file.write(b';')
        self.file.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input', type=Path, default=Path('mazes'))
    parser.add_argument('--output', type=Path, default=Path('collages'))
    parser.add_argument('--columns', type=int, default=4)
    parser.add_argument('--layout', choices=['method-rows', 'grid'], default='method-rows',
                        help='Padrao: um metodo por linha; --columns se aplica somente a grid')
    parser.add_argument('--hold', type=float, default=4)
    parser.add_argument('--preview-width', type=int, default=1800)
    args = parser.parse_args()
    if args.columns < 1 or args.hold <= 0 or args.preview_width < 1:
        parser.error('columns, hold e preview-width precisam ser positivos')
    args.output.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='maze-collage-') as tmp:
        sources, skipped = {}, []
        for folder in sorted(args.input.iterdir()):
            if not folder.is_dir():
                continue
            for name, _ in KINDS:
                original = folder / name
                if not original.exists():
                    continue
                snapshot = Path(tmp) / (folder.name + '_' + name)
                try:
                    before = original.stat()
                    shutil.copyfile(original, snapshot)
                    after = original.stat()
                    if (before.st_size, before.st_mtime_ns) != (after.st_size, after.st_mtime_ns):
                        raise ValueError('arquivo mudou durante a copia')
                    sources[folder.name, name] = Source(snapshot, original)
                except (OSError, ValueError, EOFError) as exc:
                    skipped.append({'path': str(original), 'reason': str(exc)})
        if not sources:
            raise SystemExit('Nenhuma imagem completa encontrada.')
        groups = sorted({key[0] for key in sources})
        methods, values, rows = method_rows(groups, args.input / 'index.csv')
        png_w = max((s.size[0] for k, s in sources.items() if k[1] == 'maze.png'), default=800)
        png_h = max((s.size[1] for k, s in sources.items() if k[1] == 'maze.png'), default=800)
        gif_w = max((s.size[0] for k, s in sources.items() if k[1] != 'maze.png'), default=400)
        gif_h = max((s.size[1] for k, s in sources.items() if k[1] != 'maze.png'), default=400)
        pad, label, title = 16, 28, 48
        tile_w = png_w + 2 * gif_w + 4 * pad
        tile_h = title + max(png_h + label, 2 * (gif_h + label) + pad) + pad
        row_header = 48 if args.layout == 'method-rows' else 0
        if args.layout == 'method-rows':
            columns = len(values)
            group_positions = [(group, col, row) for row, items in enumerate(rows)
                               for col, group in enumerate(items) if group is not None]
            row_count = len(rows)
        else:
            columns = min(args.columns, len(groups))
            group_positions = [(group, i % columns, i // columns) for i, group in enumerate(groups)]
            row_count = math.ceil(len(groups) / columns)
        width, height = columns * tile_w, 100 + row_count * (tile_h + row_header)
        base = Image.new('RGB', (width, height), BG)
        draw = ImageDraw.Draw(base)
        draw.text((24, 12), 'LABIRINTOS | geracao e comparacao de buscas', font=font(34), fill='white')
        draw.text((24, 57), 'Resolucao original | inicio simultaneo | ultimo quadro mantido | pausa final: %.2g s' % args.hold,
                  font=font(22), fill='#b8c8df')
        placements = []
        if args.layout == 'method-rows':
            for row, method in enumerate(methods):
                y = 100 + row * (tile_h + row_header)
                draw.text((16, y+6), 'METODO: ' + method.upper(), font=font(30), fill='#84d5ff')
                for col, value in enumerate(values):
                    draw.text((col*tile_w+tile_w-300, y+10), f'open_walls = {value:g}',
                              font=font(26), fill='#c8ddf4')
        for group, grid_col, grid_row in group_positions:
            x, y = grid_col * tile_w, 100 + grid_row * (tile_h + row_header) + row_header
            draw.rounded_rectangle((x+4, y+4, x+tile_w-4, y+tile_h-4), radius=12, fill='#1c293d')
            draw.text((x+pad, y+8), group, font=font(30), fill='white')
            for j, (name, caption) in enumerate(KINDS):
                if j == 0:
                    px, py, sw, sh = x+pad, y+title, png_w, png_h
                else:
                    col, row = (j-1) % 2, (j-1) // 2
                    px = x + png_w + 2*pad + col*(gif_w+pad)
                    py = y + title + row*(gif_h+label+pad)
                    sw, sh = gif_w, gif_h
                draw.text((px, py), caption, font=font(21), fill='#c8ddf4')
                src = sources.get((group, name))
                if src:
                    placements.append((src, px+(sw-src.size[0])//2, py+label+(sh-src.size[1])//2))
                else:
                    draw.text((px+16, py+label+30), 'Ainda nao disponivel', font=font(20), fill='#9baac0')

        longest = max(s.duration for s in sources.values())
        boundaries = sorted({0, longest} | {t for s in sources.values() if s.duration for t in s.ends if t < longest})
        hold_ms = max(10, round(args.hold*1000/10)*10)
        def render(ms):
            canvas = base.copy()
            for src, x, y in placements:
                canvas.paste(src.frame(ms), (x, y))
            return canvas

        final = render(longest)
        final.save(args.output / 'colagem-final.png')
        preview_size = (min(width, args.preview_width), max(1, round(height*min(1, args.preview_width/width))))
        final.resize(preview_size, Image.Resampling.LANCZOS).save(args.output / 'previa-final.png')
        # Uma paleta compartilhada evita troca de cores entre quadros.
        samples = Image.new('RGB', (1024, 1024*3), BG)
        for i, ms in enumerate([0, longest//2, longest]):
            sample = render(ms)
            sample.thumbnail((1024, 1024))
            samples.paste(sample, (0, i*1024))
        palette = samples.quantize(colors=256)
        writers = [GifWriter(args.output / 'colagem-animada.gif', palette),
                   GifWriter(args.output / 'previa-animada.gif', palette)]
        print(f'{len(groups)} configuracoes, {len(sources)} imagens; {width}x{height}; {(longest+hold_ms)/1000:.2f}s; {len(boundaries)} quadros', flush=True)
        started = time.monotonic()
        try:
            for i, ms in enumerate(boundaries):
                duration = boundaries[i+1]-ms if i+1 < len(boundaries) else hold_ms
                canvas = render(ms)
                writers[0].append(canvas, duration)
                writers[1].append(canvas.resize(preview_size, Image.Resampling.LANCZOS), duration)
                if i % 10 == 0 or i == len(boundaries)-1:
                    print(f'Quadro {i+1}/{len(boundaries)} ({time.monotonic()-started:.0f}s)', flush=True)
        finally:
            for writer in writers:
                writer.close()
            for src in sources.values():
                src.im.close()
        manifest = {'size': [width, height], 'preview_size': preview_size, 'duration_ms': longest+hold_ms,
                    'final_hold_ms': hold_ms, 'frames': len(boundaries), 'groups': groups, 'skipped': skipped,
                    'layout': args.layout, 'method_order': methods, 'open_walls_columns': values,
                    'index_path': str((args.input / 'index.csv').resolve()),
                    'grid': rows if args.layout == 'method-rows' else None,
                    'sources': [{'path': s.original, 'size': s.size, 'duration_ms': s.duration,
                                 'frames': len(s.ends)} for s in sources.values()]}
        (args.output / 'manifest.json').write_text(json.dumps(manifest, indent=2), encoding='utf-8')
        print('Concluido: ' + str(args.output.resolve()), flush=True)


if __name__ == '__main__':
    main()
