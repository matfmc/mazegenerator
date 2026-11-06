"""Monta as imagens da galeria do site a partir de mazes2/.

Gera, a partir da saida de scripts/generate_mazes2.R:

- man/figures/gallery/<labirinto>/{generation,solve_astar}.gif: GIFs reduzidos
  para a pagina vignettes/articles/gallery.Rmd;
- man/figures/collage.gif: os 14 GIFs de geracao lado a lado, sincronizados,
  usado no README e no topo da galeria.

Uso (na raiz do repo, com Pillow instalado):

    python scripts/gallery_assets.py [--input mazes2] [--width 360] [--tile 160]
"""
import argparse
import csv
from bisect import bisect_right
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont, ImageSequence

GALLERY_FILES = ("generation.gif", "solve_astar.gif")
LABELS = {
    "dfs": "DFS", "prim": "Prim", "kruskal": "Kruskal", "wilson": "Wilson",
    "eller": "Eller", "division": "Division", "aldous_broder": "Aldous-Broder",
    "hunt_kill": "Hunt-and-kill", "binary_tree": "Binary tree",
    "sidewinder": "Sidewinder", "growing_tree_newest": "Growing tree: newest",
    "growing_tree_random": "Growing tree: random",
    "growing_tree_oldest": "Growing tree: oldest",
    "growing_tree_mixed": "Growing tree: mixed",
}


def read_frames(path, width):
    """Quadros em RGB redimensionados e a duracao de cada um (ms)."""
    frames, durations = [], []
    with Image.open(path) as im:
        for fr in ImageSequence.Iterator(im):
            rgb = fr.convert("RGB")
            h = round(rgb.height * width / rgb.width)
            frames.append(rgb.resize((width, h), Image.LANCZOS))
            durations.append(max(20, fr.info.get("duration", im.info.get("duration", 100))))
    return frames, durations


def save_gif(path, frames, durations):
    pal = [f.quantize(colors=64, method=Image.MEDIANCUT, dither=Image.NONE) for f in frames]
    path.parent.mkdir(parents=True, exist_ok=True)
    pal[0].save(path, save_all=True, append_images=pal[1:], duration=durations,
                loop=0, optimize=True, disposal=1)


def resize_one(src, dst, width):
    frames, durations = read_frames(src, width)
    save_gif(dst, frames, durations)
    return f"{dst.parent.name}/{dst.name}: {src.stat().st_size/1e6:.1f} -> {dst.stat().st_size/1e6:.2f} MB"


def maze_order(root):
    """Ordem do index.csv (a mesma do script R); pastas extras vao ao fim."""
    order = []
    index = root / "index.csv"
    if index.exists():
        with index.open(encoding="utf-8-sig", newline="") as fh:
            for row in csv.DictReader(fh):
                sel = row.get("selection") or ""
                name = row["algorithm"] + (f"_{sel}" if sel and sel != "NA" else "")
                if name not in order and (root / name).is_dir():
                    order.append(name)
    order += [d.name for d in sorted(root.iterdir()) if d.is_dir() and d.name not in order]
    return order


def collage(root, mazes, out, tile, columns, step=100, hold=3000):
    font = None
    for name in ("C:/Windows/Fonts/arialbd.ttf", "DejaVuSans-Bold.ttf"):
        try:
            font = ImageFont.truetype(name, 13)
            break
        except OSError:
            pass
    font = font or ImageFont.load_default()
    clips = []
    for m in mazes:
        frames, durations = read_frames(root / m / "generation.gif", tile)
        ends, total = [], 0
        for d in durations:
            total += d
            ends.append(total)
        clips.append((m, frames, ends))
    pad, label = 8, 20
    rows = -(-len(clips) // columns)
    tile_h = clips[0][1][0].height
    W = columns * (tile + pad) + pad
    H = rows * (tile_h + label + pad) + pad
    length = max(c[2][-1] for c in clips) + hold
    out_frames = []
    for t in range(0, length, step):
        canvas = Image.new("RGB", (W, H), "white")
        draw = ImageDraw.Draw(canvas)
        for i, (m, frames, ends) in enumerate(clips):
            x = pad + (i % columns) * (tile + pad)
            y = pad + (i // columns) * (tile_h + label + pad)
            canvas.paste(frames[min(bisect_right(ends, t), len(frames) - 1)], (x, y))
            text = LABELS.get(m, m)
            tw = draw.textlength(text, font=font)
            draw.text((x + (tile - tw) / 2, y + tile_h + 3), text, fill="#1f2937", font=font)
        out_frames.append(canvas)
    save_gif(out, out_frames, [step] * len(out_frames))
    return f"{out}: {len(out_frames)} quadros, {W}x{H}, {out.stat().st_size/1e6:.2f} MB"


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--input", type=Path, default=Path("mazes2"))
    p.add_argument("--figures", type=Path, default=Path("man/figures"))
    p.add_argument("--width", type=int, default=360, help="largura dos GIFs da galeria")
    p.add_argument("--tile", type=int, default=160, help="largura de cada labirinto na colagem")
    p.add_argument("--columns", type=int, default=7)
    a = p.parse_args()
    mazes = maze_order(a.input)
    jobs = [(a.input / m / f, a.figures / "gallery" / m / f) for m in mazes for f in GALLERY_FILES]
    with ProcessPoolExecutor() as ex:
        done = ex.map(resize_one, *zip(*jobs), [a.width] * len(jobs))
        col = ex.submit(collage, a.input, mazes, a.figures / "collage.gif", a.tile, a.columns)
        for line in done:
            print(line, flush=True)
        print(col.result(), flush=True)


if __name__ == "__main__":
    main()
