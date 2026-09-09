import tempfile
import unittest
from pathlib import Path

from PIL import Image
from collage import Source, GifWriter


class TimingTests(unittest.TestCase):
    def test_original_timing_and_freeze(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'source.gif'
            red = Image.new('RGB', (8, 8), 'red')
            blue = Image.new('RGB', (8, 8), 'blue')
            red.save(path, save_all=True, append_images=[blue], duration=[100, 300], loop=0)
            src = Source(path, path)
            try:
                self.assertEqual(src.duration, 400)
                for ms in [0, 99]:
                    self.assertEqual(src.frame(ms).getpixel((0, 0)), (255, 0, 0))
                for ms in [100, 399, 400, 10000]:
                    self.assertEqual(src.frame(ms).getpixel((0, 0)), (0, 0, 255))
            finally:
                src.im.close()

    def test_streamed_gif_final_hold(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'output.gif'
            red = Image.new('RGB', (8, 8), 'red')
            blue = Image.new('RGB', (8, 8), 'blue')
            palette_image = Image.new('RGB', (16, 8))
            palette_image.paste(red, (0, 0))
            palette_image.paste(blue, (8, 0))
            writer = GifWriter(path, palette_image.quantize())
            writer.append(red, 100)
            writer.append(blue, 4000)
            writer.close()
            with Image.open(path) as im:
                self.assertEqual(im.n_frames, 2)
                self.assertEqual(im.info['loop'], 0)
                self.assertEqual(im.info['duration'], 100)
                im.seek(1)
                self.assertEqual(im.info['duration'], 4000)
                self.assertEqual(im.convert('RGB').getpixel((0, 0)), (0, 0, 255))


if __name__ == '__main__':
    unittest.main()
