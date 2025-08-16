from dataclasses import dataclass, field

from PIL import Image

WIDTH = 800
HEIGHT = 480


def blank() -> Image.Image:
    return Image.new("1", (WIDTH, HEIGHT), 255)


@dataclass
class Widget:
    name: str
    z: int
    bw: Image.Image = field(default_factory=blank)
    red: Image.Image = field(default_factory=blank)
