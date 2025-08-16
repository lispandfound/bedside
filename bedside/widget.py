from dataclasses import dataclass, field

from PIL import Image

WIDTH = 800
HEIGHT = 480


def blank() -> Image.Image:
    return Image.new("RGBA", (WIDTH, HEIGHT), (255, 255, 255, 0))


@dataclass
class Widget:
    name: str
    z: int
    bw: Image.Image = field(default_factory=blank)
    red: Image.Image = field(default_factory=blank)
