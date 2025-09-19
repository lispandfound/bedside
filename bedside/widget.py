from dataclasses import dataclass, field

import numpy as np
from PIL import Image

WIDTH = 800
HEIGHT = 480


def blank() -> Image.Image:
    return Image.new("RGBA", (WIDTH, HEIGHT), (255, 255, 255, 0))


@dataclass
class Widget:
    name: str
    z: int
    image: Image.Image = field(default_factory=blank)

    @property
    def bw(self) -> Image.Image:
        data = np.asarray(self.image)
        # Abusing the fact that colours are only red or black.
        black_mask = (data[:, :, 3] == 255) & (data[:, :, 0] == 0)
        out = np.zeros_like(data)
        out[black_mask] = [0, 0, 0, 255]

        return Image.fromarray(out)

    @property
    def red(self) -> Image.Image:
        data = np.asarray(self.image).copy()
        red_mask = (
            (data[:, :, 3] == 255)  # opaque
            & (data[:, :, 0] == 255)  # red
            & (data[:, :, 1] == 0)  # green
        )

        out = np.zeros_like(data)
        out[red_mask] = [0, 0, 0, 255]

        return Image.fromarray(out)
