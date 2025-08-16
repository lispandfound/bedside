from dataclasses import dataclass
from enum import StrEnum
from importlib import resources
from random import choice

from PIL import Image
from widget import Widget

import bedside

_MEWO_WIDGET = "mewo"


class MewoState(StrEnum):
    SLEEP = "sleep"
    DESK = "desk"
    floor = "floor"


def _mewo_img(state: MewoState, z: int) -> Widget:
    with resources.open_binary(bedside, "assets", "mewo", f"{state}.bmp") as f:
        mewo = Image.open(f).convert(mode="RGBA")
    return Widget(name=_MEWO_WIDGET, z=z, bw=mewo)


@dataclass
class Mewo:
    z: int = -99
    state: MewoState | None = None
    asleep: bool = False

    def sleep(self) -> Widget | None:
        if self.asleep:
            return
        new = _mewo_img(MewoState.SLEEP, self.z) if self.state != MewoState.SLEEP else None
        self.state = MewoState.SLEEP
        self.asleep = True
        return new

    def awake(self) -> None:
        self.asleep = False

    def random(self) -> Widget | None:
        if not self.asleep:
            self.state = choice(list(MewoState))
            return _mewo_img(self.state, self.z)
