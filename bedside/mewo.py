from dataclasses import dataclass
from enum import StrEnum
from importlib import resources
from random import choice
from typing import Callable

from PIL import Image
from widget import Widget

import bedside

_MEWO_WIDGET = "mewo"


class MewoState(StrEnum):
    SLEEPING = "sleeping"
    DESK = "desk"
    floor = "floor"


def _mewo_img(state: MewoState, z: int) -> Widget:
    with resources.open_binary(bedside, "assets", "mewo", f"{state}.bmp") as f:
        mewo = Image.open(f).convert(mode="1")
    return Widget(name=_MEWO_WIDGET, z=z, bw=mewo)


@dataclass
class Mewo:
    z: int = -99
    state: MewoState | None = None
    asleep: bool = False

    def sleep(self) -> Widget | None:
        if self.asleep:
            return
        new = _mewo_img(MewoState.SLEEPING, self.z) if self.state != MewoState.SLEEPING else None
        self.state = MewoState.SLEEPING
        self.asleep = True
        return new

    def awake(self) -> None:
        self.asleep = False

    def random(self) -> Widget | None:
        if not self.asleep:
            self.state = choice(list(MewoState))
            return _mewo_img(self.state, self.z)
