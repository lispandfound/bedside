import datetime
from enum import Enum, auto
from importlib import resources

from PIL import Image
from widget import Widget

import bedside


class Season(Enum):
    SUMMER = auto()
    AUTUMN = auto()
    WINTER = auto()
    SPRING = auto()


def get_season(now: datetime.date | None = None) -> Season:
    now = now or datetime.datetime.now().date()
    match now.month:
        case 12 | 1 | 2:
            return Season.SUMMER
        case 3 | 4 | 5:
            return Season.AUTUMN
        case 6 | 7 | 8:
            return Season.WINTER
        case _:
            return Season.SPRING


_BERT_WIDGET = "bert"


def get_bert() -> Widget:
    season = get_season()
    name_lookup = {Season.AUTUMN: "leafless", Season.WINTER: "leafless", Season.SPRING: "bloom", Season.SUMMER: "bloom"}
    name = name_lookup[season]
    with resources.open_binary(bedside, "assets", "bert", f"{name}.bmp") as f:
        bert = Image.open(f).convert(mode="RGBA")
    return Widget(name=_BERT_WIDGET, z=-99, bw=bert)
