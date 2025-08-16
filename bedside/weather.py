from enum import StrEnum
from importlib import resources

import aiohttp
from PIL import Image

import bedside
from bedside.widget import Widget, blank

WEATHER_URL = "https://api.open-meteo.com/v1/forecast?latitude=-43.5333&longitude=172.6333&daily=weather_code&timezone=Pacific%2FAuckland&forecast_days=1"
_WEATHER_WIDGET = "weather"


class Weather(StrEnum):
    CLOUDY = "cloudy"
    OVERCAST = "overcast"
    RAIN = "rain"
    SUNNY = "sunny"

    @classmethod
    def from_wmo(cls, code: int):
        match code:
            case 0:
                return Weather.SUNNY
            case 1 | 2:
                return Weather.CLOUDY
            case 3:
                return Weather.OVERCAST
            case 51 | 53 | 55 | 56 | 57 | 61 | 63 | 65 | 66 | 67:
                return Weather.RAIN
        return Weather.SUNNY


async def get_weather_code() -> Weather:
    async with aiohttp.ClientSession() as session:
        async with session.get(WEATHER_URL) as response:
            weather_payload = await response.json()
    return Weather.from_wmo(weather_payload["daily"]["weather_code"][0])


async def get_weather() -> Widget:
    weather_code = await get_weather_code()
    if weather_code == Weather.SUNNY:
        return Widget(name=_WEATHER_WIDGET, z=-99, bw=blank())
    with resources.open_binary(bedside, "assets", "weather", f"{weather_code}.bmp") as f:
        weather = Image.open(f).convert(mode="RGBA")
        return Widget(name=_WEATHER_WIDGET, z=-99, bw=weather)
