import datetime
from enum import StrEnum
from importlib import resources

import aiohttp
from PIL import Image
from suntime import Sun
from tzfpy import get_tz
from yarl import URL

import bedside
from bedside.widget import Widget, blank

_WEATHER_WIDGET = "weather"


def _weather_url(latitude: float, longitude: float) -> URL:
    timezone = get_tz(longitude, latitude)
    return URL.build(
        scheme="https",
        host="api.open-meteo.com",
        path="/v1/forecast",
        query={
            "latitude": str(latitude),
            "longitude": str(longitude),
            "timezone": timezone,
            "daily": "weather_code",
            "forecast_days": "1",
        },
    )


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


async def get_weather_code(latitude: float, longitude: float) -> Weather:
    async with aiohttp.ClientSession() as session:
        async with session.get(_weather_url(latitude, longitude)) as response:
            weather_payload = await response.json()
    return Weather.from_wmo(weather_payload["daily"]["weather_code"][0])


async def get_weather(latitude: float, longitude: float) -> Widget:
    weather_code = await get_weather_code(latitude, longitude)
    if weather_code == Weather.SUNNY:
        return Widget(name=_WEATHER_WIDGET, z=-99, bw=blank())
    with resources.open_binary(bedside, "assets", "weather", f"{weather_code}.bmp") as f:
        weather = Image.open(f).convert(mode="RGBA")
        return Widget(name=_WEATHER_WIDGET, z=-99, bw=weather)


def get_night() -> Widget:
    with resources.open_binary(bedside, "assets", "weather", "night.bmp") as f:
        night = Image.open(f).convert(mode="RGBA")
        return Widget(name=_WEATHER_WIDGET, z=-99, bw=night)


def get_next_sunrise(latitude: float, longitude: float) -> datetime.datetime:
    sun = Sun(latitude, longitude)
    right_now = datetime.datetime.now().astimezone()
    sunrise_a = sun.get_sunrise_time(right_now, right_now.tzinfo)
    sunrise_b = sun.get_sunrise_time(right_now + datetime.timedelta(days=1), right_now.tzinfo)
    if sunrise_a > right_now:
        return sunrise_a.replace(tzinfo=None)
    return sunrise_b.replace(tzinfo=None)


def get_next_sunset(latitude: float, longitude: float) -> datetime.datetime:
    sun = Sun(latitude, longitude)
    right_now = datetime.datetime.now().astimezone()
    sunset_a = sun.get_sunset_time(right_now, right_now.tzinfo)
    sunset_b = sun.get_sunset_time(right_now + datetime.timedelta(days=1), right_now.tzinfo)
    if sunset_a > right_now:
        return sunset_a.replace(tzinfo=None)
    return sunset_b.replace(tzinfo=None)
