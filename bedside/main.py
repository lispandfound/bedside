import platform


def is_raspberry_pi():
    return "raspberrypi" in platform.uname().node.lower()


import asyncio
from asyncio.queues import Queue
from dataclasses import dataclass
from datetime import datetime
from importlib import resources

from PIL import Image, ImageDraw, ImageFont

import bedside

if is_raspberry_pi():
    from waveshare_epd import epd7in5b_V2

from bedside.mock import MockEPD


@dataclass
class Widget:
    bw: Image.Image
    red: Image.Image
    name: str
    z: int


WIDTH = 800
HEIGHT = 480


async def process_event_loop(queue: Queue[Widget]) -> None:
    if is_raspberry_pi():
        epd = epd7in5b_V2.EPD()
    else:
        epd = MockEPD()

    epd.init()
    epd.Clear()
    widgets: dict[str, Widget] = {}
    while True:
        new_widget = await queue.get()
        widgets[new_widget.name] = new_widget
        bw = Image.new("1", (epd.width, epd.height), 255)
        red = Image.new("1", (epd.width, epd.height), 255)
        for widget in sorted(widgets.values(), key=lambda w: w.z):
            bw.paste(widget.bw)
            red.paste(widget.red)
        epd.display(epd.getbuffer(bw), epd.getbuffer(red))
        await asyncio.sleep(2)
        epd.sleep()


async def background(queue: Queue[Widget]) -> None:
    with resources.open_binary(bedside, "assets", "background.bmp") as f:
        background_image = Image.open(f).convert(mode="1")
    red = Image.new("1", (WIDTH, HEIGHT), 255)
    widget = Widget(bw=background_image, red=red, name="background", z=-100)
    await queue.put(widget)


async def main():
    queue = Queue(10)
    event_loop = asyncio.create_task(process_event_loop(queue))
    background_task = asyncio.create_task(background(queue))
    await asyncio.gather(event_loop, background_task)


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        if is_raspberry_pi():
            epd7in5b_V2.epdconfig.module_exit(cleanup=True)
