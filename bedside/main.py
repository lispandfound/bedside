import platform


def is_raspberry_pi():
    return "raspberrypi" in platform.uname().node.lower()


import asyncio
from asyncio.queues import Queue
from dataclasses import dataclass
from datetime import datetime

from PIL import Image, ImageDraw, ImageFont

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


async def show_time(queue: Queue[Widget]) -> None:
    while True:
        bw = Image.new("1", (WIDTH, HEIGHT), 255)
        red = Image.new("1", (WIDTH, HEIGHT), 255)
        bw_draw = ImageDraw.ImageDraw(bw)
        font24 = ImageFont.truetype("Font.ttc", 48)
        now = datetime.now()
        bw_draw.text((200, 20), now.strftime("%H:%M"))
        widget = Widget(name="time", bw=bw, red=red, z=0)
        await queue.put(widget)
        await asyncio.sleep(60)


async def main():
    queue = Queue(10)
    event_loop = asyncio.create_task(process_event_loop(queue))
    time = asyncio.create_task(show_time(queue))
    await asyncio.gather(event_loop, time)


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        if is_raspberry_pi():
            epd7in5b_V2.epdconfig.module_exit(cleanup=True)
