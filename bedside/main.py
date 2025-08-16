import asyncio
import datetime
from asyncio.queues import Queue
from importlib import resources

from PIL import Image
from scheduler import Scheduler

import bedside
from bedside import epd7in5b_V2
from bedside.mewo import Mewo
from bedside.widget import Widget


async def process_event_loop(queue: Queue[Widget]) -> None:
    epd = epd7in5b_V2.EPD()

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
    widget = Widget(bw=background_image, name="background", z=-100)
    await queue.put(widget)


async def draw_widget_maybe(queue: Queue[Widget], widget: Widget | None) -> None:
    if widget:
        await queue.put(widget)


def schedule_mewo(scheduler: Scheduler, queue: Queue[Widget]):
    mewo = Mewo(z=-99)
    scheduler.minutely(lambda: draw_widget_maybe(queue, mewo.random()))
    scheduler.daily(datetime.time(hour=21, minute=0), lambda: draw_widget_maybe(queue, mewo.sleep()))
    scheduler.daily(datetime.time(hour=7, minute=0), lambda: mewo.awake())


async def run_scheduler(queue: Queue[Widget]):
    scheduler = Scheduler()
    schedule_mewo(scheduler, queue)
    while True:
        await asyncio.sleep(1)


async def main():
    queue = Queue(10)
    event_loop = asyncio.create_task(process_event_loop(queue))
    background_task = asyncio.create_task(background(queue))
    scheduler_task = asyncio.create_task(run_scheduler(queue))
    await asyncio.gather(
        event_loop,
        background_task,
        scheduler_task,
    )


if __name__ == "__main__":
    try:
        asyncio.run(main())
    finally:
        epd7in5b_V2.epdconfig.module_exit(cleanup=True)
