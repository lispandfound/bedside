import asyncio
import datetime
from asyncio.queues import Queue
from importlib import resources
from random import randint
from typing import Any, Coroutine

from PIL import Image
from scheduler.asyncio import Scheduler

import bedside
from bedside import epd7in5b_V2
from bedside.mewo import Mewo
from bedside.weather import get_weather
from bedside.widget import Widget


async def process_event_loop(queue: Queue[Widget]) -> None:
    epd = epd7in5b_V2.EPD()

    widgets: dict[str, Widget] = {}
    while True:
        new_widget = await queue.get()
        epd.init()
        epd.Clear()
        widgets[new_widget.name] = new_widget
        bw = Image.new("RGBA", (epd.width, epd.height), (255, 255, 255, 0))
        red = Image.new("RGBA", (epd.width, epd.height), (255, 255, 255, 0))

        for widget in sorted(widgets.values(), key=lambda w: w.z):
            bw.alpha_composite(widget.bw)  # preserves alpha
            red.alpha_composite(widget.red)

        bw_final = bw.convert("1")
        red_final = red.convert("1")

        epd.display(epd.getbuffer(bw_final), epd.getbuffer(red_final))
        await asyncio.sleep(2)
        epd.sleep()


async def background(queue: Queue[Widget]) -> None:
    with resources.open_binary(bedside, "assets", "background.bmp") as f:
        background_image = Image.open(f).convert("RGBA")
    widget = Widget(bw=background_image, name="background", z=-100)
    await queue.put(widget)


async def draw_widget_maybe(queue: Queue[Widget], widget: Coroutine[Any, Any, Widget] | Widget | None) -> None:
    if isinstance(widget, Widget):
        await queue.put(widget)
    elif widget is not None:
        widget_real = await widget
        await queue.put(widget_real)


def schedule_mewo(scheduler: Scheduler, queue: Queue[Widget]):
    mewo = Mewo(z=-99)
    scheduler.hourly(datetime.time(minute=randint(0, 59), second=0), lambda: draw_widget_maybe(queue, mewo.random()))
    scheduler.daily(datetime.time(hour=21, minute=0), lambda: draw_widget_maybe(queue, mewo.sleep()))
    scheduler.daily(datetime.time(hour=7, minute=0), lambda: mewo.awake())
    scheduler.minutely(datetime.time(second=0), lambda: draw_widget_maybe(queue, get_weather()))
    # scheduler.daily(datetime.time(hour=6, minute=0), lambda: draw_widget_maybe(queue, get_weather()))


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
    except Exception as e:
        print("Bailing!")
        print(e)
    finally:
        print("Closing EPD")
        epd7in5b_V2.epdconfig.module_exit(cleanup=True)
