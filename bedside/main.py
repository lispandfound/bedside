import argparse
import asyncio
import datetime
import logging
from asyncio.queues import Queue
from importlib import resources
from random import randint
from typing import Any, Coroutine

from PIL import Image
from scheduler.asyncio import Scheduler

import bedside
from bedside import epd7in5b_V2
from bedside.mewo import Mewo
from bedside.weather import get_next_sunrise, get_next_sunset, get_night, get_weather
from bedside.widget import Widget

logger = logging.getLogger(__name__)


def display_widgets(epd: epd7in5b_V2.EPD, widgets: dict[str, Widget]) -> None:
    logger.debug("Entering display_widgets with %d widgets", len(widgets))
    epd.init()
    logger.info("EPD initialized")
    epd.Clear()
    logger.info("EPD cleared")

    bw = Image.new("RGBA", (epd.width, epd.height), (255, 255, 255, 0))
    red = Image.new("RGBA", (epd.width, epd.height), (255, 255, 255, 0))

    logger.debug("Starting widget composition")
    for widget in sorted(widgets.values(), key=lambda w: w.z):
        logger.debug("Compositing widget '%s' at z=%d", widget.name, widget.z)
        bw.alpha_composite(widget.bw)
        red.alpha_composite(widget.red)

    bw_final = bw.convert("1")
    red_final = red.convert("1")

    logger.debug("Sending composed image to EPD")
    epd.display(epd.getbuffer(bw_final), epd.getbuffer(red_final))
    logger.info("Display updated with %d widgets", len(widgets))


async def process_event_loop(queue: Queue[Widget], initial_widgets: list[Widget]) -> None:
    logger.debug("Starting process_event_loop")
    epd = epd7in5b_V2.EPD()
    widgets: dict[str, Widget] = {widget.name: widget for widget in initial_widgets}
    logger.info("Initialised event loop with %d widgets", len(widgets))

    while True:
        try:
            logger.debug("Refreshing display with current widgets")
            display_widgets(epd, widgets)
            await asyncio.sleep(2)
            epd.sleep()
            logger.debug("EPD put to sleep")

            logger.debug("Waiting for widget from queue...")
            new_widget = await queue.get()
            logger.info("Received widget '%s' from queue", new_widget.name)
            widgets[new_widget.name] = new_widget
        except Exception:
            logger.exception("Error in process_event_loop")


async def draw_widget_maybe(queue: Queue[Widget], widget: Coroutine[Any, Any, Widget] | Widget | None) -> None:
    logger.debug("Entering draw_widget_maybe with widget=%s", type(widget).__name__)
    try:
        if isinstance(widget, Widget):
            logger.debug("Widget is already materialized: %s", widget.name)
            await queue.put(widget)
        elif widget is not None:
            logger.debug("Awaiting coroutine to produce widget")
            widget_real = await widget
            logger.debug("Coroutine produced widget '%s'", widget_real.name)
            await queue.put(widget_real)
        else:
            logger.debug("No widget to draw (None passed)")
    except Exception:
        logger.exception("Error in draw_widget_maybe")


def schedule_mewo(scheduler: Scheduler, queue: Queue[Widget]) -> None:
    logger.debug("Scheduling Mewo events")
    mewo = Mewo()
    scheduler.hourly(
        datetime.time(minute=randint(0, 59), second=0),
        lambda: draw_widget_maybe(queue, mewo.random()),
    )
    scheduler.daily(
        datetime.time(hour=21, minute=0),
        lambda: draw_widget_maybe(queue, mewo.sleep()),
    )
    scheduler.daily(datetime.time(hour=7, minute=0), lambda: mewo.awake())
    logger.info("Mewo scheduling complete")


async def schedule_sunrise_sunset(
    scheduler: Scheduler, queue: Queue[Widget], latitude: float, longitude: float
) -> None:
    logger.debug("Computing next sunrise/sunset for lat=%s lon=%s", latitude, longitude)
    sunrise = get_next_sunrise(latitude, longitude)
    sunset = get_next_sunset(latitude, longitude)
    logger.info("Next sunrise: %s, sunset: %s", sunrise, sunset)

    scheduler.once(
        sunrise,
        lambda: draw_widget_maybe(queue, get_weather(latitude, longitude)),
    )
    logger.debug(f"Scheduled weather update at {sunrise=}")
    reset = max(sunrise, sunset) + datetime.timedelta(minutes=5)
    scheduler.once(
        reset,
        schedule_sunrise_sunset,
        args=(scheduler, queue, latitude, longitude),
    )
    logger.debug(f"Scheduled recursive sunrise/sunset update check at {reset=}")

    scheduler.once(
        sunset,
        lambda: draw_widget_maybe(queue, get_night()),
    )
    logger.debug(f"Scheduled night mode at {sunset=}")


async def run_scheduler(queue: Queue[Widget], latitude: float, longitude: float):
    logger.debug("Starting scheduler")
    scheduler = Scheduler()
    schedule_mewo(scheduler, queue)
    await schedule_sunrise_sunset(scheduler, queue, latitude, longitude)

    logger.info("Scheduler running")
    logger.info(scheduler)
    while True:
        await asyncio.sleep(1)


async def initialise(latitude: float, longitude: float) -> list[Widget]:
    logger.debug("Initialising widgets")
    with resources.open_binary(bedside, "assets", "background.bmp") as f:
        background_image = Image.open(f).convert("RGBA")
    background_widget = Widget(bw=background_image, name="background", z=-100)
    logger.info("Background widget loaded")

    mewo = Mewo().random()
    widgets = [background_widget]
    if mewo:
        logger.info("Adding Mewo widget: %s", mewo.name)
        widgets.append(mewo)

    weather = await get_weather(latitude, longitude)
    logger.info("Initial weather widget: %s", weather.name)
    widgets.append(weather)

    logger.debug("Initial widgets prepared: %s", [w.name for w in widgets])
    return widgets


async def main(latitude: float, longitude: float):
    logger.info("Starting main with lat=%s lon=%s", latitude, longitude)
    queue = Queue(10)
    event_loop = asyncio.create_task(process_event_loop(queue, await initialise(latitude, longitude)))
    scheduler_task = asyncio.create_task(run_scheduler(queue, latitude, longitude))

    try:
        await asyncio.gather(event_loop, scheduler_task)
    except Exception:
        logger.exception("Fatal error in main loop")
        raise


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
    )

    parser = argparse.ArgumentParser(prog="bedside", description="Bedside room display")
    parser.add_argument("latitude", type=float)
    parser.add_argument("longitude", type=float)
    args = parser.parse_args()

    try:
        asyncio.run(main(args.latitude, args.longitude))
    except Exception as e:
        logger.exception("Bailing due to fatal error")
    finally:
        logger.info("Closing EPD")
        epd7in5b_V2.epdconfig.module_exit(cleanup=True)
