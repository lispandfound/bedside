# Rewrite of the original manufacturer driver to be less bad

import gpiozero
import spidev

class EPD:
    RST_PIN: int = 17
    DC_PIN: int = 25
    CS_PIN: int = 8
    BUSY_PIN: int = 24
    PWR_PIN: int = 18
    MOSI_PIN: int = 10
    SCLK_PIN: int = 11

    def __init__(self) -> None:
        self.reset_pin =
