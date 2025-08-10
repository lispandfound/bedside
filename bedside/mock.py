from PIL import Image


class MockEPD:
    width = 800  # use your display resolution
    height = 480

    def init(self):
        print("MockEPD init")

    def Clear(self):
        print("MockEPD clear")

    def getbuffer(self, image):
        # Convert PIL Image to raw bytes (simulate)
        return image.tobytes()

    def display(self, bw_buffer, red_buffer):
        # Convert raw bytes back to images and show them side by side
        bw_img = Image.frombytes("1", (self.width, self.height), bw_buffer)
        red_img = Image.frombytes("1", (self.width, self.height), red_buffer)

        # For visibility, convert bw to 'L' and invert colors so black pixels are black
        bw_img = Image.frombytes("1", (self.width, self.height), bw_buffer)
        red_img = Image.frombytes("1", (self.width, self.height), red_buffer)

        bw_pixels = bw_img.load()
        red_pixels = red_img.load()

        combined = Image.new("RGB", (self.width, self.height), "white")
        combined_pixels = combined.load()

        for y in range(self.height):
            for x in range(self.width):
                if red_pixels[x, y] != 255:  # red pixel set
                    combined_pixels[x, y] = (255, 0, 0)
                elif bw_pixels[x, y] != 255:  # bw pixel set
                    combined_pixels[x, y] = (0, 0, 0)
                else:
                    combined_pixels[x, y] = (255, 255, 255)  # white

        combined.show(title="Simulated EPD")

    def sleep(self):
        print("MockEPD sleep")
