import torch

class art_input:
    def __init__(self):
        self.input_compression = torch.rand(20, 1000)
        self.input_decompression = torch.rand(5, 1000)

    def advection(self, H):
        advected = H + 0.1
        print("Advection is done")
        return advected