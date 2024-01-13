import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset

# Assuming X is your input data with shape [n, x*y*z]
# and you want to reduce its dimensionality to [d, x*y*z]
n, x, y, z = 100, 10, 10, 10  # Example dimensions
d = 20  # Target dimensionality
X = torch.randn(n, x*y*z)  # Example data

# Define the Restricted Boltzmann Machine (RBM)
class RBM(nn.Module):
    def __init__(self, n_vis, n_hid):
        super(RBM, self).__init__()
        self.W = nn.Parameter(torch.randn(n_hid, n_vis) * 0.01)
        self.v_bias = nn.Parameter(torch.zeros(n_vis))
        self.h_bias = nn.Parameter(torch.zeros(n_hid))

    def sample_h(self, v):
        h_prob = torch.sigmoid(torch.mm(v, self.W.t()) + self.h_bias)
        return h_prob, torch.bernoulli(h_prob)

    def sample_v(self, h):
        v_prob = torch.sigmoid(torch.mm(h, self.W) + self.v_bias)
        return v_prob, torch.bernoulli(v_prob)

    def contrastive_divergence(self, v, k=1):
        v0 = v
        h0_prob, h0 = self.sample_h(v0)
        for _ in range(k):
            v_prob, v = self.sample_v(h0)
            h_prob, h = self.sample_h(v)

        return v0, v, h0, h

    def forward(self, x):
        return self.sample_h(x)[0]

# Define the Autoencoder
class Autoencoder(nn.Module):
    def __init__(self, input_shape, d):
        super(Autoencoder, self).__init__()
        self.encoder = nn.Sequential(
            nn.Linear(input_shape, d),
            nn.ReLU(),
        )
        self.decoder = nn.Sequential(
            nn.Linear(d, input_shape),
            nn.ReLU(),
        )

    def forward(self, x):
        encoded = self.encoder(x)
        decoded = self.decoder(encoded)
        return decoded


class train:
    def __init__(self, rank, num_epochs = 50, lr=(0.01, 1e-3), batch_size=10):
        self.rank = rank
        self.num_epochs = num_epochs
        self.lr_rmb, self.lr_ae = lr    #lr: tuple(rmb, autoencoder)
        self.batch_size = batch_size

    def rmb_train(self):
        rbm = RBM(self.n, d)
        optimizer = optim.Adam(rbm.parameters(), lr=self.lr_rmb)
        for epoch in range(self.num_epochs):
            total_loss = 0
            for data in DataLoader(self.V, batch_size=self.batch_size):
                data = data[0]  # DataLoader returns a tuple
                v0, v, h0, h = rbm.contrastive_divergence(data)
                loss = torch.mean(torch.sum((v0 - v) ** 2, dim=1))
                optimizer.zero_grad()
                loss.backward()
                optimizer.step()
                total_loss += loss.item()
            print(f'Epoch {epoch + 1}, Loss {total_loss / len(X)}')
        return rbm

    def autoencoder_train(self, rbm):
        autoencoder = Autoencoder(self.n, self.rank)
        autoencoder.encoder[0].weight.data = rbm.W.data.t()
        autoencoder.encoder[0].bias.data = rbm.h_bias.data
        optimizer = optim.Adam(autoencoder.parameters(), lr=self.lr_ae)
        criterion = nn.MSELoss()

        for epoch in range(self.num_epochs):
            total_loss = 0
            for data in DataLoader(TensorDataset(X), batch_size=self.batch_size):
                data = data[0]
                optimizer.zero_grad()
                output = autoencoder(data)
                loss = criterion(output, data)
                loss.backward()
                optimizer.step()
                total_loss += loss.item()
            print(f'Epoch {epoch + 1}, Loss {total_loss / len(X)}')
        return autoencoder

    def train(self, V):
        self.n = V.shape[1] * V.shape[2] * V.shape[3]
        self.V = V.reshape((V.shape[0], self.n))
        rbm = self.rmb_train()
        autoencoder = self.autoencoder_train(rbm)
        return rbm, autoencoder

class compute:
    def __init__(self, autoencoder):
        self.autoencoder = autoencoder

    def encoder(self, V):
        H = self.autoencoder.encoder(V)
        return H

    def decoder(self, H):
        V_prime = self.autoencoder.decoder(H)
        return V_prime