import torch
from torchnmf.nmf import NMF
from torchnmf.metrics import beta_div
import matplotlib.pyplot as plt

#V shape = 20, 10
H = torch.rand(20, 5)
W = torch.rand(10, 5)
V = torch.rand(20,10)
rank = 5

print("Source matrix: %s, H: %s, W: %s" %(V.shape, H.shape, W.shape))
plt.figure()
ims = plt.imshow(V)
plt.colorbar(ims)
plt.title("Source V")
plt.show()
plt.close()

print("Training using .fit()")
nmf_base = NMF(H=H, W=W, rank=rank)
for i in range(20):
    nmf_base.fit(V)

plt.figure()
ims = plt.imshow(nmf_base.H.detach())
plt.colorbar(ims)
plt.title("trained H")
plt.show()
plt.close()

plt.figure()
ims = plt.imshow(nmf_base.forward().detach())
plt.colorbar(ims)
beta_loss = beta_div(V, nmf_base.forward().detach())
plt.title("reconstructed V | Beta Div = %s" %(beta_loss))
plt.show()
plt.close()