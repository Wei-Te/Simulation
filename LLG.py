import numpy as np
import pandas as pd
def LLG(z_0, size_LLG):
    z_LLG = [z_0]
    m_LLG = 2 ** 31 - 1
    if(z_0 > 0):
        for i in range(size_LLG):
            z_LLG = np.vstack(([z_LLG, 16807*z_LLG[i]%m_LLG]))
        z_sample = z_LLG/m_LLG
        z_seed = z_LLG[size_LLG]
        ans = pd.Series(z_sample, z_seed, index = ['sample', 'seed'])
        return(ans)
    else:
        print("z_0 must > 0")
