import torch
import torch.nn as nn
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys
import functools
import operator

def python_save_model(model, filename):
  torch.save(model, filename)

def python_load_model(filename):
  model = torch.load(filename)
  model.eval()
  return(model)

def python_save_csv(data, filename):      
  data.to_csv(filename, index=False)
