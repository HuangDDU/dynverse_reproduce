import dynclipy
import pandas as pd

cell_ids = pd.Series(["a", "b", "c"])
dataset = dynclipy.wrap_data(cell_ids = cell_ids)
dataset.add_linear_trajectory(pseudotime = pd.Series({"a":1, "b":0.8, "c":0.1}))
dataset.add_dimred(pd.DataFrame({"cell_id":cell_ids, "comp_1":[0, 0, 4], "comp_2":[1,1, 4]}))
dataset.write_output("test.h5")