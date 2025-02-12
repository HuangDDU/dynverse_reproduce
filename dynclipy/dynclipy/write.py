import rpy2.robjects as ro
import rpy2.rinterface as rinterface

from rpy2.robjects import pandas2ri

import pandas as pd

def write_output(x, file, output_ids):
    assert isinstance(x, dict)

    ro.globalenv["x"] = x
    ro.globalenv["file"] = file
    ro.globalenv["output_ids"] = output_ids
    ro.r("dyncli::write_output(x = x, file = file, output_ids = output_ids)")


@ro.conversion.py2rpy.register(dict)
def convert_dict(obj):
    return ro.ListVector(obj)


@ro.conversion.py2rpy.register(list)
def convert_dict(obj):
    if all([isinstance(x, str) for x in obj]):
        return ro.StrVector(obj)
    elif all([isinstance(x, int) | isinstance(x, float) for x in obj]):
        return ro.IntVector(obj)
    elif all([isinstance(x, bool) for x in obj]):
        return ro.BoolVector(obj)
    elif all([isinstance(x, float) for x in obj]):
        return ro.FloatVector(obj)

    return ro.ListVector(obj)
