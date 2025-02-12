import rpy2.robjects as ro
from rpy2.robjects.packages import importr
import random
import string

class Dataset():
    id = None
    def __init__(self, id):
        self.id = id
    
    def call_function_add(self, function, *args, **kwargs):
        call_function(self.id, function, True, *args, **kwargs)

    def write_output(self, file):
        ro.r(f"dyncli::write_output({self.id}, '{file}')")

def add_adder(cls, function):
    def adder(self, *args, **kwargs):
        self.call_function_add(function, *args, **kwargs)
        return self
    setattr(cls, function, adder)

# add函数的定义来源于wrapper包里的函数
# add for every allowed output in dynwrap a function to Dataset
for output in ro.r("dynwrap::allowed_outputs$output_id"):
    add_adder(Dataset, "add_" + output)

# 函数的调用直接调用R
# call a function in R, without returning the object in python
def call_function(id, function, add, *args, **kwargs):
    ro.globalenv["args"] = ro.ListVector([["wazzup", x] for x in args])
    ro.globalenv["kwargs"] = ro.ListVector([[name, x] for name, x in kwargs.items()])

    ro.r("names(args) <- NULL")

    if add:
        fun = f"{id} = do.call(dynwrap::{function}, c(list({id}), args, kwargs));NULL"
        ro.r(fun)
    else:
        fun = f"{id} = do.call(dynwrap::{function}, c(args, kwargs))"
        ro.r(fun)

    return id

# 除了add函数外,这里还有wrap数据的两个函数,直接在这里写就行了
# create a Dataset() based on data
def wrap_data(*args, **kwargs):
    id = random_id() # R与Python的对象就是靠着这个ID来连接的, ID就是R中dataset的变量名
    call_function(id, "wrap_data", False, *args, **kwargs)
    return Dataset(id) 

# create 
def wrap_expression(*args, **kwargs):
    id = random_id()
    call_function(id, "wrap_expression", False, *args, **kwargs)
    return Dataset(id)

def random_id():
    return ''.join(random.choice(string.ascii_uppercase) for _ in range(20))