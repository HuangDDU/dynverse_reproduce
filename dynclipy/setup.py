from setuptools import setup

setup(
    name='dynclipy',
    version='0.1',
    description='Wrapper around dynwrap/cli',
    url='http://github.com/dynverse/dynclipy',
    author='Wouter Saelens',
    author_email='wouter.saelens@gmail.com',
    license='MIT',
    packages=['dynclipy'],
    install_requires = [
        "rpy2",
        "scipy",
        "numpy",
        "pandas>=1.0",
        "tzlocal", # necessary for pandas2ri
        "ipython"
    ],
    zip_safe=False,
    test_suite='nose.collector',
    tests_require=['nose'],
)
