from unittest import TestCase
import dynclipy

class TestR2Py(TestCase):
    def test_data(self):
        data = dynclipy.ro.r("""
        list(
            matrix = matrix(runif(100), nrow = 10, ncol = 10),
            sparse_matrix = Matrix::sparseMatrix(i = sample(100), j = sample(100), x = runif(100)),
            nested_list = list(
                atom = 1,
                unnamed_list = list(
                    1, 2, 3
                ),
                named_list = list(
                    a = "1",
                    b = 2,
                    c = TRUE
                )
            ),
            df = data.frame(x = 1:10, y = letters[1:10]),
            tibble = tibble::tibble(x = 1:10, y = letters[1:10])
        )
        """)

        self.assertTrue(dynclipy.check_conversion_rpy2py(data))

matrix = dynclipy.ro.r("matrix(runif(100), nrow = 10, ncol = 10)")
sparse_matrix = dynclipy.ro.r("Matrix::sparseMatrix(i = sample(100), j = sample(100), x = runif(100))")
df = dynclipy.ro.r("df = data.frame(x = 1:10, y = letters[1:10])")
tibble = dynclipy.ro.r("tibble::tibble(x = 1:10, y = letters[1:10])")

data = dynclipy.ro.r("""
list(
    sparse_matrix = Matrix::sparseMatrix(i = sample(100), j = sample(100), x = runif(100)),
    nested_list = list(
        atom = 1,
        unnamed_list = list(
            1, 2, 3
        ),
        named_list = list(
            a = "1",
            b = 2,
            c = TRUE
        )
    ),
    df = data.frame(x = 1:10, y = letters[1:10]),
    tibble = tibble::tibble(x = 1:10, y = letters[1:10])
)
""")