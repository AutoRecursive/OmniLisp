#lang racket

(import (numpy np)
        (:from sklearn.linear_model LinearRegression))

(define x-train
  (py-call np.array
           (list (list 1 1)
                 (list 1 2)
                 (list 2 2)
                 (list 2 3))
           #:dtype "float64"))

(define y-train
  (py-call np.array
           (list 6 8 9 11)
           #:dtype "float64"))

(define model (py-call LinearRegression))
(py-call model.fit x-train y-train)

(define prediction
  (py-call model.predict
           (py-call np.array
                    (list (list 3 5))
                    #:dtype "float64")))

(py-call print prediction)
(py-call print model.coef_)
(py-call print model.intercept_)
