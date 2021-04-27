#lang racket/base


(module+ main
  #reader "triple-quoted-string.rkt"
  """
  hello world
  how are you
  """)
