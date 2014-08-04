# -*- coding: utf-8 -*-

import random

def rnd_name(size=10, alphabet="abcdefghijklmnopqrstuvxwyzABCDEFGHIJKLMNOPQRSTUVXWYZ0123456789_- "):
    return("".join(random.sample(alphabet, min(size, len(alphabet)))))
