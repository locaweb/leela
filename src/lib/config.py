import os
import ConfigParser

config = ConfigParser.ConfigParser()
if ("LEELA_CFG" in os.environ):
    config.read(os.environ["LEELA_CFG"])
else:
    config.read("/etc/leela/leela.conf")


def get(*args, **kwargs):
    return(config.get(*args, **kwargs))


def getint(*args, **kwargs):
    return(config.getint(*args, **kwargs))
