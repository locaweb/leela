import os
import ConfigParser

def default_config_file():
    config = ConfigParser.ConfigParser()
    if ("LEELA_CFG" in os.environ):
        return(os.environ["LEELA_CFG"])
    else:
        return("/etc/leela/leela.conf")

def read_config(f=default_config_file()):
    cfg = ConfigParser.ConfigParser()
    cfg.read(f)
    return(cfg)
