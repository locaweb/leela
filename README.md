# LEELA

Leela is a simple, but scalable, property-graph database where
properties can be either time-series or key-value.

It was created to store information about about our datacenter and its
many clients like applications and users of those applications. The
properties provides an additional layer of information that allows to
store historical data about entities like servers, supporting our
monitoring system.

For an example, these entities can be easily represented in leela:


```
      o cpu-usage [time-series]
      |
      |  o hwinfo [json-data]
      |  |
   +---------+                        +--------+
   | machine |----------------------->| switch |
   +---------+                        +--------+
                                           |
           +------------+    +-----+       |
           | datacenter |<---| rak |<------+
           +------------+    +-----+
```
           
## Development

Refer to ```doc/docs/devel-guide/environment.md```

## Install

Refer to ```doc/docs/admin-guide/install-leela.md```

## Contribute

Any help is welcome and there is no formal process. Just remember:

* use good commit messages and provide a good description about what
  you are trying to achieve;
* make sure your patch applies cleanly;
* include/update tests;
* update the documentation;

## Documentation

* http://leela.rtfd.org/

## License

APACHE-2.0

## Author

* dgvncsz0f <dsouza@c0d3.xxx>

## Contributors

* Luiz Ozaki
* Rodrigo Vaz
* Andre Ferraz
* Juliano Martines [former author (v0.0.9)]
* Willian Mollinari

