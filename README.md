<div align="center">
<h1>Leffe</h1>
</div>

<p align="center">
<em>The <strong>L</strong>abeled <strong>Eff</strong>ects <strong>E</strong>xtension to PureScript</em>
</p>

<hr>

PureScript's `Eff` type is really nice. Sometimes, however, you might want to
associate _resources_ with side-effects, such as file descriptors, sockets, or
byte buffer. Manually passing around such resources to related side-effecting
operations is error-prone, and the compiler might not be able to catch your
mistakes. With _Leffe_, resources are bundled into the monadic side-effecting
operations, and thus safe from being passed around incorrectly.

**This library is highly experimental at this stage, and should probably not
be used for anything but playful things.**

## License

[Mozilla Public License Version 2.0](LICENSE)
