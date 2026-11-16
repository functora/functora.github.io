# Política de seguridad

Cryptonote es una aplicación para compartir notas. Este documento explica qué
protege, qué no protege a propósito y qué problemas de seguridad se han
encontrado y corregido.

## Cómo reportar una vulnerabilidad

Reporta las vulnerabilidades que sospeches en privado a
**functora@proton.me**. No abras un issue público para un exploit que funcione.
Incluye la versión del crate, la plataforma, los pasos para reproducirlo y el
impacto que observaste. Puedes esperar una confirmación en una semana.

## Activos

| Activo | Dónde vive | Exposición |
| --- | --- | --- |
| Cuerpo de la nota | `TemporaryState.note` en memoria, mostrado en la interfaz | Se copia al portapapeles, se exporta como archivo o se muestra en pantalla |
| Contraseña | `TemporaryState.password` en memoria, pasada a la KDF | Nunca se escribe en la URL, el archivo, el registro ni el disco |
| Clave derivada | `Zeroizing<Vec<u8>>` dentro de la capa criptográfica | Se borra al descartarse |
| Texto cifrado | Cadena de consulta de la URL, archivo `.cryptonote` | Público por diseño una vez compartido |
| Metadatos de la nota | Cadena de consulta de la URL, `metadata.json` del archivo | Público por diseño una vez compartido |

## Límites de confianza

1. **El dispositivo.** Cryptonote confía en el sistema operativo en el que se
   ejecuta. Un sistema operativo comprometido, un teclado malicioso o una
   sesión de pantalla compartida pueden leer la nota y la contraseña.
2. **Cualquiera que reciba el enlace.** Un enlace compartido es la nota.
   Quien lo reciba - por correo, chat, una captura de pantalla o un lector de
   códigos QR - puede abrirla. En modo Texto plano la leerán directamente. En
   modo cifrado reciben el texto cifrado, y la contraseña es lo único que se
   interpone.
3. **La barra de direcciones de la página web.** En la versión web la nota
   viaja dentro de la URL, así que forma parte de la dirección. Todo lo que
   gestiona URLs - el historial del navegador, la ruta de red por la que se
   descargó la página - puede verla. Las versiones nativas no tienen esa
   dirección.
4. **El archivo.** Un archivo `.cryptonote` es entrada no confiable. Todos sus
   campos, incluidos los nombres de las entradas y los tamaños declarados,
   están controlados por el atacante.

## Modelo de amenazas

**Adversarios.**

- *Observador de la red.* Ve el enlace, no el texto en claro.
- *Poseedor del enlace.* Tiene el texto cifrado y la sal y puede adivinar
  contraseñas sin conexión, sin límite de intentos.
- *Cualquiera que lea la URL.* Todo el contenido de la nota viaja en la
  dirección, así que todo lo que gestiona direcciones ve el texto cifrado o,
  en modo Texto plano, el cuerpo de la nota.
- *Autor malicioso de un archivo.* Crea un archivo `.cryptonote` para agotar la
  memoria, bloquear la aplicación o escapar del directorio de descarga.

**Fuera de alcance.**

- Un dispositivo, una extensión del navegador o un sistema operativo
  comprometidos.
- El análisis del tráfico y la correlación de patrones de enlaces.
- Los destinatarios maliciosos que reciben una nota que elegiste compartir con
  ellos.
- El criptoanálisis de AES-GCM, ChaCha20-Poly1305 o Argon2id.

## Lo que Cryptonote garantiza

- **Confidencialidad y detección de manipulaciones en las notas cifradas.**
  AES-256-GCM o ChaCha20-Poly1305 con una clave de 256 bits, un nonce aleatorio
  de 96 bits y una sal aleatoria de 32 bytes por nota.
- **Derivación de clave a partir de una contraseña.** Argon2id con un coste
  fijado en tiempo de compilación de 64 MiB, 3 iteraciones y paralelismo 1. El
  coste es una constante de la biblioteca: ninguna variable de entorno, marca
  de compilación o archivo de configuración puede bajarlo.
- **Metadatos autenticados.** El cifrado, la KDF y el prefijo de la aplicación
  se vinculan como datos asociados del AEAD, y los metadatos del archivo
  registran la KDF que se usó realmente.
- **Los metadatos no pueden elegir la rama.** Cada carga útil de un archivo
  empieza con un marco de 16 bytes que indica la rama en la que se escribió. El
  marco se vincula como datos asociados del AEAD en la rama cifrada, y los
  metadatos que no coinciden con el marco se rechazan en lugar de cambiar de
  rama.
- **Un texto cifrado vacío es un fallo de descifrado.** Todo texto cifrado,
  incluido el texto en claro vacío, lleva una etiqueta de autenticación que
  debe verificarse.
- **Análisis de archivos limitado.** Como máximo 4096 entradas, 256 MiB por
  entrada y 512 MiB en total. Los tamaños declarados nunca se usan para
  reservar memoria.
- **Un archivo cifrado no se puede abrir sin contraseña.** Una contraseña
  vacía se rechaza de antemano.
- **Nombres de adjuntos seguros.** Los nombres de las entradas del archivo se
  reducen a un único componente de ruta, sin separadores ni puntos iniciales,
  antes de llegar a la interfaz o al sistema de archivos.
- **Ninguna fuga accidental de la contraseña.** La contraseña nunca forma parte
  de una URL, un archivo, una línea de registro ni una representación `Debug`.
  Tampoco el cuerpo de la nota ni el contenido de los adjuntos.

## Exclusiones explícitas y riesgos aceptados

Estas son propiedades que la aplicación no tiene. Son decisiones de diseño, no
errores.

1. **Sin autenticidad del emisor.** El AEAD detecta manipulaciones, no un
   autor falsificado. Cualquiera que conozca la contraseña puede producir una
   nota que se descifra sin problemas. Cryptonote no firma las notas y no puede
   decirte quién escribió una.
2. **Sin secreto hacia adelante.** La clave se deriva solo de la contraseña. Si
   la contraseña se filtra más tarde, todas las notas compartidas con ella se
   vuelven legibles.
3. **Sin secreto hacia adelante frente a un texto cifrado grabado.** Un enlace
   compartido expone el texto cifrado, la sal y el nonce, así que la contraseña
   se puede atacar sin conexión al coste completo de Argon2id.
4. **El modo Texto plano no es cifrado.** Una nota en Texto plano pone el
   cuerpo en la URL en base64url, lo cual es codificación, no cifrado. Cualquiera
   que reciba el enlace puede leer el texto. Cryptonote avisa antes de
   compartir.
5. **Las contraseñas débiles se avisan, no se rechazan.** Una contraseña de
   menos de 12 caracteres activa un aviso no bloqueante; el uso compartido
   continúa. Cryptonote nunca le quita la elección al usuario, pero el aviso es
   la única mitigación que la aplicación puede ofrecer frente al descifrado sin
   conexión.
6. **El portapapeles es una fuga.** Copiar la nota descifrada la deja donde
   otras aplicaciones pueden leerla. Cryptonote avisa cuando lo haces.
7. **La exportación sin cifrar es una fuga.** `cryptonote-unlocked.zip` contiene
   los adjuntos en claro. Cryptonote avisa cuando la exportas.
8. **La URL es el transporte.** El contenido de la nota está en el propio
   enlace, así que todos los que reciben el enlace pueden leer la nota: el
   navegador que la abre, la aplicación de chat que la transporta, la persona a
   la que se reenvía. Es un intercambio deliberado por el uso compartido
   únicamente con enlaces y no va a cambiar.
9. **La versión web no funciona sin conexión.** Es una página web, así que
   necesita conexión para cargarse. Las versiones nativas no necesitan
   conexión de red.

## Parámetros criptográficos

| Parámetro | Valor |
| --- | --- |
| Cifrados | AES-256-GCM, ChaCha20-Poly1305 |
| KDF | Argon2id, v1.3 |
| Memoria de Argon2 | 65536 KiB (64 MiB) |
| Iteraciones de Argon2 | 3 |
| Paralelismo de Argon2 | 1 |
| Longitud de la clave | 32 bytes |
| Longitud de la sal | 32 bytes, aleatoria por nota |
| Longitud del nonce | 12 bytes, aleatorio por nota |
| Bloque del flujo | 64 KiB |
| Etiqueta del flujo | 16 bytes |
| Marco de la carga útil | 16 bytes, indica la rama |
| Límites de los archivos | 4096 entradas, 256 MiB por entrada, 512 MiB en total |

### Formato del archivo

Un archivo `.cryptonote` contiene dos entradas: `metadata.json` y
`payload.cpt`. La carga útil empieza con un marco de 16 bytes que indica la
rama en la que se escribió: `cryptonote-plain` o `cryptonote-encry`. El marco
forma parte de los datos asociados del AEAD de la rama cifrada, así que el
texto cifrado se autentica junto con él, y un `metadata.json` editado no puede
mover un archivo a la otra rama. Los archivos escritos antes de que existiera
el marco no se pueden leer con esta versión.

## Dependencias con avisos conocidos y aceptados

`cargo audit` informa de un pequeño número de avisos *sin mantenimiento* para
dependencias transitivas sin una ruta de actualización segura: `basic-toml`
(a través de `askama`), `ttf-parser` (a través de `ab_glyph`) y `zbus_names`,
`zvariant` y `zvariant_derive` (a través de la pila de portales de escritorio
Linux). Ninguno es una vulnerabilidad, y ninguno forma parte del camino
binario de Cryptonote que gestiona el contenido de las notas.

## Verificación

Desde el directorio `rust/`:

```sh
nix develop -c audit
```

Eso ejecuta `cargo audit` y `cargo deny check` sobre todos los crates que
tienen un `Cargo.lock`, usando la política de `rust/deny.toml`. Las garantías
de arriba están cubiertas por las pruebas de regresión del directorio `tests/`
del crate al que pertenecen:

- `functora-core/tests/security_tests.rs` y `functora-core/tests/kdf_policy_tests.rs`
  cubren la derivación de la clave, el texto cifrado y el formato del archivo.
- `functora-core/tests/files_tests.rs` cubre la ocultación del contenido de
  los archivos.
- `cryptonote/tests/security_crypto_tests.rs` cubre el contenido de la URL
  compartida y de la carga útil del archivo.
- `cryptonote/tests/security_archive_tests.rs` cubre la rama del archivo y los
  nombres de los adjuntos.
- `cryptonote/tests/security_warning_tests.rs` cubre los avisos de contraseña,
  portapapeles y exportación.
- `cryptonote/tests/security_screen_tests.rs` cubre la pantalla de seguridad.
- `cryptonote/tests/markdown_tests.rs` mantiene sincronizados los tres
  documentos de política.

## Alcance de este documento

Este documento describe la aplicación `cryptonote` y las partes de
`functora-core` y `functora-egui` de las que depende. No describe los demás
crates del monorepo, que no forman parte del camino de gestión de notas.
