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
| Límites de los archivos | 4096 entradas, 256 MiB por entrada, 512 MiB en total |

## Hallazgos de la auditoría y estado

La auditoría cubrió `cryptonote`, la pila criptográfica compartida
(`functora-core`) y la pila de interfaz compartida (`functora-egui`). Cada
corrección de la lista viene con una prueba de regresión en el directorio
`tests/` del crate.

| Hallazgo | Gravedad | Ubicación | Estado |
| --- | --- | --- | --- |
| 1. Se aceptaba una contraseña de menos de 12 caracteres sin ningún aviso. | Alta | `cryptonote/src/hooks.rs` | Corregido: aviso no bloqueante |
| 2. El coste de Argon2id se podía bajar con `FUNCTORA_KDF_M_COST_KIB` / `FUNCTORA_KDF_T_COST` en compilaciones de depuración, convirtiendo un texto cifrado público en un objetivo barato de descifrado sin conexión. | Alta | `functora-core/src/crypto.rs` | Corregido: el coste es una constante de tiempo de compilación |
| 3. La contraseña se guardaba en un `String` normal, nunca se ponía a cero y el cuerpo de la nota, la contraseña y el contenido de los adjuntos eran alcanzables desde la implementación derivada de `Debug`. | Media-Alta | `cryptonote/src/state.rs` | Corregido: `Zeroizing<String>` y un `Debug` que oculta datos |
| 4. Un texto cifrado de longitud cero producía un texto en claro vacío sin ninguna verificación AEAD, así que una nota truncada se autenticaba como una nota vacía. | Media | `functora-core/src/crypto.rs` | Corregido: el texto en claro vacío se sella y se verifica como cualquier otro |
| 5. `Vec::with_capacity(file.size())` confiaba en el tamaño declarado de la entrada, así que un archivo manipulado podía pedir una reserva arbitraria de memoria. | Media | `functora-core/src/package.rs`, `functora-core/src/zip.rs` | Corregido: límites por entrada y totales, sin reservar memoria a partir de tamaños no confiables |
| 6. Los metadatos del archivo no están autenticados y eligen la rama cifrada o la de texto plano. Un archivo cifrado se abría con una contraseña vacía porque la contraseña vacía era una clave válida. | Media | `functora-core/src/package.rs` | Corregido: un archivo cifrado rechaza una contraseña vacía |
| 7. Copiar la nota descifrada al portapapeles y exportar `cryptonote-unlocked.zip` dejaban el texto en claro sin ningún aviso. | Media | `cryptonote/src/screens/view.rs` | Corregido: avisos no bloqueantes |
| 8. `egui_commonmark` se compilaba con `load-images`, `svg` y `fetch`, así que mostrar una nota podía lanzar peticiones de red salientes con URLs de imágenes controladas por el atacante. | Media | `functora-egui/Cargo.toml` | Corregido: las funciones de descarga son opcionales (`markdown-fetch`) y Cryptonote no las activa |
| 9. `egui_extras` se compilaba con los cargadores `file` y `http`, que podían leer archivos locales o descargar URLs remotas desde el contenido de una nota. | Media | `functora-egui/Cargo.toml` | Corregido: movidas a la función opcional `images-remote` |
| 10. La dependencia `zip` 2.4.2 arrastraba cuatro avisos, entre ellos un escape crítico por enlaces simbólicos en `ZipArchive::extract` (RUSTSEC-2026-0369). | Media | `Cargo.lock` | Corregido: actualizado a `zip` 9.5.1 |
| 11. El saneador `ammonia` 4.1.4 arrastraba cuatro avisos de XSS, entre ellos XSS por mutación a través de SVG y MathML incrustados (RUSTSEC-2026-0533). | Media | `functora-core/Cargo.toml` | Corregido: actualizado a `ammonia` 4.2.5 |
| 12. El analizador `pulldown-cmark` 0.13.4 podía provocar un crecimiento exponencial o cuadrático de eventos con enlaces wiki anidados (RUSTSEC-2026-0414, RUSTSEC-2026-0433). | Alta | `functora-core/Cargo.toml` | Corregido: actualizado a `pulldown-cmark` 0.13.6 |
| 13. La dependencia `rustls` 0.23.43 arrastraba cuatro avisos en la ruta que usa el cargador de imágenes remotas. | Media | `Cargo.lock` | Corregido: actualizado a `rustls` 0.23.46 |
| 14. Los nombres de los adjuntos tomados de un archivo se usaban tal cual, así que un archivo manipulado podía incluir componentes de ruta `../`. | Baja | `cryptonote/src/archive.rs` | Corregido: los nombres se reducen a un único componente seguro |
| 15. `StreamParts::recover` siempre derivaba la clave con Argon2id e ignoraba la KDF registrada en los metadatos. | Baja | `functora-core/src/crypto.rs` | Corregido: se respeta la KDF registrada |
| 16. `derive_key` devolvía un `Vec<u8>` normal, así que la clave derivada se quedaba en memoria liberada. | Baja | `functora-core/src/crypto.rs` | Corregido: el tipo de retorno es `Zeroizing<Vec<u8>>` |
| 17. La compilación no tenía ningún escáner de vulnerabilidades de dependencias, así que un aviso nuevo en el árbol no se habría detectado. | Baja | `rust/flake.nix` | Corregido: `cargo-audit` y `cargo-deny` están en el entorno de desarrollo, `rust/deny.toml` contiene la política y `nix develop -c audit` vigila todo el espacio de trabajo |
| 18. El README y la pantalla Acerca de la aplicación afirmaban que la aplicación era "totalmente sin conexión" y que el cifrado autenticado aportaba "autenticidad". Ambas afirmaciones son inexactas. | Documentación | `cryptonote/README.md`, `cryptonote/src/messages.rs` | Corregido: corregido en inglés, español y ruso |
| 19. Sin autenticidad del emisor y sin secreto hacia adelante. | Diseño | - | Aceptado: documentado arriba como exclusión |

### Dependencias con avisos conocidos y aceptados

`cargo audit` informa de un pequeño número de avisos *sin mantenimiento* para
dependencias transitivas sin una ruta de actualización segura: `basic-toml`
(a través de `askama`), `ttf-parser` (a través de `ab_glyph`) y `zbus_names`,
`zvariant` y `zvariant_derive` (a través de la pila de portales de escritorio
Linux). Ninguno es una vulnerabilidad, y ninguno forma parte del camino
binario de Cryptonote que gestiona el contenido de las notas.

## Cómo reproducir la auditoría

Desde el directorio `rust/`:

```sh
nix develop -c audit
```

Eso ejecuta `cargo audit` y `cargo deny check` sobre todos los crates que
tienen un `Cargo.lock`, usando la política de `rust/deny.toml`. Las pruebas de
regresión de cada hallazgo están en el directorio `tests/` del crate que le
corresponde:

- `functora-core/tests/security_tests.rs` y `functora-core/tests/kdf_policy_tests.rs`
- `cryptonote/tests/security_crypto_tests.rs`
- `cryptonote/tests/security_archive_tests.rs`
- `cryptonote/tests/security_warning_tests.rs`
- `cryptonote/tests/security_screen_tests.rs`

## Alcance de este documento

Este documento describe la aplicación `cryptonote` y las partes de
`functora-core` y `functora-egui` de las que depende. No describe los demás
crates del monorepo, que no forman parte del camino de gestión de notas.
