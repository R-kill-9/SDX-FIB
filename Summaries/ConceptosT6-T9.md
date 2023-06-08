## Tema 6

### Flat naming

- Los identificadores suelen ser strings de bits aleatorios.
- **PROBLEMA**: Dado un flat name como loaclizamos su dirección asociada?
	- Simple solutions for LANs
	- Home-based approaches
	- Distributed Hash Tables

#### Simple solutions
- Broadcasting
	- Hace broadcast del identificador, haciendo request  de la entity para que se devuelva la dirección.
- Forwarding pointers
	- Usado para localizar entidades móviles.
	- Cada vez que una entidadse mueve deja una referéncia de su nueva localización.

#### Home-based approaches
- Introduce una *home location* que trakea la localización actual de la entidad.
- Los clientes siempre revisan primero la *home location* y posteriormente la localización externa.
![[Pasted image 20230608201134.png]]

#### Distributed Hash Tables
- La hash Table asocia data con keys.
	- La key se hashea para encontrar el bucket en la tabla.
- Para calcular el nodo dado una key podemos usar *key % N*.


### Structured naming
- Los flat names son cómodospara las máquinas pero no para los humanos.
- Debido a esto, se usan nombres estructurados.
	- Ej: file naming: /home/steen/mbox
	- Ej: naming on the Internet: www.cs.vu.nl
- El **Structured name space** es potencialmente infinito.

#### Name spaces
- Un **name space** se representa con un grafo con dos tipos de nodos, hojas y nodos de directorios.
- Cada name space tiene mínimo un nodo root.
- Te puedes referir a un nodo con path names.

#### Name space distribution
- Los name spaces para sistemas grandes se suelen distribuir entre varios name servers, organizados jerárquicamente.
- Para hacerlo de forma eficiente se suele particionar en tres layers lógicos:
	- **Global layer**
		- Directorio de nodos de más alto nivel (root y sus hijos).
		- Se administra por distintos administradores.
		- No suele cambiar nunca.
	- **Administrator layer**
		- Directorio de nodos de nivel medio administrados por una única organización (departamentos en una organización).
		- Cambia poco.
	- **Managerial layer**
		- Directorio de nodos de nivel bajo manejado por una única organización (hosts en una network local).
		- Los nodos cambian con frecuéncia.
![[Pasted image 20230608202542.png]]

### Name resolution
- Cada cliente accede a un name resolver local.
- Se necesita un **Closure mechanism**
	- Forma implicita de saber el nodo inicial en un name space para saber por donde empezar la name resolution.
		- Ej: *UNIX file system*: saber la localización del inodo root.
		- Ej: *DNS*: Saber el nombre del root server.




## Tema 7

### Architecture

#### Client-server architectures
- Modelo de acceso remoto(remote file service)
	- Casi todo el trabajo se hace en el servidor.
	- Coste de comunicación bajo para  abrir ficheros, alto para operar con ellos.
- Modelo Upload/Downkoad
	- Casi todo el trabajo se hace en el cliente.
	- Coste de comunicación alto para abrir ficheros, bajo para operar con ellos.
- Stateful server
	- Server provee operaciones open & close de ficheros.
	- Server mantiene el estado del cliente entre distintos requests del mismo fichero.
- Stateless server
	- Server no mantiene ninguna información del cliente entre requests.

##### NFS
- Client-server file system.
- Modelo de file system similar a UNIX
- Anteriormente era stateless, la v4 es stateful

**VFS**: Provee acceso a los archivos locales y remotos de forma uniforme y *transparente*.
![[Pasted image 20230608185840.png]]
##### Coda
- Basado en AFS v2
- Filosofia: **Scalability and avaliability are more important than consistency**.
- Usa modelo Upload/download
- Tiene unos pocos Vice file servers y muchos Virtue clients.
![[Pasted image 20230608185810.png]]

#### Cluster-based architectures
- Para mejorar el funcionamiento en los servidores situados en clusters usan File-striping.
- **File-striping**: Técnica con la que los ficheros se obtienen en paralelo.



### Communication

#### NFS communication
- Comunicación basada en RPCs.
- NFS v4 soprta *commpound procedures*.
	- **compound procedures**: Agrupación de múltiples RPCs en una única request.
	- Se tratan por separado.
	- Si una falla el resto se terminan.

#### Coda communication
- Basada en RPC2
- Transmisión confiable sobre UDP.
- Soporte para multicast transparente para el cliente.

### Naming

#### NFS naming
- Acceso *Transparente* a un file system remoto.
	-  Permite al cliente montar un file system remoto en su file system local.
- *Pathnames* no son globalmente únicos, dependen del local name spaces del cliente.
	- Para solucionarlo se estandarizan name spaces como /usr/bin por ejemplo.
- En v4 el name resolution también puede ser recursivo.
![[Pasted image 20230608185908.png]]

#### Coda naming
- *Único* name space compartido de forma global, **/afs**.
- Los archivos se agrupan en volúmenes.
	- Un archivo está solo en un volumen.
	- Los volumenes deben estar montados.
	- Los volumenes tienen que estar replicados en los servidores.

![[Pasted image 20230608185925.png]]


### Synchronization

#### Semantics of file sharing
- En una sola máquina cuando después de write hacemos read el valor que retorna el read es el que se acaba de escribir.
- En un sistema distribuido con caching, se puede dar el casode devolver valores obsoletos.
- 4 métodos para tratar esto:
	- **UNIX semantics**: Cada operación en un archivo debe ser instantáneamente visible para todos los procesos.
	- **Session semantics**: No se muestran los cambios al resto hasta cerrar el archivo.
	- **Immutable files**: No permite updates.
	- **Transactions**: Todos los cambios ocurren automáticos.
- Coda revisa las condiciones de file sharing al abrirse.
- Coda trata a las sesiones como transacciones.

#### File locking
- NFS v1-v3 usaba un lock manager, v4 lo tiene integrado.
- Se pueden hacer reservas compartidas.
- En las reservas compartidas **no** pueden escribir dos a la vez.

### Consistency and replication

#### NFS client-side caching
- Técnicas para mejorar el rendimiento
	- Data and metadata caching
		- Cachea los resultados de las operaciones read, write, etc.
		- Hace los writes localmente.
		- Valida los cache blockscuando se usan con un procedimiento basado en timestamps.
	- Close-to-open cahce consistency
		- Extiende el procedimiento anterior para soportar los típicos escenarios de file sharing.
		- El cliente fuerza un cache validity check con sel servidor *cuando se abreel fichero*.
		- Cuando se cierra el archivo los cambios se flushean al servidor para que el siguiente en abrir el archivo los vea.
- NFS v4 soprta **open delegation**
	- El servidor voluntariamente y temporalmente cede el control de las operaciones en un archivo al cliente.
	- Durante la delegación el cliente puede tratar el archivo sin preocuparse por los conflictos de acceso.
	- Las delegaciones se pueden recuperar por parte del servidor usando *callbacks*.
	- *Cuando un archivo está delegado el propietario maneja los requests para abrir el archivo de lectura.*
	- *Cuando otro cliente hace un request para escribir en un archivo delegado el servidor hace recall del archivo.

#### Coda client-side caching
- Coda crea una copia local del archivo cuando se abre.
- Se mantiene de forma constante usando *callbacks*.
	- Cuando se abre un archivo el cliente recibe una **callback promise**.
	- Cuando otro archivo hace un update al file el servidor rompe la promesa.

### Fault tolerance

#### NFS fault tolerance
- *Communication failures*: La mayoría de operaciones se pueden retirar antes de recibir un reply.
- *Client failures*: El server puede recuperar locks y delegaciones.
- *Server failure*: Cuando se reincia colabora con los clientes para recuperar el estado anterior a la fallada.

#### Coda fault tolerance
- *Disconnected operation*: **high avaliability**.
	- HOARDING
	- EMULATION
	- REINTEGRATION