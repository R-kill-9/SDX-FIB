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
	- Cada vez que una entidad se mueve deja una referéncia de su nueva localización.

#### Home-based approaches
- Introduce una *home location* que trackea la localización actual de la entidad.
- Los clientes siempre revisan primero la *home location* y posteriormente la localización externa.
![](Pasted%20image%2020230608201134.png)

#### Distributed Hash Tables
- La hash Table asocia data con keys.
	- La key se hashea para encontrar el bucket en la tabla.
- Para calcular el nodo dado una key podemos usar *key % N*.


### Structured naming
- Los flat names son cómodos para las máquinas pero no para los humanos.
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
![](Pasted%20image%2020230608202542.png)

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
![](Pasted%20image%2020230608185840.png)
##### Coda
- Basado en AFS v2
- Filosofia: **Scalability and avaliability are more important than consistency**.
- Usa modelo Upload/download
- Tiene unos pocos Vice file servers y muchos Virtue clients.
![](Pasted%20image%2020230608185810.png)

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
![](Pasted%20image%2020230608185908.png)

#### Coda naming
- *Único* name space compartido de forma global, **/afs**.
- Los archivos se agrupan en volúmenes.
	- Un archivo está solo en un volumen.
	- Los volumenes deben estar montados.
	- Los volumenes tienen que estar replicados en los servidores.

![](Pasted%20image%2020230608185925.png)


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
	- *Cuando otro cliente hace un request para escribir en un archivo delegado el servidor hace recall del archivo*.

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

## Tema 8

## Architecture

#### Traditional Web-based system
- Los documentos pueden sre texto plano, HTML, XML, imágenes, etc.
- Pueden incluir *scripts* que se ejecuten en el cliente.
#### Multitiered architectures
- Tienen una *Common Gateway Interface* 
	- Server ejecuta programas usando la data del usuario como input.
- Esto nos lleva a la *three-tiered architecture*:
	- Web server <-> CGI <-> Database Server

#### Web server clusters 
- Sepueden hacer clusters con los servidores para mejorar el rendimiento
- Como un cluster son varios servidores que comparten un front-end, el front-end tiene que estar bien dieseñado para no sobrecargarse.
- **Request routing**: Mecanismo para enrutar los requests del cliente al servidor que ha seleccionado.
	- Puede usar una aqrquitectura *two-way* o *one-way*.
		- En la arquitectura *one-way* solo los requests de entrada pasan por el switch.
	- Puede usar un switch *layer 4* o *layer 7*.
		- El *layer 4* proporciona packet rewriting, packet tunneling y packet forwarding.
		- El *layer 7* proporcionaTCP gateway, TCP splicing, TCP hand-off.

- **Request diapatching**: Políticas para seleccionar el servidor que se considera mejor situado para tratar el request del cliente.
	- *Content-blind request dispatching*: Usa un algoritmo estático donde no usa ninguna información o uno dinámico donde usa la información del estado cliente/servidor.
	- *Content-aware request dispatching*: Hace lo mismo que los dinámicos de content-blind.

### Communication

#### HTTP
- Protocolo de transferencia entre cliente/servidor.
- Basado en TCP.
- v1.0 usa conexiones no persistentes, la v1.1 si persistentes.
- **Pipelining**: El cliente puede hacer varias requests seguidas sin haber recibido la respuesta para la primera.

#### Simple Object Access Protocol (SOAP)
- Protocolo estándar para la comunicación entre Web services.
- Basado en XML.
- Los mensajes SOAP tienen un *Header* opcional y un *Body* obligatorio.

#### Representative State Transfer (REST WS)
- Los clientes usan operaciones HTTP (GET; PUT, etc) y URLs para manipular recursos que están en XML/JSON.

#### Web Services Description Language (WSDL)
- Lenguaje formal para describir de forma precisa un servicio proporcionado por WS.
- Basado en XML.

### Naming
- Los documentos se identifican por *URIs* (Uniform Resource Identifiers)
	- *URN* (Uniform Resource Name): Globalmente único, independiente, persistente.
	- *URL* (uniform Resource Locator): Incluye información de cómo y dónde acceder al documento.
#### Universal Description Discovery Interface (UDDI)
- Guarda descripciones de los servicios en documentos WSDL.
- Los clientes pueden buscar un servicio por su nombre o atributo(directorio de servicio).
- Los cambios en una descripción se deben hacer en el owner .
- Los cambios se propagan un un anillo lógico.

### Synchronization

#### Google Docs Collaboration
- El documentose guarda en un server com una lista de cambios cronológicos.
- Usa un protocolo colaborativo para sincronizar los cambiios.

### Consistency & replication

#### Client-side caching
- Cahce del buscador.
- .Web proxy caching
	- Se instala un servidor proxy que para todos requests locales del cliente al servidor.
	- La caches se representan en documentos.
		- *Hierarchical caches*: Las caches cubren una región. 
		- *Cooperative caching*: Cuando hay un fallo de cache el proxy revisa los proxys vecinos.
#### Content Distribution Networks (CDN)
- Se replica el contenido de los servidores para poder ofrecer un mejor servicio a menor latencia.
- Para hacer las redirecciones se puede hacer:
	- Redirección HTTP
	- URL rewritting
	- Redirección DNS

## Tema 9

### Introduction
- Los sistemas Peer to Peer no distinguen entre cliente y servidor.
	- Todos los peers contribuyen con sus recursos de forma simétrica.
	- Las cargas se balancean.
	- Operan independientemente de un nodo central.
- Se organizan en una network donde los nodos son los peers y los links los posibles canales de comunicación.

### Unstructured P2P systems

#### Centralized model: Napster
- Consulta un sistema de índice centralizado que devuelve los peers que almacenan el archivo requerido. 
- Transferir el archivo desde el o los peers dados.

#### Centralized model: BitTorrent
- Sistema colaborativo que provee el contenido usando *file swarming*.
	- Parte un archivo en pequeñas partes y los nodos piden esas partes a sus vecinos.
- Combina un modelo cliente-servidor para alocatar las piezas con un P2P protocol de bajada.
- Los archivos torrent conteienen *metadata* y la URL del tracker.
- **Tracker**: Trakea todos los peers que tienen un archivo y que parte de archivo tienen.
- **Seeds**: Peers que tienen una copia entera del archivo.
- **Leechers**: Peers que no tienen una copia entera de un archivo.

###### BitTorrent Piece selection
- El orden en el que se seleccionan las piezas es crítico para un buen funcionamiento.
- Se usa el método **Rarest Piece First** mediante el cual se baja primero las piezas que son más difíciles de encontrar.
- Al principio de la descarga se usa **Random First Piece** para que obtenga lomás rápido posible cualquier pieza. No se usa *Rarest First Piece* porque podira ser muy lento y al iniciar no nos interesa.
- Al finalizar se usa **End Game Mode** Cuando todas las sub-piezas ya se han pedido, las no recibidas se piden a todos los nodos que las contienen.
#### Centralized model: Choking
- Quiere garantizar que todos los peers suben y bajan archivos de forma recíproca e igualitaria.
- **Chocking** es refusar de forma temporal una subida.

#### Decentralized model: Gnutella
- Basado en el mecanismo de *flooding*.
- Para encontrar un archivo: 
	- Un peer envía una query a todos los vecinos.
- Los vecinos hacen de forma recursiva un multicast del descriptor.
	- El peer que tiene el archivo recibe el descriptor y envía una QueryHit.
- Añade TTL (Time-To-Live) para controlar el flooding.

#### Hierarchical model: FastTrack
- Hay unos pocos nodos especiales llamados **super-peers**, que en su interior contienen peers normales.
- Los super-peers saben todos los archivos que tienen sus peers.
- Los peers le hacen las querys a sus super-peers y estos se comunican con el resto de super-peers.

### Structured P2P systems
- Se usan Hash Table Distribuidas para asegurarse de que un item siempre se puede encontrar.

#### Chord
- Identificador de m bits para keys y nodos.
- Los identificadores se ordenan en un círculo de 2^m.
- Para mapear un nodo se usa como key el ID y se guarda en el nodo *sucesor* del nodo con mayor ID.
![](Pasted%20image%2020230609113245.png)
- Cada nodo conoce solo al predecesor y al sucesor.
- Los requests se envían a través del anillo gracias a que se enrutan a los sucesoers de los nodos.

##### Finger Tables
- Se usan para acelerar las búsquedas.
![](Pasted%20image%2020230609113611.png)
##### Node joining
- Cuando se une un nuevo nodo se usa la **Stabilization**.
- Es un método para tratar joins concurrentes actualizando los punteros de sucesor para una actualización coherente.

##### Node failures
- Los fallos en los nodos pueden causar búsquedas incorrectas.
- Si un nodo percibe que su sucesor ha fallado lo sustituye con el próximo sucesor vivo más cercano.

#### Kademlia
- Identificador de 160 bits para keys y nodos.

##### Binary Tree
- Los nodos se tratan omo hojas de un binary tree.
![](Pasted%20image%2020230609114315.png)

##### Protocol
- El protocolo de Kademlia consta de 4 RPCs:
	- PING
	- STORE
	- FIND_NODE
	- FIND_VALUE

##### Node lookup
- Se hace una búsqueda iterativa por prefijo.
- El nodo origen es el responsable de la búsqueda.
- Usa *parallel routing* para hacer los lookups más rápidos.

##### Node join
- Para unirse el nuevo nodo debe conocer a uno de los nodos ya existentes en el sistema.