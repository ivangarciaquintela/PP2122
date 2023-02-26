Practica 4 Algoritmos : Algoritmo de Prim
AUTORES : IVAN GARCIA QUINTELA | ALVARO REGUEIRA VILAR | CANDELA IGLESIAS LORENZO
LOGIN 	: ivan.garcia.quintela |        a.rvilar       |    candela.iglesias
GRUPO 	: 1.1 MIERCOLES 10:30 - 12:30
FECHA	: 04/12/21

La ejecuci�n del algoritmos as� como la medici�n de sus tiempos han 
sido probados en una maquina con las siguientes especificaciones:

- CPU 		: Intel(R) Core(TM) i7-10750H CPU @ 2.60GHz
- MEMORIA 	: 16GB DDR4
- SISTEMA 	: Kubuntu 21.04

INTRODUCCI�N: 

En el informe de esta pr�ctica vamos a tratar el algoritmo de Prim.

comprobaremos el correcto funcionamiento de la implementacion del algoritmo con
los ejemplos dados en la pr�ctica y, a posteriori mediremos el tiempo que tarda
en ejecutarse el algoritmo con gragos generados aleatoriamente.

Los grafos tendran un numero n de nodos igual a 40, 80, 160, 320 y 640.
Los tiempos que se mostrar�n est�n medidos en microsegundos.

A mayores, los casos en los que la ejecucion esta por debajo de nuestro umbral 
de confianza (500 microsegundos), se marcaran con (*) y se ejecutaran 1000 
veces el algoritmo para que sea medido de nuevo. Adem�s,el codigo se ejecutar� 
3 veces seguidas para que la CPU dedique los recursos suficientes para que las 
mediciones se realicen correctamente.

-------------------------------------------------------------------------------
MEDICI�N:

Aristas:  (0,1), (1,2), (0,3), (3,4)
Peso: 10
Aristas:  (0,1), (1,2), (2,3)
Peso: 6
Aristas:  (0,3), (3,5), (0,1), (1,4), (4,2), (4,6)
Peso: 39

        n          t(n)      t(n)/n^1.6     t(n)/n^2.0     t(n)/n^2.3
(*)    40          6.592     0.00412000     0.00212093     0.00094205
(*)    80         31.979     0.00499672     0.00227054     0.00086586
(*)   160        140.177     0.00547566     0.00219632     0.00071910
      320        631.000     0.00616211     0.00218174     0.00061329
      640       2678.000     0.00653809     0.00204333     0.00049315
      
                             C.Subest       C.Ajust        C.Sobreest
                                            Cte = 0.0021

Observaciones:
La cota subestimada (n^1.6) tiende a infinito claramente.
La cota ajustada (n^2.0) tiende a una constante Cte > 0 cuyo valor es 0.0021
aprox.
La cota sobreestimada (n^2.3) tiende a 0 a medida que aumentamos el valor de n.
No hay anomal�as.
-------------------------------------------------------------------------------
CONCLUSIONES:
Como podemos observar el algoritmo tiene una complejidad cuadr�tica por lo que
su tiempo de ejecuci�n aumenta exponencialmente a medida que aumenta  tambi�n 
el n�mero de nodos de los grafos. Por lo que para grafos de gran tama�o no ser�
optimo.

Respecto a las anomal�as, en nuestro caso no se encontro ninguna en la
ejecuci�n de este algoritmo.
