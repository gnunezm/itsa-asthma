# Interrumpted Time Series Analysis


En este repositorio, muestro como crear una Serie Interrumpida de Tiempo para evaluar la efectividad de una política pública. Como ejemplo de política pública utilizo la Garantía Explicita de Salud de Asma para personas mayores de 15 años, ejectuda en Chile, que inició en el año 2008. Como datos, se utilizan todas las defunciones ocurridas en Chile durante el período 2001-2021. 

Dado que las enfermedades están clasificadas por CIE-10, se utilizan los códigos J45 y J46 para definir muerte por Asma.

Este análisis esta realizado en R, usando las librerías 'tidyverse','ggplot2','lubridate','haven','zoo','broom', tal como se ve en el archivo R adjunto.

Los primeras 10 líneas sirven para detectar si R tiene instaladas o no las librerías necesarias y, posteriormente, procede a realizar la carga de ellas. En caso de que no tengas instalada la librería, procede a instalarla junto con las dependencias necesarias para el buen funcionamiento


La BBDD original, se transforma para obtener los intervalos de tiempo que se requieren para generar los conteos necesarios, posteriormente, se realiza un conteo de fechas individual y se genera una nueva base de datos con dicho conteo.

Se obtiene del Instituto Nacional de Estadística, las esperanzas de vida por quinquenio para la población chilena, esto con el fin de poder determinar los Años de Vida Potencialmente Perdidos (AVPP, o YPLL por su sigla en Inglés) y poder definir la efectividad por población afectada y esperanza de vida


Adjunto la BBDD, que es pública, en el siguiente link: https://www.icloud.com/iclouddrive/0f3BLG8ewbIcxZs7mQLOlbaCw#BD_Defunciones_Chile
