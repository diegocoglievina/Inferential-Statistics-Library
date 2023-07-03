# Diego Coglievina Díaz
# Estaística inferencial

# PROBABILIDAD DE LA DISTRIBUCIÓN MEDIA (x) DE UNA MUESTRA (de tamaño n) CON MEDIA (mean).
    # TABLA Z Si la stDev es poblacional o se tienen más de 30 datos.
        pnormZLessThan <- function(x, stdev, mean, n) {
        z = (x - mean)/(stdev/sqrt(n));
        p = pnorm(z); 

        return(p);
        }

        pnormZGreaterThan <- function(x, stdev, mean, n) {
        z = (x - mean)/(stdev/sqrt(n));
        p = 1 - pnorm(z); 
        if(p < 0 || p > 1) {
            stop("Error: p < 0 || p > 1")
            }
        return(p);
        } ## Un alumno de la Anahuac quiere realizar un estudio de cuanto tiempo toman los alumnos en netflix y encuentra que la población tiene una media de tiempo frente a netflix de 168 minutos con una desviación estándar de 54 minutos. Si se toma una muestra de 64 alumnos ¿Cual es la probabilidad de la media muestral sea mayor a 184 minutos?

        pnormZBetween <- function(x0, x1, stdev, mean, n) {
        p1 = pnormLessThan(x1, stdev, mean, n);
        p0 = pnormLessThan(x0, stdev, mean, n);
        p = p1 - p0;
        if(p < 0 || p > 1) {
            stop("Error: p < 0 || p > 1")
        }
        return(p);
        }
        
        pnormZNotBetween <- function(x0, x1, stdev, mean, n) {
        p1 = pnormGreaterThan(x1, stdev, mean, n);
        p0 = pnormLessThan(x0, stdev, mean, n);
        p = p1 + p0;
        
        if(p < 0 || p > 1) {
            stop("Error: p < 0 || p > 1")
        }
        return(p);
        }

        # PERCENTILES CON TABLA Z (regresa x0 s.t. P(x0) = p)
        pnormZInversePercentile <-function(stdev, p, n, mean) {
            z = qnorm(p); 
            x0 = z*(stdev/sqrt(n)) + mean; 
            return(x0)
        } ## Un alumno de la Anahuac quiere realizar un estudio de cuanto tiempo toman los alumnos en netflix y encuentra que la población tiene una media de tiempo frente a netflix de 168 minutos con una desviación estándar de 54 minutos. Si se toma una muestra de 64 alumnos ¿Cuantos minutos miran netflix el 12% que menos pasan tiempo en netflix?

        pnormZPercentile <-function(stdev, p, n, mean) {
            z = qnorm(1 - p); 
            x0 = z*(stdev/sqrt(n)) + mean; 
            return(x0)
        } ## ¿Cuántos minutos miran netflix el 12% que más pasan tiempo en netflix?
        


    # TABLA T Si la stDev es muestral y se tienen menos de 30.
        pnormTLessThan <-function(x, stdev, mean, n) {
        t = (x  - mean)/(stdev/sqrt(n)); 
        gl = n - 1
        return(pt(t, gl))
        }

        pnormTGreaterThan <-function(x, stdev, mean, n) {
        t = (x  - mean)/(stdev/sqrt(n)); 
        gl = n - 1
        return (1- pt(t, gl))
        } ## El estudio realizado sobre lectores reveló que los jóvenes en méxico leen en promedio 5 libros al año. Se tomó una muestra de 23 jóvenes y se encontró una desviación estándar de 2 libros. ¿Cuál es la probabilidad de encontrar a un jóven que lea 6 libros al año o más?

        pnormTbetween <-function(x0, x1, stdev, mean, n) {
        t1 =(x1 - mean)/(stdev/sqrt(n)); 
        t0 = (x0 - mean)/(stdev/sqrt(n)); 
        gl = n - 1

        pt1 = pt(t1, gl);
        pt0 = pt(t0, gl);

        return (pt1 - pt0)
        }

        pnormTnotBetween <-function(x0, x1, stdev, mean, n) {
        t1 =(x1 - mean)/(stdev/sqrt(n)); 
        t0 = (x0 - mean)/(stdev/sqrt(n)); 
        gl = n - 1

        pt1 = pt(t1, gl);
        pt0 = pt(t0, gl);

        retur (1 - (pt1 + pt0))
        }

    # PERCENTILES CON TABLA T (regresa x0 s.t. P(x0) = p)
    pnormTPercentile <-function(stdev, p, n, mean) {
        df = n - 1;
        t = qt(p, df); 
        x = t*(stdev/sqrt(n)) + mean
    } 

    pnormTInversePercentile <-function(stdev, p, n, mean) {
        df = n - 1;
        t = qt(1-p, df); 
        x = t*(stdev/sqrt(n)) + mean
    } 


# PROBABILIDAD DE LA PROPORCIÓN: UN EVENTO (pm) EN UNA MUESTRA (tamaño n) CON PROBABILIDAD POBLACIONAL (PT)
    sampleDistLessThan <- function(PT, n, pm) { 
        Q = 1 - PT
        
        if(PT*n < 5 && Q*n < 5){
            stop("No hay suficiente datos en la muestra. ")   
        }

        z = (pm - PT)/(sqrt(PT*Q/n)); 

        return (pnorm(z))

    } ## 2: Si 15% de la población de alumnos falta los viernes y se toma una muestra de un salón de 55 alumnos ¿Cuál es la probabilidad de que 11% o menos falten el siguiente viernes?

    sampleDistGreaterThan <- function(PT, n, pm) { 
        Q = 1 - PT
        
        if(PT*n < 5 && Q*n < 5){
            stop("No hay suficiente datos en la muestra. ")   
        }

        z = (pm - PT)/(sqrt(PT*Q/n)); 

        return (pnorm(z, lower.tail = FALSE))
    }

    sampleDistBetween <- function(PT, n, pm0, pm1) {
        Q = 1 - PT
        
        if(PT*n < 5 && Q*n < 5){
            stop("No hay suficiente datos en la muestra. ")   
        }

        z0 = (pm0 - PT)/(sqrt(PT*Q/n)); 
        z1 = (pm1 - PT)/(sqrt(PT*Q/n)); 
        return (pnorm(z1 - z0, lower.tail = FALSE))
    }

# PROBABILIDAD DE LA DISTRIBUCIÓN DE LA VARIANZA DE UNA MUESTRA (sampleVar)
    varDistLessThan <-function(n, sampleVar, popVar) {
        df = n - 1
        x = (df)*sampleVar/popVar; 
        x = pchisq(x, df); 
    }

    varDistGreaterThan <-function(n, sampleVar, popVar) {
        df = n - 1
        x = (df)*sampleVar/popVar; 
        x = pchisq(x, df, lower.tail=FALSE); 
    } ## 6: Una cerveceria en querétaro produce cervezas pilsner que tienen entre 6 y 7 grados de alcohol con una varianza de 0.25 grados^2. Se analizaron 7 muestras. ¿Cuál es la probabilidad de que la muestra obtenga una varianza superior a 0.35 grados^2?

    varDistNotBetween <-function(n, sampleVar0, sampleVar1, popVar) {
        df = n - 1; 
        x0 = (df)*sampleVar0/popVar;
        x0 = pchisq(x0, df, lower.tail=FALSE); 
        x1 =(df)*sampleVar1/popVar
        x1 = pchisq(x1, df, lower.tail = TRUE); 
        return(x1 + x0)
    }


# INTERVALO DE CONFIANZA: 
    # Para la media
        cIntervalPopMean <-function(sampleN, popN, CI) {
        q = sampleN/popN;
        p = 1 - q;
        a0 = (1/2)*(1-CI);
        a1 = 1 - a0;

        z0 = qnorm(a0); 
        z1 = qnorm(a1);

        m0 = q + z0*sqrt(p*q/sampleN); 
        m1 = q + z1*sqrt(p*q/sampleN); 
        return(paste(m0, " < popMean < ", m1))
      }
    # Para la varianza
        cIntervalPopVar <-function(sampleN, sampleDev, CI) {

        a0 = (1/2)*(1-CI);
        a1 = 1 - a0; 
        sampleVar = sampleDev^2; 

        x2.0 = qchisq(a0, df, lower.tail=FALSE); 
        x2.1 = qchisq(a1, df, lower.tail=FALSE);

        varP0 = df*sampleVar/x2.0; varP0
        varP1 = df*sampleVar/x2.1; varP1
        
        return(paste(VarP0, " < popDev < ", VarP1)); 
      }
    # Para la desviación
        cIntervalPopDev <-function(sampleN, sampleDev, CI) {

        a0 = (1/2)*(1-CI);
        a1 = 1 - a0; 
        sampleVar = sampleDev^2; 

        x2.0 = qchisq(a0, df, lower.tail=FALSE); 
        x2.1 = qchisq(a1, df, lower.tail=FALSE);

        varP0 = df*sampleVar/x2.0; varP0
        varP1 = df*sampleVar/x2.1; varP1

        popDev0 = sqrt(varP0)
        popDev1 =  sqrt(varP1)

        return(paste(popDev0, " < popDev < ", popDev1));

       }



# PRUEBA DE HIPÓTESIS PARA LA MEDIA
    # UTILIZANDO Z
        nullHypLessZ <- function(sampleMean, popMean, popDev, n, alpha) {
            z = (sampleMean - popMean)/(popDev/sqrt(n)); 
            criticalV = qnorm(1 - alpha);

            if (z > criticalV) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }

            ans = c(ans, z, criticalV)

            return(ans)
        }

        nullHypGreaterZ <- function(sampleMean, popMean, popDev, n, alpha) {
            z = (sampleMean - popMean)/(popDev/sqrt(n)); 
            criticalV = qnorm(1 - alpha);

            if (z < criticalV) {
                ans = paste("Co
                n un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }
            ans = c(ans, z, criticalV)
            return(ans)
        }

        nullHypBetweenZ <-function(sampleMean, popMean, popDev, n, alpha) {
            alpha = alpha/2;
            z = (sampleMean - popMean)/(popDev/sqrt(n));    
            criticalV0 = qnorm(alpha); 
            criticalV1 = qnorm(1 - alpha);

            if(z < criticalV0 || z > criticalV1) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }
            ans = c(ans, paste("valor de Z: ", z), paste("valor crítico: ", criticalV0, " < Z < ", criticalV1))
            return(ans)
        }

        nullHypNotBetweenZ <-function(sampleMean, popMean, popDev, n, alpha) {
            alpha = alpha/2;
            z = (sampleMean - popMean)/(popDev/sqrt(n));    
            criticalV0 = qnorm(alpha); 
            criticalV1 = qnorm(1 - alpha);

            if(z > criticalV0 || z < criticalV1) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }
            ans = c(ans, paste("valor de Z: ", z))
            return(ans)
        }

    # UTILIZANDO T
        # Debe tener: Muestra menor a 30, y Desviación Estándar/Varianza de la Muestra

        nullHypLessT <-function(sampleMean, popMean, sampleDev, n, alpha) {
            df = n - 1; 
            alpha = alpha/2;
            t = (sampleMean - popMean)/(sampleDev/sqrt(n)); 
            criticalV = qt(1 - alpha, df);

            if (t > criticalV) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }

            ans = c(ans, 
            paste("t", t),
            paste("CriticalV", criticalV))
            return(ans)
        }

        nullHypGreaterT <-function(sampleMean, popMean, sampleDev, n, alpha) {
            df = n - 1; 
            alpha = alpha/2;
            t = (sampleMean - popMean)/(sampleDev/sqrt(n)); 
            criticalV = qt(1 - alpha, df);

            if (t < criticalV) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }
            
            ans = c(ans, t, criticalV)
            return(ans)
        }

        nullHypBetweenT <-function(sampleMean, popMean, sampleDev, n, alpha) {
            df = n - 1; 
            alpha = alpha/2;
            t = (sampleMean - popMean)/(sampleDev/sqrt(n)); 
            criticalV0 = qt(alpha, df); 
            criticalV1 = qt(1 - alpha, df);

            if(t < criticalV0 || t > criticalV1) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }

            return(ans)
        }


# PRUEBA DE HIPÓTESIS PARA LA PROPORCIÓN
    # Cola Izquierda
        propHypLessZ <-function(pSample, pPop, n, alpha) {
            p = pSample;
            q = 1 - p;
            p0 = pPop;
            q0 = 1 - p0;
            z = (p - p0)/sqrt((p0*q0)/n);
            criticalV = qnorm(alpha);

            if (z > criticalV) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }

            ans = c(ans, z, criticalV)
            return(ans)
        }

    # Cola Derecha
        propHypGreaterZ <-function(pSample, pPop, n, alpha) {
            p = pSample;
            q = 1 - p;

            p0 = pPop;
            q0 = 1 - p0;
            
            z = (p - p0)/sqrt((p0*q0)/n);
            criticalV = qnorm(1 - alpha);

            if (z < criticalV) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }

            ans = c(ans, 
            paste("z", z),
            paste("CriticalV",  criticalV))
            return(ans)
        }

        propHypBetween <- function(pSample, pPop, n, alpha) {
            p = pSample;
            q = 1 - p;

            p0 = pPop;
            q0 = 1 - p0;
            
            z = (p - p0)/sqrt((p0*q0)/n);
            criticalV0 = qnorm(alpha/2); 
            criticalV1 = qnorm(1 - alpha/2);

            if(z < criticalV0 || z > criticalV1) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }

            ans = c(ans, 
            paste("z", z),
            paste("CriticalV",  criticalV0, " < Z < ", criticalV1))
            return(ans)
        }


# PRUEBA DE HIPÓTESIS PARA LA VARIANZA 
    #cola izquierda
        varHypLess = function(sampleVar, popVar, n, alpha) {
            chi2 = (n - 1)*sampleVar/popVar;
            criticalV = qchisq(1 - alpha, n - 1);
            if (chi2 > criticalV) {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", alpha*100, "%", "se acepta la hipótesis.")
            }

            ans = c(ans,
            paste("chi2", chi2),
            paste("CriticaV", criticalV))
            return(ans)
        }

    # Dos colas
        varHypBetween = function(sampleVar, popVar, n, alpha) {
            df = n - 1
            chi2 = df*sampleVar/popVar;
            
            critical0 = qchisq(alpha, df)
            criticalV1 = qchisq(1 - alpha, df)

            if (chi2 < critical0 || chi2 > criticalV1) {
                ans = paste("Con un nivel de significancia de ", 2*alpha*100, "%", "se rechaza la hipótesis.")
            } 
            else {
                ans = paste("Con un nivel de significancia de ", 2*alpha*100, "%", "se acepta la hipótesis.")
            }

            ans = c(ans,
            paste("chi2", chi2),
            paste("CriticaV0", critical0, "CriticalV1", criticalV1))
            return(ans)
        }

# GRAFICAR: 

    # Normal Distribution
        # Graficar cola izquierda
            graphNormDistLeft = function(mean, sd, z, criticalV) {
                x = seq(-4*sd, 4*sd, length.out = 100)
                bounds = c(mean - 4*sd, mean + 4*sd)
                curve(dnorm(x, mean, sd), xlim=bounds)

                # Acceptance Region
                lower.x = -4*sd + mean
                upper.x = criticalV + mean
                step = (upper.x - lower.x)/100
                
                cord.x = c(lower.x, seq(lower.x, upper.x, step), upper.x)
                cord.y = c(0, dnorm(seq(lower.x, upper.x, step), mean, sd), 0)
                polygon(cord.x, cord.y, col = "#3CC201")
                
                # Rejection Region
                lower.x = upper.x
                upper.x = mean + 4*sd
                step = (upper.x - lower.x)/100
                cord.x = c(lower.x, seq(lower.x, upper.x, step), upper.x)
                cord.y = c(0, dnorm(seq(lower.x, upper.x, step), mean, sd), 0)
                polygon(cord.x, cord.y, col = "#DB3E00")

                abline(v = criticalV + mean, col = "green")

                arrows(x0=z+mean,
                        x1=z+mean,
                        y0=0.39,
                        y1=0.01,
                        length=.2,
                        col="#0600FF",
                        lwd=1.3, 
                        lty ="dashed")

                dn = dnorm(mean, mean, sd)
                legend(mean - 3.5*sd, dn,
                legend=c(
                    paste("V. Crítico: ", round(criticalV,digits=4)),
                    paste("z:",round(z,digits=4))),
                    col=c("Green","red"),lty=1:3,cex=0.79,box.lty=0)
            }

        # Graficar cola Derecha 
            graphNormDistRight = function(mean, sd, z, criticalV) {
                x = seq(-4*sd, 4*sd, length.out = 100)
                bounds = c(mean - 4*sd, mean + 4*sd)
                curve(dnorm(x, mean, sd), xlim=bounds)

                # Rejection Region
                lower.x = -4*sd + mean
                upper.x = criticalV + mean
                step = (upper.x - lower.x)/100
                cord.x = c(lower.x, seq(lower.x, upper.x, step), upper.x)
                cord.y = c(0, dnorm(seq(lower.x, upper.x, step), mean, sd), 0)
                polygon(cord.x, cord.y, col = "#DB3E00")
                
                # Acceptance Region DB3E00
                lower.x = upper.x
                upper.x = mean + 4*sd
                step = (upper.x - lower.x)/100
                cord.x = c(lower.x, seq(lower.x, upper.x, step), upper.x)
                cord.y = c(0, dnorm(seq(lower.x, upper.x, step), mean, sd), 0)
                polygon(cord.x, cord.y, col = "#3CC201")

                abline(v = criticalV + mean, col = "green")

                arrows(x0=z+mean,
                        x1=z+mean,
                        y0=0.39,
                        y1=0.01,
                        length=.2,
                        col="#0600FF",
                        lwd=1.3, 
                        lty ="dashed")

                dn = dnorm(mean, mean, sd)
                legend(mean - 3.5*sd, dn,
                legend=c(
                    paste("V. Crítico: ", round(criticalV,digits=4)),
                    paste("z:",round(z,digits=4))),
                    col=c("Green","red"),lty=1:3,cex=0.79,box.lty=0)
            }

        # Graficar dos colas
            graphNormDistTwoTails = function(mean, sd, z, criticalV1, criticalV2) {
                x = seq(-4*sd, 4*sd, length.out = 100)

                # Right tail
                lower.x = criticalV1
                upper.x = mean + 4*sd
                step = (upper.x - lower.x)/100
                bounds = c(mean - 4*sd, mean + 4*sd)
                cord.x = c(lower.x, seq(lower.x, upper.x, step), upper.x)
                cord.y = c(0, dnorm(seq(lower.x, upper.x, step), mean, sd), 0)
                
                curve(dnorm(x, mean, sd), xlim=bounds)
                polygon(cord.x, cord.y, col = "#1AC871")
                abline(v = criticalV1 + mean, col = "green")

                # Left tail
                lower.x = mean - 4*sd
                upper.x = mean + criticalV2
                step = (upper.x - lower.x)/100
                cord.x = c(lower.x, seq(lower.x, upper.x, step), upper.x)
                cord.y = c(0, dnorm(seq(lower.x, upper.x, step), mean, sd), 0)
                polygon(cord.x, cord.y, col = "#1AC871")

                arrows(x0=z+mean,x1=z+mean,y0=0.39,y1=0.01,length=.2,col="red",lwd=1.3, lty ="dashed")

                dn = dnorm(mean, mean, sd)
                legend(mean - 3.5*sd, dn,
                legend=c(
                    paste("V1. Crítico: ", round(criticalV1,digits=4)),
                    paste("V2. Crítico: ", round(criticalV2,digits=4)), 
                    paste("z:",round(z,digits=4))),
                    col=c("Green","Green", "Red"),lty=1:3,cex=0.79,box.lty=0)
            }
    # Beta Distributio
        # 

propHypLessBinom = function(x, p, n) {
    
}


## 1.	La administración de White Industries analiza una nueva técnica para armar un carro de golf; la técnica actual requiere 42.3 minutos en promedio. El tiempo medio de montaje de una muestra aleatoria de 24 carros, con la nueva técnica, fue de 40.6 minutos, y la desviación estándar, de 2.7 minutos. Con un nivel de significancia de 0.10, ¿puede concluir que el tiempo de montaje con la nueva técnica es más breve? ¿Cuál es el valor de la prueba?

    # H0: p >= 42.3
    # H1: p < 42.3
    # alpha = 0.10
    # n = 24
    # x = 40.6
    # sd = 2.7

    # z = (x - p) / (sd / sqrt(n))
    # z = (40.6 - 42.3) / (2.7 / sqrt(24))
    # z = -0.63

    # p-value = 0.265

    # Como p-value > alpha, no se rechaza H0

    # z = (x - p) / (sd / sqrt(n))
    # z = (40.6 - 42.3) / (2.7 / sqrt(24))
    # z = -0.63

    # p-value = 0.265

    # Como p-value > alpha, no se rechaza H0

    # z = (x - p) / (sd / sqrt(n))
    # z = (40.6 - 42.3) / (2.7 / sqrt(24))
    # z = -0.63

    # p-value = 0.265

    # Como p-value > alpha, no se rechaza H0

    # z = (x - p) / (sd / sqrt(n))
    # z = (40.6 - 42.3) / (2.7 / sqrt(24))
    # z = -0.63

    # p-value = 0.265

    # Como p-value > alpha, no se rechaza H0

    # z = (x - p) / (sd / sqrt(n))
    # z = (40.6 - 42.3) / (2.7 / sqrt(24))
    # z = -0.63

    # p-value = 0.265

    # Como p-value > alpha, no se rechaza H0

    # z = (x - p) / (sd / sqrt(n))
    # z = (40.6 - 42.3) / (2.7 / sqrt(24))
    # z = -0.63

    # p-value = 0.265

# Según una revista de marketing, 9 de cada 10 mujeres profesionales dicen que la planificación financiera es más importante ahora que hace cinco años. 47% usan un asesor financiero y 26% usan fuentes escritas. Suponga que estas cantidades se obtuvieron al tomar una muestra de 560 mujeres profesionales quienes dijeron que la planificación financiera es más importante ahora de lo que fue hace cinco años. Construya un intervalo de confianza al 90% para la proporción de mujeres que usan un asesor financiero.

ans16 = function(x, p, n) {
    z = qnorm(0.95)
    sd = sqrt(p * (1 - p) / n)
    lower = x - z * sd
    upper = x + z * sd
    return(c(lower, upper))
}

ans16(0.47, 0.26, 560)

# De acuerdo con una encuesta realizada por Accountemps, 48% de los ejecutivos piensan que los empleados son más productivos los jueves. Suponga que al azar se encuestan 200 ejecutivos. ¿Cuál es la probabilidad de que menos de 90 de los ejecutivos encuestados piensen que los empleados son más productivos en jueves?

# H0: p >= 0.48
# H1: p < 0.48
# alpha = 0.10
# n = 200
# x = 0.48
# sd = 0.48 * (1 - 0.48) / 200


qt(0.05, 60)
qnorm(0.05)


# Los resultados de una encuesta, revelaron que el 60% de los egresados de la universidad, consiguen trabajo después del primer mes de haberse graduado. Una muestra aleatoria de 200 egresados, reveló que 112 habían conseguido trabajo después del primer mes. ¿Hubo una reducción significativa en la proporción de egresados que consiguen trabajo después del primer mes de haberse graduado? Realice la prueba con un nivel de significancia del 5%. ¿Cuál será la conclusión de la prueba de hipótesis?

ans = function(x, p, n) {
    z = (x - p) / (sqrt(p * (1 - p) / n))
    pvalue = pnorm(z)
    return(pvalue)
}

ans(112, 0.6, 200)


# H0: p >= 0.48
# H1: p < 0.48

