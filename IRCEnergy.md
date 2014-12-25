### A function to extract the energy values from a cml file obtained by splicing together two halves of an Intrinsic Reaction Coordinate file obtained from Gamess (US)

```{r}
IRCEnergy  <- function(x) {
                     # x is a cml file
a  <- readLines(x) 
b  <- grep("\\\"Energy",a,value=TRUE) 
rm(a) 

b  <- 627.51 * as.numeric(sapply(strsplit(b,split="[<></]"),"[[",3))
b  <- b-b[1]
plot(b, xlab="Geometric Structure", ylab="Energy /KJ/mol",
      main="Intrinsic Reaction Coordinat")
}
```
