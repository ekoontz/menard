# Separable verbs


## Finite

### Subcat

- `optreden[finite]<:noun,:prep[op]>`

### Example Trees

```
S
|_____VP_________2:op<>
|     |
1:ik  treed<1,2> 
     
```


```
S
|______VP_________2:PP<>__op<3>
|      |          | 
1:ik   treed<1,2> 3:het
      
```

```
S
|____VP__________________2:PP<>________________op<3>
|    |                     |
ik   treed<1,2[:prep]>     NP<3>___toneelstuk 
                           |
                           het
```

## Infinitive

### Subcat

- `optreden[infinitive]<:noun>`
- `optreden[infinitive]<:noun,:noun>`

### Example Trees

```
S
|____VP<1>__optreden<1>
|    |
1:ik |
     zal
```

```
S
|____VP<1>____VP<1>__optreden<1,2[:noun]>
|    |        |
1:ik |        |
     zal      2:het
```

```
S
|____VP<1>____VP<1>_____________optreden<1,2[:noun]>
|    |        |
1:ik |        |
     zal      2:NP__toneelstuk
              |
			  het

```






