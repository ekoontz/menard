# Summary

# Citations 

- F. 61,79
- BvdH p201
- Osterhoff p144

# Testing

```
(->> "u heeft u nodig" parse (map dag_unify.serialization/serialize) set vec (map dag_unify.serialization/deserialize) (map #(u/get-in % [:reflexive?])) (map u/pprint))
(->> "u heeft u nodig" parse (filter #(= (u/get-in % [:reflexive?]) false)) (map #(u/get-in % [:reflexive])))
```

# True Reflexive verbs

## _zich afvragen_

...

# Non-true reflexive verbs

## Non emphatic 

### _zien_

| nl             | en                                                     |
|----------------|--------------------------------------------------------|
| ik zie me      | I see myself                                           |
| je ziet je     | you (informal) see yourself                            |
| hij ziet zich  | he sees himself                                        |
| ze ziet zich   | she sees himself                                       |
| u ziet u       | you (formal) see yourself, you (formal) see yourselves |
| u ziet zich    | you (formal) see yourself, you (formal) see yourselves |
| wij zien ons   | we see ourselves                                       |
| jullie zien je | you (informal,plural) see yourselves                   |
| ze zien zich   | they see themselves                                    |

### _hebben nodig_

| nl                     | en                            |
|------------------------|-------------------------------|
| ik heb me nodig        | I need myself                 |
| je hebt je nodig       | you need yourself             |
| hij heeft zich nodig   | he needs himself              |
| ze heeft zich nodig    | she needs herself             |
| u heeft u nodig        | you need yourself, yourselves |
| u heeft zich nodig     | you need yourself, yourselves |
| wij hebben ons nodig   | we need ourselves             |
| jullie hebben je nodig | you need yourselves           |
| ze hebben zich nodig   | they need themselves          |

## Emphatic 

### _zien_

| nl                                  | en                                      |
|-------------------------------------|-----------------------------------------|
| ik zie niet Ina maar mezelf         | I see not Ina but MYSELF                |
| je ziet niet Ina maar jezelf        | you see not Ina but YOURSELF            |
| hij ziet niet Ina maar zichzelf     | he sees not Ina but HIMSELF             |
| ze ziet niet Ina maar zichzelf      | she sees not Ina but HERSELF            |
| u ziet niet Ina maar zichzelf       | you see not Ina but YOURSELF,YOURSELVES |
| wij zien niet Ina maar zien onszelf | we see not Ina but OURSELVES            |
| jullie zien niet Ina maar jezelf    | you see Ina but YOURSELVES              |
| ze zien niet Ina maar zichzelf      | they see not Ina but THEMSELVES         |

#### parse tree

[s .ik +[vp +zie [conjp [negp +niet .Ina] +[conjp-bar +maar .mezelf]]]]
[s .I +[vp +see [conjp [negp +not .Ina] +[conjp-bar +but .MYSELF]]]]
### _hebben nodig_

| nl                                 | en                                       |
|------------------------------------|------------------------------------------|
| ik heb niet Ina maar mezelf nodig  | I need not Ina but MYSELF                |
| je hebt niet Ina maar jezelf       | you need not Ina but YOURSELF            |
| hij heeft niet Ina maar zichzelf   | he needs not Ina but HIMSELF             |
| ze heeft niet Ina maar zichzelf    | she needs not Ina but HERSELF            |
| u heeft niet Ina maar zichzelf     | you need not Ina but YOURSELF,YOURSELVES |
| wij hebben niet Ina maar onszelf   | we need not Ina but OURSELVES            |
| jullie hebben niet Ina maar jezelf | you need not Ina but YOURSELVES          |
| ze hebben niet Ina maar zichzelf   | they need not Ina but THEMSELVES         |
