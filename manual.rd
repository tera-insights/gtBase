# Introduction

**Rgrokit** is a collection of **R** packages intended to interface with the **grokit** software published by Tera Insights. The primary purpose of these libraries is to act as a front-end for writing, running, and viewing database queries performed by **grokit**. Additional tools include visualization of native **R** objects and limited **R** to **C++** cross compilation. It is recommended that these packages be used online at [Tera Insights](terainsights.net); offline usage is supported but has limited functionality and is primarily employed for debugging. 

[TOC]

# gtBase

Short for **grokit base**, this library establishes the basic waypoint scheme used in queries  and provides access to several aggregate functions. Expanding upon the former, the **datapath** system employed by **Grokit** pipelines the data through a series of waypoints at which various types of computation are performed. These waypoints can be thought of roughly equivalent to sub-queries in **SQL** and the overall structure of a query can be visualized as an inverted tree, as demonstrated later. As a pre-cursor to the overview of the waypoint system, first the individual components of them must be discussed.

> It should be noted that no actual computation using the data is done using **rgrokit**. The **R** code formats a user query and calls a system command to begin **datapath** once a result is desired. As such, some basic checks are impossible to perform, one key example being type checking. It may be the case that a user will write a query using **rgrokit** that has an error in the first line with an exception not be thrown until the very end of query when the query is actually run. Because of this, it is highly recommended that the user run the queries using either the online application or in batches. Interactive use is offered with no guarantees and while it will be at least as good as batch use, there are few benefits.

## Data

Implemented in **R** using the **S3** class system, each waypoint has class **data** and at least one additional class describing which type of waypoint it represents. Although every waypoint is characterized by the output of tuples, the method of computation and required inputs var,; the various classifications of which are described below. When constructed organically by the provided functions, every instance of **data** exists as a **list** and is guaranteed an element **schema** which is a named **character** whose names correspond to the attribute names of the dataset represented. For those unfamiliar with tabular data, these attribute names can be thought of as the column names of a **data frame**. Each row represents a tuple. To query the available attributes of object  **x** with class **data**, simply use `names(x$schema)`.

>For those interested in the back-end functionality:

>The actual elements of **schema** are attributes used by the system in the back-end, which can sometimes differ from those that the user is exposed to, typically for the sake of user convenience. Although the user generally does need to be aware of these, nonetheless one example for the curious is that a **join** on two attributes will seemingly retain both attributes, although in truth only the one on the left is actually kept.


## Templates

Similar to the templating mechanism in **C++**, each waypoint is passed template parameters in addition to the inputs, which are used to further customize the functionality of that waypoint. The pairing of a waypoint and its templates is referred to as a **templated function** and exists independently of the data that the waypoint will process. One basic example is the `sep` argument in `ReadCSV`, which is used in the same manner as in `base::read.csv`. As in this example, the user is typically not directly exposed to templates and instead specifies them using arguments in the provided functions. As in **C++**, each template parameter is either a type or a constant value, although there are less restrictions on the format of such a constant than in **C++**. Similar to **R** and unlike **C++**, each template argument is usually required to have a fixed name, as template arguments are stored as associative arrays (i.e. a list in **R**).

### Types

Although less visual to the typical user than templated function, **rgrokit** also supports a type system in which *every* type can be though of as templated, although some types do not require any actual template arguments. In general, templated types are specified by either a single symbol, such as `integer`, or a call. Within such a call, each argument should be appropriately named and either a literal value or a templated type itself. Within **gtBase**, types are currently used only in **ReadCSV**. Below are some commonly used types:

Name | Description | Template Arguments
-----|-------------------
Int | 32-bit signed integer | N/A
Double | 64-bit signed double | N/A
Category | A static enumeration  | `dict`, `values`, `start.at`
Factor | A dynamic enumeration | `dict`
Vector | A fixed-size vector | `size`, `direction`, `type`
Matrix | A fixed-size matrix | `nrow`, `ncol`, `type`

## Expressions

At its most basic level, an expression is either a constant, an identifier, or a call. In the latter case, the call may be an operator such as `+` and each argument must itself be an expression. Some examples are `1`, `a`, `a + 1`, and `f(a + 1, b * 2)`. Because `R` is a functional language, it interprets many control structures as calls. For example, `if(a == 1) 2 else 3)` is equivalent to ```if`(a == 1, 2, 3)``. In fact, even the function declaration `f <- function(x) { x + 1 }` is is expressible in this format as `` `<-`(f, `function`(x(), {x + 1}))``. In short, almost anything that **R** can interpret is parsed as an expression.

Within **grokit base** there are several uses for expressions that are discussed later. In general, each expression is evaluated within the environment of some waypoint. As such, the variables which can be referenced correspond to the names of the columns of said waypoint. In addition, every expression can be thought of as strongly typed and hence functions require certain types for their arguments. It should be noted that type checking occurs only at run-time. The following is a description of how expressions are interpreted for the purposes of this library:

### Constants

Constant values are translated as is. No discrepancy is marked between integer and doubles, e.g. `1L`, `1`, and `1.0` are equivalent. The supported classes are **character**, **Date**, **numeric**, and **logical**.

### Variables

Each variable is specified by a symbol and will be required to be an attribute of the data being used almost always.

### Operators

Due to the reliance on the native **R** parser, only a subset of operators valid in **R** are supported, although the purpose of some has been changed. It is important to note that every operator is restricted to certain types.

The following table is organized in a descending fashion based on precedence.

Operator(s) | Purpose | Associativity<sup><a href="#one">1</a></sup>
------------|---------|-------------------
:: | Library Access | Left-to-Right
\$ | Method Call | Left-to-Right
[ | UDF Call | Left-to-Right
$(\cdots)$ | Function Call<sup><a href="#two">2</a></sup> | Left-to-Right
^ | Bitwise XOR | Right-to-Left
-, + | Unary Plus, Minus | Right-to-Left
$\%\cdots\%$ | Various Operators | Right-to-Left
*, / |Multiplication, Division | Left-to-Right
+, - | Addition, Subtraction | Left-to-Right
<, >, <=, >=, ==, != | Comparison | N/A<sup><a href="#three">3</a></sup>
! | Logical Negation | Right-to-Left
&&, & | Logical, Bitwise AND | Left-to-Right
\| \|, \| | Logical, Bitwise OR | Left-to-Right


> 1: <a name="one"></a>Associativity refers to the parser and does not guarantee legality. For example, both `a[][]` and `a::b::c` are parsable but illegal.
> 2: <a name="two"></a>Technically not an operator, this appears as a note of its precedence.
> 3: <a name="three"></a>An expression such as `a < b < c` is not parsable and will result in an error during parsing.

### Function Call

A function call can appear throughout an expression, specified exactly as one would call a function in native **R**. Due to the non-existence of nested namespaces,the name of the function must either be a symbol or have a single scope operator, e.g. `a::b()`. Nested scope operators, as seen in `a::b::c()` will cause an error to be thrown. Note that these calls are performed using **C++**; as such, type checking occurs which may result in an error at run-time and no such function may take a variable number of arguments.

#### Dot Function

The dot function, `.()`, can be used to perform evaluation within an expression in the environment of **R**. Requiring a single expression, it will immediately evaluate its argument and insert the result as a constant into the expression. Such a process is required to create values of some classes, such as **Date**, within an expression. The following demonstrates its usages.
```
## Filters the data on the condition b < a
data[b < a]

## Filters the data on the condition b < 4 
a <- 4
data[b < .(a)]

## b cannot be found an an error is thrown
data[.(b < a)]

```
### UDF Call

In addition to standard function calls, the user also has access to user-defined functions. Such functions typically require template arguments and are much more generalized, one example of which is the ability to possibly take an arbitrary number of inputs. Such a call is represented in **R** via a combination of the subset operator and a standard call, e.g. `a[b = c](d)`. Recall from before that a templated function exists separately from its input and that this example should be conceptualized as a call to `a[b = c]` given a single argument `d`. Also, remember that template arguments are usually named; if this is the case, the user is required to use argument names in the call, *regardless of the ordering of the arguments*. Lastly, although some <strong>UDF</strong>s do not require any template arguments, the method in which they are called is the same, e.g. `a[](d)`. Due to the way in which expressions are parsed, *the call to `[` cannot be omitted*.

### Ternary Conditional Operator

Known as the ternary operator in most languages, the inline `if` expression is also supported, e.g. `1 + if (bool) 2 else 3 > 4`. Unlike standard **R**, an `else` branch must be given and both return values must have the same type. Implicit type conversion is allowed, in which case the type of the first branch is used. For example, `if (FALSE) 1 else 2.5` will return 2. As a note of warning, the parser is greedy when deciding what to include in the branch expressions. The first example in this section is parsed such that the result of the conditional is `3 > 4` when `a` evaluates to `TRUE`, which results in a type error as a boolean is not freely convertible to an integer. Parenthesis are required for the behavior in `1 + (if (a) 2 else 3) > 4`. Users are recommended to *always* include parenthesis.

### Control Stuctures

While technically functions in **R**, loops, switch,es and conditionals are not supported.

### Method

In general, a method is a function that is evaluated within a given environment. For those of you experienced with **Java** or **C++**, this is most likely already known. However, most **R** users probably have not come across a method call, although they are present in the form of **reference classes**. As such, they are called in the same way using the `$` operator, e.g. `a$b(c, d)`. In this example, the method `b` is called on `a` and given arguments `c` and `d`.

## Waypoints {#Waypoints}

As discussed above, every waypoint exists as a **list** with class **data** in addition to at least one other class that differentiates the various classifications of waypoints. Currently, there exists the following types:

Class | Description | Templated
----|---------------- | ----
GI | The least standardized class, <strong>G</strong>eneralized <strong>I</strong>nputs read data from the disk. | Yes
GLA | <strong>G</strong>eneral <strong>L</strong>inear <strong>A</strong>ggregates are responsible for the majority of computation. | Yes
Filter | Essentially the analogue of the subset operator `[` for waypoints. | No
GF | A <strong>G</strong>eneralized <strong>F</strong>ilter is used for tasks too complex for a **filter** to handle. | Yes
Generator | The simplest way in which new columns are appended to the data. | No
GT | <strong>G</strong>eneralized <strong>T</strong>ransforms can create new columns using previous results. | Yes
Join | Combines two datasets row-by-row based on whether given attributes are equal. | No

### GI

To begin a query, first the dataset(s) must be read from the disk and then used as a source for various computation. This is the only type of waypoint that does not require input data. Examples include `Read`, `ReadCSV`, and `ReadGI`; which read relational, character-separated, and generalized data; respectively.

Also falling in this category is `as.data` which is responsible for transforming **R** objects such as **data frames** into datasets usable by the system. Beneath the surface, this is essentially a wrapper for serializing `R` objects, writing to the disk, and then calling `ReadCSV`.

### GLA

A **GLA** is a function that is arbitrarily parallelizable. As such, it is required that such a function return the same result regardless of the order in which it receives the tuples. With regards to aggregation, the waypoint maintains a state that is possibly altered while processing tuples and then returns information about that state. Additionally, <strong>GLA</strong>s are allowed to be iterable in which case they pass over the data multiple times. The specification of the inputs and outputs passed to a **GLA** are discussed below.

### Filter

Accessed using the subset operator, this waypoint filters the data based on a given boolean-valued expression. The expression is evaluated for each tuple in the input data and returns only those tuples for which the expression evaluated to `TRUE`.

Within **R**, the function `data.[` is responsible for the production of every **filter**. For example, the filter `a[b > 3]` returns the subset of the data represented by `a` in which attribute `b` is greater than 3.

### GF

A more powerful version of the **filter** in which information about previous tuples can be used. One example is a random sampling in which the probability in which the probability of being selected is based on the number of tuples already selected. In contrast, a **filter** could not handle anything more complex than a simple random sample.

### Generator

Like a **filter**, a **generator** processes each tuple independently. Given a list of named expressions by the user, it evaluates the expressions for each tuples and appends the values under the given names. The `Generate` function is responsible for creation of all  such  waypoints.

Given data set `a`, `Generated(a, d = b + c, e = b * c)` appends two columns, labelled `d` and `e`, to the data represented by `a` and returns the result. The value of attribute `d` in any given row is equal to the value of attribute `b` in that row plus the value of attribute `c` in that row; the same is true for `e` with multiplication occuring instead of addition.

### GT

Functioning similarly to a **Generator**, a **GT** takes inputs from a single other waypoint and appends a constant number of values. However, it can also use the result of other waypoints and the values of previous tuples during computation. For example, one can produce a generalized linear model of the data using a **GLA** to produce the model and then use the model in a **GT** to perform prediction on an entirely new data set.

### Join

In order to combine two datasets row-by-row, a **join** can be used. Given a number of attributes from one waypoint (the *left*) and the same amount of attributes from another (the *right*), the **join** will produce the subset of their Cartesian product in which the attributes from the left were each equal to the corresponding attribute of the right. In layman terms, the join will proceed through the left row-by-row. For each tuple, it will seek to match the given attributes to tuples in the right. Whenever it finds a match, it will concatenate the two matching tuples and add them to the result. If possible, the user should place the smalelr of the two tables on the right; doing so improves both run-time and memory usage.

In order to match values that are based on an expression and not a single attribute, it is necessary to first use a **Generator**.

A $\theta$-join can be performed using a join with no matching attributes given and filtering the result.

It is possible for a join to produce a table with more rows than either the left or the right, as demonstrated in the following.

Left | | Right | &#x200B; | Join | &#x200B; | &#x200B; | &#x200B; | &#x200B;
--|
ID1 | Name1 | ID2 | Name2 | ID1 | Name1 | ID2 | Name2
1 | A | 1 | Z | 1 | A | 1 | Z
1 | B | 1 | Y | 1 | A | 1 | Y
2 | A | 2 | Z | 1 | B | 1 | Z
2 | B | 2 | Y | 1 | B | 1 | Y
&#x200B; |||| 2 | A | 2 | Z
&#x200B; |||| 2 | A | 2 | Y
&#x200B; |||| 2 | B | 2 | Z
&#x200B; |||| 2 | B | 2 | Y





## Inputs and Outputs

As previously mentioned, both <strong>GLA</strong>s and <strong>GT</strong>s have input data coming from another waypoint and outputs. Rather than receiving data directly from a previous waypoint, the user is usually allowed to transform the output between the point at which it was produced and where it is used as input. Consider `Median`, which is a very typical example of a function that constructs such a waypoint. The header is as follows:
```
Median(data, number.bins = 1000, sort.threshold = 1000, inputs = AUTO, outputs = result)
```
The general format of these arguments, and not specifically for `Median`, is discussed below:

 data
 :   An object of class **data**, i.e. a waypoint, that is the source of the input.
 
 number.bins, sort.threshold
 :  Template arguments to the **GLA**.
 
 inputs
 :  Inputs should be a set of literal expressions, given as arguments in a call to `c` or possibly a single expression in the case of a single input. Examples include `c(a)`, `c(a, b + d)`, `a + b + d`, and `c(a, b, d)`, where `a`, `b`, and `d` are attributes of `data`. Although it would appear that using this format might result in errors because `a`, `b`, and `d` are not actual defined variables, the expressions are not fully evaluated within the scope of **R**.
 
 outputs
 :   Specified similarly to `inputs` except that only symbols, and not expressions, are allowed. These symbols are then used as the names of the attributes produced by this waypoint. For example, `c(a, b, d)` is allowed but not `c(a + b, d)`.
 
There are usually additional checks depending on the waypoint being constructed. For example, in `Median` the number of outputs must be one and both template arguments must be whole numbers.

In some extreme cases, not all of these arguments may be required. For example, `GLM` in **gtStats** and `GroupBy` do not take `inputs` as an argument. In addition, the structure of the templated arguments can be quite complex and even affect the inputs to the waypoints. For `Median`, the template arguments are simply two integers but in a  `GroupBy` the template argument `orderAtts` explicitly specifies additional inputs.

Lastly, quite often `inputs`, `outputs`, or both have a default value of `AUTO`; this is used simply as an internal keyword that is interpreted on a case-by-case basis and is discussed in the documentation for each waypoint function in which it appears. In the case of `Median`, `inputs = AUTO` is directive to use every attribute of `data` as an input to the **GLA**. If a default value other than `AUTO` appears, it is treated the same as if the user had supplied the value. In the above example, `outputs = result` simply means that the output of the waypoint is labelled as `result` unless the user supplies some other value.


## Usage Notes
Currently, the re-use of a waypoint is disallowed. In addition, the names of the outputs for every waypoint must be distinct.

## Results

The last step of the query is to write the result to the disk and additionally either import it into **R** or visualize it using the web application. Both `as.object` and `as.data.frame` will return the final result as an **R** object, either as a **list** or a **data frame**, respectively. The latter is not recommended for results that are **JSON** as the structure is not tabular like a data frame. In order to visualize the object, `View` is used.

In some cases, the user will want to store the result on the disk but not use it further. `Store` and `GetResult` will write the dataset represented by a waypoint to the disk, either as a binary relation or a text file, respectively. Currently supported formats for the text file are `.json`and `.csv`.

## Tutorials

### Basic Query

At its most basic level, a query will involve at least one **Read** waypoint and a **result** call. In order to perform any sort of computation on the data, it will involved at least one other waypoint, as demonstrated in the following:

```
library(gtBase) ## loads the library

data <- Read(lineitem100g) ## data is assigned a waypoint of type Read

agg <- Median(data, number.bins = 100, inputs = c(l_tax, l_extendedprice)) ## agg is a GLA waypoint

View(agg) ## query has been set-up and is now run
```

In this example, the median is computed for both the `l_tax` and `l_extendedprice` columns in the relational data labeled `lineitem100g`. The `Median` function requires two template arguments, `number.bins` and `sort.threshold`, the latter of which was given a default value.

### TPC-H Benchmarks

The following queries are adapted from the benchmarks for **TPC-H**, which can be found [here](http://www.tpc.org/tpch/spec/tpch2.17.0.pdf) in section 2.4. Although not crucial to the understanding of these queries, this document contains supplementary information such as queries written in an **SQL**-like language and a description of the data sets being used.

For those unfamiliar with the database operations **Group By** and **Order By**, a brief description can be found [here](http://en.wikipedia.org/wiki/SQL#Queries).


#### Pricing Summary Report

This query first filters the data by selection only those tuples whose value of `l_shipdate` is at least 90 days before `1998-12-01`, employing the dot function to do so. Afterwards,  a group is constructed for each distinct pairing of `l_returnflag` and `l_linestatus` and various statistics are computed for each group.

The syntax for the **GLA** `GroupBy` can be quite complex. Currently, there are two supported formats, the other of which is used below. The abreviated form is used below and the next query will demonstrate the other.
```
lineitem <- Read(lineitem1t)

selline <- lineitem[l_shipdate <= .(as.Date("1998-12-01")) - 90]

groupby <- GroupBy(
  selline,
  group = c(rf = l_returnflag, ls = l_linestatus),
  sum_disc_price = Sum(l_extendedprice * (1 - l_discount)),
  sum_charge = Sum(l_extendedprice * (1 - l_discount) * (1 + l_tax)),
  avg_qty = Average(l_quantity),
  count_order = Count(),
  sum_qty = Sum(l_quantity),
  avg_price = Average(l_extendedprice),
  sum_base_price = Sum(l_extendedprice),
  avg_disc = Average(l_discount)
)

orderby <- OrderBy(
  groupby,
  asc(rf),
  dsc(ls),
  rank = rank
)

View(orderby)
```

#### Minimum Cost Supplier

In this next query, the minimum cost for each part whose name ends in "BRASS" and whose size is 15 is found and returned alongside information regarding the supplier for which the price was minimal. Of these suppliers, only the hundred with the largest account balance are returned. Only suppliers located in "Europe" are considered. Note that there may be be less than 100 suppliers depending on how the data was generated.

Because this query is rather complex, each part is labelled and discussed below.
```
## Reads
part <- Read(part1t)
supplier <- Read(supplier1t)
partsupp <- Read(partsupp1t)
nation <- Read(nation1t)
region <- Read(region1t)

## Filters
selpart <- part[match(p_type$ToString(), ".*BRASS") && p_size == 15]
selregion <- region[r_name == "EUROPE"]

## Joins
j1 <- Join(nation, n_regionkey, selregion, r_regionkey)
j2 <- Join(supplier, s_nationkey, j1, n_nationkey)
j3 <- Join(partsupp, ps_partkey, selpart, p_partkey)
j4 <- Join(j3, ps_suppkey, j2, s_suppkey)

## GLAs
groupby <- GroupBy(j4,
                   group = p_partkey,
                   ExtremeTuples(inputs = c(s_acctbal, s_name, n_name, p_mfgr, s_address, s_phone, s_comment),
                                 min(ps_supplycost))
                  )

orderby <- OrderBy(groupby, dsc(s_acctbal), asc(n_name), asc(s_name), asc(p_partkey), limit = 100)

## Result
View(orderby)
```
Reads
:   This section reads in the five relations that will be used.

Filters
:   The first filter selects the parts whose names end in "BRASS" and whose size is 15. This is done by calling the `ToString` method on the `p_type` attribute and then comparing it to a regular expression using `match` in addition to checking that `p_size == 15`.
:   The second filter selects the region whose name is "Europe".

Joins
:   The first join is between nation and region by which only European nations are kept. This is done by matching the `n_regionkey` attribute to `r_regionkey` in `selregion`, which was filtered to only contain region keys corresponding to the "Europe" region. Because each region encompasses multiple countries, we know that `selregion` is smaller than `nation` and hence it goes on the right.
:   The next join proceeds in a similar fashion in order to select suppliers that are located in a country present in the first join, i.e. a country in the "Europe" region. Once again, we can ensure that the smaller table is on the right and do so.
:   Next, the suppliers of those parts in the previous filter are found. There are four suppliers per part and hence the suppliers are on the left in the join.
:   Lastly, the information regarding the location of each supplier is concatenated with the information for each supplier for each part. Choosing which table to put on the right is much more involved here than it was above. Initially, `part` is 20 times as large as `supplier` and hence `partsupp` is eighty times as large. However, `part` is filtered by a much stricter condition than `supplier` is.  Approximately one in five suppliers is chosen, as five out of 25 possible nations for a supplier are in "Europe". Only around one in 250 parts is kept because there are 50 equally likely sizes and each type has a one fifth chance of ending in "BRASS". Therefore,  `j3` is roughly $ \frac{4 \cdot 20 \cdot 5}{250}$ times as large as `j2` and hence `j2` is placed on the right.

GLAs
:   A GroupBy is performed based on `p_partkey`. For each distinct part, the lowest value of `ps_supplycost` is found and only those tuples that achieve this minimum are kept.
:   The final step of the query is to order the result based on `s_acctbal` in descending order and select the first hundred tuples.

#### Shipping Priority Queue

This query finds orders made by customers in the "Building" industry before `1995-03-15` that had the largest value.  Up to ten orders are returned, with the dates of those orders breaking ties between orders of equal value. 
```
customer <- Read(customer1t)
orders <- Read(orders1t)
lineitem <- Read(lineitem1t)

selcust <- customer[c_mktsegment == "BUILDING"]
selord <- orders[o_orderdate < .(as.Date("1995-03-15"))]
selline <- lineitem[l_shipdate > .(as.Date("1995-03-15"))]

j1 <- Join(selord, o_custkey, selcust, c_custkey)
j2 <- Join(selline, l_orderkey, j1, o_orderkey)

groupby <- GroupBy(j2,
                   group = c(l_orderkey, o_orderdate, o_shippriority),
                   revenue = Sum(l_extendedprice * (1 - l_discount))
                  )

orderby <- OrderBy(groupby, dsc(revenue), asc(o_orderdate), limit = 10)

View(orderby)
```

Originally, for each order there are between one and seven rows in `lineitem`, the exact number being random. Because `l_shipdate` is always greater than `o_orderdate` of the corresponding order, we can filter `lineitem` as above and make the second join more efficient. Also note that the the `selline` is the larger of the two tables and so it is placed on the left. 

We can also nearly guarantee that in the smaller table is on the right for the first join as well. It is is given that the number of orders is exactly ten times the number of customers. Furthermore, approximately one in five customers is selected, as there are five equally likely industries for a customer to be a member of. Likewise, approximately one in two orders is selected, as each `o_orderdate` is randomly picked from a uniform distribution in which `1995-03-15` is near the middle. Therefore, `selord` is approximately 25 times as large as `selcust`.

Lastly, note that `o_orderdate` and `o_shippriority` are grouping attributes in addition to `l_orderkey`. For each order, both of these will be constant and hence the inclusion of them does not change how groups are formed. They are included so that they will be included in the final result. In general, this is how information about each group should be retained during a **Group By**.

#### Local Supplier Volume

This query computes, for each Asian nation, how much internal trade there was in the year 1994. Here, internal trade refers to a transaction in which the customer and the supplier are in the same country. Each entry in `lineitem` is considered separately, independent of the others associated with the order it is part of.

```
customer <- Read(customer1t)
orders <- Read(orders1t)
lineitem <- Read(lineitem1t)
supplier <- Read(supplier1t)
nation <- Read(nation1t)
region <- Read(region1t)

selregion <- region[r_name == "ASIA"]
selord <- orders[o_orderdate >= .(as.Date("1994-01-01")) && o_orderdate <= .(as.Date("1995-01-01"))]

j1 <- Join(nation, n_regionkey, selregion, r_regionkey)
j2 <- Join(customer, c_nationkey, j1, n_nationkey)
j3 <- Join(selord, o_orderkey, j2, c_custkey)
j4 <- Join(lineitem, l_orderkey, j3, o_orderkey)
j5 <- Join(j4, c(l_suppkey, c_nationkey), supplier, c(s_suppkey, s_nationkey))

groupby <- GroupBy(j5,
                   group = n_name,
                   revenue = Sum(l_extendedprice * (1 - l_discount))
                  )

orderby <- OrderBy(groupby, dsc(revenue))

View(orderby)
```

The crux off the computation in this query is the joins and their efficiency. In order, the joins are used to selection cuntries in Asia, select customers in those countries, select their orders, select the entries in `lineitem` associated with each order, and, lastly, keep only those entries that are supplied intranationally. This last step is performed by using a multi-join; by joining on both `suppkey` and `nationkey` we are finding the supplier for each entry in `j4` and then keeping only those for which the country matches the origin of the order.

It is crucial that the last join is a multi-join between suppliers and orders. Had we combined the two tables before the last step, the result would have been monstrously large and infeasible to use. As mentioned before, it is possible for a join to produce a table much larger than either of its inputs. In this case, each supplier in a given country would be matched with every customer in that country. If a country had 100 thousand of each, the result would be 10 billion rows per country. However, this is avoided by waiting to merge the two until the end. Instead, we effectively look-up the supplier for each entry in `lineitem` and only take that entry into account if the supplier is in the country in which that entry was ordered.

#### Forecasting Revenue Change

This query computes the additional revenue that would be brought in by not applying discount between 5% and 7% on items shipped in 1994 when less than 24 were bought at once.
```
lineitem <- Read(lineitem1t)

selline <- lineitem[   l_shipdate >= .(as.Date("1994-01-01"))
                    && l_shipdate <  .(as.Date("1995-01-01"))
                    && l_discount <= 0.07
                    && l_discount >= 0.05
                    && l_quantity <  24]

agg <- Sum(selline, l_extendedprice * l_discount, revenue)

View(agg)
```


# gtStats

Building upon **gtBase**, **grokit statistics** introduces several new <strong>GLA</strong>s with a focus on machine learning, including clustering and generalized linear models. Due to the complex nature of these algorithms, specifying the template arguments and inputs is a more involved process and one should carefully read their respective help pages before using them.

# gtTranslator

**grokit translator** makes it possible for the user to define <strong>GLA</strong>s in **R** and then have them cross-compiled to **C++** before being used in a query. Although this lacks the full flexibility of writing <strong>GLA</strong>s directly in the back-end, the user does gain partial access to template arguments. This translation is performed by the function `MakeGLA` which returns a function that has similar structure to other functions providing access to <strong>GLA</strong>s, such as `Median`.

The format of the arguments to `MakeGLA`resembles a hybrid between those of `setClass` and `setRefClass`, which are used to specify **S4** classes and **reference classes** respectively. Because each **GLA** is its own **C++** class, both types and fields must be specified, with the former also being given types.

Because `MakeGLA` relies heavily on the **R** parser to perform cross-compilation, many of its arguments are not fully evaluated before they are used. Therefore, it is imperative that the user passes literal **R** code when directed to. For example, the `representation` argument is required to be a call to `list`. It cannot be an identifier that represents a `list`.

## Arguments

types
:   An unnamed call to `list` in which the elements are symbols. These symbols are used to label the input types to the **GLA** and can be later referenced in the `constants` arguments to provide access to template arguments.

constants
:   A named call to `list` in which the names correspond to the fields of the class that are constants. Values should either be constant-valued expressions, which requires them to not contain any identifiers, or be in the form of `a$b`, where `a` is an element of `types` and `b` is a property of the type that `a` represents. 

representation
:   A named call to `list`in which the names correspond to the fields of the class that are not constants and the values evaluate to templated types as discussed below.

prototype
:   Either a literal call to `function` or an identifier of a previously constructed function. This function acts as the constructor of the class and while closely resembling the equivalent argument in `setClass`, it is much more flexible. 

AddItem
:   Either a literal call to `function` or an identifier of a previously constructed function. The arguments of this function should be used as they would in other **R** functions. Each argument is allowed the option of being assigned a default value. If specified, the default value must be a templated type; this type is then used as the type of the parameter and is compared to the type of the input data to check for compatibility. If no default value is given, the type of the parameter is assumed to be the type of the input data. This function is called once for each tuple in the input data, with the first expression given being used as the first argument and so forth.

AddState
:   Either a literal call to `function` or an identifier of a previously constructed function with exactly one argument given. No default value should be given because the type of this parameter will always be the class of this **GLA**. As such, this argument will have fields that can be accessed in the body of this function using the `$` operator, which will act like the **S4** operator `@`. This function is used to combine two states that have been constructed on separate cores in parallel.

GetResult
:   Either a literal call to `function` or an identifier of a previously constructed function. The arguments to this function will be used as the default value of `outputs` in the function returned by `MakeGLA`. In addition, every argument is requirerequiredd to have a default value that is a templated type which are then used as the output types of this **GLA**. These types can either be basic types, such as **DOUBLE** and **INTEGER**, or **JSON**. Other complex types such as **VECTOR** are not currently supported. In the case of **JSON** output, only a single output is allowed. Within the body of the function, there should be at least one call to `return`. These calls are not restricted to a single argument as in `base::return`; instead, if basic output types were used, there should be a number of unnamed arguments equal to the number of outputs. If **JSON** was used, each argument is required to have a name and there is no restriction on the amount.

## Statements

Each function body is either composed of a single statement, such as `f(a, b) a + b`, or a call to the bracket function, `{`, whose arguments are all statements. Regardless of the format, these are treated the same in this translator. The following table briefly describes the various types of statements and their support in this translator:

Type | Example(s) | Description | Support
-----|---------|-------------|----------
constant | `1`, `var` | a constant value or a single identifier | Ignored
declaration | `a <- type` | declares a new value with the specified type | Yes
assignment | `a <- val` | assigns an expressions to a variable | Yes
for-each | `for` | a `for-each` loop | No 
loops | `repeat`, `while` | various types of loops | Yes
control | `break`, `next` | control statements within a loop | Yes
switch | `switch` | switch on an integer argument | No
locals | `locals` | compact format for multiple declarations | Yes
return | `return` | used for function return, no implicit return values | Yes
block | `{ block }` | delimits a new block and namespace | Yes
expression | a + b | expression seen in place of a statement | Yes

### Constant

A constant statement is comprised solely of either an identifier or a literal logical, numeric, or character value. One example is ` function(x) {1; 2}`. In **R**, this can be useful as such a statement is used as the value of the body of the code and possibly as a return statement; for example, the above example will always return `2` due to the second constant-valued statement, although the first line is entirely useless. Because implicit return statements are not supported in **C++**, statements of this type are ignored with a warning.

### Declaration

Unlike **R**, variables in **C++** are declared with a fixed type. As a compromise between the two, both implicit and explicit type declaration is supported; however, in both cases the type is still constant for the lifetime of the object. Declaration statements allow the user to explicitly define the type of a new object. The format of a declaration is `a <- type`, where `type` is a templated type and `a` is a symbol representing an identifier. Attempting to declare an already declared variable or an object other than a symbol will result in an error.

### Assignment

Building upon **C++11**, assignment statements are supported for undeclared variables through the use of the `auto` keyword. However, this is less safe than explicit declaration and can result in mismatched type errors. For example, `a <- 1L; a <- "b"` is perfectly in **R** but the equivalent in **C++** will cause a compilation error. This is because `a` is both declared with type `int`, initialized with value `1`, and then assigned a string value "b", which is disallowed because strings are not implicitly convertible to integers. On the other hand, `a <- 1L; a <- 1.2` is perfectly valid in **C++**; although `a` has type `int` and is assigned a value of type `double`, no error is thrown because the value `1.2` is implicitly cast as an `int`. Due to these nuances of **C++** that are not present in **R**, it is recommended that users inexperienced with **C++** use explicit type declaration wherever possible.

### For-EachEach

Although commonly referred to as a "for loop", **R** only supports iteration over an object. Due to inefficiency at run-time, such loops are not supported. In order to use a traditional for loop, a while loop within a new block is an exact substitution.

### Loops

Both `while` and `repeat` are supported, the latter functioning as a while-true loop.

### Control

Both `break` and `next` statements are supported. Currently, no checks are performed to ensure these are with-in the body a loop, which would result in an error at compile-time. Although the labeling feature of **C++** would improve the versatility of such commands, this is currently not supported.

### Switch

Switch is currently not supported.

### Locals

The `locals` function functions as a compact representation of multiple declarations. The argument list should be in the form of `name = type`. Each pairing is treated as a separate declaration and translated accordingly

### Return

Outside of `GetResult`, `return` is directly translated and expects a single argument. Otherwise, it functions as described previously. In either case, no checks are executed in order to guarantee that there exists a return statement in every possible branch. If such is the case, an error is thrown at compile-time.

### Block

Although generally unnecessary and therefore unused, the **R** parser supports blocks of code outside of control structures such as `function`, `if`, and `for`. For example, `function(x) { a <- 1; { b <- 2 }; b}` is perfectly valid. Unlike **R**, blocks in **C++** serve to enclose namespaces and affect which variables are defined. If translated to **C++**, the previous example would be invalid. Because `b` was declared within the inner block, it would not be able to be referenced outside of said block. One of the primary uses of this feature is to create a for-loop, as demonstrated below:

<div style="width:100%;position:relative;">
<div style="width:50%;position:relative;">
<pre class="prettyprint"><code>## Translatable Code
function(x) {
    sum = 0
    {
        counter = 0
        while (counter <= x) {
            sum = sum + counter
            counter = counter + 1
        }
    }
    return(sum)
}
</code></pre></div>
<div style="width:50%;position:absolute;top:0;left:50%;">
<pre class="prettyprint"><code>## Standard R
function(x) {
    sum <- 0
    
    
    for (i in 0:x)
        sum <- sum + x
        
    
    
    sum
}
</code></pre></div></div>

The block of code on the left is a translatable equivalent to the code on the right. If the inner block was not used, then the variable `counter` would continue to exist after the completion of the loop.

### Expression

Alternatively, a statement could be a single expression, the specification of which is discussed below. In such a case, it is unlikely that the statement will actually have any functionality. For example, the statement `1 + 2` is not at all useful. Although such a statement can be meaningful in **R** by affecting the return value of a function, this is not in the case in **C++**. The rare case in which a single expression may be meaningful is a call to a function whose arguments are taken by reference. In addition, an unnecessary statement does not have a negative impact on the running of the **GLA** because the compiler will recognize it as inconsequential and therefore the expression will never be computed.

## Expressions

The general definition of an expression in **R** was described previously when discussing **grokit base**. In order to accommodate translation to **C++**, `MakeGLA` interprets expressions differently in several key aspects:

### Constants

Translated as is. Distinctions are made as much as possible between types in order to allow the user more control. For example, `1L` is translated as an integer but both `1` and `1.0` are treated as a double.

In addition, any variable declared as part of `constants` is treated as a constant. As such, it is allowed as a template argument of types.

### Variables

Variables used in an expression must have been previously declared and initialized with one exception. When on the left hand side of an assignment, a variable does not need to be declared so long as it is being directly assigned to. For example, if `a` is not previously declared, `a = 1` is legal but `a[[1]] = 1` is not.

A variable can be declared as an argument of `representation`, as an argument of `locals`, in a declaration statement, or by being assigned a value.

A variable can be initialized as an argument of `prototype` or by being assigned a value.

In order to being a valid identifier, the name of the variable must being with an alphabetical character followed by any number of alphanumeric characters and underscores. Note that this is significantly different from the restrictions in **R** in which periods are allowed anywhere in the name.

### Types

A type can be constructed in one of three ways: as part of `types`, using the `typeof` function, or as a templated type. In the latter case, the type is translated as described in **grokit base**.

`typeof` is interpreted as `decltype` in **C++11** and takes a single argument. That argument must still comply with the rules for specifying an expression, including the use of initialized variables.

### Field Access

In the body of `AddState`, it is possible to access the fields of the single argument. This is done via the `$` operator. For example, if the header of `AddState` is `AddState(o)`, then within the body of `AddState` the expression `o$x` is valid if `x` is the name of an argument in `representation`; otherwise, an error is thrown. Note that this includes that the case that `x` is the name of a constant as there is no need to access `o` to retrieve the constant value.

### Operators

The following is the set of operators listed in order of precedence. However, some of these operators may fail when types are checked. For example, if `a` is an integer then `a[[1]]` is legal in **R** but a compilation error occur upon being translated to **C++**.

Operator(s) | Purpose | Associativity<sup><a href="#one">1</a></sup>
------------|---------|-------------------
:: | Library Access | Left-to-Right
\$ | Field Access | Left-to-Right
[[ | Element Access | Left-to-Right
$(\cdots)$ | Function Call<sup><a href="#two">2</a></sup> | Left-to-Right
^ | Bitwise XOR | Right-to-Left
** | Exponentiation | Right-to-Left
-, + | Unary Plus, Minus | Right-to-Left
%% | Modulus | Right-to-Left
%*% | Schur Product | Right-to-Left
*, / |Multiplication, Division | Left-to-Right
+, - | Addition, Subtraction | Left-to-Right
<, >, <=, >=, ==, != | Comparison | N/A<sup><a href="#three">3</a></sup>
! | Logical Negation | Right-to-Left
&&, & | Logical, Bitwise AND | Left-to-Right
\| \|, \| | Logical, Bitwise OR | Left-to-Right

### Functions

Functions call are translated as is. Any periods present in the name of the function are replaced with underscores.

### Casting

Type conversion is performed using the family of `as.` functions, such as `as.integer`. The portion of the call following `as.` must be a valid type. For example, `as.matrix(nrow = 3, ncol = 3)(a)` is a valid expression that converts converts `a` to an object with type `matrix(nrow = 3, ncol = 3)`. In addition to templated types, both types defined in `types` and a call to `typeof` may be used. An example of the latter is `as.typeof(b)(a)`.

## Tutorials

### Average Value

In the following example, we construct a **GLA** that computes the total number of elements in a set and their sum. It returns both of these and the average value in a **JSON** format.
```
library(gtTranslator)

gla <- MakeGLA(representation = list(count = integer, sum = double),
               prototype = function(count = 0L, sum = 0){},
               AddItem = function(x) {sum = sum + x; count = count + 1L},
               AddState = function(o) {sum = sum + o$sum; count = count + o$count},
               GetResult = function(result = JSON) {return(average = sum / count, sum = sum, count = count)})

data <- Read(lineitem10g)

agg <- gla(data, inputs = l_tax, outputs = avg_tax)

View(agg)
```

representation
:   There will be two non-constant fields in this class, the integer `count` and the double `sum`. The first is used to track the total sum of the inputs received; the latter, the total number of inputs seen.

prototype
:   Initially, both of these are zero. Note the discrepancy between the types of the zero values.

AddItem
:   Each input value is added to the total sum and the count is incremented to account for having processed one more input.

AddState
:   In order to acculmulate two different states, their respective values of `count` and `sum` are added together.

GetResult
:   The default name of the output returned by this **GLA** is `result` and the output type is **JSON**. Alternatively, three outputs could have been used with types **DOUBLE**, **DOUBLE**, and **INTEGER**, respectively. If this were the case, then the arguments to `return` should be unnamed.

The rest of the query is straightforward and follows the methodology demonstrated in the examples of **grokit base**.

### Linear Regression

Next, the creation of a more complex **GLA** that makes use of **matrices** will be demonstrated. For those unfamiliar with the computation done during ordinary least squares, a brief description can be found [here](http://en.wikipedrepresentationia.org/wiki/Ordinary_least_squares#Estimation). For sake of brevity, the proceeding discussion will use the same names for the various objects.

It may not be immediately obvious how such an algorithm is performed in parallel. Let $n$ be the size of the sample data and let $p $ be the number of predictors. Hence, $ X $, a matrix in which each row is a vector of predictors, is an $ n \times p $ matrix and $ X^T X $ is a $ p \times p $ matrix, the size of which is entire independent of $ n $. Furthermore, $ X^T X = \sum x_i^T x_i $, where $ x_i $ is the $i$th row of $ X $, i.e. the set of predictor values for the $i$th point in the data. Because addition is associative and commutative, we can break this summation up over several cores and add the results. The same is true of $ X^T Y = \sum x_i^T y $.
```
library(gtTranslator)

gla <- MakeGLA(types = list(xType),
               constants = list(size = xType$size),
               representation = list(count = integer, XXT = translator::matrix(ncol = size, nrow = size), XY = xType),
               prototype = function(count = 0, XXT = zeros(size, size), XY = zeros(size)){},
               AddItem = function(x, y) {
                 count = count + 1L
                 XXT = XXT + x * x$t()
                 XY = XY + x * y
               },
               AddState = function(o) {
                 count = count + o$count
                 XXT = XXT + o$XXT
                 XY = XY + o$XY
               },
               GetResult = function(result = JSON) {
                 return(beta = XXT$i() * XY, count = count)
               })

data <- Read(lineitem10g)

agg <- gla(data, inputs = c(translator::MakeVector[](l_extendedprice, l_tax, l_discount), l_quantity))

result <- as.object(agg)
```
types
:   In general, linear regression expects a set of sample data, in which element is vector of predictors paired with a response value. In order to make this **GLA** as generalized as possible, the exact type of the vector is not hard-coded which allows for the vector to be of any size and contain any type of data. In order to so, we assign `xType` the type of the first input, the vector of predictors.

constants
:   The size of the input vector in assigned to the variable `size`. This will allow for various data structures to be constructed with the appropriate size.

representation
: `count` is used to compute $ n $. While this is not strictly necessary, it is computed for sake of analytics. `XXT` is used to compute the value of $ X X^T $. This differs from the above discussion because the input is expected to be represented as a column and not a row. Likewise, `XY` is used to compute the value of $ Xy$. By requiring the input to be in column, the matrix $ Xy $ is no longer required to be transposed prior to computing $ \hat{\beta} $. 

prototype
:  Each variable is computed additively and hence their initial values are all zero.

AddItem
:  The count is incremented. The values $ x_i x_i^T $ and $ x_i y_i $ are added to the respective summations.

AddState
:   As each computation is additive, it is a simple matter to combine two states simply by summing their fields.

GetResult
:  A **JSON** is return containing $ \hat{\beta} = (X X^T)^{-1} Xy $ and the total number of elements processed. Tte latter is returned purely as a diagnostic in order to demonstrate that all tuples were processed.


The only point of interest in the rest of the query is use of `MakeVector`. This is a **UDF** transforms a list of inputs into a vector. As impressed earlier, the brackets were still used despite the absence of template arguments. Additionally, had the template argument `direction = "row"` been used, the algorithm would more closely resemble the computations on the linked page. This is omitted for the sake of simplicity.


