# klow

### K(ind of s)LOW

Text prediction written in clojure

### Usage

`Usage: clj -X klow/main -t <twt|txt> -i <filepath>`

By using this command, the program generates a new
set of grams from the input data.

Text files must be formatted with each gram as such, with each gram being its own line.

```
<s> Left is start token, right is end token. </s>
<s> This is a second gram! </s>
```

If Twitter archive data is used, then you must edit
the original tweets.js to look like the following

```
{"tweets" : [
  {
    "tweet" : {
                ...
    }
  },
  {
    "tweet" : {
                ...
    }
  }
  ...
]}
```

### Info

This project was created to teach myself clojure/functional programming. 
While there may be many aspects that could see some improvement, 
I would like to think of the project as a success as now I can use clojure.

