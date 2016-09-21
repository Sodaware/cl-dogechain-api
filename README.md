# dogechain-api


## Description

**dogechain-api** is Common Lisp library for working with the dogechain.info
API.


## Installation (with Quicklisp)

Clone the repository into your `~/quicklisp/local-projects/` directory:

```bash
cd ~/quicklisp/local-projects/
git clone https://github.com/Sodaware/cl-dogechain-api.git
```

Include the code in whatever you're doing:

```lisp
(ql:quickload :dogechain-api)
```

Now you're all ready to have fun!


## Testing

Tests use [prove](https://github.com/fukamachi/prove) and live in the `/t/`
directory. With SBCL installed they can be executed by running `make test`.

Alternatively, fire up a REPL from the repository directory and run the
following commands:

```bash
(load "dogechain-api.asd")
(load "dogechain-api-test.asd")
(asdf:test-system :dogechain-api)
```


## Function Documentation

### Simple Query API

The DogeChain simple query API docs can be found here:
http://dogechain.info/api/simple

The API functions are mapped as follows:

API Method           | Local Function
---------------------|--------------------------------------
addressbalance       | dogechain-api:get-address-balance
addresstohash        | dogechain-api:address-to-hash
checkaddress         | dogechain-api:valid-address-p
decode_address       | dogechain-api:decode-address
getblockcount        | dogechain-api:get-block-count
getdifficulty        | dogechain-api:get-difficulty
getreceivedbyaddress | dogechain-api:get-address-received
getsentbyaddress     | dogechain-api:get-address-sent
getbc                | dogechain-api:get-total-mined


#### dogechain-api:get-address-balance *address*

Get the total amount ever received, minus the total amount ever sent for
*address*.

Will raise a `dogechain-api-error` if the address is invalid.

```lisp
(dogechain-api:get-address-balance "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> 12345.6789
```

#### dogechain-api:address-to-hash *address*

Get the public key hash for *address*.

Will raise a `dogechain-api-error` if the address is invalid.

```lisp
(dogechain-api:address-to-hash "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> "F8783344AF8532A73DFA97EBDDFCC7527A2C6E5A"
```

#### dogechain-api:valid-address-p *address*

Check *address* for validity.

```lisp
(dogechain-api:valid-address-p "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> T

(dogechain-api:valid-address-p "INVALID")
=> NIL
```

#### dogechain-api:decode-address *address*

Get the version prefix and hash encoded in *address*. Returns an association
list containing `:version` and `:hash` keys.

Will raise a `dogechain-api-error` if the address is invalid.

```lisp
(dogechain-api:decode-address "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> ((:version . "1E")
    (:hash . "F8783344AF8532A73DFA97EBDDFCC7527A2C6E5A"))
```

#### dogechain-api:get-block-count

Get the current block number.

```lisp
(dogechain-api:get-block-count)
=> 123456
```

#### dogechain-api:get-difficulty

Get the difficulty of the last solved block.

```lisp
(dogechain-api:get-difficulty)
=> 123456
```

#### dogechain-api:get-address-received *address*

Get the total amount ever received by *address*.

Will raise a `dogechain-api-error` if the address is invalid.

```lisp
(dogechain-api:get-received-by-address "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> 123456.7890
```

#### dogechain-api:get-address-sent *address*

Get the total amount ever sent by *address*.

Will raise a `dogechain-api-error` if the address is invalid.

```lisp
(dogechain-api:get-sent-by-address "DTnt7VZqR5ofHhAxZuDy4m3PhSjKFXpw3e")
=> 123456.7890
```

#### dogechain-api:get-total-mined

Get the total amount of currency ever mined.

```lisp
(dogechain-api:get-total-mined)
=> 123456.7890
```

## Licence

Copyright (C) 2015 Phil Newton

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.
