# DISPATCH

## Synopsis

Implementation of the function `SPECIALIZER-INTERSECTION`
    
## API

* `find-method-ambiguities` --  Return a list of plists.
Each plist has keys `:qualifiers` `:methods` `:arg-types`.
Each plist represents a method ambiguity, i.e., methods which have the same precedence if
argument precedence is ignored.


* `specializer-intersections` --    Compute and return a list of all the specializers (classes for
example) which inherit from both `spec1` and `spec2`, but disregarding
specializers which inherit from another one of the calculated
specializers.  E.g., If spec1=`<class A>` and spec2=`<class B>`, then
calculate the list of all classes inheriting from both A and B.  But
if C and D both inherit from A and B, but C also inherits from D then
omit C in the return list.

## License

```
Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```