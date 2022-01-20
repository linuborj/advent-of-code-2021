Making operations out of instructions
-------------------------------------
If we split the list of instructions up by the input instructions we'll get 14 "operations". We can represent how these operations affect the `z` registry by building expressions based on the instructions. Below is the list of expressions given by the `printOperations` function, where `w₂ⁱ` is the second input to the `w` registry, and `z₅` is the value of the `z` registry after the first 5 operations have been run.
```
*Main> printOperations
z₁:=(z₀*((25*((((z₀%26)+15)=w₁ⁱ)=0))+1))+((w₁ⁱ+9)*((((z₀%26)+15)=w₁ⁱ)=0))
z₂:=(z₁*((25*((((z₁%26)+11)=w₂ⁱ)=0))+1))+((w₂ⁱ+1)*((((z₁%26)+11)=w₂ⁱ)=0))
z₃:=(z₂*((25*((((z₂%26)+10)=w₃ⁱ)=0))+1))+((w₃ⁱ+11)*((((z₂%26)+10)=w₃ⁱ)=0))
z₄:=(z₃*((25*((((z₃%26)+12)=w₄ⁱ)=0))+1))+((w₄ⁱ+3)*((((z₃%26)+12)=w₄ⁱ)=0))
z₅:=((z₄/26)*((25*((((z₄%26)+-11)=w₅ⁱ)=0))+1))+((w₅ⁱ+10)*((((z₄%26)+-11)=w₅ⁱ)=0))
z₆:=(z₅*((25*((((z₅%26)+11)=w₆ⁱ)=0))+1))+((w₆ⁱ+5)*((((z₅%26)+11)=w₆ⁱ)=0))
z₇:=(z₆*((25*((((z₆%26)+14)=w₇ⁱ)=0))+1))+(w₇ⁱ*((((z₆%26)+14)=w₇ⁱ)=0))
z₈:=((z₇/26)*((25*((((z₇%26)+-6)=w₈ⁱ)=0))+1))+((w₈ⁱ+7)*((((z₇%26)+-6)=w₈ⁱ)=0))
z₉:=(z₈*((25*((((z₈%26)+10)=w₉ⁱ)=0))+1))+((w₉ⁱ+9)*((((z₈%26)+10)=w₉ⁱ)=0))
z₁₀:=((z₉/26)*((25*((((z₉%26)+-6)=w₁₀ⁱ)=0))+1))+((w₁₀ⁱ+15)*((((z₉%26)+-6)=w₁₀ⁱ)=0))
z₁₁:=((z₁₀/26)*((25*((((z₁₀%26)+-6)=w₁₁ⁱ)=0))+1))+((w₁₁ⁱ+4)*((((z₁₀%26)+-6)=w₁₁ⁱ)=0))
z₁₂:=((z₁₁/26)*((25*((((z₁₁%26)+-16)=w₁₂ⁱ)=0))+1))+((w₁₂ⁱ+10)*((((z₁₁%26)+-16)=w₁₂ⁱ)=0))
z₁₃:=((z₁₂/26)*((25*((((z₁₂%26)+-4)=w₁₃ⁱ)=0))+1))+((w₁₃ⁱ+4)*((((z₁₂%26)+-4)=w₁₃ⁱ)=0))
z₁₄:=((z₁₃/26)*((25*((((z₁₃%26)+-2)=w₁₄ⁱ)=0))+1))+((w₁₄ⁱ+9)*((((z₁₃%26)+-2)=w₁₄ⁱ)=0))
```
We can rewrite this by hand to get something slightly more readable.
```
 u₁:=((z₀ %26)+15)≠w₁ⁱ     v₁:=(w₁ⁱ +09)*u₁     z₁:=(z₀*((25*u₁)+1))+v₁
 u₂:=((z₁ %26)+11)≠w₂ⁱ     v₂:=(w₂ⁱ +01)*u₂     z₂:=(z₁*((25*u₂)+1))+v₂
 u₃:=((z₂ %26)+10)≠w₃ⁱ     v₃:=(w₃ⁱ +11)*u₃     z₃:=(z₂*((25*u₃)+1))+v₃
 u₄:=((z₃ %26)+12)≠w₄ⁱ     v₄:=(w₄ⁱ +03)*u₄     z₄:=(z₃*((25*u₄)+1))+v₄
 u₅:=((z₄ %26)-11)≠w₅ⁱ     v₅:=(w₅ⁱ +10)*u₅     z₅:=((z₄/26)*((25* u₅)+1))+v₅
 u₆:=((z₅ %26)+11)≠w₆ⁱ     v₆:=(w₆ⁱ +05)*u₆     z₆:=(z₅*((25*u₆)+1))+v₆
 u₇:=((z₆ %26)+14)≠w₇ⁱ     v₇:=(w₇ⁱ +00)*u₇     z₇:=(z₆*((25*u₇)+1))+v₇
 u₈:=((z₇ %26)-06)≠w₈ⁱ     v₈:=(w₈ⁱ +07)*u₈     z₈:=((z₇/26)*((25*u₈)+1))+v₈
 u₉:=((z₈ %26)+10)≠w₉ⁱ     v₉:=(w₉ⁱ +09)*u₉     z₉:=(z₈*((25*u₉)+1))+v₉
u₁₀:=((z₉ %26)-06)≠w₁₀ⁱ   v₁₀:=(w₁₀ⁱ+15)*u₁₀   z₁₀:=((z₉/26)*((25*u₁₀)+1))+v₁₀
u₁₁:=((z₁₀%26)-06)≠w₁₁ⁱ   v₁₁:=(w₁₁ⁱ+04)*u₁₁   z₁₁:=((z₁₀/26)*((25*u₁₁)+1))+v₁₁
u₁₂:=((z₁₁%26)-16)≠w₁₂ⁱ   v₁₂:=(w₁₂ⁱ+10)*u₁₂   z₁₂:=((z₁₁/26)*((25*u₁₂)+1))+v₁₂
u₁₃:=((z₁₂%26)-04)≠w₁₃ⁱ   v₁₃:=(w₁₃ⁱ+04)*u₁₃   z₁₃:=((z₁₂/26)*((25*u₁₃)+1))+v₁₃
u₁₄:=((z₁₃%26)-02)≠w₁₄ⁱ   v₁₄:=(w₁₄ⁱ+09)*u₁₄   z₁₄:=((z₁₃/26)*((25*u₁₄)+1))+v₁₄
```
This is looking good, there is clearly a pattern! And furthermore it is clear that we do not need to care about the `x` and `y` registries. The only things that matter are `z` and the input values.

Classification of operations
----------------------------
The first thing to note is that there seems to be two different flavors of operations. The first we will call the `α-operation` and it has the form
```
# α-operation
u:=((z%26)+r)≠wⁱ  v:=(wⁱ+s)*u   z':=(z*((25*u)+1))+v
```
We will call the second one the `β-operation`
```
# β-operation
u:=((z%26)+r)≠wⁱ  v:=(wⁱ+s)*u   z':=((z/26)*((25* u)+1))+v
```
For both operations the `r` and `s` represent the given parameters.


What does it mean?
------------------
The first thing to notice is that `u` is `0` or `1`. For the `α-operation` this means
```
# α-operation when u=0
z':=z

# α-operation when u=1
z':=(z*26)+wⁱ+s
```
and for the `β-operation`
```
# β-operation when u=0
z':=z/26

# β-operation when u=1
z':=((z/26)*26)+wⁱ+s
```

Note that the only reliable way to reduce the value of `z` is the `β-operation` when `u=0`. And since the plan is to have `z=0` it seems likely that we want that to happen as many times as possible. Especially since there are an equal number of the two different operations in our input. In fact, it might be that we should only consider the cases where we have an `α-operation` with `u=1` or a `β-operation` with `u=0`. The observation that leads to this is that both seem to work in base-26. We can in fact rewrite them in base-26 as
```
# α-operation when u=1
z':=(z<<1)+wⁱ+s

# β-operation when u=0
z':=z>>1
```
which looks an aweful lot like a stack - which is familiar territory. And something we should be able to manipulate to be empty at the end.

Solution
--------
Simply search for `w` such that `u=1` when it is an `α-operation` and `u=0` when it is a  `β-operation`.

