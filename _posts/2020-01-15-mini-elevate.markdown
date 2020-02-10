---
layout: post
title:  "Mini-Elevate"
date:   2020-01-24 12:30:00 +0000
---

<details>
<summary>{$\begin{equation*} \begin{split} \tau \mathrel{\mathop:}= \ &\mathcal{A} \mid \textbf{Nat} \mid \textbf{Bool} \mid \tau_1 \to \tau_2 \mid \llbrace \rho \rrbrace \mid \langle \rho \rangle \mid \mathcal{A} \textbf{ as } \langle \rho \rangle \ \mid \\ &\langle \rho \rangle \hookrightarrow \tau \mid \sigma [\tau_1, \dots, \tau_m][\rho_1, \dots, \rho_n]\end{split} \end{equation*}$}<span style="float:right;">Types</span></summary>
<ul>
<li class="item-description"><span>{$\mathcal{A}$}</span><span>Type variable</span></li>
<li class="item-description"><span>{$\textbf{Nat}$}</span><span>Natural number type</span></li>
<li class="item-description"><span>{$\textbf{Bool}$}</span><span>Boolean type</span></li>
<li class="item-description"><span>{$\tau_1 \to \tau_2$}</span><span>Function type</span></li>
<li class="item-description"><span>{$\llbrace \rho \rrbrace$}</span><span>Record type</span></li>
<li class="item-description"><span>{$\langle \rho \rangle$}</span><span>Variant type</span></li>
<li class="item-description"><span>{$\mathcal{A} \textbf{ as } \langle \rho \rangle$}</span><span>Recursive variant type</span></li>
<li class="item-description"><span>{$\langle \rho \rangle \hookrightarrow \tau$}</span><span>Case type</span></li>
<li class="item-description"><span>{$\sigma [\tau_1, \dots, \tau_m][\rho_1, \dots, \rho_n]$}</span><span>Fully applied type scheme</span></li>
</ul>
</details>

<details>
<summary>{$\rho \mathrel{\mathop:}= \mathcal{B} \mid \# \mid l: \tau \text{ , } \rho$}<span style="float:right;">Row-types</span></summary>
<ul>
<li class="item-description"><span>{$\mathcal{B}$}</span><span>Row-type variable</span></li>
<li class="item-description"><span>{$\#$}</span><span>Empty field sequence</span></li>
<li class="item-description"><span>{$l: \tau \text{ , } \rho$}</span><span>Extending {$\rho$} with a field labelled as {$l$} of type {$\tau$}</span></li>
</ul>
</details>

<details>
<summary>{$\kappa \mathrel{\mathop:}= \{ l_1, \dots, l_n \}$}<span style="float:right;">Kinds</span></summary>
<ul>
<li class="item-description"><span>{$\{ l_1, \dots, l_n \}$}</span><span>A set of labels that a row-type variable must not contain</span></li>
</ul>
</details>

<details>
<summary>{$\sigma \mathrel{\mathop:}= \Sigma \mid \forall(\mathcal{A}_1, \dots, \mathcal{A}_m).\forall(\mathcal{B}_1 : \kappa_1, \dots, \mathcal{B}_n : \kappa_n).\tau$}<span style="float:right;">Type Schemes</span></summary>
<ul>
<li class="item-description"><span>{$\Sigma$}</span><span>Type scheme alias</span></li>
<li class="item-description"><span>{$\forall(\mathcal{A}_1, \dots, \mathcal{A}_m).\forall(\mathcal{B}_1 : \kappa_1, \dots, \mathcal{B}_n : \kappa_n).\tau$}</span><span>A type {$\tau$} containing type variables {$\{\mathcal{A}_1, \dots, \mathcal{A}_m\}$} and row-type variables {$\{\mathcal{B}_1, \dots, \mathcal{B}_n\}$} bound by the universal quantifiers, where the kinds of row-type variables are respectively given by {$\{\kappa_1, \dots, \kappa_n\}$}</span></li>
</ul>
</details>

<details>
<summary>{$\delta \mathrel{\mathop:}= l \mid \delta \cdot \pi$}<span style="float:right;">Data-patterns</span></summary>
<ul>
<li class="item-description"><span>{$l$}</span><span>Label as nullary constructor</span></li>
<li class="item-description"><span>{$\delta \cdot \pi$}</span><span>Application in patterns</span></li>
</ul>
</details>

<details>
<summary>{$\pi \mathrel{\mathop:}= x \mid \delta$}<span style="float:right;">Patterns</span></summary>
<ul>
<li class="item-description"><span>{$x$}</span><span>Variable</span></li>
<li class="item-description"><span>{$\delta$}</span><span>Data-pattern</span></li>
</ul>
</details>

<details>
<summary>{$\iota \mathrel{\mathop:}= \mathbb{N} \mid \mathbf{True} \mid \mathbf{False}$}<span style="float:right;">Literal Values</span></summary>
<ul>
<li class="item-description"><span>{$\mathbb{N}$}</span><span>Natural number</span></li>
<li class="item-description"><span>{$\mathbf{True}$}</span><span>Boolean value True</span></li>
<li class="item-description"><span>{$\mathbf{False}$}</span><span>Boolean value False</span></li>
</ul>
</details>

<details>
<summary>{$\begin{equation*} \begin{split} t \mathrel{\mathop:}= \ &\iota \mid l \mid x \mid t_1 \ t_2 \mid \mathbf{type} \ \Sigma = \sigma \ \mathbf{in} \ t \ \mid \\ &\mathbf{let} \ f : (x_1 : \tau_1) \to \dots \to (x_n : \tau_n) \to \tau_t = t \ \mathbf{in} \ t_2 \ \mid \\ &\llbrace l_i = t_i\rrbrace^n_{i = 1} \mid \langle\pi_i \Rightarrow t_i\rangle^n_{i = 1} \ \mid \\ &t.l \mid \mathbf{match} \ t_1 \ \mathbf{with} \ t_2\end{split} \end{equation*}$}<span style="float:right;">Terms</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

$$
  \begin{align*}
  &\typedef{\tsvar{Result}}{
    \polyType{\tvar{A},\tvar{B}}{
      \variantType{
        \extendRow{Success}{\tvar{A}}{
          \extendRow{Failure}{\tvar{B}}{
            \emptyRow}}}}}\\
  &\typedef{\tsvar{Strategy}}{
    \polyType{\tvar{P}, \tvar{Q}}{
      \recVariantType{\tvar{S}}{
        \extendRow{Strategy}{\tvar{P} \to \instType{\tsvar{Result}}{\tvar{Q}, \tvar{S}}}{
          \emptyRow}}}}\\
  &\letBreak{\var{id}}{\instType{\tsvar{Strategy}}{\tvar{P}, \tvar{P}}}{
      \app{\tag{Strategy}}{(
      \let{\var{\_}}{\typed{\var{x}}{\tvar{P}} \to \variantType{\extendRow{Success}{\tvar{P}}{\rvar{X}}}}{
        \app{\tag{Success}}{\var{x}}}
      \var{\_})}}\\
  &\letBreak{\var{fail}}{\instType{\tsvar{Strategy}}{\tvar{P}, \variantType{\emptyRow}}}{
      \app{\tag{Strategy}}{(
      \let{\var{\_}}{
        \typed{\var{x}}{\tvar{P}} \to
        \variantType{\extendRow{Failure}{\instType{\tsvar{Strategy}}{\tvar{P}, \variantType{\emptyRow}}}{\rvar{X}}}}{
        \app{\tag{Failure}}{\var{fail}}}
      \var{\_})}}\\
  &\letBreak{\var{apply}}{
    \typed{\var{s}}{\instType{\tsvar{Strategy}}{\tvar{P}, \tvar{Q}}} \to 
    \typed{\var{x}}{\tvar{P}} \to \instType{\tsvar{Result}}{\tvar{Q}, \instType{\tsvar{Strategy}}{\tvar{P}, \tvar{Q}}}}{
      \match{\var{s}}{
        \case{\papp{\tag{Strategy}}{\pvar{s}}}{\app{\var{s}}{\var{x}}}
      }}\\
  &\letBreak{\var{seq}}{
    \typed{\var{fs}}{\instType{\tsvar{Strategy}}{\tvar{P}, \tvar{Q}}} \to 
    \typed{\var{ss}}{\instType{\tsvar{Strategy}}{\tvar{Q}, \tvar{R}}} \to 
    \instType{\tsvar{Strategy}}{\tvar{P}, \tvar{R}}}{
      \var{???}}\\
  &\var{???}
  \end{align*}
$$
