---
layout: post
title:  "Mini-Elevate"
date:   2020-01-15 21:42:38 +0000
---

<details>
<summary>{$\tau \mathrel{\mathop:}= \alpha \mid \textbf{Nat} \mid \textbf{Bool} \mid \tau_1 \to \tau_2 \mid \llbrace \rho \rrbrace \mid \langle \rho \rangle \mid \alpha \textbf{ as } \sigma \mid \langle \rho \rangle \hookrightarrow \tau$}<span style="float:right;">Types</span></summary>
<ul>
<li class="item-description"><span>{$\alpha$}</span><span>Type variable</span></li>
<li class="item-description"><span>{$\textbf{Nat}$}</span><span>Natural number type</span></li>
<li class="item-description"><span>{$\textbf{Bool}$}</span><span>Boolean type</span></li>
<li class="item-description"><span>{$\tau_1 \to \tau_2$}</span><span>Function type</span></li>
<li class="item-description"><span>{$\llbrace \rho \rrbrace$}</span><span>Record type</span></li>
<li class="item-description"><span>{$\langle \rho \rangle$}</span><span>Variant type</span></li>
<li class="item-description"><span>{$\alpha \textbf{ as } \sigma$}</span><span>Recursive type scheme</span></li>
<li class="item-description"><span>{$\langle \rho \rangle \hookrightarrow \tau$}</span><span>Case type</span></li>
</ul>
</details>

<details>
<summary>{$\rho \mathrel{\mathop:}= \beta \mid \# \mid l: \tau \text{ , } \rho$}<span style="float:right;">Row-types</span></summary>
<ul>
<li class="item-description"><span>{$\beta$}</span><span>Row-type variable</span></li>
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
<summary>{$\sigma \mathrel{\mathop:}= \forall(\alpha_1, \dots, \alpha_m).\forall(\beta_1 : \kappa_1, \dots, \beta_n : \kappa_n).\tau$}<span style="float:right;">Type Schemes</span></summary>
<ul>
<li class="item-description"><span>{$\forall(\alpha_1, \dots, \alpha_m).\forall(\beta_1 : \kappa_1, \dots, \beta_n : \kappa_n).\tau$}</span><span>A type {$\tau$} containing type variables {$\{\alpha_1, \dots, \alpha_m\}$} and row-type variables {$\{\beta_1, \dots, \beta_n\}$} bound by the universal quantifiers, where the kinds of row-type variables are respectively given by {$\{\kappa_1, \dots, \kappa_n\}$}</span></li>
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
<summary>{$\begin{equation*} \begin{split} t \mathrel{\mathop:}= \ &\iota \mid l \mid \mathbf{def} \ f (\pi_1 : \tau_1, \dots, \pi_n : \tau_n) : \tau_t = t \mid t_1 \ t_2 \mid \mathbf{let} \ \pi : \tau = t_1 \ \mathbf{in} \ t_2 \ \mid \\ &\llbrace l_i = t_i\rrbrace^n_{i = 1} \mid t_1 \otimes l = t_2 \mid t \oslash l \ \mid \\ &\langle\pi_i \Rightarrow t_i\rangle^n_{i = 1} \mid t_1 \oplus \pi \Rightarrow t_2 \mid t \ominus l \ \mid \\ &t.l \mid \mathbf{match} \ t_1 \ \mathbf{with} \ t_2\end{split} \end{equation*}$}<span style="float:right;">Terms</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>
