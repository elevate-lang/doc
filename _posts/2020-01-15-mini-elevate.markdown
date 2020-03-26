---
layout: post
title:  "Mini-Elevate"
date:   2020-01-24 12:30:00 +0000
---
## Syntax

<details>
<summary>{$\begin{equation*} \begin{split} \tau \mathrel{\mathop:}= \ &\alpha \cmid \tau_1 \to \tau_2 \cmid \{ \rho \} \cmid \langle \rho \rangle \cmid \alpha \textbf{ as } \langle \rho \rangle \cmid \sigma \many{\ \tau_i}{}{i}{0}{m} \ \many{[\rho_i]}{}{i}{0}{n}\end{split} \end{equation*}$}<span style="float:right;">Types</span></summary>
<ul>
<li class="item-description"><span>{$\alpha$}</span><span>Type variable</span></li>
<li class="item-description"><span>{$\tau_1 \to \tau_2$}</span><span>Function type</span></li>
<li class="item-description"><span>{$\{ \rho \}$}</span><span>Record type</span></li>
<li class="item-description"><span>{$\langle \rho \rangle$}</span><span>Variant type</span></li>
<li class="item-description"><span>{$\alpha \textbf{ as } \langle \rho \rangle$}</span><span>Recursive variant type</span></li>
<li class="item-description"><span>{$\sigma \ \tau_1 \dots \tau_m \ [\rho_1] \dots [\rho_n]$}</span><span>Fully applied type scheme</span></li>
</ul>
</details>

<details>
<summary>{$\rho \mathrel{\mathop:}= \beta \cmid \cdot \cmid l: \tau \mid \rho$}<span style="float:right;">Row-types</span></summary>
<ul>
<li class="item-description"><span>{$\beta$}</span><span>Row-type variable</span></li>
<li class="item-description"><span>{$\cdot$}</span><span>Empty field sequence</span></li>
<li class="item-description"><span>{$l: \tau \text{ , } \rho$}</span><span>Extending {$\rho$} with a field labelled as {$l$} of type {$\tau$}</span></li>
</ul>
</details>

<details>
<summary>{$\kappa \mathrel{\mathop:}=\optional{\neg}\{\many{l_i}{,}{i}{0}{n}\}$}<span style="float:right;">Presence</span></summary>
<ul>
<li class="item-description"><span>{$\{ l_1, \dots, l_n \}$}</span><span>A set of labels that a row-type variable must not contain</span></li>
</ul>
</details>

<details>
<summary>{$\sigma \mathrel{\mathop:}= \Sigma \cmid \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\tau$}<span style="float:right;">Type Schemes</span></summary>
<ul>
<li class="item-description"><span>{$\Sigma$}</span><span>Type scheme alias</span></li>
<li class="item-description"><span>{$\forall \ \alpha_1 \dots \alpha_m \ [\beta_1 : \kappa_1] \dots [\beta_n : \kappa_n].\tau$}</span><span>A type {$\tau$} containing type variables {$\{\alpha_1, \dots, \alpha_m\}$} and row-type variables {$\{\beta_1, \dots, \beta_n\}$} bound by the universal quantifiers, where the kinds of row-type variables are respectively given by {$\{\kappa_1, \dots, \kappa_n\}$}</span></li>
</ul>
</details>

<details>
<summary>{$\delta \mathrel{\mathop:}= \pi \cmid \{\many{l_i: \pi_i}{\mid}{i}{0}{n}\}$}<span style="float:right;">Pattern Fields</span></summary>
<ul>
<li class="item-description"><span>{$\pi$}</span><span>pattern</span></li>
<li class="item-description"><span>{$\{l_1: \pi_1 \mid \dots \mid l_n: \pi_n\}$}</span><span>Record pattern</span></li>
</ul>
</details>

<details>
<summary>{$\pi \mathrel{\mathop:}= x \cmid l \ \delta$}<span style="float:right;">Patterns</span></summary>
<ul>
<li class="item-description"><span>{$x$}</span><span>Variable</span></li>
<li class="item-description"><span>{$l \ \delta$}</span><span>Application in patterns</span></li>
</ul>
</details>

<details>
<summary>{$\begin{equation*} \begin{split} e \mathrel{\mathop:}= \ &l \cmid x \cmid e_1 \ e_2 \cmid \mathbf{type} \ \Sigma = \sigma \ \mathbf{in} \ e \ \cmid \\ &\mathbf{let} \ f :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\many{(x_i : \tau_i)\to}{}{i}{0}{p} \tau_t = e_1 \ \mathbf{in} \ e_2 \ \cmid \\ &\lambda :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\many{(x_i : \tau_i)\to}{}{i}{1}{p} \tau_t = e \ \cmid \\ &\{\many{l_i = e_i}{\mid}{i}{0}{n}\} \cmid e.\optional{+}\{\many{l_i = e_i}{\mid}{i}{0}{n}\} \cmid e.l \cmid e.-l \ \cmid \\ &\mathbf{match} \ e \ \mathbf{with} \ \langle\many{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n}\rangle\end{split} \end{equation*}$}<span style="float:right;">Terms</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<hr class = "split">

## Static Semantics

<details>
<summary>{$\Gamma \mathrel{\mathop:}= \cdot \cmid \Gamma, x: \tau$}<span style="float:right;">Typing Context</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<details>
<summary>{$\Delta \mathrel{\mathop:}= \cdot \cmid \Delta, \beta: \kappa$}<span style="float:right;">Presence Context</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<details>
<summary>Type Alias Unfolding</summary>
<details>
<summary>{$\Xi \mathrel{\mathop:}= \cdot \cmid \Xi, \Sigma \mapsto  \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\tau$}<span style="float:right;">Type Alias Context</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>
$$\frac{}{\Xi \vdash l \unfoldRel l}$$
$$\frac{}{\Xi \vdash x \unfoldRel x}$$
$$\frac{\Xi \vdash e_1 \unfoldRel e_3 \quad \Xi \vdash e_2 \unfoldRel e_4}{\Xi \vdash e_1 \ e_2 \unfoldRel e_3 \ e_4}$$
$$\frac{\Xi, \Sigma \mapsto \sigma[\Xi] \vdash e \unfoldRel e_1}{\Xi \vdash \mathbf{type} \ \Sigma = \sigma \ \mathbf{in} \ e \unfoldRel e_1}$$
$$\frac{\Xi \vdash e_1 \unfoldRel e_3 \quad \Xi \vdash e_2 \unfoldRel e_4}{\Xi \vdash \mathbf{let} \ f :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\many{(x_i : \tau_i)\to}{}{i}{0}{p} \tau_t = e_1 \ \mathbf{in} \ e_2 \unfoldRel \\ \quad \mathbf{let} \ f :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\many{(x_i : \tau_i[\Xi])\to}{}{i}{0}{p} \tau_t[\Xi] = e_3 \ \mathbf{in} \ e_4}$$
$$\frac{\Xi \vdash e \unfoldRel e_1}{\Xi \vdash \lambda :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\many{(x_i : \tau_i)\to}{}{i}{1}{p} \tau_t = e \unfoldRel \\ \quad \lambda :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\many{(x_i : \tau_i[\Xi])\to}{}{i}{1}{p} \tau_t[\Xi] = e_1}$$
</details>

<details>
<summary>Type Well-formedness</summary>
$$\frac{}{\Delta \vdash \alpha \wellFormed}$$

$$\frac{\Delta \vdash \tau_1 \wellFormed \quad \Delta \vdash \tau_2 \wellFormed}{\Delta \vdash \tau_1 \to \tau_2 \wellFormed}$$

$$\frac{\Delta \vdash \rho \wellFormed}{\Delta \vdash \{ \rho \} \wellFormed}$$

$$\frac{\Delta \vdash \rho \wellFormed}{\Delta \vdash \langle \rho \rangle \wellFormed}$$

$$\frac{\Delta \vdash \rho \wellFormed}{\Delta \vdash \alpha \textbf{ as } \langle \rho \rangle \wellFormed}$$

$$\frac{}{\Delta \vdash \beta \wellFormed}$$

$$\frac{}{\Delta \vdash \cdot \wellFormed}$$

$$\frac{\Delta \vdash \rho \wellFormed \quad \Delta \vdash \rho \lackRel \psi \quad l \in \psi \quad \Delta \vdash \tau \wellFormed}{\Delta \vdash l: \tau \mid \rho \wellFormed}$$
</details>

<details>
<summary>Lack and Presence Relation</summary>
$$\frac{\beta : \neg\{\many{l_i}{,}{i}{0}{n}\} \in \Delta}{\Delta \vdash \beta \lackRel \{\many{l_i}{,}{i}{0}{n}\}}$$

$$\frac{\beta : \{\many{l_i}{,}{i}{0}{n}\} \in \Delta}{\Delta \vdash \beta \lackRel (\star \setminus \{\many{l_i}{,}{i}{0}{n}\})}$$

$$\frac{}{\Delta \vdash \cdot \lackRel \star}$$

$$\frac{\Delta \vdash \rho \lackRel \psi \quad l \in \psi}{\Delta \vdash (l: \tau \mid \rho) \lackRel (\kappa \setminus l)}$$

$$\frac{\Delta \vdash \rho \lackRel \psi}{\Delta \vdash \rho \presRel (\star \setminus \psi)}$$

<!--
$$\frac{\Delta \vdash \rho_1 \lackRel \kappa_1 \quad \Delta \vdash \rho_2 \lackRel \kappa_2}{\Delta \vdash (\rho_1 \vee \rho_2) \lackRel (\kappa_1 \cup \kappa_2)}$$

$$\frac{\Delta \vdash \rho_1 \lackRel \kappa_1 \quad \Delta \vdash \rho_2 \lackRel \kappa_2}{\Delta \vdash (\rho_1 \wedge \rho_2) \lackRel (\kappa_1 \cap \kappa_2)}$$
-->
</details>

<details>
<summary>Equi-recursive Variant Type</summary>
$$\frac{\Delta;\Gamma \vdash e : \alpha \textbf{ as } \langle \rho \rangle}{\Delta;\Gamma \vdash e : \langle \rho \rangle[\alpha \mapsto \alpha \textbf{ as } \langle \rho \rangle]}$$

$$\frac{\Delta;\Gamma \vdash e : \langle \rho \rangle[\alpha \mapsto \alpha \textbf{ as } \langle \rho \rangle]}{\Delta;\Gamma \vdash e : \alpha \textbf{ as } \langle \rho \rangle}$$
</details>

<details>
<summary>Poset of Rows</summary>
$$\frac{}{\Delta \vdash \cdot \sim \cdot} \ \ruleName{RowInst_{base0}}$$

$$\frac{}{\Delta \vdash \beta_1[\beta_1 \mapsto \cdot] \sim \cdot} \ \ruleName{RowInst_{base1}}$$

$$\frac{\Delta \vdash \beta_1 \presRel \kappa_1 \quad \Delta \vdash \beta_2 \presRel \kappa_2 \quad \kappa_2 \subseteq \kappa_1}{\Delta \vdash \beta_1[\beta_1 \mapsto \beta_2] \sim \beta_2} \ \ruleName{RowInst_{base2}}$$

$$\frac{\Delta \vdash \rho_1[\mathcal{I_1}] \sim \rho_2 \quad \Delta \vdash \tau_1[\mathcal{I_2} \circ \mathcal{I_1}] \sim \tau_2}{\Delta \vdash (l: \tau_1 \mid \rho_1)[\mathcal{I_2} \circ \mathcal{I_1}] \sim (l: \tau_2 \mid \rho_2)} \ \ruleName{RowInst_{ind0}}$$

$$\frac{\Delta \vdash \rho_1[\beta_1 \mapsto \rho_i, \mathcal{I_1}] \sim \rho_2}{\Delta \vdash \rho_1[\beta_1 \mapsto (l: \tau \mid \rho_i), \mathcal{I_1}] \sim (l: \tau \mid \rho_2)} \ \ruleName{RowInst_{ind1}}$$

$$\frac{\Delta \vdash \rho_1[\mathcal{I_1}] \sim \rho_3 \quad \Delta \vdash \rho_2[\mathcal{I_2}] \sim \rho_3}{\Delta \vdash \rho_1 \sqcup \rho_2 \sim \rho_3} \ \ruleName{RowMerge}$$

$$\frac{\forall i. \Delta \vdash \rho_1 \sqcup \rho_2 \sim \rho_i, \rho_3[\mathcal{I_3}] \sim \rho_i}{\Delta \vdash \rho_1 \vee \rho_2 \sim \rho_3} \ \ruleName{RowJoin}$$

$$\textcolor{red}{\frac{\Delta \vdash \forall i. \rho_x[\beta_i \mapsto \rho_i] \sim \rho_y, \rho_i \presRel \kappa_i, \beta_i^{contra} \notin \mathit{frv}(\Delta), \rho_x[\beta_i \mapsto \beta_i^{contra}] \sim \rho_z}{\Delta \vdash \forall i. \Delta, \beta_i^{contra} : \kappa_i \vdash \rho_x \contraRel \rho_z} \ \ruleName{RowContra}}$$
</details>

<details>
<summary>Pattern Typing</summary>
$$\frac{\alpha \notin \mathit{ftv}(\Gamma)}{\Delta;\Gamma \ptWith x \ptRel \Delta;\Gamma, x: \alpha \ptWith x: \alpha}$$
$$\frac{\Delta;\Gamma \ptWith \delta \ptRel \Delta_1;\Gamma_1 \ptWith \delta: \tau \quad \beta \notin \mathit{frv}(\Delta_1)}{\Delta;\Gamma \ptWith l \ \delta \ptRel \Delta_1, \beta: \neg\{l\};\Gamma_1 \ptWith l \ \delta: \langle l: \tau \mid \beta \rangle}$$
$$\frac{}{\Delta;\Gamma \ptWith \{\} \ptRel \Delta;\Gamma \ptWith \{\}: \{\cdot\}}$$
$$\frac{\Delta;\Gamma \ptWith \pi \ptRel \Delta_1;\Gamma_1 \ptWith \pi: \tau \quad \beta \notin \mathit{frv}(\Delta_1)}{\Delta;\Gamma \ptWith \{l: \pi\} \ptRel \Delta_1, \beta: \neg\{l\};\Gamma_1 \ptWith \{l: \pi\}: \{l: \tau \mid \beta\}}$$
$$\frac{\Delta;\Gamma \ptWith \{\many{l_i: \pi_i}{\mid}{i}{1}{n}\} \ptRel \Delta_1, \beta: \neg\{\many{l_i}{,}{i}{1}{n}\};\Gamma_1 \ptWith \{\many{l_i: \pi_i}{\mid}{i}{1}{n}\}: \{\many{l_i: \tau_i}{\mid}{i}{1}{n} \mid \beta\} \\ \Delta_1, \beta: \neg\{\many{l_i}{,}{i}{1}{n}\};\Gamma_1 \ptWith \pi \ptRel \Delta_2, \beta: \neg\{\many{l_i}{,}{i}{1}{n}\};\Gamma_2 \ptWith \pi: \tau}{\Delta;\Gamma \ptWith \{\many{l_i: \pi_i}{\mid}{i}{1}{n} \mid l: \pi\} \ptRel \\ \quad \Delta_2, \beta: \neg\{\many{l_i}{,}{i}{1}{n}, l\};\Gamma_2 \ptWith \{\many{l_i: \pi_i}{\mid}{i}{1}{n} \mid l: \pi\}: \{\many{l_i: \tau_i}{\mid}{i}{1}{n} \mid l: \tau \mid \beta\}}$$
<!--
$$\frac{\Delta;\Gamma \vdash \delta : \tau}{\Delta;\Gamma \vdash \{\many{l_i: \pi_i}{\mid}{i}{0}{n}\}:\{\many{l_i: \pi_i}{\mid}{i}{0}{n} \mid \beta \}}$$
-->
</details>

<details>
<summary>Term Typing</summary>
$$\frac{x: \tau\in\Gamma}{\Delta;\Gamma \vdash x: \tau} \ \ruleName{Var}$$

$$\frac{}{\Delta;\Gamma \vdash l: \forall \ \alpha \ [\beta: \neg\{l\}]. \langle l: \alpha \mid \beta\rangle} \ \ruleName{Label}$$

$$\frac{\Delta;\Gamma \vdash f: \tau_1 \to \tau_2 \quad \Delta;\Gamma \vdash e: \tau_1}{\Delta;\Gamma \vdash f \ e: \tau_2} \ \ruleName{App}$$

$$\frac{\Delta;\Gamma \vdash e: \{\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_1\} \\ \beta_2 \notin \mathit{frv}(\Delta) \quad \Delta, \beta_2 : \neg \{\many{l_j}{,}{j}{0}{n}\};\Gamma \vdash \{\many{l_j = e_j}{\mid}{j}{0}{n}\} : \{\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \beta_2\} \\ \psi_2 = \{\many{l_j}{,}{j}{0}{n}\} \quad \Delta \vdash \beta_1 \# \psi_1 \quad \psi_1 \cap \psi_2 = \emptyset \\ \Delta, \beta_2 : \neg \{\many{l_j}{,}{j}{0}{n}\} \vdash \\ \quad (\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_1) \vee  (\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \beta_2) \sim (\many{l_k: \tau_k}{\mid}{k}{0}{m+n} \mid \beta_3)}{\Delta;\Gamma \vdash e.+\{\many{l_j = e_j}{\mid}{j}{0}{n}\} : \{\many{l_k: \tau_k}{\mid}{k}{0}{m+n} \mid \beta_3\}} \ \ruleName{Record Extension}$$

$$\frac{\Delta;\Gamma \vdash e: \{\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_1\} \\ \Delta;\Gamma \vdash \{\many{l_j = e_j}{\mid}{j}{0}{n}\} : \{\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \cdot\} \\ \Delta \vdash (\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \cdot) \presRel \kappa_2 \quad \Delta \vdash (\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_1) \presRel \kappa_1 \quad \kappa_1 \cap \kappa_2 = \emptyset \\ \beta_2, \beta_3 \notin \mathit{frv}(\Delta) \\ \Delta, \beta_2: \neg \{\many{l_j}{,}{j}{0}{n}\}, \beta_3: \neg \{\many{l_i}{,}{i}{0}{m}\} \vdash \\ \quad (\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_3) \sqcup (\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \beta_2) \sim (\many{l_k: \tau_k}{\mid}{k}{0}{m+n} \mid \beta_1)}{\Delta;\Gamma \vdash e.+\{\many{l_j = e_j}{\mid}{j}{0}{n}\} : \{\many{l_k: \tau_k}{\mid}{k}{0}{m+n} \mid \beta_1\}} \ \ruleName{Record Extension}$$

$$\frac{\Delta;\Gamma \vdash e: \langle\cdot\rangle}{\Delta;\Gamma \vdash \mathbf{match} \ e \ \mathbf{with} \ \langle\rangle : \forall \alpha. \alpha}$$

$$\frac{\Delta;\Gamma \vdash e: \langle\rho\rangle \\ \forall i \in [0, n+1], \\ \quad \Delta;\Gamma \ptWith \pi_i \ptRel \Delta_i;\Gamma_i \ptWith \pi_i : \langle\rho_i\rangle \\ \quad \Delta_i \vdash \rho_i[\mathcal{I}_i] \sim \rho \\ \quad \Delta_i;\Gamma_i[\mathcal{I_i}] \vdash e_i: \tau}{\Delta;\Gamma \vdash \mathbf{match} \ e \ \mathbf{with} \ \langle\many{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n+1}\rangle : \tau}$$
</details>