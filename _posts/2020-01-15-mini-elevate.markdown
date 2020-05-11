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
<summary>{$\kappa \mathrel{\mathop:}=\optional{\neg}\{\many{l_i}{,}{i}{0}{n}\}$}<span style="float:right;">Presences</span></summary>
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

<!--
<details>
<summary>{$\delta \mathrel{\mathop:}= \pi \cmid \{\many{l_i: \delta_i}{\mid}{i}{0}{n}\}$}<span style="float:right;">Pattern Fields</span></summary>
<ul>
<li class="item-description"><span>{$\pi$}</span><span>pattern</span></li>
<li class="item-description"><span>{$\{l_1: \pi_1 \mid \dots \mid l_n: \pi_n\}$}</span><span>Record pattern</span></li>
</ul>
</details>
-->

<details>
<summary>{$\pi \mathrel{\mathop:}= x \cmid l \ \optional{\pi} \cmid \{\many{l_i: \pi_i}{\mid}{i}{0}{n}\}$}<span style="float:right;">Patterns</span></summary>
<ul>
<li class="item-description"><span>{$x$}</span><span>Variable</span></li>
<li class="item-description"><span>{$l \ \pi$}</span><span>Application in patterns</span></li>
</ul>
</details>

<details>
<summary>{$\begin{equation*} \begin{split} e \mathrel{\mathop:}= \ &l \cmid x \cmid e_1 \ e_2 \cmid \mathbf{type} \ \Sigma = \sigma \ \mathbf{in} \ e \ \cmid \\ &\mathbf{let} \ f :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\many{(x_i : \tau_i)\to}{}{i}{0}{p} \tau_t = e_1 \ \mathbf{in} \ e_2 \ \cmid \\ &\lambda :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\some{(x_i : \tau_i)\to}{}{i}{0}{p} \tau_t = e \ \cmid \\ &\{\many{l_i = e_i}{\mid}{i}{0}{n}\} \cmid e.\optional{+}\{\many{l_i = e_i}{\mid}{i}{0}{n}\} \cmid e.l \cmid e.-l \ \cmid \\ &\mathbf{match} \ e \ \mathbf{with} \ \langle\many{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n}\rangle\end{split} \end{equation*}$}<span style="float:right;">Terms</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<hr class = "split">

## Static Semantics

<details>
<summary>{$\epsilon \mathrel{\mathop:}= \mathbb{T} \cmid \mathbb{R}$}<span style="float:right;">Kinds</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<details>
<summary>{$\Gamma \mathrel{\mathop:}= \cdot \cmid \Gamma, x: \tau$}<span style="float:right;">Typing Contexts</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<details>
<summary>{$\Delta \mathrel{\mathop:}= \cdot \cmid \Delta, \beta: \kappa$}<span style="float:right;">Presence Contexts</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<details>
<summary>{$\Theta \mathrel{\mathop:}= \cdot \cmid \Theta, x$}<span style="float:right;">Pattern Binding Contexts</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<details>
<summary>Type Alias Unfolding</summary>
<details class = "inner">
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
$$\frac{\Xi \vdash e \unfoldRel e_1}{\Xi \vdash \lambda :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\some{(x_i : \tau_i)\to}{}{i}{0}{p} \tau_t = e \unfoldRel \\ \quad \lambda :\!\!\optional{!}\ \optional{\forall \ \many{\alpha_i}{}{i}{0}{m} \ \many{[\beta_i : \kappa_i]}{}{i}{0}{n}.}\some{(x_i : \tau_i[\Xi])\to}{}{i}{0}{p} \tau_t[\Xi] = e_1}$$
</details>

<details>
<summary>Function Desugaring</summary>
</details>

<details>
<summary>Type Well-formedness and Kinding</summary>
$$\frac{}{\Delta \vdash \alpha \ofKind{T}}$$

$$\frac{\Delta \vdash \tau_1 \ofKind{T} \quad \Delta \vdash \tau_2 \ofKind{T}}{\Delta \vdash \tau_1 \to \tau_2 \ofKind{T}}$$

$$\frac{\Delta \vdash \rho \ofKind{R}}{\Delta \vdash \{ \rho \} \ofKind{T}}$$

$$\frac{\Delta \vdash \rho \ofKind{R}}{\Delta \vdash \langle \rho \rangle \ofKind{T}}$$

$$\frac{\Delta \vdash \rho \ofKind{R}}{\Delta \vdash \alpha \textbf{ as } \langle \rho \rangle \ofKind{T}}$$

$$\frac{}{\Delta \vdash \beta \ofKind{R}}$$

$$\frac{}{\Delta \vdash \cdot \ofKind{R}}$$

$$\frac{\Delta \vdash \rho \ofKind{R} \quad \Delta \vdash \rho \lackRel \psi \quad l \in \psi \quad \Delta \vdash \tau \ofKind{T}}{\Delta \vdash l: \tau \mid \rho \ofKind{R}}$$
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

$$\textcolor{red}{\frac{\Delta \vdash \forall i. \rho_x[\beta_i \mapsto \rho_i] \sim \rho_y, \rho_i \presRel \kappa_i, \beta_i^{contra} \notin \Delta, \rho_x[\beta_i \mapsto \beta_i^{contra}] \sim \rho_z}{\Delta \vdash \forall i. \Delta, \beta_i^{contra} : \kappa_i \vdash \rho_x \contraRel \rho_z} \ \ruleName{RowContra}}$$
</details>

<details>
<summary>Poset of Types</summary>
</details>

<details>
<summary>Pattern Typing</summary>
$$\frac{\alpha \notin \mathit{ftv}(\Gamma) \quad x \notin \Theta}{\Delta;\Gamma;\Theta \ptWith x: \alpha \ptRel \Delta;\Gamma, x: \alpha; \Theta, x}$$
$$\frac{\Delta;\Gamma;\Theta \ptWith \pi: \tau \ptRel \Delta_1;\Gamma_1;\Theta_1 \quad \beta \notin \Delta_1}{\Delta;\Gamma;\Theta \ptWith l \ \pi: \langle l: \tau \mid \beta \rangle \ptRel \Delta_1, \beta: \neg\{l\};\Gamma_1;\Theta_1}$$
$$\frac{\beta \notin \Delta}{\Delta;\Gamma;\Theta \ptWith l: \langle l: \{\cdot\} \mid \beta \rangle \ptRel \Delta, \beta: \neg\{l\};\Gamma;\Theta}$$
$$\frac{\beta \notin \Delta}{\Delta;\Gamma;\Theta \ptWith \{\}: \{\beta\} \ptRel \Delta, \beta: \neg\{\};\Gamma;\Theta}$$
$$\frac{\Delta;\Gamma;\Theta \ptWith \pi: \tau \ptRel \Delta_1;\Gamma_1;\Theta_1 \quad \beta \notin \Delta_1}{\Delta;\Gamma;\Theta \ptWith \{l: \pi\}: \{l: \tau \mid \beta\} \ptRel \Delta_1, \beta: \neg\{l\};\Gamma_1;\Theta_1}$$
$$\frac{\Delta;\Gamma;\Theta \ptWith \{\some{l_i: \pi_i}{\mid}{i}{0}{n}\}: \{\some{l_i: \tau_i}{\mid}{i}{0}{n} \mid \beta\} \ptRel \Delta_1, \beta: \neg\{\some{l_i}{,}{i}{0}{n}\};\Gamma_1;\Theta_1 \\ \Delta_1, \beta: \neg\{\some{l_i}{,}{i}{0}{n}\};\Gamma_1;\Theta_1 \ptWith \pi: \tau \ptRel \Delta_2, \beta: \neg\{\some{l_i}{,}{i}{0}{n}\};\Gamma_2;\Theta_2}{\Delta;\Gamma;\Theta \ptWith \\ \quad \{\some{l_i: \pi_i}{\mid}{i}{0}{n} \mid l: \pi\}: \{\some{l_i: \tau_i}{\mid}{i}{0}{n} \mid l: \tau \mid \beta\} \ptRel \Delta_2, \beta: \neg\{\some{l_i}{,}{i}{0}{n}, l\};\Gamma_2;\Theta_2}$$
<!--
$$\frac{\Delta;\Gamma \vdash \delta : \tau}{\Delta;\Gamma \vdash \{\many{l_i: \pi_i}{\mid}{i}{0}{n}\}:\{\many{l_i: \pi_i}{\mid}{i}{0}{n} \mid \beta \}}$$
-->
</details>

<!--
<details>
<summary>Pattern Elaboration</summary>
<details class = "inner">
<summary>{$\tilde{\pi} \mathrel{\mathop:}= x \cmid l \ \optional{\tilde{\pi}}$}<span style="float:right;">Elaborated Patterns</span></summary>
</details>
<details class = "inner">
<summary>Preoder Set of Elaborated Patterns</summary>
$$\frac{}{x[x \mapsto y] \ptpre y}$$
$$\frac{}{l[] \ptpre l}$$
$$\frac{\tilde{\pi}_1[\mathcal{S}] \ptpre \tilde{\pi}_2}{l \ \tilde{\pi}_1[\mathcal{S}] \ptpre l \ \tilde{\pi}_2}$$
</details>
<details class = "inner">
<summary>Record Pattern Elimination</summary>
<details class = "inner">
<summary>{$R \mathrel{\mathop:}= \cdot \cmid R, l$}<span style="float:right;">Label Occurrence Context</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>
<details class = "inner">
<summary>{$E \mathrel{\mathop:}= \cdot \cmid E, (x\many{.l_i}{}{i}{0}{n}, \pi)$}<span style="float:right;">Record Pattern Expansion</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>

<br/>

<p style="text-align: center;">{$E$} is always implcitly sorted by a dictionary order {$\sqsubseteq$} of {$x\many{.l_i}{}{i}{0}{n}$}.</p>

$$\frac{v \ \mathbf{fresh}}{E;R \vdash \{\} \rpeRel{x} E, (x, v); R}$$
$$\frac{l \notin R}{E;R \vdash \{l : \pi\} \rpeRel{x} E, (x.l, \pi);R, l}$$
$$\frac{l \notin R \quad y \ \mathbf{fresh} \quad \cdot;\cdot \vdash \{\some{l_i: \delta_i}{\mid}{i}{0}{n}\} \rpeRel{y} E_1;R_1}{E;R \vdash \{l : \{\many{l_i: \delta_i}{\mid}{i}{0}{n}\}\} \rpeRel{x} E_1[y \mapsto x.l]; R, l}$$
$$\frac{E;R \vdash \{l: \delta\} \rpeRel{x} E_1;R_1 \quad E_1;R_1 \vdash \{\some{l_i: \delta_i}{\mid}{i}{0}{n}\} \rpeRel{x} E_2;R_2}{E;R \vdash \{l: \delta \mid \some{l_i: \delta_i}{\mid}{i}{0}{n}\} \rpeRel{x} E_2;R_2}$$
</details>
<details class = "inner">
<summary>{$\begin{equation*} \begin{split} \varpi \mathrel{\mathop:}= \tilde{\pi} \Rightarrow_\cdot e \cmid \tilde{\pi} \ \some{\Rightarrow_k \mathbf{match} \ x\many{.l^k_i}{}{i}{0}{n} \ \mathbf{with} \ \langle\some{\varpi^k_j}{\mid}{j}{0}{m}\rangle}{;}{k}{0}{p} \end{split} \end{equation*}$}<span style="float:right;">Pattern Cluster</span></summary>
</details>
<details class = "inner">
<summary>Pattern Cluster Generation</summary>
$$\frac{}{\tilde{\pi} \Rightarrow rhs \expRel \tilde{\pi} \Rightarrow_\cdot rhs}$$

<p style="text-align: center;">{$\elabph$} is a non-syntactic placeholder</p>

$$\frac{x \ \mathbf{fresh} \quad \cdot; \cdot \vdash \{\many{l_j: \delta_j}{\mid}{j}{0}{m}\} \rpeRel{x} E; R \\ \mathbf{size}(E) = p \\ \forall k \in [0, p - 2], \mathbf{snd}(E(k)) \Rightarrow_k \elabph \expRel \varpi^k_0 \\ \varpi^{p - 1}_0 = \mathbf{snd}(E(p - 1)) \Rightarrow_{p - 1} rhs}{\some{l_i \ }{}{i}{0}{n} \{\many{l_j: \delta_j}{\mid}{j}{0}{m}\} \Rightarrow rhs \expRel \\ \quad \some{l_i \ }{}{i}{0}{n} x \ \some{\Rightarrow_k \mathbf{match} \ \mathbf{fst}(E(k)) \ \mathbf{with} \ \langle\varpi^k_0\rangle}{;}{k}{0}{p - 1}}$$
</details>
<details class = "inner">
<summary>Pattern Cluster Merging</summary>
$$\frac{\tilde{\pi}_2[\mathcal{S}] \ptpre \tilde{\pi}_1}{\mergeRel{\tilde{\pi}_1 \Rightarrow_\cdot \elabph}{\tilde{\pi}_2 \Rightarrow_\cdot \elabph}{\tilde{\pi}_1 \Rightarrow_\cdot \elabph}}$$
$$\frac{}{\mergeRel{\tilde{\pi}_1 \Rightarrow_\cdot rhs_1}{\tilde{\pi}_2 \Rightarrow_\cdot rhs_2}{(\tilde{\pi}_1 \Rightarrow_\cdot rhs_1 \mid \tilde{\pi}_2 \Rightarrow_\cdot rhs_2)}}$$
$$\frac{\tilde{\pi}_2[\mathcal{S}] \ptpre \tilde{\pi}_1 \\ \forall \ k \in [0, p], \mergeRel{\some{\varpi^k_j}{\mid}{j}{0}{m}}{(x \Rightarrow_\cdot rhs[\mathcal{S}])}{\some{\varpi^k_s}{\mid}{s}{0}{q}}}{\mergeRel{\tilde{\pi}_1 \ \some{\Rightarrow_k \mathbf{match} \ x\many{.l^k_i}{}{i}{0}{n} \ \mathbf{with} \ \langle\some{\varpi^k_j}{\mid}{j}{0}{m}\rangle}{;}{k}{0}{p}}{(\tilde{\pi}_2 \Rightarrow_\cdot rhs)}{\\ \quad \tilde{\pi}_1 \ \some{\Rightarrow_k \mathbf{match} \ x\many{.l^k_i}{}{i}{0}{n} \ \mathbf{with} \ \langle \some{\varpi^k_s}{\mid}{s}{0}{q} \rangle}{;}{k}{0}{p}}}$$
</details>
</details>
-->

<details>
<summary>Pattern Elaboration<span style="float:right;">{$\mathbf{match} \ e \ \mathbf{with} \ \langle\some{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n}\rangle \elabRel \varpi$}</span></summary>
<details class = "inner">
<summary>{$\tilde{\pi} \mathrel{\mathop:}= x \cmid l \ \optional{x}$}<span style="float:right;">Elaborated Patterns</span></summary>
</details>
<details class = "inner">
<summary>{$\delta \mathrel{\mathop:}= x\optional{.l}\optional{.\{\}}$}<span style="float:right;">Field-access Forms</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>
<details class = "inner">
<summary>{$\ell \mathrel{\mathop:}= \some{\mathbb{N}_i}{\mid}{i}{0}{n}$}<span style="float:right;">Match IDs</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>
<details class = "inner">
<summary>{$\varpi \mathrel{\mathop:}= e \cmid \mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle\some{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n}\rangle$}<span style="float:right;">Match Chain</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>
<details class = "inner">
<summary>{$R \mathrel{\mathop:}= \cdot \cmid R, l$}<span style="float:right;">Label Occurrence Context</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>
<details class = "inner">
<summary>{$\varsigma \mathrel{\mathop:}= \ \varPat \cmid \nvarPat$}<span style="float:right;">Pattern Property</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>
<details class = "inner">
<summary>Pattern Expansion<span style="float:right;">{$R \vdash^{\ell}_{x} \mathbf{match} \ \delta \ \mathbf{with} \ \langle \pi \Rightarrow e \rangle \expRel \varpi$}</span></summary>
$$\frac{}{R \vdash^{\ell}_{x} \mathbf{match} \ \delta \ \mathbf{with} \ \langle v \Rightarrow e \rangle \expRel \mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle v \Rightarrow e \rangle}$$

$$\frac{}{R \vdash^{\ell}_{x} \mathbf{match} \ \delta \ \mathbf{with} \ \langle l \Rightarrow e \rangle \expRel \mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle l \Rightarrow e \rangle}$$

$$\frac{}{R \vdash^{\ell}_{x} \mathbf{match} \ \delta \ \mathbf{with} \ \langle l \ v \Rightarrow e \rangle \expRel \mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle l \ v \Rightarrow e \rangle}$$

$$\frac{v \ \mathbf{fresh} \quad \cdot \vdash^{\ell \mid 0}_{v} \mathbf{match} \ v \ \mathbf{with} \ \langle \pi \Rightarrow e \rangle \expRel \varpi}{R \vdash^{\ell}_{x} \mathbf{match} \ \delta \ \mathbf{with} \ \langle l \ \pi \Rightarrow e \rangle \expRel \mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle l \ v \Rightarrow \varpi \rangle}$$

$$\frac{v \ \mathbf{fresh}}{R \vdash^{\ell}_{x} \mathbf{match} \ \delta \ \mathbf{with} \ \langle \{\} \Rightarrow e \rangle \expRel \mathbf{match}^{\ell} \ \delta.\{\} \ \mathbf{with} \ \langle v \Rightarrow e \rangle}$$

$$\frac{l \notin R \\ R \vdash^{\ell}_{v} \mathbf{match} \ v.l \ \mathbf{with} \ \langle \pi \Rightarrow e \rangle \expRel \varpi}{R \vdash^{\ell}_{v} \mathbf{match} \ v \ \mathbf{with} \ \langle \{l: \pi\} \Rightarrow e \rangle \expRel \varpi}$$

$$\frac{l \notin R \\ R,l \vdash^{\ell \mid (n + 1)}_{v} \mathbf{match} \ v \ \mathbf{with} \ \langle \{\some{l_i: \pi_i}{\mid}{i}{0}{n}\} \Rightarrow e \rangle \expRel \varpi_1 \\ R \vdash^{\ell \mid n}_{v} \mathbf{match} \ v.l \ \mathbf{with} \ \langle \pi \Rightarrow \varpi_1 \rangle \expRel \varpi_2}{R \vdash^{\ell \mid n}_{v} \mathbf{match} \ v \ \mathbf{with} \ \langle \{l: \pi \mid \some{l_i: \pi_i}{\mid}{i}{0}{n}\} \Rightarrow e \rangle \expRel \varpi_2}$$

$$\frac{v \ \mathbf{fresh} \\ \cdot \vdash^{\ell \mid 0}_{v} \mathbf{match} \ v \ \mathbf{with} \ \langle \{\some{l_i: \pi_i}{\mid}{i}{0}{n}\} \Rightarrow e \rangle \expRel \varpi}{R \vdash^{\ell}_{x} \mathbf{match} \ \delta \ \mathbf{with} \ \langle \{\some{l_i: \pi_i}{\mid}{i}{0}{n}\} \Rightarrow e \rangle \expRel \mathbf{match}^{\ell} \ \delta.\{\} \ \mathbf{with} \ \langle v \Rightarrow \varpi \rangle}$$
</details>
<details class = "inner">
<summary>Prefix Relation of Match IDs<span style="float:right;">{$\ell \prefRel \ell$}</span></summary>
$$\frac{n \in \mathbb{N}}{n \prefRel n \mid \ell}$$
$$\frac{\ell_1 \prefRel \ell_2 \quad n \in \mathbb{N}}{n \mid \ell_1 \prefRel n \mid \ell_2}$$
</details>
<details class = "inner">
<summary>Pattern Property Judgement<span style="float:right;">{$\tilde{\pi} \ofProp \ \varsigma$}</span></summary>
$$\frac{}{x \ofProp \varPat}$$
$$\frac{}{l \ofProp \nvarPat}$$
$$\frac{}{l \ x \ofProp \nvarPat}$$
</details>
<details class = "inner">
<summary>Match Chain Sorting<span style="float:right;">{$\varpi \sortRel \varpi$}</span></summary>
<details class = "inner">
<summary>Match Chain Tagging<span style="float:right;">{$\varpi \tagRel (\mathbb{N}, {}^{\varsigma}\varpi)$}</span></summary>

$$\frac{}{e \tagRel (0, {}^{\varPat}e)}$$

$$\frac{\tilde{\pi} \ofProp \nvarPat}{\mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow e \rangle \tagRel (0, {}^{\nvarPat}\mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow {}^{\varPat}e \rangle)}$$

$$\frac{\tilde{\pi} \ofProp \varPat}{\mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow e \rangle \tagRel (\mathbf{size} \ \ell - 1, {}^{\varPat}\mathbf{match}^{\ell} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow {}^{\varPat}e \rangle)}$$

$$\frac{\tilde{\pi}_1 \ofProp \nvarPat \\ \mathbf{match}^{\ell_2} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \tagRel (c, {}^{\varsigma}\varpi_1)}{\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow \mathbf{match}^{\ell_2} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \rangle \tagRel \\ (0, {}^{\nvarPat}\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow {}^{\varsigma}\varpi_1 \rangle)}$$

$$\frac{\mathbf{match}^{\ell_1 \mid n} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \tagRel (0, {}^{\varsigma}\varpi_1)}{\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow \mathbf{match}^{\ell_1 \mid n} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \rangle \tagRel \\ (0, {}^{\nvarPat}\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow {}^{\varsigma}\varpi_1 \rangle)}$$

$$\frac{\tilde{\pi}_1 \ofProp \varPat \\ \mathbf{match}^{\ell_1 \mid n} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \tagRel (c, {}^{\varsigma}\varpi_1)}{\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow \mathbf{match}^{\ell_1 \mid n} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \rangle \tagRel \\ (c - 1, {}^{\varPat}\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow {}^{\varsigma}\varpi_1 \rangle)}$$

$$\frac{\tilde{\pi}_1 \ofProp \varPat \\ \mathbf{match}^{\ell_1 \mid n_2} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \tagRel (c, {}^{\varsigma}\varpi_1)}{\mathbf{match}^{\ell_1 \mid n_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow \mathbf{match}^{\ell_1 \mid n_2} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \rangle \tagRel \\ (c, {}^{\varPat}\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow {}^{\varsigma}\varpi_1 \rangle)}$$

$$\frac{\tilde{\pi}_1 \ofProp \varPat \\ \mathbf{match}^{\ell_2} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \tagRel (c, {}^{\varsigma}\varpi_1)}{\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow \mathbf{match}^{\ell_2} \ \delta_2 \ \mathbf{with} \ \langle \tilde{\pi}_2 \Rightarrow \varpi \rangle \rangle \tagRel \\ (c + \mathbf{size} \ \ell_1 - \mathbf{size} \ \ell_2, {}^{\varPat}\mathbf{match}^{\ell_1} \ \delta_1 \ \mathbf{with} \ \langle \tilde{\pi}_1 \Rightarrow {}^{\varsigma}\varpi_1 \rangle)}$$
</details>
<details class = "inner">
<summary>Match Chain Grouping<span style="float:right;">{$({}^{\varsigma}\varpi, \varpi) \grpRel \varpi$}</span></summary>
<details class = "inner">
<summary>Match Chain Reversing<span style="float:right;">{$(\varpi, \varpi) \revRel \varpi$}</span></summary>
$$\frac{}{(e, \varpi_{accum}) \revRel \varpi_{accum}}$$
$$\frac{(\varpi, \mathbf{match} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow \varpi_{accum} \rangle) \revRel \varpi_1}{(\mathbf{match} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow \varpi \rangle, \varpi_{accum}) \revRel \varpi_1}$$
</details>
$$\frac{(\varpi_{\varPat}, e) \revRel \varpi}{({}^{\varPat}e, \varpi_{\varPat}) \grpRel \varpi}$$
$$\frac{({}^{\varsigma}\varpi, \mathbf{match} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow \varpi_{\varPat} \rangle) \grpRel \varpi_1}{({}^{\varPat}\mathbf{match} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow {}^{\varsigma}\varpi \rangle, \varpi_{\varPat}) \grpRel \varpi_1}$$
$$\frac{({}^{\varsigma}\varpi, \varpi_{\varPat}) \grpRel \varpi_1}{({}^{\nvarPat}\mathbf{match} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow {}^{\varsigma}\varpi \rangle, \varpi_{\varPat}) \grpRel \mathbf{match} \ \delta \ \mathbf{with} \ \langle \tilde{\pi} \Rightarrow \varpi_1 \rangle}$$
</details>
$$\frac{\varpi \tagRel (c, {}^{\varsigma}\varpi_{tagged}) \quad ({}^{\varsigma}\varpi_{tagged}, \{\}) \grpRel \varpi_{sorted}}{\varpi \sortRel \varpi_{sorted}}$$
</details>
<details class = "inner">
<summary>Variable Substitution in Elaborated Patterns<span style="float:right;">{$\tilde{\pi}[\mathcal{S}] \ptpre \tilde{\pi}$}</span></summary>
$$\frac{}{x[x \mapsto y] \ptpre y}$$
$$\frac{}{l[] \ptpre l}$$
$$\frac{x_1[\mathcal{S}] \ptpre x_2}{l \ x_1[\mathcal{S}] \ptpre l \ x_2}$$
</details>
<details class = "inner">
<summary>Field-access Form Merging<span style="float:right;">{$\mergeRel{\delta}{\delta}{\delta}$}</span></summary>
$$\frac{}{\mergeRel{x}{x}{x}}$$
$$\frac{}{\mergeRel{x.l}{x.l}{x.l}}$$
$$\frac{}{\mergeRel{x.\{\}}{x.\{\}}{x.\{\}}}$$
$$\frac{}{\mergeRel{x.\{\}}{x}{x.\{\}}}$$
$$\frac{}{\mergeRel{x}{x.\{\}}{x.\{\}}}$$
$$\frac{}{\mergeRel{x.l.\{\}}{x.l}{x.l.\{\}}}$$
$$\frac{}{\mergeRel{x.l}{x.l.\{\}}{x.l.\{\}}}$$
<p style="text-align: center;">Field-access forms cannot be merged otherwise.</p>
</details>
<details class = "inner">
<summary>Elaborated Patterns to Terms<span style="float:right;">{$\tilde{\pi} \pteRel e$}</span></summary>
$$\frac{}{x \pteRel x}$$
$$\frac{}{l \pteRel l}$$
$$\frac{}{l \ x \pteRel l \ x}$$
$$\frac{}{(x \mapsto y) \pteRel (x \mapsto y)}$$
</details>
<details class = "inner">
<summary>Match Chain Merging<span style="float:right;">{$\mergeRel{\varpi}{\varpi}{\varpi}$}</span></summary>
<br/>
<p style="text-align: center;">In {$\mergeRel{\varpi_a}{\varpi_b}{\varpi_c}$}, the first element of {$\varpi_b$} is lifted to the head of the chain according the underlined premises. If multiple candidates are available, the first one will be chosen.</p>

$$\frac{\keyCond{\mathbf{size} \ \ell_a \sim \mathbf{size} \ \ell_b} \quad \keyCond{\mergeRel{\delta_a}{\delta_b}{\delta'}} \\ \keyCond{\tilde{\pi}_b \ofProp \varPat} \quad \keyCond{\tilde{\pi}_b[\mathcal{S}] \ptpre \tilde{\pi}_a} \quad \mathcal{S} \pteRel \mathcal{S}' \quad \mergeRel{\varpi_a}{(\varpi_b[\mathcal{S}'])}{\varpi'_a} \\ \forall \ i \in [0, n], \quad (\tilde{\pi}_b \mapsto \tilde{\pi}_i) \pteRel \mathcal{S}'' \quad \mergeRel{\varpi_i}{(\varpi_b[\mathcal{S}''])}{\varpi''_i}}{\mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\many{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_a \Rightarrow \varpi_a\rangle}{\\ \mathbf{match}^{\ell_b} \ \delta_b \ \mathbf{with} \ \langle \tilde{\pi}_b \Rightarrow \varpi_b \rangle}{\\ \mathbf{match}^{\ell_a} \ \delta' \ \mathbf{with} \ \langle\many{\tilde{\pi}_i \Rightarrow \varpi''_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_a \Rightarrow \varpi'_a\rangle}}$$

$$\frac{\keyCond{\mathbf{size} \ \ell_a \sim \mathbf{size} \ \ell_b} \quad \keyCond{\mergeRel{\delta_a}{\delta_b}{\delta'}} \\ \keyCond{\tilde{\pi}_b \ofProp \nvarPat} \quad \keyCond{\tilde{\pi}_b[\mathcal{S}] \ptpre \tilde{\pi}_a} \quad \mathcal{S} \pteRel \mathcal{S}' \quad \mergeRel{\varpi_a}{(\varpi_b[\mathcal{S}'])}{\varpi'_a}}{\mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\many{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_a \Rightarrow \varpi_a \mid \many{\tilde{\pi}_j \Rightarrow \varpi_j}{\mid}{j}{0}{m}\rangle}{\\ \mathbf{match}^{\ell_b} \ \delta_b \ \mathbf{with} \ \langle \tilde{\pi}_b \Rightarrow \varpi_b \rangle}{\\ \mathbf{match}^{\ell_a} \ \delta' \ \mathbf{with} \ \langle\many{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_a \Rightarrow \varpi'_a \mid \many{\tilde{\pi}_j \Rightarrow \varpi_j}{\mid}{j}{0}{m}\rangle}}$$

$$\frac{\keyCond{\mathbf{size} \ \ell_a \sim \mathbf{size} \ \ell_b} \quad \keyCond{\mergeRel{\delta_a}{\delta_b}{\delta'}} \quad \keyCond{\tilde{\pi}_a \ofProp \nvarPat} \quad \keyCond{\tilde{\pi}_b \ofProp \nvarPat}}{\mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\many{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_a \Rightarrow \varpi_a \rangle}{\\ \mathbf{match}^{\ell_b} \ \delta_b \ \mathbf{with} \ \langle \tilde{\pi}_b \Rightarrow \varpi_b \rangle}{\\ \mathbf{match}^{\ell_a} \ \delta' \ \mathbf{with} \ \langle\many{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_a \Rightarrow \varpi_a \mid \tilde{\pi}_b \Rightarrow \varpi_b \rangle}}$$

$$\frac{\keyCond{\mathbf{size} \ \ell_a \sim \mathbf{size} \ \ell_b} \quad \keyCond{\mergeRel{\delta_a}{\delta_b}{\delta'}} \quad \keyCond{\tilde{\pi}_a \ofProp \varPat} \quad \keyCond{\tilde{\pi}_b \ofProp \nvarPat} \\ (\tilde{\pi}_a \mapsto \tilde{\pi}_b) \pteRel \mathcal{S} \quad \mergeRel{\varpi_a[\mathcal{S}]}{\varpi_b}{\varpi'_b}}{\mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\many{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_a \Rightarrow \varpi_a \rangle}{\\ \mathbf{match}^{\ell_b} \ \delta_b \ \mathbf{with} \ \langle \tilde{\pi}_b \Rightarrow \varpi_b \rangle}{\\ \mathbf{match}^{\ell_a} \ \delta' \ \mathbf{with} \ \langle\many{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_b \Rightarrow \varpi'_b \mid \tilde{\pi}_a \Rightarrow \varpi_a \rangle}}$$

$$\frac{\keyCond{\mathbf{size} \ \ell_a \sim \mathbf{size} \ \ell_b} \quad \keyCond{\mergeRel{\delta_a}{\delta_b}{\delta'}} \quad \keyCond{\tilde{\pi}_b \ofProp \varPat} \\ \forall \ i \in [0, n], \quad (\tilde{\pi}_b \mapsto \tilde{\pi}_i) \pteRel \mathcal{S}'' \quad \mergeRel{\varpi_i}{(\varpi_b[\mathcal{S}''])}{\varpi''_i}}{\mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\some{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n}\rangle}{\\ \mathbf{match}^{\ell_b} \ \delta_b \ \mathbf{with} \ \langle \tilde{\pi}_b \Rightarrow \varpi_b \rangle}{\\ \mathbf{match}^{\ell_a} \ \delta' \ \mathbf{with} \ \langle\some{\tilde{\pi}_i \Rightarrow \varpi''_i}{\mid}{i}{0}{n} \mid \tilde{\pi}_b \Rightarrow \varpi_b\rangle}}$$

$$\frac{\keyCond{\mathbf{size} \ \ell_a \sim \mathbf{size} \ \ell_b} \quad v \ \mathbf{fresh} \\ \mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\some{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n}\rangle}{\\ \mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle v \Rightarrow \mathbf{match}^{\ell_b} \ \delta_b \ \mathbf{with} \ \langle \tilde{\pi}_b \Rightarrow \varpi_b \rangle\rangle}{\varpi'}}{\mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\some{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n}\rangle}{\\ \mathbf{match}^{\ell_b} \ \delta_b \ \mathbf{with} \ \langle \tilde{\pi}_b \Rightarrow \varpi_b \rangle}{\varpi'}}$$

$$\frac{v \ \mathbf{fresh} \\ \mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\some{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n}\rangle}{\\ \mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle v \Rightarrow \varpi_b \rangle}{\varpi'}}{\mergeRel{\mathbf{match}^{\ell_a} \ \delta_a \ \mathbf{with} \ \langle\some{\tilde{\pi}_i \Rightarrow \varpi_i}{\mid}{i}{0}{n}\rangle}{\varpi_b}{\varpi'}}$$
</details>
$$\frac{x \ \mathbf{fresh} \\ \cdot \vdash^{0}_{x} \mathbf{match} \ v \ \mathbf{with} \ \langle \pi \Rightarrow e \rangle \expRel \varpi \quad \varpi \sortRel \varpi'}{\mathbf{match} \ v \ \mathbf{with} \ \langle\pi \Rightarrow e \rangle \elabRel \varpi'}$$

$$\frac{x \ \mathbf{fresh} \\ \mathbf{match} \ v \ \mathbf{with} \ \langle\some{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n}\rangle \elabRel \mathbf{match}^{n} \ v \ \mathbf{with} \ \langle\some{\tilde{\pi}_j \Rightarrow \varpi_j}{\mid}{j}{0}{m}\rangle \\ \cdot \vdash^{n + 1}_{x} \mathbf{match} \ v \ \mathbf{with} \ \langle \pi \Rightarrow e \rangle \expRel \varpi \quad \varpi \sortRel \varpi' \\ \mergeRel{\mathbf{match}^{n} \ v \ \mathbf{with} \ \langle\some{\tilde{\pi}_j \Rightarrow \varpi_j}{\mid}{j}{0}{m}\rangle}{\varpi'}{\varpi''}}{\mathbf{match} \ v \ \mathbf{with} \ \langle\some{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n} \mid \pi \Rightarrow e \rangle \elabRel \varpi''}$$

$$\frac{v \ \mathbf{fresh} \\ \mathbf{match} \ v \ \mathbf{with} \ \langle\some{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n}\rangle \elabRel \mathbf{match}^{\ell} \ v \ \mathbf{with} \ \langle\some{\tilde{\pi}_j \Rightarrow \varpi_j}{\mid}{j}{0}{m}\rangle}{\mathbf{match} \ e \ \mathbf{with} \ \langle\some{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n}\rangle \elabRel \mathbf{match}^{\ell} \ e \ \mathbf{with} \ \langle\some{\tilde{\pi}_j \Rightarrow \varpi_j}{\mid}{j}{0}{m}\rangle}$$
</details>

<details>
<summary>Term Typing<span style="float:right;">{$\Delta;\Gamma \vdash e: \tau$}</span></summary>
$$\frac{x: \tau\in\Gamma}{\Delta;\Gamma \vdash x: \tau} \ \ruleName{Var}$$

$$\frac{}{\Delta;\Gamma \vdash l: \forall \ \alpha \ [\beta: \neg\{l\}]. \langle l: \alpha \mid \beta\rangle} \ \ruleName{Label}$$

$$\frac{\Delta;\Gamma \vdash f: \tau_1 \to \tau_2 \quad \Delta;\Gamma \vdash e: \tau_1}{\Delta;\Gamma \vdash f \ e: \tau_2} \ \ruleName{App}$$

<!--
$$\frac{\Delta;\Gamma \vdash e: \{\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_1\} \\ \beta_2 \notin \Delta \quad \Delta, \beta_2 : \neg \{\many{l_j}{,}{j}{0}{n}\};\Gamma \vdash \{\many{l_j = e_j}{\mid}{j}{0}{n}\} : \{\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \beta_2\} \\ \psi_2 = \{\many{l_j}{,}{j}{0}{n}\} \quad \Delta \vdash \beta_1 \# \psi_1 \quad \psi_1 \cap \psi_2 = \emptyset \\ \Delta, \beta_2 : \neg \{\many{l_j}{,}{j}{0}{n}\} \vdash \\ \quad (\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_1) \vee  (\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \beta_2) \sim (\many{l_k: \tau_k}{\mid}{k}{0}{m+n} \mid \beta_3)}{\Delta;\Gamma \vdash e.+\{\many{l_j = e_j}{\mid}{j}{0}{n}\} : \{\many{l_k: \tau_k}{\mid}{k}{0}{m+n} \mid \beta_3\}} \ \ruleName{Record Extension}$$
-->

$$\frac{\Delta;\Gamma \vdash e: \{\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_1\} \\ \Delta;\Gamma \vdash \{\many{l_j = e_j}{\mid}{j}{0}{n}\} : \{\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \cdot\} \\ \Delta \vdash (\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \cdot) \presRel \kappa_2 \quad \Delta \vdash (\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_1) \presRel \kappa_1 \\ \kappa_1 \cap \kappa_2 = \emptyset \\ \beta_2, \beta_3 \notin \Delta \\ \Delta, \beta_2: \neg \{\many{l_j}{,}{j}{0}{n}\}, \beta_3: \neg \{\many{l_i}{,}{i}{0}{m}\} \vdash \\ \quad (\many{l_i: \tau_i}{\mid}{i}{0}{m} \mid \beta_3) \sqcup (\many{l_j: \tau_j}{\mid}{j}{0}{n} \mid \beta_2) \sim (\many{l_k: \tau_k}{\mid}{k}{0}{m+n} \mid \beta_1)}{\Delta;\Gamma \vdash e.+\{\many{l_j = e_j}{\mid}{j}{0}{n}\} : \{\many{l_k: \tau_k}{\mid}{k}{0}{m+n} \mid \beta_1\}} \ \ruleName{Record Extension}$$

$$\frac{\Delta;\Gamma \vdash e: \langle\cdot\rangle}{\Delta;\Gamma \vdash \mathbf{match} \ e \ \mathbf{with} \ \langle\rangle : \forall \alpha. \alpha} \ \ruleName{MatchVoid}$$

$$\frac{\Delta;\Gamma \vdash e: \langle\rho\rangle \\ \forall i \in [0, n], \\ \quad \Delta;\Gamma;\cdot \ptWith \pi_i : \tau_i \ptRel \Delta_i;\Gamma_i;\Theta_i \\ \quad \Delta_i \vdash \tau_i[\mathcal{I}_i] \sim \langle\rho\rangle \\ \quad \Delta_i;\Gamma_i[\mathcal{I_i}] \vdash e_i: \tau}{\Delta;\Gamma \vdash \mathbf{match} \ e \ \mathbf{with} \ \langle\some{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n}\rangle : \tau} \ \ruleName{SimpleMatch}$$

$$\frac{\Delta;\Gamma \vdash e: \langle\rho\rangle \\ \forall i \in [0, n], \\ \quad \Delta;\Gamma;\cdot \ptWith \pi_i : \langle\rho_i\rangle \ptRel \Delta_i;\Gamma_i;\Theta_i \\ \quad \Delta_i \vdash \rho_i[\mathcal{I}_i] \sim \rho \\ \quad \Delta_i;\Gamma_i[\mathcal{I_i}] \vdash e_i: \tau}{\Delta;\Gamma \vdash \mathbf{match} \ e \ \mathbf{with} \ \langle\some{\pi_i \Rightarrow e_i}{\mid}{i}{0}{n}\rangle : \tau} \ \ruleName{Match}$$
</details>

<hr class = "split">

## Dynamic Semantics

<details>
<summary>{$\begin{equation*} \begin{split} v \mathrel{\mathop:}= \ &l \ \optional{v} \cmid x \cmid \{\many{l_i = e_i}{\mid}{i}{0}{n}\} \cmid \mathbf{rec} \ f (\some{x_i}{,}{i}{0}{p}) = e \ \cmid \\ &\lambda (\some{x_i}{,}{i}{0}{p}) = e\end{split} \end{equation*}$}<span style="float:right;">Values</span></summary>
<ul>
<li class="item-description"><span>{$$}</span><span>TODO</span></li>
</ul>
</details>