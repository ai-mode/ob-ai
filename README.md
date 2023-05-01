# AI support for Org Babel source blocks

[![LICENSE](https://img.shields.io/github/license/ai-mode/ob-ai)](https://github.com/ai-mode/ob-ai/blob/master/LICENSE)


##  Overview

An extension for org-mode that allows execution of source code blocks via an AI backend.

![ob-ai extension demo](https://github.com/ai-mode/ob-ai/blob/master/ob-ai-demo.png "ob-ai demo")

## Installation


### Installation via MELPA

It's easiest/recommended to install from MELPA. Here's a minimal MELPA configuration for your ~/.emacs:

```elisp
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
```

Afterwards, M-x package-install RET ob-ai RET (you might want to M-x package-refresh-contents RET beforehand if you haven't done so recently).


### Installation via GIT

Clone this repository somewhere

```bash
$ cd ~/.emacs.d/plugins
$ git clone --recursive https://github.com/ai-mode/ai-mode
$ git clone --recursive https://github.com/ai-mode/ob-ai
```

Add the following in your .emacs file:

```elisp
(add-to-list 'load-path
              "~/.emacs.d/plugins/ai-mode")
(add-to-list 'load-path
              "~/.emacs.d/plugins/ob-ai")
(require 'ob-ai)
```

### Configuration

Add the following code to your configuration file:

```elisp

(require 'ob-ai)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))
 (add-to-list 'org-src-lang-modes '("ai" . text))
```

If you are using `use-package`, then use the following code:

```elisp
(use-package ob-ai
  :config (progn
            (org-babel-do-load-languages 'org-babel-load-languages
                                         (append org-babel-load-languages '((ai . t))))
            (add-to-list 'org-src-lang-modes '("ai" . text))))
```

To use OpenAI backends, you need to obtain an authorization key. You can get the key on the [page](https://platform.openai.com/account/api-keys).

There are several ways to assign your key to the variable `ai--openai--api-key`.

1. Put `(setq ai--openai--api-key "you key")` in the configuration file.
2. Use the command M-x `set-variable ai--openai--api-key`.
3. Use the command M-x `customize-variable ai--openai--api-key`.


#### Adding backend

Due to the specifics of org-babel, synchronous backends are used. The current package includes two backends that operate through the OpenAI API.

The extension stores all the used backends as an alist in the variable ai--org-babel-backends. The current backend is contained in the variable `ai-org-babel-backend`.

Adding backend to the list:

```elisp
(add-to-list 'ai--org-babel-backends '("SuperAI backend" . super-ai-backend))
```

If you want to write your own backend, take a look at the source code of the `ai--openai--chat-ob-sync-query` function.


## How to use

Declare a code block in a document using the `#+begin_src ai ... #+end_src` construction, place your content inside the block that will be sent to AI and execute it using the `C-c C-c` command.

### Configuring the executable block

For each executable block, you can change the execution parameters: the used model, the used backend, timeout, maximum number of tokens, etc.

Parameters are passed in the format `:param value` in the `begin_src` line.

The following customization options are available. The set may vary depending on the backend used.

- `:backend` - overrides the backend used to execute the block;
- `:model` - changes the AI model used;
- `:max-tokens` - changes the maximum number of tokens in the response;
- `:timeout` - changes the request timeout value.


## Examples of blocks

The simplest block without parameters:

```org
#+begin_src ai
What are generative AIs?
#+end_src
```

After executing this block, a block with approximately the following content will appear. The content will vary depending on the AI response.


```org
#+begin_src ai
In what year were light bulbs invented?
#+end_src

#+RESULTS:
The first light bulb was invented in 1879 by Thomas Edison.
```

```org
#+begin_src ai :backend ai--openai--completions-ob-sync-query :model text-davinci-003 :max-tokens 200 :timeout 35
What are generative AIs?
#+end_src
```

You can see other examples in the file [example.org](https://github.com/ai-mode/ob-ai/example.org)


## Available commands

`ai-change-org-babel-backend` - allows you to change the current backend to another one from the list `ai--org-babel-backends`.
