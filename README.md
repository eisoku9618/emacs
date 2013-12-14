emacs
=====

my setting file of emacs 24

required emacs lisps
-----
# package-list-packages
* auto-async-byte-compile
* auto-complete
* dired+
* helm
* helm-descbinds
* markdown-mode
* melpa
* multi-term
* popup
* popwin
* tabbar
* undo-tree
* undohist
* wgrep

# downloaded by my self
* rosemacs
* vrml


http://tokkono.cute.coocan.jp/blog/slow/index.php/programming/markdown-skills-for-github-beginners/
からの転載


プロジェクト・タイトル
======================
ここにプロジェクトの概要を書きます。
行末にスペースを2つ入れると
改行されます。

段落を分けるには、[空行](http://example.com/) を入れます。

使い方
------
### インライン ###
インラインのコードは、**バッククォート** (`` ` ``) で囲みます。

### ブロックレベル ###
    function f () {
            alert(0);  /* 先頭に4文字のスペース、
                                  もしくはタブを挿入します */
                                      }

パラメータの解説
----------------
リストの間に空行を挟むと、それぞれのリストに `<p>` タグが挿入され、行間が
広くなります。

    def MyFunction(param1, param2, ...)

+   `param1` :
    _パラメータ1_ の説明

+   `param2` :
    _パラメータ2_ の説明

関連情報
--------
### リンク、ネストしたリスト
1. [リンク1](http://example.com/ "リンクのタイトル")
    * ![画像1](http://github.com/unicorn.png "画像のタイトル")
    2. [リンク2][link]
        - [![画像2][image]](https://github.com/)

  [link]: http://example.com/ "インデックス型のリンク"
    [image]: http://github.com/github.png "インデックス型の画像"

### 引用、ネストした引用
> これは引用です。
>
> > スペースを挟んで `>` を重ねると、引用の中で引用ができますが、
> > GitHubの場合、1行前に空の引用が無いと、正しくマークアップされません。

ライセンス
----------
Copyright &copy; 2011 xxxxxx
Licensed under the [Apache License, Version 2.0][Apache]
Distributed under the [MIT License][mit].
Dual licensed under the [MIT license][MIT] and [GPL license][GPL].

[Apache]: http://www.apache.org/licenses/LICENSE-2.0
[MIT]: http://www.opensource.org/licenses/mit-license.php
[GPL]: http://www.gnu.org/licenses/gpl.html


  auto-async-byte... 20130824.... installed  Automatically byte-compile when saved
  auto-complete      20131128.233 installed  Auto Completion for GNU Emacs
  dired+             20131205.... installed  Extensions to Dired.
  helm               20131214.221 installed  Helm is an Emacs incremental and narrowing framework
  helm-descbinds     20131102.901 installed  Yet Another `describe-bindings' with `helm'.
  markdown-mode      20131210.700 installed  Emacs Major mode for Markdown-formatted text files
  melpa              20130815.... installed  special handling for the MELPA repository
  multi-term         20130108.... installed  Managing multiple terminal buffers in Emacs.
  popup              20130901.... installed  Visual Popup User Interface
  popwin             20130329.435 installed  Popup Window Manager.
  tabbar             20131106.... installed  No description available.
  undo-tree          20131118.... installed  Treat undo history as a tree
  undohist           20110331.... installed  Persistent Undo History for GNU Emacs
  wgrep              20131209.659 installed  Writable grep buffer and apply the changes to files
