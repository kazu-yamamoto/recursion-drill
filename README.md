# 再帰ドリル

再帰を学ぶためのドリルです。使用するプログラミング言語は Haskell。[Haskell Platform](http://www.haskell.org/platform/) の利用を推奨します。

1. [自然数に対する素朴な再帰](drill/1.md) ([演習1](exercise/1.hs))
2. [自然数に対する末尾再帰](drill/2.md) ([演習2](exercise/2.hs))
3. [いろいろな終わり方](drill/3.md) ([演習3](exercise/3.hs))
4. [再帰的な自然数](drill/4.md) ([演習4](exercise/4.hs))
5. [自然数に対する少し複雑な再帰](drill/5.md) ([演習5](exercise/5.hs))
6. [再帰のこころ](drill/6.md) ([演習6](6.hs))
7. [メモ化](drill/7.md) ([演習7](7.hs))
8. [リストに対する素朴な再帰](drill/8.md) ([演習8](exercise/8.hs))
9. [リストを生成する再帰](drill/9.md) ([演習9](exercise/9.hs))
10. [ループを超えた再帰](drill/10.md) ([演習10](exercise/10.hs))
11. [二分探索木(探索と挿入)](drill/11.md) ([演習11](exercise/11.hs))
12. [二分探索木(走査と削除)](drill/12.md) ([演習12](exercise/12.hs))
13. [おわりに](drill/epilogue.md)

演習に取り組む前に、まずテストに利用している hspec ライブラリを以下のようにしてインストールして下さい。

    % cabal update
    % cabal install hspec

次にこのレポジトリを clone して下さい。

    % git clone git://github.com/kazu-yamamoto/recursion-drill.git

演習は exercise ディレクトリにあります。

    % cd recursion-drill/exercise

演習は、"数字.hs" というファイル中の undefined を変更することで解いていきましょう。ある部分ができたと思ったら、テストして動作を確認して下さい。たとえば、"1.hs" を書き換えた後は、以下のようにしてテストできます。

    % runghc 1.hs

テストの結果は、成功は緑で、失敗は赤で表示されます。すべてが緑になるまで頑張りましょう。答えは answer というディレクトリにあります。考える前に答えを見てはいけません。
