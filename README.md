# Ninety-Nine Haskell Problems を解く

Haskell入門のために[Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)を解く。テストも書く。

## Hspecによるテスト
[ドキュメント](http://hspec.github.io/)

テストのサンプルがhspec\_sampleの中にある。

```sh
cabal install --only-dependencies --enable-tests
```
でいろいろセットアップできる？

src/test/にテストがある。
```sh
$ runhaskell -isrc/main -isrc/test src/test/XxxSpec.hs
```
でテストを実行できる。

```sh
$ cabal test
```
で全実行できる？
