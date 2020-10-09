module PackRatParserCombinators where

-- 算術式パーサー
data Derivs = Derivs{
    dvAdditive  :: Result Int,
    dvMultitive :: Result Int,
    dvPrimary   :: Result Int,
    dvDecimal   :: Result Int,
    dvChar      :: Result Char
} deriving(Show)

data Result v = Parsed v Derivs
              | NoParse
            deriving(Show)

-- 非終端記号と導出規則を再帰関数定義に変換
pAdditive :: Derivs -> Result Int
pAdditive d = alt1 where
    -- Additive <- Multitive '+' Additive
    alt1 = case dvMultitive d of
        Parsed vleft d' -> -- 最初のオルタナにマッチング成功→'+'を挟んだ左右の値を特定して計算し戻す
            case dvChar d' of -- 演算文字は終端記号なので読み取るパーサーが別途用意されている
                Parsed '+' d'' -> -- 真ん中にマッチング成功
                    case dvAdditive d'' of
                        Parsed vright d''' -> -- 右端にマッチング成功
                            Parsed (vleft + vright) d'''
                        _ -> alt2;  -- 以下、マッチング失敗。これは`+`の右側でAdditiveにマッチング失敗
                _ -> alt2; -- '+'文字にマッチング失敗
        _ -> alt2; -- '+'の左側でMultitiveにマッチング失敗
    -- Additive <- Multitive
    alt2 = dvMultitive d -- 次のオルタナ

pMultitive :: Derivs -> Result Int
pMultitive d = alt1 where
    alt1 = case dvPrimary d of
        Parsed vleft d' ->   -- 最初のオルタナ
            case dvChar d' of -- '*'記号マッチング
                Parsed '*' d'' -> 
                    case dvMultitive d'' of
                        Parsed vright d''' ->
                            Parsed (vleft * vright) d''';
                        _ -> alt2;
                _ -> alt2;
        _ -> alt2
    alt2 = dvPrimary d

pPrimary :: Derivs -> Result Int
pPrimary d = alt1 where
    alt1 = case dvChar d of
        Parsed '(' d' ->
            case dvAdditive d' of
                Parsed vmiddle d'' ->
                    case dvChar d'' of
                        Parsed ')' d''' -> Parsed vmiddle d''';
                        _ -> alt2;
                _ -> alt2;
        _ -> alt2
    alt2 = dvDecimal d -- alt1が失敗したらalt2の番

pDecimal :: Derivs -> Result Int
pDecimal d = alt1 where
    alt1 =   case dvChar d of   -- ここもっと手短に書けないか
        Parsed '0' d' -> Parsed 0 d'; 
        Parsed '1' d' -> Parsed 1 d';
        Parsed '2' d' -> Parsed 2 d';
        Parsed '3' d' -> Parsed 3 d';
        Parsed '4' d' -> Parsed 4 d';
        Parsed '5' d' -> Parsed 5 d';
        Parsed '6' d' -> Parsed 6 d';
        Parsed '7' d' -> Parsed 7 d';
        Parsed '8' d' -> Parsed 8 d';
        Parsed '9' d' -> Parsed 9 d';
        _           -> NoParse

-- 入力列を受け取り、メモの最終結果を返す
parse :: String -> Derivs
parse s = d where
    d    = Derivs add mult prim dec chr
    add  = pAdditive d
    mult = pMultitive d
    prim = pPrimary d
    dec  = pDecimal d
    chr  = case s of
            (c:s') -> Parsed c (parse s')
            [] -> NoParse
