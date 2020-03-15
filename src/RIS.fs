module RIS

open Fable.Parsimmon

// https://www.wikiwand.com/en/RIS_(file_format)#/Tags
// 参照のタイプ（最初のタグである必要があります）
let TY = "TY"
// 著者
let AU = "AU"
// 第一著者
let A1 = "A1"
// 副著者
let A2 = "A2"
// 三次著者
let A3 = "A3"
// 補助著者
let A4 = "A4"
// 概要
let AB = "AB"
// 著者アドレス
let AD = "AD"
// 受入番号
let AN = "AN"
// アーカイブ内の場所
let AV = "AV"
// よくわからん
let BT = "BT"
// C1 - C8 はカスタム
let C1 = "C1"
let C2 = "C2"
let C3 = "C3"
let C4 = "C4"
let C5 = "C5"
let C6 = "C6"
let C7 = "C7"
let C8 = "C8"
// キャプション
let CA = "CA"
// 電話番号
let CN = "CN"
// よくわからん
let CP = "CP"
// 未公開のリファレンスのタイトル
let CT = "CT"
// 掲載場所
let CY = "CY"
// 日付
let DA = "DA"
// データベース名
let DB = "DB"
// DOI
let DO = "DO"
// データベースプロバイダ
let DP = "DP"
// 編集者
let ED = "ED"
// 終了ページ
let EP = "EP"
// 版
let ET = "ET"
// 参照ID
let ID = "ID"
// 発行番号
let IS = "IS"
// J* のJはJournalらしいが、よくわからん
let J1 = "J1"
let J2 = "J2"
let JA = "JA"
let JF = "JF"
let JO = "JO"
// キーワード
let KW = "KW"
// リンク集, 1-4はそれぞれpdf, full text, Related Records, Imageへのリンク
let L1 = "L1"
let L2 = "L2"
let L3 = "L3"
let L4 = "L4"
// 言語
let LA = "LA"
// ラベル
let LB = "LB"
// ウェブサイトへのリンク
let LK = "LK"
// M* - よくわからん
let M1 = "M1"
let M2 = "M2"
let M3 = "M3"
// ノート
let N1 = "N1"
// アブストラクト
let N2 = "N2"
// 巻数
let NV = "NV"
// Original Publication
let OP = "OP"
// Publisher
let PB = "PB"
// Publishing Place
let PP = "PP"
// Publication year (YYYY)
let PY = "PY"
// Reviewed Item
let RI = "RI"
// Research Notes
let RN = "RN"
// Reprint Edition
let RP = "RP"
// Section
let SE = "SE"
// ISBN/ISSN
let SN = "SN"
// Start Page
let SP = "SP"
// Short Title
let ST = "ST"
// Primary Title
let T1 = "T1"
// Secondary Title (journal title, if applicable)
let T2 = "T2"
// Tertiary Title
let T3 = "T3"
// Translated Author
let TA = "TA"
// Title
let TI = "TI"
// Translated Title
let TT = "TT"
// User definable This is an alphanumeric field and there is no practical limit to the length of this field.
let U1 = "U1"
let U2 = "U2"
let U3 = "U3"
let U4 = "U4"
let U5 = "U5"
// URL
let UR = "UR"
// Volume number
let VL = "VL"
// Published Standard number
let VO = "VO"
// Primary Date
let Y1 = "Y1"
// Access Date
let Y2 = "Y2"
// End of Reference (must be empty and the last tag)
let ER = "ER"

let OrderLessTags =
  [ A1
    A2
    A3
    A4
    AB
    AD
    AN
    AU
    AV
    BT
    C1
    C2
    C3
    C4
    C5
    C6
    C7
    C8
    CA
    CN
    CP
    CT
    CY
    DA
    DB
    DO
    DP
    ED
    EP
    ET
    ID
    IS
    J1
    J2
    JA
    JF
    JO
    KW
    L1
    L2
    L3
    L4
    LA
    LB
    LK
    M1
    M2
    M3
    N1
    N2
    NV
    OP
    PB
    PP
    PY
    RI
    RN
    RP
    SE
    SN
    SP
    ST
    T1
    T2
    T3
    TA
    TI
    TT
    U1
    U2
    U3
    U4
    U5
    UR
    VL
    VO
    Y1
    Y2 ]

let AllTags =
  List.concat
    [ [ TY ]
      OrderLessTags
      [ ER ] ]

let Headers = [ "Provider"; "Database"; "Content" ]

type RISField =
  { tag: string
    value: string }


let RISOrderLessTagParser =
  OrderLessTags
  |> List.map Parsimmon.str
  |> Parsimmon.choose

// JStageからダウンロードした.risファイルには、以下のようなメタデータが含まれている
// Provider: Japan Science and Technology Agency
// Database: J-STAGE
// Content:text/plain; charset="utf-8"
let HeaderParser =
  Headers
  |> List.map Parsimmon.str
  |> Parsimmon.choose

let RISHeaderFieldParser headerParser =
  Parsimmon.seq2
    (headerParser
     |> Parsimmon.skip (Parsimmon.optionalWhitespace)
     |> Parsimmon.skip (Parsimmon.str ":."))
    (Parsimmon.regex(".*").orTry(Parsimmon.optionalWhitespace))
  |> Parsimmon.skip ((Parsimmon.str "\n"))

let RISFieldParser tagParser =
  Parsimmon.seq2
    (tagParser
     |> Parsimmon.skip (Parsimmon.optionalWhitespace)
     |> Parsimmon.skip (Parsimmon.str "-"))
    (Parsimmon.regex(".*").orTry(Parsimmon.optionalWhitespace).map(fun s -> s.Trim()))
  |> Parsimmon.skip ((Parsimmon.str "\n").orTry(Parsimmon.endOfFile))
  |> Parsimmon.map (fun tuple ->
       { tag = fst tuple
         value = snd tuple })

let RISRecordParser =
  Parsimmon.seq3 (RISFieldParser(Parsimmon.str TY)) (Parsimmon.many (RISFieldParser RISOrderLessTagParser))
    (RISFieldParser(Parsimmon.str ER))
  |> Parsimmon.map (fun (ty, orderless, er) ->
       Seq.concat
         [ [ ty ]
           orderless |> Seq.toList
           [ er ] ])