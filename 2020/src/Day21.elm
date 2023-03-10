module Day21 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import LineParser
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Set exposing (Set)


type alias Food =
    { ingredients : Set String
    , allergens : Set String
    }


parse : String -> Result String (List Food)
parse =
    String.trim
        >> Parser.run (Parser.Extra.loopLineWise foodParser)
        >> Result.mapError Parser.Extra.deadEndsToString


foodParser : Parser Food
foodParser =
    Parser.succeed Food
        |= ingredientsParser
        |. Parser.Extra.spaces
        |. Parser.symbol "("
        |. Parser.Extra.spaces
        |. Parser.keyword "contains"
        |. Parser.Extra.spaces
        |= allergensParser
        |. Parser.Extra.spaces
        |. Parser.symbol ")"


ingredientsParser : Parser (Set String)
ingredientsParser =
    Parser.getChompedString (Parser.chompWhile ((/=) '('))
        |> Parser.map (String.words >> Set.fromList)


allergensParser : Parser (Set String)
allergensParser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = Parser.Extra.spaces
        , item = Parser.getChompedString (Parser.chompWhile Char.isAlpha)
        , trailing = Parser.Forbidden
        }
        |> Parser.map Set.fromList


solution : String -> Result String ( Int, String )
solution =
    parse >> Result.andThen solve


solve : List Food -> Result String ( Int, String )
solve foods =
    let
        ingredientsPerAllergen : Dict String (Set String)
        ingredientsPerAllergen =
            foods
                |> List.foldl
                    (\food dict ->
                        food.allergens
                            |> Set.foldl
                                (\allergen ->
                                    Dict.update allergen
                                        (Maybe.withDefault food.ingredients
                                            >> Set.intersect food.ingredients
                                            >> Just
                                        )
                                )
                                dict
                    )
                    Dict.empty

        possiblySolved : Result String (List ( String, String ))
        possiblySolved =
            ingredientsPerAllergen
                |> Dict.toList
                |> untilUnchanged removeSinglesFromOthers
                |> LineParser.parseGeneral "Allergen"
                    (\( allergen, ingredients ) ->
                        allergen ++ ": " ++ (ingredients |> Set.toList |> String.join ", ")
                    )
                    (\( allergen, ingredients ) ->
                        case ingredients |> Set.toList of
                            [ single ] ->
                                Ok ( allergen, single )

                            items ->
                                Err ("Expected a single ingredient but got " ++ String.fromInt (List.length items))
                    )
    in
    Result.map (solveHelper foods) possiblySolved


solveHelper : List Food -> List ( String, String ) -> ( Int, String )
solveHelper foods solved =
    let
        allIngredients : List String
        allIngredients =
            foods |> List.concatMap (.ingredients >> Set.toList)

        noAllergens : Set String
        noAllergens =
            solved
                |> List.map Tuple.second
                |> Set.fromList
                |> Set.diff (Set.fromList allIngredients)

        numOccurrancesOfIngredientsWithoutAllergens : Int
        numOccurrancesOfIngredientsWithoutAllergens =
            allIngredients
                |> List.filter (\ingredient -> Set.member ingredient noAllergens)
                |> List.length

        canonicalDangerousIngredientsList : String
        canonicalDangerousIngredientsList =
            solved
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
                |> String.join ","
    in
    ( numOccurrancesOfIngredientsWithoutAllergens
    , canonicalDangerousIngredientsList
    )


untilUnchanged : (a -> a) -> a -> a
untilUnchanged f a =
    let
        nextA =
            f a
    in
    if nextA == a then
        nextA

    else
        untilUnchanged f nextA


removeSinglesFromOthers : List ( String, Set String ) -> List ( String, Set String )
removeSinglesFromOthers items =
    case items of
        [] ->
            []

        first :: rest ->
            removeSinglesFromOthersHelper [] first rest


removeSinglesFromOthersHelper : List ( String, Set String ) -> ( String, Set String ) -> List ( String, Set String ) -> List ( String, Set String )
removeSinglesFromOthersHelper before current after =
    case current |> Tuple.second |> Set.toList of
        [ single ] ->
            let
                nextBefore =
                    before |> List.map (Tuple.mapSecond (Set.remove single))

                nextAfter =
                    after |> List.map (Tuple.mapSecond (Set.remove single))
            in
            case nextAfter of
                [] ->
                    List.reverse (current :: nextBefore)

                next :: rest ->
                    removeSinglesFromOthersHelper (current :: nextBefore) next rest

        _ ->
            case after of
                [] ->
                    List.reverse (current :: before)

                next :: rest ->
                    removeSinglesFromOthersHelper (current :: before) next rest


main : Html Never
main =
    Html.div []
        [ showResult (solution puzzleInput)
        ]


showResult : Result String ( Int, String ) -> Html msg
showResult result =
    Html.div []
        (case result of
            Ok ( int, string ) ->
                [ Html.output []
                    [ Html.text (String.fromInt int) ]
                , Html.output []
                    [ Html.text string ]
                ]

            Err error ->
                [ Html.text error ]
        )


puzzleInput : String
puzzleInput =
    """
jsgh xhrdsl pjqzb dblzkk jpx jfdn xtx bvcrrfbr bjdqnts vsb scgpv gjnt lnkz hphzkcf mxfnql mdttst gbmk krmgj mrztqsk kcbdmf scxxn fnpgz mrtxr xzcq qkdrpq fnccgnm bcdgf kgzcf nzcr gcqpb clz vndrb nlxn dhbxtb cqncf bzgvb zrmnnk qvbrx kqcjqg qjt lpjjd bdq mgbddhr nfbx xcj scnzhj rrgczb lbnmsr nqztms dch (contains soy, fish, nuts)
lbnmsr nfbx clz mxfnql fnccgnm xrhbc fnpgz hvh qjt pshhhvn gjkgm dphrq lsfsnb xmgmvpzx mdttst kflqxq bzgvb bcdgf dblzkk dhz jfdn kgzcf rpsgs vndrb qbbv qkdrpq rchkc pjfbh sncd xlndgbp xcgtv clh dlnq xcj lmn fxhp hkpdd mgb xdrxx xtx nqztms svtjgr ptmqj lfxnbq rqjm bvcrrfbr pst frb qrtmdt bkbdt xhrdsl zxqjg cjztt xzcq cqncf dncrcx bhck ldlpt kcbdmf scnzhj blslf pprlrf mrtxr fdqt xtr jtrscmz rrgczb scxxn vsb sbrmrp (contains sesame, dairy)
mhncmb dtkn xdrxx vxvhx jzx mcnrg zlxl pshhhvn tj trsg ngthq bdq sbrmrp rgxggd cnjrhn rlfvz ldlpt cqncf sdpntg xlndgbp nlxn jdv xcgtv clz xzcq zqgj klmrg blslf mcdt zlnpt nzcr plvrn bsgsd tbbxv fnpgz lbnmsr mxfnql ltxx fxhp nfqhnxd zxqjg kqcjqg vndrb scxxn kflqj gnt khlh dhz kgzcf cjztt gbmk hphzkcf tkcs ggfr bvcrrfbr xhrdsl mtqtmp scnzhj skvmp bhjv qjt kfrv dzfkgs jfdn drtv dlf xmncxv fxtrjkp kzhtf mrztqsk pjqzb dfsrz gxhj htvrgj shjl bkpq jpx jknjtt qkdrpq bcdgf pst qqvjm clh (contains soy, dairy)
fdvfx pshhhvn jsgh dqxk gnt hvcfm dfsrz pmfrjm xmncxv rfdv htvrgj nzcr tgdrc kqcjqg xcj xrhbc tbbxv plvrn vndrb gjkgm spbl dzfkgs nqztms xmgmvpzx dhz scxxn bcdgf xhrdsl hvh xlhrqfjb zxqjg mcnrg lmn tj scgpv clh bkpq skvmp mjnvk ndcgcv xpnf lbnmsr mqhdrfd klmrg pst qbxzqr mvkz hbfrlp qqvjm kfrv gxhj mcdt clmllf tptzp ndmmhv rrgczb xtr pjqzb qkdrpq mrztqsk pprlrf zgsr kzhtf xtx jdqq xzffgv hzllnq fxhp dphrq xcgtv jpx pmtk zfq jtrscmz dzps cqmlxdl tkcs trsg bdq kcbdmf frb kflqxq nfbx cvbsp gptkkf pjfbh bvcrrfbr (contains dairy, wheat, sesame)
fdvfx sdpntg jdqq bzgvb fxhp nlxn tcxcg bcdgf sncd bkpq zpnnqqtf clh xhrdsl rpsgs mxpj ndmmhv rfdv dncrcx zxqjg lnkz vndrb mcnrg dqxk xcj ggfr lbnmsr xtr zfq qvbrx dhbxtb zlxl gdmrc kcbdmf mxfnql qbbv pprlrf pjfbh scgpv jdv xlndgbp scnzhj qkdrpq xpnf kflqxq shjl cqncf drtv hvh bvcrrfbr nqztms xcgtv fscs zfhs (contains fish, soy, dairy)
jzx jpx xmncxv drtv ndcgcv lgbx bsgsd scgpv rpsgs trsg frtgq pmfrjm bhjv clz xzffgv lbnmsr nzcr zpnnqqtf dch frb bdq bzgvb jsgh scxxn mrtxr jdv gdmrc xzcq dtkn bcdgf rlfvz kzhtf hbfrlp dtkk sdpntg rxcmb mtqtmp clh qqbsfd lbvkccl lmn mvkz tbbxv qvbrx krmgj xhrdsl dhbxtb jfdn zqk bjdqnts mxfnql cqncf lpjjd pprlrf zfq skvmp dqvct ldlpt xdrxx jgrptg zgsr kqcjqg xtr rrgczb vndrb rqjm xcgtv xztmx fxtrjkp sbrmrp zqgj lnkz (contains nuts)
pmtk dch cqncf hvcfm xdrxx rpsgs tkcs plvrn zqk mtqtmp jtrscmz nzcr vsb bmlfmj rlfvz nfqhnxd rchkc lbvkccl kcbdmf dzfkgs zrmnnk ngjqpc bcdgf xtr jdqq rcn tbbxv lgbx bzgvb jpthp jsgh zpnnqqtf vndrb kqcjqg hvh dhz mrtxr tj skvmp zqgj fnpgz xhrdsl cnjrhn xlndgbp frb pprlrf bvcrrfbr lbnmsr mrl dhbxtb pshhhvn dphrq krmgj hbfrlp mqhdrfd mrztqsk mjnvk cvbsp jzx mvkz xcgtv qqvjm (contains dairy, wheat, peanuts)
vsb dlf sncd nfqhnxd lgbx rqjm jknjtt xzffgv plvrn nlxn mrztqsk blslf rchkc qbxzqr dqvct zfq trsg tptzp lbnmsr ndcgcv khlh hkpdd cjztt scnzhj kqcjqg pst xcj skvmp scxxn rlfvz bhjv mgbddhr xhrdsl clmllf fnccgnm qqbsfd rfdv zfhs clz pjqzb lnkz zlnpt bvcrrfbr bcdgf nqztms qqvjm pmtk fdvfx mdttst dhbxtb vndrb xpnf qbbv zbtfgr ldlpt mcdt rxcmb xtx lmn fnms spbl tcxcg dfsrz jgrptg gdmrc shjl pjfbh (contains sesame, fish, soy)
qjt bmlfmj frtgq pmtk zxqjg bjdqnts rxcmb vndrb qrtmdt lbvkccl krmgj bzgvb lmn dzfkgs bcdgf gmn klmrg qjfx xtr frb qbbv cvbsp zfhs mcdt zqgj pprlrf gxhj dhbxtb dblzkk qqvjm jpthp rpsgs jzx rqjm hgp gcqpb hphzkcf xztmx bvcrrfbr mrtxr dlnq nzcr xtx scxxn sncd clh nlxn ndcgcv zlxl plvrn lgbx bkbdt jsgh xcj pmfrjm tj nfbx pst bj rlfvz hzllnq xpnf tcxcg mxfnql dqxk sdpntg fdqt cqmlxdl cqncf gjnt xhrdsl lnkz lbnmsr dfsrz mxpj trsg vsb zbtfgr jknjtt dtkn jdqq (contains soy)
cvbsp mrtxr frb kzhtf hvh pmfrjm cqmlxdl xdrxx tgdrc kflqj mgbddhr vsb jdqq gmn bcdgf vndrb qvbrx zlxl scxxn dtkn qqvjm skvmp gcqpb htvrgj bvcrrfbr lbnmsr qrtmdt scnzhj mdttst jfdn bj gnt bjdqnts rlfvz zpnnqqtf nfqhnxd xhrdsl dlf kfrv tzxkmq dfsrz dhz dhbxtb tj kcbdmf pjqzb (contains fish)
gptkkf jtrscmz qvbrx jfdn vxvhx krmgj mrl plvrn scxxn ndcgcv fdvfx rpsgs fxtrjkp ngjqpc gbmk gxhj jdqq ptmqj pjfbh mcnrg qqvjm bj mgb jgrptg bvcrrfbr scnzhj jknjtt dhbxtb dncrcx hphzkcf cqmlxdl xpnf dtkk nqztms dqxk fnms bcdgf xcgtv dtkn mqhdrfd pst xztmx zlxl lfxnbq lbnmsr jpthp zrmnnk pshhhvn qkdrpq khlh clz bkbdt vndrb fnpgz hvcfm hkpdd qrtmdt fxhp rcn kmt mhncmb skvmp gmn tkcs frtgq (contains sesame)
bcdgf lfxnbq sdpntg nqztms dqxk lnkz tkcs pst sncd qvbrx scxxn pmfrjm klmrg xcj jgrptg dqvct ndcgcv bsgsd bmlfmj pprlrf gjnt nfbx spbl mjnvk mxfnql xrhbc rgxggd xztmx zfq blrj clh lsfsnb dhz gjkgm clmllf cqmlxdl zrmnnk cnjrhn kcbdmf hkk qqbsfd hvh bhck rqjm ltxx ggfr skvmp gcqpb svtjgr kzhtf rxcmb bj qmcv zqk dhbxtb trsg bzgvb xcgtv dch pjqzb rchkc mgbddhr qgdmfm hvcfm ngjqpc bvcrrfbr tzxkmq gxhj xhrdsl vndrb (contains soy)
dqvct mgbddhr sdpntg cqncf mcdt xmgmvpzx cqmlxdl kflqj gjnt gxhj xztmx tcxcg nfqhnxd gcqpb scxxn mrl clz qkdrpq mxfnql svtjgr bcdgf zlxl zrmnnk blrj dch pjfbh bhck mhncmb qjfx vndrb dhbxtb kgzcf jzx qrqc hzllnq zfhs ndcgcv qrtmdt mcnrg mjnvk dqxk bvcrrfbr xzcq xhrdsl xcgtv cvbsp dblzkk mdttst kqcjqg (contains fish, shellfish)
qqbsfd vsb mqhdrfd scxxn xhrdsl cvbsp bdq kflqxq zlnpt tptzp dhz skvmp qqvjm krmgj xcgtv lsfsnb bzgvb gbmk fnpgz xpnf tzxkmq dblzkk scnzhj shjl dhbxtb vndrb pmfrjm mxfnql qmcv lnkz klmrg lgbx xlhrqfjb gjnt zgsr clmllf dlf tcxcg pjqzb zbtfgr xzcq lbnmsr zlxl bmlfmj qgdmfm mvkz rcn kcbdmf pprlrf clh mcnrg zfq zrmnnk sncd scgpv pmtk gxhj mdttst zqgj mhncmb kmt qkdrpq qbbv rqjm bvcrrfbr qrqc bhck dphrq (contains fish, soy, peanuts)
zfq jfdn dqxk lsfsnb fnpgz bhck dtkk mxpj kqcjqg bjdqnts rgxggd rchkc mjnvk ltxx clh lnkz qkdrpq kzhtf rrgczb hbfrlp xmgmvpzx lpjjd pprlrf shjl lgbx bvcrrfbr mhncmb xhrdsl zlxl xdrxx kflqj mgbddhr tbbxv bdq lmn xcgtv zrmnnk zlnpt jpx vsb pmfrjm nfbx qqvjm mcnrg xtr nlxn dzps jdv clmllf zpnnqqtf sdpntg gdmrc xpnf zxqjg nzcr rcn gptkkf rxcmb mcdt dtkn spbl hphzkcf rlfvz scxxn xrhbc lbnmsr vndrb ngthq dhz qjt dhbxtb jgrptg qqbsfd xlhrqfjb hvcfm ggfr zgsr xlndgbp (contains peanuts)
xlhrqfjb bjdqnts vxvhx lbnmsr fnpgz hkpdd blrj gxhj mxpj rlfvz xzffgv gcqpb qvbrx qrqc zrmnnk qbbv mxfnql qjt ptmqj dhbxtb jsgh bcdgf khlh dncrcx rchkc xcgtv mhncmb shjl rgxggd pst dzfkgs kflqj lsfsnb kflqxq mqhdrfd jpx xcj lpjjd pmtk sdpntg tgdrc cqncf dlnq zbtfgr klmrg drtv ngjqpc cqmlxdl tcxcg mcdt bvcrrfbr xzcq bmlfmj gjkgm rrgczb jzx xtx ggfr xhrdsl mtqtmp fnms krmgj bkbdt ldlpt rqjm scxxn qmcv xmgmvpzx (contains wheat, dairy)
fdqt cjztt nlxn scxxn xdrxx nzcr mvkz sbrmrp bjdqnts tzxkmq lbnmsr qjt rqjm bvcrrfbr tcxcg qrqc tgdrc fnms dphrq lbvkccl svtjgr scnzhj frtgq hvh jdv kflqj lgbx kgzcf dfsrz plvrn dlnq zrmnnk dtkn ngjqpc krmgj gdmrc clh jsgh xcgtv mjnvk qgdmfm xpnf zfhs rfdv fnccgnm shjl xlndgbp dqxk lpjjd jfdn tptzp htvrgj fxhp dtkk xhrdsl dch mxfnql qbbv dhbxtb mgb bj bcdgf gxhj gnt jpthp jzx (contains dairy)
lbnmsr scxxn pjqzb sdpntg dhbxtb cjztt bcdgf mrtxr tzxkmq dblzkk zlxl mcdt ndcgcv dtkk kflqxq rchkc klmrg hvh bkbdt pshhhvn rrgczb vndrb mrl ldlpt htvrgj ndmmhv lgbx rlfvz clh xzcq blrj jpthp rfdv xtr bsgsd kqcjqg xcj xhrdsl kgzcf zfhs xcgtv qrqc nqztms gjkgm jsgh gptkkf (contains fish, dairy, nuts)
mrl clmllf dzps blrj bhck mgb hvh blslf rlfvz xhrdsl pjfbh qjt sbrmrp cnjrhn qbbv xzffgv krmgj qkdrpq bcdgf ltxx dqvct gjnt zqk vndrb pst jfdn xcgtv mcnrg hkk lmn dblzkk kflqxq jsgh bdq mvkz nfqhnxd plvrn mrztqsk jknjtt klmrg xdrxx gmn xtr mqhdrfd qqvjm jpthp rcn mnvmh clh rqjm fxtrjkp dzfkgs shjl mxfnql xztmx lgbx nqztms zlnpt zfhs tcxcg pjqzb qvbrx mxpj fscs skvmp fxhp gxhj jgrptg mgbddhr dfsrz zrmnnk dhbxtb pmtk kflqj lfxnbq mtqtmp fnccgnm scxxn xlhrqfjb mhncmb lbnmsr sncd tzxkmq (contains peanuts, shellfish, wheat)
scxxn lbnmsr pmtk gptkkf xhrdsl nfqhnxd xpnf rrgczb zpnnqqtf mqhdrfd lmn xmgmvpzx bkbdt cjztt kcbdmf jfdn mtqtmp dfsrz mrztqsk hvcfm rcn bkpq qrtmdt qjfx fnccgnm cqmlxdl qrqc mgb plvrn mxfnql nlxn dhz jdqq gdmrc klmrg xzcq rfdv jknjtt dncrcx lfxnbq jpthp drtv zfq hbfrlp kqcjqg tj zqk gxhj hvh pjqzb zbtfgr qgdmfm fscs dhbxtb mgbddhr blslf bdq krmgj qkdrpq rchkc mcdt bcdgf tcxcg ndmmhv dblzkk nfbx kflqj bvcrrfbr pst ngthq clh xtr xdrxx xcgtv sdpntg pprlrf (contains sesame, wheat, shellfish)
bsgsd fnccgnm dhz qvbrx gmn nlxn hkpdd fxtrjkp krmgj xcj xtr bcdgf jdv bzgvb bjdqnts clz gdmrc frb pst mqhdrfd lbnmsr pmfrjm htvrgj xpnf xzcq kflqxq kgzcf mtqtmp jtrscmz blrj qbxzqr qbbv bvcrrfbr qrtmdt kflqj mdttst mgbddhr lpjjd xlhrqfjb dtkk zgsr vndrb nqztms ggfr tgdrc zxqjg gjkgm mcdt hbfrlp bhjv zpnnqqtf fxhp scxxn dncrcx mnvmh bdq gxhj ptmqj pshhhvn rrgczb xhrdsl cqncf dtkn rfdv gcqpb fscs xzffgv pjqzb bj xmgmvpzx tkcs vsb dhbxtb trsg (contains fish)
rlfvz xhrdsl ngjqpc blslf lnkz lbvkccl dzfkgs pprlrf skvmp xmgmvpzx hphzkcf gnt zfq rfdv gjkgm krmgj mcdt pst sncd dhbxtb lbnmsr fnms jpthp dncrcx jdqq vndrb bzgvb xtr dlnq qqvjm mgb bvcrrfbr svtjgr dblzkk qbxzqr ggfr hzllnq scxxn kgzcf clh zlnpt dqxk pmfrjm khlh ltxx scgpv gbmk bcdgf (contains sesame)
mxpj bhjv qjfx dncrcx mcnrg tj gnt frb vsb kgzcf mnvmh fdqt zqk xtr ltxx tptzp mqhdrfd bcdgf mrztqsk rqjm xmncxv bj blslf hkk mgb scxxn xcgtv cvbsp ldlpt dhbxtb nfbx gxhj kfrv xztmx rpsgs lbnmsr bvcrrfbr hgp vndrb ndmmhv khlh lnkz bjdqnts bzgvb cqmlxdl jsgh jpthp mdttst gbmk ngjqpc clz (contains sesame)
xcj xcgtv ndmmhv vxvhx bsgsd xhrdsl jdv zpnnqqtf zxqjg kmt shjl sncd mnvmh kgzcf qvbrx dhz ngthq tkcs gcqpb lsfsnb clz ngjqpc bzgvb lbnmsr xzcq mxpj zrmnnk dhbxtb cjztt rcn hkk fxhp zfq tj spbl gnt qqbsfd bcdgf vndrb mrtxr jtrscmz mqhdrfd hgp jpthp rpsgs zgsr bvcrrfbr zqk pjqzb (contains sesame, nuts)
tkcs xhrdsl ndmmhv tcxcg pprlrf jgrptg dhbxtb fnms lbvkccl pst gdmrc nfqhnxd bkpq bhck rrgczb sbrmrp kmt scxxn cnjrhn qbxzqr fscs bvcrrfbr hzllnq xdrxx xtx clz bcdgf dzfkgs mhncmb zbtfgr dblzkk tzxkmq hphzkcf vndrb scnzhj plvrn xzffgv xcgtv dlnq gmn qgdmfm vsb skvmp shjl blrj nzcr jzx rcn xlhrqfjb (contains shellfish)
shjl zgsr fdqt fxtrjkp qkdrpq zpnnqqtf qjt vndrb dhbxtb xcj tzxkmq mcdt mcnrg svtjgr qqbsfd zxqjg blrj dblzkk lbvkccl qjfx bkbdt vsb scxxn sbrmrp nfbx gxhj xrhbc lfxnbq lnkz bcdgf xtr rpsgs nlxn xdrxx lsfsnb xmncxv jzx xhrdsl khlh jsgh jtrscmz xcgtv tj gjnt qrqc zlnpt bj dzps tbbxv mjnvk mrtxr clh lmn pmtk bmlfmj jdqq gcqpb dhz cqmlxdl xzffgv gnt rfdv bvcrrfbr dtkk jpx zlxl jfdn tcxcg (contains dairy, wheat, peanuts)
fnpgz rrgczb pprlrf qjt frtgq lnkz dncrcx mxpj dlf rgxggd rcn dzfkgs kflqxq tzxkmq jdqq rchkc fxhp drtv xtx bhck hphzkcf lpjjd sbrmrp scgpv lmn kmt hbfrlp dqxk trsg scxxn xrhbc vsb mvkz qqbsfd tcxcg bcdgf gbmk clh ndcgcv mxfnql pst sncd xcgtv bjdqnts ngjqpc mjnvk nqztms zqk kcbdmf clmllf mgb lsfsnb fnms bvcrrfbr dch spbl xlhrqfjb qgdmfm bkbdt tbbxv scnzhj mqhdrfd tgdrc lbnmsr cqncf dhbxtb hkk xpnf vndrb jpx (contains wheat, sesame, shellfish)
mgbddhr dzps vndrb qjfx tcxcg scxxn xmgmvpzx xhrdsl krmgj tbbxv gcqpb qrqc pjfbh jpx ngjqpc nqztms ptmqj frb rgxggd zrmnnk zpnnqqtf rfdv dqvct dch ndmmhv shjl mxpj rrgczb dblzkk sbrmrp bjdqnts fnpgz qmcv dhbxtb lpjjd xlndgbp gjkgm bhjv clz qbxzqr clmllf trsg nfqhnxd svtjgr hphzkcf bcdgf dlnq qvbrx rcn dtkn pmtk xtr kmt dqxk rlfvz lnkz scgpv xpnf cqncf mgb zqgj xcj pprlrf qrtmdt jzx qgdmfm dphrq cjztt dfsrz scnzhj fnms fxtrjkp xcgtv dtkk mxfnql fnccgnm bvcrrfbr cnjrhn bkbdt nfbx pjqzb sdpntg (contains dairy, nuts)
pjfbh fnccgnm xcj xtr xhrdsl qkdrpq qqbsfd bvcrrfbr scxxn zlnpt dlnq mgbddhr hkpdd rlfvz zbtfgr mrztqsk gptkkf pst htvrgj xmncxv dqxk hvh vndrb hzllnq ngthq mdttst qjfx blrj dhbxtb hvcfm gdmrc mrl hphzkcf mnvmh jsgh hgp tptzp dncrcx mtqtmp frtgq plvrn bhck bcdgf bdq skvmp khlh gmn xmgmvpzx cnjrhn zqgj mgb clmllf rqjm lbnmsr dch jpthp ptmqj fxhp zrmnnk ggfr (contains dairy)
bvcrrfbr bcdgf gjkgm mhncmb dzps dlnq clmllf xrhbc kflqxq zqgj fxtrjkp mjnvk xzffgv mnvmh dhbxtb zfq rcn gptkkf xlhrqfjb khlh fdqt ldlpt zbtfgr lbnmsr gdmrc scgpv mtqtmp qbbv bjdqnts cjztt pjqzb qgdmfm fxhp gjnt qbxzqr tbbxv tzxkmq rxcmb kzhtf xdrxx scxxn lbvkccl dncrcx skvmp jknjtt cnjrhn qmcv sncd rfdv ndmmhv hkpdd bhjv ngjqpc sdpntg sbrmrp xhrdsl dqxk xcgtv nqztms lnkz jzx (contains wheat)
ndcgcv frb mcnrg spbl dfsrz gjkgm dhbxtb lbnmsr rcn sbrmrp zqk plvrn qrtmdt dblzkk qmcv gnt tj bkpq jgrptg scxxn qvbrx pjfbh jpx mrztqsk sncd hkk bsgsd gjnt qqvjm bcdgf jzx nfbx dzfkgs bvcrrfbr lmn xdrxx kzhtf clz xcgtv xmncxv bzgvb dncrcx xhrdsl (contains peanuts, dairy)
bcdgf mrl zlxl xhrdsl fnccgnm zqgj vxvhx kzhtf kqcjqg mqhdrfd klmrg dfsrz scxxn spbl fdvfx frb blslf bhjv xdrxx lbnmsr mxfnql gnt blrj sncd mcnrg xzcq rxcmb hkk ptmqj frtgq tj zxqjg bjdqnts qmcv qgdmfm rrgczb bkpq nzcr hphzkcf drtv khlh pprlrf dhbxtb cjztt pjqzb jdv rqjm jpthp dtkk clz vndrb pst ldlpt dqxk jzx tkcs bzgvb xzffgv bvcrrfbr gbmk xmncxv qvbrx kmt scgpv svtjgr krmgj xlndgbp qjt rchkc plvrn shjl hbfrlp bhck mgbddhr (contains nuts, shellfish, fish)
xmgmvpzx lbnmsr xcgtv cqncf mrtxr vsb dhbxtb sncd jdv scxxn rrgczb mgb fxhp xzcq dlnq xtx mhncmb fnms ndmmhv bvcrrfbr lgbx mrztqsk ngthq hkpdd mxpj lfxnbq bhck gjkgm tptzp plvrn mnvmh kmt cnjrhn kqcjqg cjztt pshhhvn vndrb fnccgnm nqztms dch jzx frb hphzkcf hbfrlp fxtrjkp gptkkf jknjtt xpnf bcdgf (contains peanuts, dairy, shellfish)
pst zqgj xzcq ndcgcv mjnvk xlhrqfjb clh gjkgm qqvjm qgdmfm xcgtv nzcr dphrq qvbrx pjfbh drtv xhrdsl xzffgv vndrb qrtmdt lbnmsr kmt bcdgf rlfvz jdv dhbxtb pmtk qjt tzxkmq svtjgr sbrmrp dlf zxqjg hvh mdttst qbbv lgbx scxxn dlnq cqmlxdl fnccgnm nlxn clmllf jpx pprlrf bsgsd ndmmhv (contains wheat, fish)
bmlfmj ngjqpc hzllnq xcj mgb mxpj cvbsp jpthp hgp jdqq xzcq krmgj tptzp mcnrg jknjtt zgsr xztmx kgzcf zxqjg xrhbc clmllf ndmmhv ndcgcv xtr mqhdrfd zqgj bvcrrfbr zlxl xcgtv lfxnbq cnjrhn fdqt vxvhx scxxn lbnmsr tzxkmq jzx bcdgf lmn xhrdsl kzhtf zqk hbfrlp mjnvk dhbxtb dqvct nzcr gbmk pprlrf hkk tbbxv jdv mrtxr zrmnnk xdrxx hkpdd xmgmvpzx shjl svtjgr pjfbh (contains fish, soy, peanuts)
gcqpb dzps vndrb bjdqnts xcj bvcrrfbr xpnf qvbrx lfxnbq nlxn mdttst hkk hphzkcf nzcr xcgtv ggfr xmncxv bcdgf jdqq mrztqsk fscs tj blslf dch tcxcg dqvct dhz dhbxtb xzcq dblzkk qrqc dtkn rcn gnt mrtxr xhrdsl zrmnnk kflqxq nfbx lbnmsr cvbsp zpnnqqtf shjl dphrq rchkc tkcs krmgj (contains dairy)
"""
