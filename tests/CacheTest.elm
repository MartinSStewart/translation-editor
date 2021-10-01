module CacheTest exposing (..)

import Cache
import Expect
import Json.Decode
import Json.Encode
import OldCache
import Serialize
import Test exposing (describe, test)


oldCache =
    """[1,[[3583763863,[["englishTexts",[[["form","termsOfService"],[1,[[[0,"By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[11,37,11,171]]]],[["form","emailAddressText"],[1,[[[0,"E-mail"]],[10,30,10,38]]]],[["form","emailAddressNotValid"],[1,[[[0,"This email is not valid"]],[9,34,9,59]]]],[["form3","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[16,37,16,181]]]],[["form3","emailAddressText"],[1,[[[0,"(Swedish) E-mail17"]],[15,30,15,50]]]],[["form3","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[14,34,14,69]]]],[["form4","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[21,37,21,181]]]],[["form4","emailAddressText"],[1,[[[0,"(Swedish) E-mail15"]],[20,30,20,50]]]],[["form4","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[19,34,19,69]]]],[["form5","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[26,37,26,181]]]],[["form5","emailAddressText"],[1,[[[0,"(Swedish) E-mail13"]],[25,30,25,50]]]],[["form5","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[24,34,24,69]]]],[["form6","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[31,37,31,181]]]],[["form6","emailAddressText"],[1,[[[0,"(Swedish) E-mail11"]],[30,30,30,50]]]],[["form6","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[29,34,29,69]]]]]],["swedishTexts",[[["emailSentInstructions"],[1,[[[0,"(Swedish) If you didn’t receive an e-mail from us, check your **Spam folder**, and make sure that the e-mail address is correctly entered."]],[42,31,42,171]]]],[["form","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[40,37,40,181]]]],[["form","emailAddressText"],[1,[[[0,"(Swedish) E-mail9"]],[39,30,39,49]]]],[["form","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[38,34,38,69]]]],[["form2","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[46,37,46,181]]]],[["form2","emailAddressText"],[1,[[[0,"(Swedish) E-mail8"]],[45,30,45,49]]]],[["form2","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[44,34,44,69]]]],[["form3","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[51,37,51,181]]]],[["form3","emailAddressText"],[1,[[[0,"(Swedish) E-mail7"]],[50,30,50,49]]]],[["form3","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[49,34,49,69]]]],[["form4","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[56,37,56,181]]]],[["form4","emailAddressText"],[1,[[[0,"(Swedish) E-mail5"]],[55,30,55,49]]]],[["form4","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[54,34,54,69]]]],[["form5","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[61,37,61,181]]]],[["form5","emailAddressText"],[1,[[[0,"(Swedish) E-mail3"]],[60,30,60,49]]]],[["form5","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[59,34,59,69]]]],[["form6","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[66,37,66,181]]]],[["form6","emailAddressText"],[1,[[[0,"(Swedish) E-mail1"]],[65,30,65,49]]]],[["form6","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[64,34,64,69]]]],[["success","emailSent"],[1,[[[0,"(Swedish) We have sent an e-mail to **"],[1,[0,[[0,0,0,0,[2,["Email"],"toString"]],[0,0,0,0,[2,[],"email"]]]]],[0,"**.Click the button in the email to log in."]],[70,33,72,61]]]],[["success","checkYourEmail"],[1,[[[0,"(Swedish) Check your e-mail"]],[69,28,69,57]]]],[["success","form3","termsOfService"],[1,[[[0,"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy]("],[1,[19,[0,0,0,0,[2,[],"urls"]],[0,0,0,0,"termsOfService"]]],[0,")."]],[76,41,76,185]]]],[["success","form3","emailAddressText"],[1,[[[0,"🚧"]],[75,34,75,37]]]],[["success","form3","emailAddressNotValid"],[1,[[[0,"(Swedish) This email is not valid"]],[74,38,74,73]]]],[["success","success","emailSent"],[1,[[[1,[0,[[0,0,0,0,[2,["Email"],"toString"]],[0,0,0,0,[2,[],"email"]]]]]],[80,37,80,57]]]],[["success","success","checkYourEmail"],[1,[[[0,""]],[79,32,79,34]]]]]]]]]]"""


tests =
    describe "Cache tests"
        [ test "Old pre-versioning cache should decode with old pre-versioning codec" <|
            \_ ->
                Json.Decode.decodeString Json.Decode.value oldCache
                    |> Debug.log "a"
                    |> Result.withDefault (Json.Encode.object [])
                    |> Serialize.decodeFromJson OldCache.codec
                    |> (\a ->
                            case a of
                                Ok _ ->
                                    Expect.pass

                                Err error ->
                                    Expect.fail "Failed to decode"
                       )
        , test "Old pre-versioning cache should fail to decode" <|
            \_ ->
                Json.Decode.decodeString Json.Decode.value oldCache
                    |> Result.withDefault (Json.Encode.object [])
                    |> Serialize.decodeFromJson Cache.codec
                    |> Expect.equal (Err Serialize.DataCorrupted)
        ]
