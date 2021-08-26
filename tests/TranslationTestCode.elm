module TranslationTestCode exposing (..)

{-| -}


{-| Code from <https://github.com/MartinSStewart/translation-test>
-}
code : String
code =
    """module A exposing (..)


englishTexts =
   { form =
       { emailAddressNotValid = "This email is not valid"
       , emailAddressText = "E-mail"
       , termsOfService = \\urls -> "By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , form3 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail17"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , form4 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail15"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , form5 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail13"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , form6 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail11"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   }


swedishTexts =
   { form =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail9"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , emailSentInstructions = "(Swedish) If you didn’t receive an e-mail from us, check your **Spam folder**, and make sure that the e-mail address is correctly entered."
   , form2 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail8"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , form3 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail7"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , form4 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail5"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , form5 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail3"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , form6 =
       { emailAddressNotValid = "(Swedish) This email is not valid"
       , emailAddressText = "(Swedish) E-mail1"
       , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
       }
   , success =
       { checkYourEmail = "(Swedish) Check your e-mail"
       , emailSent = \\email -> "(Swedish) We have sent an e-mail to **" ++ Email.toString email ++ \"\"\"**.

                 Click the button in the email to log in.\"\"\"
       , form3 =
           { emailAddressNotValid = "(Swedish) This email is not valid"
           , emailAddressText = "🚧"
           , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
           }
       , success =
           { checkYourEmail = ""
           , emailSent = \\email -> Email.toString email
           }
       }
   }

"""
