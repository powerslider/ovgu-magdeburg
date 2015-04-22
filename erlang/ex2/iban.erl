%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc IBAN Validation
%% @see http://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN

-module(iban).
-export([splitIBAN/1, validateIBAN/1]).

splitIBAN(String) ->
    { match, [ CountryCode, Checksum, BBAN ] } = 
        re:run(String, "([A-Z]{2})([0-9]{2})([0-9]{18})",
               [{capture, all_but_first, list}]),
    { CountryCode, Checksum, BBAN }.

validateIBAN(IBANTuple) -> 
    CountryCode = element(1, IBANTuple),
    CountryCodeCheckDigits = asciiIBAN(CountryCode),

    Checksum = element(2, IBANTuple),
    BBAN = element(3, IBANTuple),

    IBANCheckDigits = BBAN ++ CountryCodeCheckDigits ++ Checksum,
    list_to_integer(IBANCheckDigits) rem 97.

asciiIBAN(CountryCode) ->
     lists:concat(
       lists:map(
         fun(C) -> C - 55 end, CountryCode ++ [])).




