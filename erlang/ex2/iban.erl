%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc IBAN Validation
%% @see http://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN

-module(iban).
-export([splitIBAN/1, validateIBAN/1, checksumIBAN/2]).

% function for splitting an IBAN into:
% - country code (e. "DE")
% - checksum (2-digit number after country code)
% - BBAN (the rest 18 digits)
splitIBAN(String) ->
    % use a regex to match the 3 separate parts
    { match, [ CountryCode, Checksum, BBAN ] } = 
        re:run(String, "([a-zA-Z]{2})([0-9]{2})([0-9]{18})",
               % { capture, ValueSpec, Type } 
               % defines what to return from the function upon 
               % successful matching:
               % - capture - defines regex grouping
               % - ValueSpec - which groups to be returned
               % - Type - in what type to return each group (e. list)
               % @see http://erlang.org/doc/man/re.html
               [{capture, all_but_first, list}]),
    { CountryCode, Checksum, BBAN }.

% defines algorithm for IBAN validation
validateIBAN(IBANString) -> 
    % split IBAN to a triple
    IBANTuple = splitIBAN(IBANString),

    % get country code as first element of the tuple
    CountryCode = element(1, IBANTuple),
    % convert it to ascii check digits
    CountryCodeCheckDigits = asciiIBAN(CountryCode),

    % get checksum as second element of the tuple 
    Checksum = element(2, IBANTuple),

    % get BBAN as third element of the tuple
    BBAN = element(3, IBANTuple),

    % rearrange the IBAN components in the order:
    % - BBAN, ASCII country code, checksum
    % and concatenate them
    IBANCheckDigits = BBAN ++ CountryCodeCheckDigits ++ Checksum,

    % convert to integer and calculate rest
    % { check_digits mod 97 } should yield 1
    list_to_integer(IBANCheckDigits) rem 97 == 1.

% defines algorithm for checksum calculation
checksumIBAN(BankCode, AccountNumber) -> 

    PaddedAccountNumber = string:right(AccountNumber, 10, $0),
    CountryCodeCheckDigits = asciiIBAN("DE"),

    % rearrange the IBAN components in the order:
    % - ASCII "DE", bank code, account number, "00"
    % and concatenate them
    IBANCheckDigits = BankCode ++ PaddedAccountNumber ++ CountryCodeCheckDigits ++ "00",

    % convert to integer and calculate rest
    Rest = list_to_integer(IBANCheckDigits) rem 97,

    % calculate checksum
    string:right(integer_to_list(98 - Rest), 2, $0).


% convert country code to (ASCII codes - 55).
% Example: A -> 10, B -> 11 ...
asciiIBAN(CountryCode) ->
     lists:concat(
       lists:map(
         fun(C) -> C - 55 end, CountryCode ++ [])).




