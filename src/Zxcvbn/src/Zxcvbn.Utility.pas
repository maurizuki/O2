unit Zxcvbn.Utility;

interface
uses
  System.Classes,
  System.SysUtils,
  Zxcvbn.Result;

resourcestring
	SZxcvbnInstant = 'instant';
	SZxcvbnMinutes = 'minutes';
	SZxcvbnHours = 'hours';
	SZxcvbnDays = 'days';
	SZxcvbnMonths = 'months';
	SZxcvbnYears = 'years';
	SZxcvbnCenturies = 'centuries';

	SZxcvbnStraightRow = 'Straight rows of keys are easy to guess.';
	SZxcvbnShortKeyboardPatterns = 'Short keyboard patterns are easy to guess.';
	SZxcvbnRepeatsLikeAaaEasy = 'Repeats like "aaa" are easy to guess.';
	SZxcvbnRepeatsLikeAbcSlighterHarder = 'Repeats like "abcabcabc" are only slightly harder to guess than "abc".';
	SZxcvbnSequenceAbcEasy = 'Sequences like abc or 6543 are easy to guess.';
	SZxcvbnRecentYearsEasy = 'Recent years are easy to guess.';
	SZxcvbnDatesEasy = 'Dates are often easy to guess.';
	SZxcvbnTop10Passwords = 'This is a top-10 common password.';
	SZxcvbnTop100Passwords = 'This is a top-100 common password.';
	SZxcvbnCommonPasswords = 'This is a very common password.';
	SZxcvbnSimilarCommonPasswords = 'This is similar to a commonly used password.';
	SZxcvbnWordEasy = 'A word by itself is easy to guess.';
	SZxcvbnNameSurnamesEasy = 'Names and surnames by themselves are easy to guess.';
	SZxcvbnCommonNameSurnamesEasy = 'Common names and surnames are easy to guess.';

	SZxcvbnAddAnotherWordOrTwo = 'Add another word or two. Uncommon words are better.';
	SZxcvbnUseLongerKeyboardPattern = 'Use a longer keyboard pattern with more turns.';
	SZxcvbnAvoidRepeatedWordsAndChars = 'Avoid repeated words and characters.';
	SZxcvbnAvoidSequences = 'Avoid sequences.';
	SZxcvbnAvoidYearsAssociatedYou = 'Avoid recent years and years that are associated with you.';
	SZxcvbnAvoidDatesYearsAssociatedYou = 'Avoid dates and years that are associated with you.';
	SZxcvbnCapsDontHelp = 'Capitalization doesn''t help very much.';
	SZxcvbnAllCapsEasy = 'All-uppercase is almost as easy to guess as all-lowercase.';
	SZxcvbnReversedWordEasy = 'Reversed words aren''t much harder to guess.';
	SZxcvbnPredictableSubstitutionsEasy = 'Predictable substitutions like ''@'' instead of ''a'' don''t help very much.';

  /// <summary>
  /// Convert a number of seconds into a human readable form. Rounds up.
  /// To be consistent with zxcvbn, it returns the unit + 1 (i.e. 60 * 10 seconds = 10 minutes would come out as "11 minutes"
  /// this is probably to avoid ever needing to deal with plurals
  /// </summary>
  /// <param name="ASeconds">The time in seconds</param>
  /// <returns>A human-friendly time string</returns>
  function DisplayTime(ASeconds: Double): String;

  /// <summary>
  /// Reverse a string in one call
  /// </summary>
  /// <param name="AStr">String to reverse</param>
  /// <returns>String in reverse</returns>
  function StringReverse(const AStr: String): String;

  /// <summary>
  /// A convenience for parsing a substring as an int and returning the results. Uses TryStrToInt, and so returns zero where there is no valid int
  /// </summary>
  /// <param name="AStr">String to get substring of</param>
  /// <param name="AStartIndex">Start index of substring to parse</param>
  /// <param name="ALength">Length of substring to parse</param>
  /// <param name="AResult">Substring parsed as int or zero</param>
  /// <returns>True if the parse succeeds</returns>
  function IntParseSubstring(const AStr: String; AStartIndex, ALength: Integer; out AResult: Integer): Boolean;

  /// <summary>
  /// Quickly convert a string to an integer, uses TryStrToInt so any non-integers will return zero
  /// </summary>
  /// <param name="AStr">String to parse into an int</param>
  /// <returns>Parsed int or zero</returns>
  function ToInt(const AStr: String): Integer;

  /// <summary>
  /// Returns a list of the lines of text from an embedded resource in the assembly.
  /// </summary>
  /// <param name="AResourceName">The name of the resource to get the contents of</param>
  /// <returns>A string list of text in the resource or nil if the resource does not exist</returns>
  function GetEmbeddedResourceLines(AResourceName: String): TStringList;

  /// <summary>
  /// Get a translated string of the Warning
  /// </summary>
  /// <param name="AWarning">Warning enum to get the string from</param>
  /// <returns>Warning string in the right language</returns>
  function GetWarning(AWarning: TZxcvbnWarning): String;

  /// <summary>
  /// Get a translated string of the Suggestion
  /// </summary>
  /// <param name="ASuggestion">Suggestion enum to get the string from</param>
  /// <returns>Suggestion string in the right language</returns>
  function GetSuggestion(ASuggestion: TZxcvbnSuggestion): String;

implementation
uses
  Winapi.Windows,
  System.StrUtils,
  System.Math;

function DisplayTime(ASeconds: Double): String;
var
  minute, hour, day, month, year, century: Int64;
begin
  minute := 60;
  hour := minute * 60;
  day := hour * 24;
  month := day * 31;
  year := month * 12;
  century := year * 100;

  if (ASeconds < minute) then Result := SZxcvbnInstant
  else if (ASeconds < hour) then Result := Format('%d %s', [1 + Ceil(ASeconds / minute), SZxcvbnMinutes])
  else if (ASeconds < day) then Result := Format('%d %s', [1 + Ceil(ASeconds / hour), SZxcvbnHours])
  else if (ASeconds < month) then Result := Format('%d %s', [1 + Ceil(ASeconds / day), SZxcvbnDays])
  else if (ASeconds < year) then Result := Format('%d %s', [1 + Ceil(ASeconds / month), SZxcvbnMonths])
  else if (ASeconds < century) then Result := Format('%d %s', [1 + Ceil(ASeconds / year), SZxcvbnYears])
  else Result := SZxcvbnCenturies;
end;

function StringReverse(const AStr: String): String;
begin
  Result := ReverseString(AStr);
end;

function IntParseSubstring(const AStr: String; AStartIndex, ALength: Integer; out AResult: Integer): Boolean;
begin
  Result := TryStrToInt(AStr.Substring(AStartIndex, ALength), AResult);
end;

function ToInt(const AStr: String): Integer;
var
  r: Integer;
begin
  r := 0;
  TryStrToInt(AStr, r);
  Result := r;
end;

function GetEmbeddedResourceLines(AResourceName: String): TStringList;
var
  rs: TResourceStream;
  lines: TStringList;
begin
  Result := nil;

  if (FindResource(hInstance, PChar(AResourceName), RT_RCDATA) = 0) then Exit;

  rs := TResourceStream.Create(hInstance, AResourceName, RT_RCDATA);
  try
    lines := TStringList.Create;
    lines.LoadFromStream(rs);
    Result := lines;
  finally
    rs.Free;
  end;
end;

function GetWarning(AWarning: TZxcvbnWarning): String;
begin
  case AWarning of
    zwStraightRow:
      Result := SZxcvbnStraightRow;
    zwShortKeyboardPatterns:
      Result := SZxcvbnShortKeyboardPatterns;
    zwRepeatsLikeAaaEasy:
      Result := SZxcvbnRepeatsLikeAaaEasy;
    zwRepeatsLikeAbcSlighterHarder:
      Result := SZxcvbnRepeatsLikeAbcSlighterHarder;
    zwSequenceAbcEasy:
      Result := SZxcvbnSequenceAbcEasy;
    zwRecentYearsEasy:
      Result := SZxcvbnRecentYearsEasy;
    zwDatesEasy:
      Result := SZxcvbnDatesEasy;
    zwTop10Passwords:
      Result := SZxcvbnTop10Passwords;
    zwTop100Passwords:
      Result := SZxcvbnTop100Passwords;
    zwCommonPasswords:
      Result := SZxcvbnCommonPasswords;
    zwSimilarCommonPasswords:
      Result := SZxcvbnSimilarCommonPasswords;
    zwWordEasy:
      Result := SZxcvbnWordEasy;
    zwNameSurnamesEasy:
      Result := SZxcvbnNameSurnamesEasy;
    zwCommonNameSurnamesEasy:
      Result := SZxcvbnCommonNameSurnamesEasy;
    else
      Result := '';
  end;
end;

function GetSuggestion(ASuggestion: TZxcvbnSuggestion): String;
begin
  case ASuggestion of
    zsAddAnotherWordOrTwo:
      Result := SZxcvbnAddAnotherWordOrTwo;
    zsUseLongerKeyboardPattern:
      Result := SZxcvbnUseLongerKeyboardPattern;
    zsAvoidRepeatedWordsAndChars:
      Result := SZxcvbnAvoidRepeatedWordsAndChars;
    zsAvoidSequences:
      Result := SZxcvbnAvoidSequences;
    zsAvoidYearsAssociatedYou:
      Result := SZxcvbnAvoidYearsAssociatedYou;
    zsAvoidDatesYearsAssociatedYou:
      Result := SZxcvbnAvoidDatesYearsAssociatedYou;
    zsCapsDontHelp:
      Result := SZxcvbnCapsDontHelp;
    zsAllCapsEasy:
      Result := SZxcvbnAllCapsEasy;
    zsReversedWordEasy:
      Result := SZxcvbnReversedWordEasy;
    zsPredictableSubstitutionsEasy:
      Result := SZxcvbnPredictableSubstitutionsEasy;
    else
      Result := '';
  end;
end;

end.
