{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

{
Contribution Credits
--------------------
- Pavel Stugel - revisions for support of older Delphi versions and FPC
}


Unit MarkdownProcessor;

interface

uses
  SysUtils;

Type
  EMarkdownProcessor = class (Exception);

  TMarkdownProcessor = class abstract
  private
    FOpenLinksInNewWindow: boolean;
  protected
    function GetAllowUnSafe: boolean; virtual; abstract;
    procedure SetAllowUnSafe(const Value: boolean); virtual; abstract;
  public
    // when AllowUnsafe = true, then the processor can create scripts etc.
    property AllowUnsafe : boolean read GetAllowUnSafe write SetAllowUnSafe;
    property OpenLinksInNewWindow : boolean read FOpenLinksInNewWindow write FOpenLinksInNewWindow;
    function process(source : String) : String; virtual; abstract;
  end;

implementation

end.
