(*

    WinBox Reloaded R2 - An universal GUI for many emulators

    Copyright (C) 2020, Laci bá'

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*)

unit uWinBoxLib;

interface

uses Classes;

type
  TDiskGeometry = record
    C, H, S: integer;
    function IsEmpty: boolean;
    function Size: uint64;
  end;

  TDiskData = record
    dgPhysicalGeometry,
    dgTranslatedGeometry: TDiskGeometry;
    dwNominalSize, dwLandZone, dwWritePreComp: integer;
    szManufacturer: array [0..34] of char;
    szModel: array [0..29] of char;
    szConnector: array [0..19] of char;
  end;

  ISelectHDD = interface
    ['{BCC01793-CEDA-4491-B1FF-98F9723BB6AA}']
    function Execute(const Silent: boolean = false): boolean; stdcall;
    function GetDiskData: TDiskData; stdcall;
    procedure SetDiskData(const Data: TDiskData); stdcall;
    procedure SetConnectorFilter(const Lock: boolean); stdcall;
    procedure SetCHSFilter(const Enabled: boolean); stdcall;

    function LocatePhysCHS: boolean; stdcall;
    function LocateTransCHS: boolean; stdcall;

    property DiskData: TDiskData read GetDiskData write SetDiskData;
  end;

  IAutoUpdate = interface
    ['{768238E2-A485-4DE5-AEA8-51E506DD81BC}']
    function Execute(const ByCommand: boolean = false): boolean; stdcall;
    function HasUpdate: boolean; stdcall;
    function AutoUpdate: boolean; stdcall;
  end;

  IWizardHDD = interface
    ['{57E94996-C896-45BD-811D-08BA692D0B49}']
    function GetIsVHD: boolean; stdcall;
    procedure SetIsVHD(const Value: boolean); stdcall;
    function GetSparse: boolean; stdcall;
    procedure SetSparse(const Value: boolean); stdcall;
    function GetFileName: PChar; stdcall;
    procedure SetFileName(const Value: PChar); stdcall;
    function GetDiskData: TDiskData; stdcall;
    procedure SetDiskData(const Value: TDiskData); stdcall;
    procedure SetConnectorFilter(const Lock: boolean); stdcall;
    function Execute(const AutoCreate: boolean): boolean; stdcall;
    function TryCreate: boolean; stdcall;

    property Sparse: boolean read GetSparse write SetSparse;
    property FileName: PChar read GetFileName write SetFileName;
    property DiskData: TDiskData read GetDiskData write SetDiskData;
  end;

  IWizardVM = interface
    ['{1AD7EC11-8D44-4A44-8D73-13C29AA4A33C}']
    function GetProfileID: PChar; stdcall;
    function GetWorkingDirectory: PChar; stdcall;
    procedure SetWorkingDirectory(const Value: PChar); stdcall;
    function GetFriendlyName: PChar; stdcall;
    function GetDiskData: TDiskData; stdcall;
    function GetOpenSettings: boolean; stdcall;
    procedure SetFriendlyName(const Value: PChar); stdcall;

    procedure RecreateProfileID; stdcall;

    function Execute(const AutoCreate: boolean): boolean; stdcall;
    function TryCreate: boolean; stdcall;

    property DiskData: TDiskData read GetDiskData;
    property WorkingDirectory: PChar read GetWorkingDirectory write SetWorkingDirectory;
    property ProfileID: PChar read GetProfileID;
    property FriendlyName: PChar read GetFriendlyName write SetFriendlyName;
    property OpenSettings: boolean read GetOpenSettings;
  end;

const
  libWinBox = 'libWinBox.dll';

function CreateWizardVM(const AOwner: TComponent): IWizardVM; stdcall; external libWinBox;
function CreateWizardHDD(const AOwner: TComponent): IWizardHDD; stdcall; external libWinBox;
function CreateSelectHDD(const AOwner: TComponent): ISelectHDD; stdcall; external libWinBox;
function CreateAutoUpdate(const AOwner: TComponent): IAutoUpdate; stdcall; external libWinBox;
procedure SetLanguage(const AFileName, ALocale: PChar); stdcall; external libWinBox;

implementation

{ TDiskGeometry }

function TDiskGeometry.IsEmpty: boolean;
begin
  Result := (C = 0) or (H = 0) or (S = 0);
end;

function TDiskGeometry.Size: uint64;
begin
  Result := uint64(C) * longword(H) * longword(S) * 512;
end;

end.
