function [waveleng,readings]=ReadOceanOpticsData(fnam)
%
%  Reads Ocean Optics scan for Roesler 
%  
%  Lines 1-17 = skip these
%  Line 18-2065 = The wavelength and data from Ocean Optics
%  Line 2066 = last line
%
% INPUTS:
%   fnam = character array listing the directory and filename of the Ocean
%   Optics data to read.  Example in matlab:
%       >> fnam='c:\ProfilerData\04April2007\51336870_PPB_CTD.txt';
%
% OUTPUTS:
%   waveleng = wavelengths
%   readings = output readings from Ocean Optics.
%
% Written by:
%  Andrew Barnard, WET Labs, Inc (andrew@wetlabs.com)
% Version: 1.1
%;

% Open the input file
fid=fopen(fnam);

% Skip past the first 17 lines of the data.  Note I don't check these lines
% at the moment, I just ignore them.
ff=fgetl(fid);  % Line 1
ff=fgetl(fid);  % Line 2
ff=fgetl(fid);  % Line 3
ff=fgetl(fid);  % Line 4
ff=fgetl(fid);  % Line 5
ff=fgetl(fid);  % Line 6
ff=fgetl(fid);  % Line 7
ff=fgetl(fid);  % Line 8
ff=fgetl(fid);  % Line 9
ff=fgetl(fid);  % Line 10
ff=fgetl(fid);  % Line 11
ff=fgetl(fid);  % Line 12
ff=fgetl(fid);  % Line 13
ff=fgetl(fid);  % Line 14
ff=fgetl(fid);  % Line 15
ff=fgetl(fid);  % Line 16
ff=fgetl(fid);  % Line 17
tmp=textscan(fid,'%f%f'); % Sets the format, reads 2 floating points to the end of the file
fclose(fid);  % close the file

waveleng=cell2mat(tmp(1))'; % set the
readings=cell2mat(tmp(2))';


