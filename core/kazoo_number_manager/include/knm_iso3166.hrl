%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc defines iso3166 country code mappings
%%% @end
%%%-----------------------------------------------------------------------------
-type country() :: map().

%%------------------------------------------------------------------------------
%% @doc static iso3166 A2 to A3 mapping
%% @end
%%------------------------------------------------------------------------------
-define(A2_DB, [{<<"AW">>, <<"ABW">>}
               ,{<<"AF">>, <<"AFG">>}
               ,{<<"AO">>, <<"AGO">>}
               ,{<<"AI">>, <<"AIA">>}
               ,{<<"AX">>, <<"ALA">>}
               ,{<<"AL">>, <<"ALB">>}
               ,{<<"AD">>, <<"AND">>}
               ,{<<"AE">>, <<"ARE">>}
               ,{<<"AR">>, <<"ARG">>}
               ,{<<"AQ">>, <<"ARM">>}
               ,{<<"AS">>, <<"ARM">>}
               ,{<<"AQ">>, <<"ATA">>}
               ,{<<"TF">>, <<"ATF">>}
               ,{<<"AG">>, <<"ATG">>}
               ,{<<"AU">>, <<"AUS">>}
               ,{<<"AT">>, <<"AUT">>}
               ,{<<"AZ">>, <<"AZE">>}
               ,{<<"BI">>, <<"BDI">>}
               ,{<<"BE">>, <<"BEL">>}
               ,{<<"BJ">>, <<"BEN">>}
               ,{<<"BQ">>, <<"BES">>}
               ,{<<"BF">>, <<"BFA">>}
               ,{<<"BD">>, <<"BGD">>}
               ,{<<"BG">>, <<"BGR">>}
               ,{<<"BH">>, <<"BHR">>}
               ,{<<"BS">>, <<"BHS">>}
               ,{<<"BA">>, <<"BIH">>}
               ,{<<"BL">>, <<"BLM">>}
               ,{<<"BY">>, <<"BLR">>}
               ,{<<"BZ">>, <<"BLZ">>}
               ,{<<"BM">>, <<"BMU">>}
               ,{<<"BO">>, <<"BOL">>}
               ,{<<"BR">>, <<"BRA">>}
               ,{<<"BB">>, <<"BRB">>}
               ,{<<"BN">>, <<"BRN">>}
               ,{<<"BT">>, <<"BTN">>}
               ,{<<"BV">>, <<"BVT">>}
               ,{<<"BW">>, <<"BWA">>}
               ,{<<"CF">>, <<"CAF">>}
               ,{<<"CA">>, <<"CAN">>}
               ,{<<"CC">>, <<"CCK">>}
               ,{<<"CH">>, <<"CHE">>}
               ,{<<"CL">>, <<"CHL">>}
               ,{<<"CN">>, <<"CHN">>}
               ,{<<"CI">>, <<"CIV">>}
               ,{<<"CM">>, <<"CMR">>}
               ,{<<"CD">>, <<"COD">>}
               ,{<<"CG">>, <<"COG">>}
               ,{<<"CK">>, <<"COK">>}
               ,{<<"CO">>, <<"COL">>}
               ,{<<"KM">>, <<"COM">>}
               ,{<<"CV">>, <<"CPV">>}
               ,{<<"CR">>, <<"CRI">>}
               ,{<<"CU">>, <<"CUB">>}
               ,{<<"CW">>, <<"CUW">>}
               ,{<<"CX">>, <<"CXR">>}
               ,{<<"KY">>, <<"CYM">>}
               ,{<<"CY">>, <<"CYP">>}
               ,{<<"CZ">>, <<"CZE">>}
               ,{<<"DE">>, <<"DEU">>}
               ,{<<"DJ">>, <<"DJI">>}
               ,{<<"DM">>, <<"DMA">>}
               ,{<<"DK">>, <<"DNK">>}
               ,{<<"DO">>, <<"DOM">>}
               ,{<<"DZ">>, <<"DZA">>}
               ,{<<"EC">>, <<"ECU">>}
               ,{<<"EG">>, <<"EGY">>}
               ,{<<"ER">>, <<"ERI">>}
               ,{<<"EH">>, <<"ESH">>}
               ,{<<"ES">>, <<"ESP">>}
               ,{<<"EE">>, <<"EST">>}
               ,{<<"ET">>, <<"ETH">>}
               ,{<<"FI">>, <<"FIN">>}
               ,{<<"FJ">>, <<"FJI">>}
               ,{<<"FK">>, <<"FLK">>}
               ,{<<"FR">>, <<"FRA">>}
               ,{<<"FO">>, <<"FRO">>}
               ,{<<"FM">>, <<"FSM">>}
               ,{<<"GA">>, <<"GAB">>}
               ,{<<"GB">>, <<"GBR">>}
               ,{<<"GE">>, <<"GEO">>}
               ,{<<"GG">>, <<"GGY">>}
               ,{<<"GH">>, <<"GHA">>}
               ,{<<"GI">>, <<"GIB">>}
               ,{<<"GN">>, <<"GIN">>}
               ,{<<"GP">>, <<"GLP">>}
               ,{<<"GM">>, <<"GMB">>}
               ,{<<"GN">>, <<"GNB">>}
               ,{<<"GQ">>, <<"GNQ">>}
               ,{<<"GR">>, <<"GRC">>}
               ,{<<"GD">>, <<"GRD">>}
               ,{<<"GL">>, <<"GRL">>}
               ,{<<"GT">>, <<"GTM">>}
               ,{<<"GF">>, <<"GUF">>}
               ,{<<"GU">>, <<"GUM">>}
               ,{<<"GY">>, <<"GUY">>}
               ,{<<"HK">>, <<"HKG">>}
               ,{<<"HM">>, <<"HMD">>}
               ,{<<"HN">>, <<"HND">>}
               ,{<<"HR">>, <<"HRV">>}
               ,{<<"HT">>, <<"HTI">>}
               ,{<<"HU">>, <<"HUN">>}
               ,{<<"ID">>, <<"IDN">>}
               ,{<<"IM">>, <<"IMN">>}
               ,{<<"IN">>, <<"IND">>}
               ,{<<"IO">>, <<"IOT">>}
               ,{<<"IE">>, <<"IRL">>}
               ,{<<"IR">>, <<"IRN">>}
               ,{<<"IQ">>, <<"IRQ">>}
               ,{<<"IS">>, <<"ISL">>}
               ,{<<"IL">>, <<"ISR">>}
               ,{<<"IT">>, <<"ITA">>}
               ,{<<"JM">>, <<"JAM">>}
               ,{<<"JE">>, <<"JEY">>}
               ,{<<"JO">>, <<"JOR">>}
               ,{<<"JP">>, <<"JPN">>}
               ,{<<"KZ">>, <<"KAZ">>}
               ,{<<"KE">>, <<"KEN">>}
               ,{<<"KG">>, <<"KGZ">>}
               ,{<<"KH">>, <<"KHM">>}
               ,{<<"KI">>, <<"KIR">>}
               ,{<<"KN">>, <<"KNA">>}
               ,{<<"KR">>, <<"KOR">>}
               ,{<<"KW">>, <<"KWT">>}
               ,{<<"LA">>, <<"LAO">>}
               ,{<<"LB">>, <<"LBN">>}
               ,{<<"LR">>, <<"LBR">>}
               ,{<<"LY">>, <<"LBY">>}
               ,{<<"LC">>, <<"LCA">>}
               ,{<<"LI">>, <<"LIE">>}
               ,{<<"LK">>, <<"LKA">>}
               ,{<<"LS">>, <<"LSO">>}
               ,{<<"LT">>, <<"LTU">>}
               ,{<<"LU">>, <<"LUX">>}
               ,{<<"LV">>, <<"LVA">>}
               ,{<<"MO">>, <<"MAC">>}
               ,{<<"MF">>, <<"MAF">>}
               ,{<<"MA">>, <<"MAR">>}
               ,{<<"MC">>, <<"MCO">>}
               ,{<<"MD">>, <<"MDA">>}
               ,{<<"MG">>, <<"MDG">>}
               ,{<<"MV">>, <<"MDV">>}
               ,{<<"MX">>, <<"MEX">>}
               ,{<<"MH">>, <<"MHL">>}
               ,{<<"MK">>, <<"MKD">>}
               ,{<<"ML">>, <<"MLI">>}
               ,{<<"MT">>, <<"MLT">>}
               ,{<<"MM">>, <<"MMR">>}
               ,{<<"ME">>, <<"MNE">>}
               ,{<<"MN">>, <<"MNG">>}
               ,{<<"MP">>, <<"MNP">>}
               ,{<<"MZ">>, <<"MOZ">>}
               ,{<<"MR">>, <<"MRT">>}
               ,{<<"MS">>, <<"MSR">>}
               ,{<<"MQ">>, <<"MTQ">>}
               ,{<<"MU">>, <<"MUS">>}
               ,{<<"MW">>, <<"MWI">>}
               ,{<<"MY">>, <<"MYS">>}
               ,{<<"YT">>, <<"MYT">>}
               ,{<<"NA">>, <<"NAM">>}
               ,{<<"NC">>, <<"NCL">>}
               ,{<<"NE">>, <<"NER">>}
               ,{<<"NF">>, <<"NFK">>}
               ,{<<"NG">>, <<"NGA">>}
               ,{<<"NI">>, <<"NIC">>}
               ,{<<"NU">>, <<"NIU">>}
               ,{<<"NL">>, <<"NLD">>}
               ,{<<"NO">>, <<"NOR">>}
               ,{<<"NP">>, <<"NPL">>}
               ,{<<"NR">>, <<"NRU">>}
               ,{<<"NZ">>, <<"NZL">>}
               ,{<<"OM">>, <<"OMN">>}
               ,{<<"PK">>, <<"PAK">>}
               ,{<<"PA">>, <<"PAN">>}
               ,{<<"PN">>, <<"PCN">>}
               ,{<<"PE">>, <<"PER">>}
               ,{<<"PH">>, <<"PHL">>}
               ,{<<"PW">>, <<"PLW">>}
               ,{<<"PG">>, <<"PNG">>}
               ,{<<"PL">>, <<"POL">>}
               ,{<<"PR">>, <<"PRI">>}
               ,{<<"KP">>, <<"PRK">>}
               ,{<<"PT">>, <<"PRT">>}
               ,{<<"PY">>, <<"PRY">>}
               ,{<<"PS">>, <<"PSE">>}
               ,{<<"PF">>, <<"PYF">>}
               ,{<<"QA">>, <<"QAT">>}
               ,{<<"RE">>, <<"REU">>}
               ,{<<"RO">>, <<"ROU">>}
               ,{<<"RU">>, <<"RUS">>}
               ,{<<"RW">>, <<"RWA">>}
               ,{<<"SA">>, <<"SAU">>}
               ,{<<"SD">>, <<"SDN">>}
               ,{<<"SN">>, <<"SEN">>}
               ,{<<"SG">>, <<"SGP">>}
               ,{<<"GS">>, <<"SGS">>}
               ,{<<"SH">>, <<"SHN">>}
               ,{<<"SJ">>, <<"SJM">>}
               ,{<<"SB">>, <<"SLB">>}
               ,{<<"SL">>, <<"SLE">>}
               ,{<<"SV">>, <<"SLV">>}
               ,{<<"SM">>, <<"SMR">>}
               ,{<<"SO">>, <<"SOM">>}
               ,{<<"PM">>, <<"SPM">>}
               ,{<<"RS">>, <<"SRB">>}
               ,{<<"SS">>, <<"SSD">>}
               ,{<<"ST">>, <<"STP">>}
               ,{<<"SR">>, <<"SUR">>}
               ,{<<"SK">>, <<"SVK">>}
               ,{<<"SI">>, <<"SVN">>}
               ,{<<"SE">>, <<"SWE">>}
               ,{<<"SZ">>, <<"SWZ">>}
               ,{<<"SX">>, <<"SXM">>}
               ,{<<"SC">>, <<"SYC">>}
               ,{<<"SY">>, <<"SYR">>}
               ,{<<"TC">>, <<"TCA">>}
               ,{<<"TD">>, <<"TCD">>}
               ,{<<"TG">>, <<"TGO">>}
               ,{<<"TH">>, <<"THA">>}
               ,{<<"TJ">>, <<"TJK">>}
               ,{<<"TK">>, <<"TKL">>}
               ,{<<"TM">>, <<"TKM">>}
               ,{<<"TL">>, <<"TLS">>}
               ,{<<"TO">>, <<"TON">>}
               ,{<<"TT">>, <<"TTO">>}
               ,{<<"TN">>, <<"TUN">>}
               ,{<<"TR">>, <<"TUR">>}
               ,{<<"TV">>, <<"TUV">>}
               ,{<<"TW">>, <<"TWN">>}
               ,{<<"TZ">>, <<"TZA">>}
               ,{<<"UG">>, <<"UGA">>}
               ,{<<"UA">>, <<"UKR">>}
               ,{<<"UM">>, <<"UMI">>}
               ,{<<"UY">>, <<"URY">>}
               ,{<<"US">>, <<"USA">>}
               ,{<<"UZ">>, <<"UZB">>}
               ,{<<"VA">>, <<"VAT">>}
               ,{<<"VC">>, <<"VCT">>}
               ,{<<"VE">>, <<"VEN">>}
               ,{<<"VG">>, <<"VGB">>}
               ,{<<"VI">>, <<"VIR">>}
               ,{<<"VN">>, <<"VNM">>}
               ,{<<"VU">>, <<"VUT">>}
               ,{<<"WF">>, <<"WLF">>}
               ,{<<"WS">>, <<"WSM">>}
               ,{<<"YE">>, <<"YEM">>}
               ,{<<"ZA">>, <<"ZAF">>}
               ,{<<"ZM">>, <<"ZMB">>}
               ,{<<"ZW">>, <<"ZWE">>}
               ]
       ).

%%------------------------------------------------------------------------------
%% @doc defines iso3661 data based keyed off A3
%%
%% Term Format:
%%
%% {A3_CountryCode, {A2_CountryCode, Country_Name}}
%%
%% OR
%%
%% {A3_CountryCode, {A2_CountryCode, Country_Name,  CCTLD_Exception}}
%%
%% CCTLD_Exception is for countries that whose cc tld doesn't map directly to
%% their A2 code(i.e. Great Britain/UK).
%%
%% @end
%%------------------------------------------------------------------------------
-define(A3_DB, [{<<"ABW">>, {<<"AW">>, <<"Aruba">>}}
               ,{<<"AFG">>, {<<"AF">>, <<"Afghanistan">>}}
               ,{<<"AGO">>, {<<"AO">>, <<"Angola">>}}
               ,{<<"AIA">>, {<<"AI">>, <<"Anguilla">>}}
               ,{<<"ALA">>, {<<"AX">>, <<"Åland Islands">>}}
               ,{<<"ALB">>, {<<"AL">>, <<"Albania">>}}
               ,{<<"AND">>, {<<"AD">>, <<"Andorra">>}}
               ,{<<"ARE">>, {<<"AE">>, <<"United Arab Emirates">>}}
               ,{<<"ARG">>, {<<"AR">>, <<"Argentina">>}}
               ,{<<"ARM">>, {<<"AQ">>, <<"Armenia">>}}
               ,{<<"ASM">>, {<<"AS">>, <<"American Samoa">>}}
               ,{<<"ATA">>, {<<"AQ">>, <<"Antarctica">>}}
               ,{<<"ATF">>, {<<"TF">>, <<"French Southern Territories">>}}
               ,{<<"ATG">>, {<<"AG">>, <<"Antigua and Barbuda">>}}
               ,{<<"AUS">>, {<<"AU">>, <<"Australia">>}}
               ,{<<"AUT">>, {<<"AT">>, <<"Austria">>}}
               ,{<<"AZE">>, {<<"AZ">>, <<"Azerbaijan">>}}
               ,{<<"BDI">>, {<<"BI">>, <<"Burundi">>}}
               ,{<<"BEL">>, {<<"BE">>, <<"Belgium">>}}
               ,{<<"BEN">>, {<<"BJ">>, <<"Benin">>}}
               ,{<<"BES">>, {<<"BQ">>, <<"Bonaire, Sint Eustatius and Saba">>}}
               ,{<<"BFA">>, {<<"BF">>, <<"Burkina Faso">>}}
               ,{<<"BGD">>, {<<"BD">>, <<"Bangladesh">>}}
               ,{<<"BGR">>, {<<"BG">>, <<"Bulgaria">>}}
               ,{<<"BHR">>, {<<"BH">>, <<"Bahrain">>}}
               ,{<<"BHS">>, {<<"BS">>, <<"Bahamas">>}}
               ,{<<"BIH">>, {<<"BA">>, <<"Bosnia and Herzegovina">>}}
               ,{<<"BLM">>, {<<"BL">>, <<"Saint Barthélemy">>}}
               ,{<<"BLR">>, {<<"BY">>, <<"Belarus">>}}
               ,{<<"BLZ">>, {<<"BZ">>, <<"Belize">>}}
               ,{<<"BMU">>, {<<"BM">>, <<"Bermuda">>}}
               ,{<<"BOL">>, {<<"BO">>, <<"Bolivia (Plurinational State of)">>}}
               ,{<<"BRA">>, {<<"BR">>, <<"Brazil">>}}
               ,{<<"BRB">>, {<<"BB">>, <<"Barbados">>}}
               ,{<<"BRN">>, {<<"BN">>, <<"Brunei Darussalam">>}}
               ,{<<"BTN">>, {<<"BT">>, <<"Bhutan">>}}
               ,{<<"BVT">>, {<<"BV">>, <<"Bouvet Island">>}}
               ,{<<"BWA">>, {<<"BW">>, <<"Botswana">>}}
               ,{<<"CAF">>, {<<"CF">>, <<"Central African Republic">>}}
               ,{<<"CAN">>, {<<"CA">>, <<"Canada">>}}
               ,{<<"CCK">>, {<<"CC">>, <<"Cocos (Keeling) Islands">>}}
               ,{<<"CHE">>, {<<"CH">>, <<"Switzerland">>}}
               ,{<<"CHL">>, {<<"CL">>, <<"Chile">>}}
               ,{<<"CHN">>, {<<"CN">>, <<"China">>}}
               ,{<<"CIV">>, {<<"CI">>, <<"Côte d'Ivoire">>}}
               ,{<<"CMR">>, {<<"CM">>, <<"Cameroon">>}}
               ,{<<"COD">>, {<<"CD">>, <<"Congo, Democratic Republic of the">>}}
               ,{<<"COG">>, {<<"CG">>, <<"Congo">>}}
               ,{<<"COK">>, {<<"CK">>, <<"Cook Islands">>}}
               ,{<<"COL">>, {<<"CO">>, <<"Colombia">>}}
               ,{<<"COM">>, {<<"KM">>, <<"Comoros">>}}
               ,{<<"CPV">>, {<<"CV">>, <<"Cabo Verde">>}}
               ,{<<"CRI">>, {<<"CR">>, <<"Costa Rica">>}}
               ,{<<"CUB">>, {<<"CU">>, <<"Cuba">>}}
               ,{<<"CUW">>, {<<"CW">>, <<"Curaçao">>}}
               ,{<<"CXR">>, {<<"CX">>, <<"Christmas Island">>}}
               ,{<<"CYM">>, {<<"KY">>, <<"Cayman Islands">>}}
               ,{<<"CYP">>, {<<"CY">>, <<"Cyprus">>}}
               ,{<<"CZE">>, {<<"CZ">>, <<"Czechia">>}}
               ,{<<"DEU">>, {<<"DE">>, <<"Germany">>}}
               ,{<<"DJI">>, {<<"DJ">>, <<"Djibouti">>}}
               ,{<<"DMA">>, {<<"DM">>, <<"Dominica">>}}
               ,{<<"DNK">>, {<<"DK">>, <<"Denmark">>}}
               ,{<<"DOM">>, {<<"DO">>, <<"Dominican Republic">>}}
               ,{<<"DZA">>, {<<"DZ">>, <<"Algeria">>}}
               ,{<<"ECU">>, {<<"EC">>, <<"Ecuador">>}}
               ,{<<"EGY">>, {<<"EG">>, <<"Egypt">>}}
               ,{<<"ERI">>, {<<"ER">>, <<"Eritrea">>}}
               ,{<<"ESH">>, {<<"EH">>, <<"Western Sahara">>}}
               ,{<<"ESP">>, {<<"ES">>, <<"Spain">>}}
               ,{<<"EST">>, {<<"EE">>, <<"Estonia">>}}
               ,{<<"ETH">>, {<<"ET">>, <<"Ethiopia">>}}
               ,{<<"FIN">>, {<<"FI">>, <<"Finland">>}}
               ,{<<"FJI">>, {<<"FJ">>, <<"Fiji">>}}
               ,{<<"FLK">>, {<<"FK">>, <<"Falkland Islands (Malvinas)">>}}
               ,{<<"FRA">>, {<<"FR">>, <<"France">>}}
               ,{<<"FRO">>, {<<"FO">>, <<"Faroe Islands">>}}
               ,{<<"FSM">>, {<<"FM">>, <<"Micronesia (Federated States of)">>}}
               ,{<<"GAB">>, {<<"GA">>, <<"Gabon">>}}
               ,{<<"GBR">>, {<<"GB">>, <<"United Kingdom of Great Britain and Northern Ireland">>, <<".uk">>}}
               ,{<<"GEO">>, {<<"GE">>, <<"Georgia">>}}
               ,{<<"GGY">>, {<<"GG">>, <<"Guernsey">>}}
               ,{<<"GHA">>, {<<"GH">>, <<"Ghana">>}}
               ,{<<"GIB">>, {<<"GI">>, <<"Gibraltar">>}}
               ,{<<"GIN">>, {<<"GN">>, <<"Guinea">>}}
               ,{<<"GLP">>, {<<"GP">>, <<"Guadeloupe">>}}
               ,{<<"GMB">>, {<<"GM">>, <<"Gambia">>}}
               ,{<<"GNB">>, {<<"GN">>, <<"Guinea-Bissau">>}}
               ,{<<"GNQ">>, {<<"GQ">>, <<"Equatorial Guinea">>}}
               ,{<<"GRC">>, {<<"GR">>, <<"Greece">>}}
               ,{<<"GRD">>, {<<"GD">>, <<"Grenada">>}}
               ,{<<"GRL">>, {<<"GL">>, <<"Greenland">>}}
               ,{<<"GTM">>, {<<"GT">>, <<"Guatemala">>}}
               ,{<<"GUF">>, {<<"GF">>, <<"French Guiana">>}}
               ,{<<"GUM">>, {<<"GU">>, <<"Guam">>}}
               ,{<<"GUY">>, {<<"GY">>, <<"Guyana">>}}
               ,{<<"HKG">>, {<<"HK">>, <<"Hong Kong">>}}
               ,{<<"HMD">>, {<<"HM">>, <<"Heard Island and McDonald Islands">>}}
               ,{<<"HND">>, {<<"HN">>, <<"Honduras">>}}
               ,{<<"HRV">>, {<<"HR">>, <<"Croatia">>}}
               ,{<<"HTI">>, {<<"HT">>, <<"Haiti">>}}
               ,{<<"HUN">>, {<<"HU">>, <<"Hungary">>}}
               ,{<<"IDN">>, {<<"ID">>, <<"Indonesia">>}}
               ,{<<"IMN">>, {<<"IM">>, <<"Isle of Man">>}}
               ,{<<"IND">>, {<<"IN">>, <<"India">>}}
               ,{<<"IOT">>, {<<"IO">>, <<"British Indian Ocean Territory">>}}
               ,{<<"IRL">>, {<<"IE">>, <<"Ireland">>}}
               ,{<<"IRN">>, {<<"IR">>, <<"Iran (Islamic Republic of)">>}}
               ,{<<"IRQ">>, {<<"IQ">>, <<"Iraq">>}}
               ,{<<"ISL">>, {<<"IS">>, <<"Iceland">>}}
               ,{<<"ISR">>, {<<"IL">>, <<"Israel">>}}
               ,{<<"ITA">>, {<<"IT">>, <<"Italy">>}}
               ,{<<"JAM">>, {<<"JM">>, <<"Jamaica">>}}
               ,{<<"JEY">>, {<<"JE">>, <<"Jersey">>}}
               ,{<<"JOR">>, {<<"JO">>, <<"Jordan">>}}
               ,{<<"JPN">>, {<<"JP">>, <<"Japan">>}}
               ,{<<"KAZ">>, {<<"KZ">>, <<"Kazakhstan">>}}
               ,{<<"KEN">>, {<<"KE">>, <<"Kenya">>}}
               ,{<<"KGZ">>, {<<"KG">>, <<"Kyrgyzstan">>}}
               ,{<<"KHM">>, {<<"KH">>, <<"Cambodia">>}}
               ,{<<"KIR">>, {<<"KI">>, <<"Kiribati">>}}
               ,{<<"KNA">>, {<<"KN">>, <<"Saint Kitts and Nevis">>}}
               ,{<<"KOR">>, {<<"KR">>, <<"Korea, Republic of">>}}
               ,{<<"KWT">>, {<<"KW">>, <<"Kuwait">>}}
               ,{<<"LAO">>, {<<"LA">>, <<"Lao People's Democratic Republic">>}}
               ,{<<"LBN">>, {<<"LB">>, <<"Lebanon">>}}
               ,{<<"LBR">>, {<<"LR">>, <<"Liberia">>}}
               ,{<<"LBY">>, {<<"LY">>, <<"Libya">>}}
               ,{<<"LCA">>, {<<"LC">>, <<"Saint Lucia">>}}
               ,{<<"LIE">>, {<<"LI">>, <<"Liechtenstein">>}}
               ,{<<"LKA">>, {<<"LK">>, <<"Sri Lanka">>}}
               ,{<<"LSO">>, {<<"LS">>, <<"Lesotho">>}}
               ,{<<"LTU">>, {<<"LT">>, <<"Lithuania">>}}
               ,{<<"LUX">>, {<<"LU">>, <<"Luxembourg">>}}
               ,{<<"LVA">>, {<<"LV">>, <<"Latvia">>}}
               ,{<<"MAC">>, {<<"MO">>, <<"Macao">>}}
               ,{<<"MAF">>, {<<"MF">>, <<"Saint Martin (French part)">>}}
               ,{<<"MAR">>, {<<"MA">>, <<"Morocco">>}}
               ,{<<"MCO">>, {<<"MC">>, <<"Monaco">>}}
               ,{<<"MDA">>, {<<"MD">>, <<"Moldova, Republic of">>}}
               ,{<<"MDG">>, {<<"MG">>, <<"Madagascar">>}}
               ,{<<"MDV">>, {<<"MV">>, <<"Maldives">>}}
               ,{<<"MEX">>, {<<"MX">>, <<"Mexico">>}}
               ,{<<"MHL">>, {<<"MH">>, <<"Marshall Islands">>}}
               ,{<<"MKD">>, {<<"MK">>, <<"North Macedonia">>}}
               ,{<<"MLI">>, {<<"ML">>, <<"Mali">>}}
               ,{<<"MLT">>, {<<"MT">>, <<"Malta">>}}
               ,{<<"MMR">>, {<<"MM">>, <<"Myanmar">>}}
               ,{<<"MNE">>, {<<"ME">>, <<"Montenegro">>}}
               ,{<<"MNG">>, {<<"MN">>, <<"Mongolia">>}}
               ,{<<"MNP">>, {<<"MP">>, <<"Northern Mariana Islands">>}}
               ,{<<"MOZ">>, {<<"MZ">>, <<"Mozambique">>}}
               ,{<<"MRT">>, {<<"MR">>, <<"Mauritania">>}}
               ,{<<"MSR">>, {<<"MS">>, <<"Montserrat">>}}
               ,{<<"MTQ">>, {<<"MQ">>, <<"Martinique">>}}
               ,{<<"MUS">>, {<<"MU">>, <<"Mauritius">>}}
               ,{<<"MWI">>, {<<"MW">>, <<"Malawi">>}}
               ,{<<"MYS">>, {<<"MY">>, <<"Malaysia">>}}
               ,{<<"MYT">>, {<<"YT">>, <<"Mayotte">>}}
               ,{<<"NAM">>, {<<"NA">>, <<"Namibia">>}}
               ,{<<"NCL">>, {<<"NC">>, <<"New Caledonia">>}}
               ,{<<"NER">>, {<<"NE">>, <<"Niger">>}}
               ,{<<"NFK">>, {<<"NF">>, <<"Norfolk Island">>}}
               ,{<<"NGA">>, {<<"NG">>, <<"Nigeria">>}}
               ,{<<"NIC">>, {<<"NI">>, <<"Nicaragua">>}}
               ,{<<"NIU">>, {<<"NU">>, <<"Niue">>}}
               ,{<<"NLD">>, {<<"NL">>, <<"Netherlands">>}}
               ,{<<"NOR">>, {<<"NO">>, <<"Norway">>}}
               ,{<<"NPL">>, {<<"NP">>, <<"Nepal">>}}
               ,{<<"NRU">>, {<<"NR">>, <<"Nauru">>}}
               ,{<<"NZL">>, {<<"NZ">>, <<"New Zealand">>}}
               ,{<<"OMN">>, {<<"OM">>, <<"Oman">>}}
               ,{<<"PAK">>, {<<"PK">>, <<"Pakistan">>}}
               ,{<<"PAN">>, {<<"PA">>, <<"Panama">>}}
               ,{<<"PCN">>, {<<"PN">>, <<"Pitcairn">>}}
               ,{<<"PER">>, {<<"PE">>, <<"Peru">>}}
               ,{<<"PHL">>, {<<"PH">>, <<"Philippines">>}}
               ,{<<"PLW">>, {<<"PW">>, <<"Palau">>}}
               ,{<<"PNG">>, {<<"PG">>, <<"Papua New Guinea">>}}
               ,{<<"POL">>, {<<"PL">>, <<"Poland">>}}
               ,{<<"PRI">>, {<<"PR">>, <<"Puerto Rico">>}}
               ,{<<"PRK">>, {<<"KP">>, <<"Korea (Democratic People's Republic of)">>}}
               ,{<<"PRT">>, {<<"PT">>, <<"Portugal">>}}
               ,{<<"PRY">>, {<<"PY">>, <<"Paraguay">>}}
               ,{<<"PSE">>, {<<"PS">>, <<"Palestine, State of">>}}
               ,{<<"PYF">>, {<<"PF">>, <<"French Polynesia">>}}
               ,{<<"QAT">>, {<<"QA">>, <<"Qatar">>}}
               ,{<<"REU">>, {<<"RE">>, <<"Réunion">>}}
               ,{<<"ROU">>, {<<"RO">>, <<"Romania">>}}
               ,{<<"RUS">>, {<<"RU">>, <<"Russian Federation">>}}
               ,{<<"RWA">>, {<<"RW">>, <<"Rwanda">>}}
               ,{<<"SAU">>, {<<"SA">>, <<"Saudi Arabia">>}}
               ,{<<"SDN">>, {<<"SD">>, <<"Sudan">>}}
               ,{<<"SEN">>, {<<"SN">>, <<"Senegal">>}}
               ,{<<"SGP">>, {<<"SG">>, <<"Singapore">>}}
               ,{<<"SGS">>, {<<"GS">>, <<"South Georgia and the South Sandwich Islands">>}}
               ,{<<"SHN">>, {<<"SH">>, <<"Saint Helena, Ascension and Tristan da Cunha">>}}
               ,{<<"SJM">>, {<<"SJ">>, <<"Svalbard and Jan Mayen">>}}
               ,{<<"SLB">>, {<<"SB">>, <<"Solomon Islands">>}}
               ,{<<"SLE">>, {<<"SL">>, <<"Sierra Leone">>}}
               ,{<<"SLV">>, {<<"SV">>, <<"El Salvador">>}}
               ,{<<"SMR">>, {<<"SM">>, <<"San Marino">>}}
               ,{<<"SOM">>, {<<"SO">>, <<"Somalia">>}}
               ,{<<"SPM">>, {<<"PM">>, <<"Saint Pierre and Miquelon">>}}
               ,{<<"SRB">>, {<<"RS">>, <<"Serbia">>}}
               ,{<<"SSD">>, {<<"SS">>, <<"South Sudan">>}}
               ,{<<"STP">>, {<<"ST">>, <<"Sao Tome and Principe">>}}
               ,{<<"SUR">>, {<<"SR">>, <<"Suriname">>}}
               ,{<<"SVK">>, {<<"SK">>, <<"Slovakia">>}}
               ,{<<"SVN">>, {<<"SI">>, <<"Slovenia">>}}
               ,{<<"SWE">>, {<<"SE">>, <<"Sweden">>}}
               ,{<<"SWZ">>, {<<"SZ">>, <<"Eswatini">>}}
               ,{<<"SXM">>, {<<"SX">>, <<"Sint Maarten (Dutch part)">>}}
               ,{<<"SYC">>, {<<"SC">>, <<"Seychelles">>}}
               ,{<<"SYR">>, {<<"SY">>, <<"Syrian Arab Republic">>}}
               ,{<<"TCA">>, {<<"TC">>, <<"Turks and Caicos Islands">>}}
               ,{<<"TCD">>, {<<"TD">>, <<"Chad">>}}
               ,{<<"TGO">>, {<<"TG">>, <<"Togo">>}}
               ,{<<"THA">>, {<<"TH">>, <<"Thailand">>}}
               ,{<<"TJK">>, {<<"TJ">>, <<"Tajikistan">>}}
               ,{<<"TKL">>, {<<"TK">>, <<"Tokelau">>}}
               ,{<<"TKM">>, {<<"TM">>, <<"Turkmenistan">>}}
               ,{<<"TLS">>, {<<"TL">>, <<"Timor-Leste">>}}
               ,{<<"TON">>, {<<"TO">>, <<"Tonga">>}}
               ,{<<"TTO">>, {<<"TT">>, <<"Trinidad and Tobago">>}}
               ,{<<"TUN">>, {<<"TN">>, <<"Tunisia">>}}
               ,{<<"TUR">>, {<<"TR">>, <<"Turkey">>}}
               ,{<<"TUV">>, {<<"TV">>, <<"Tuvalu">>}}
               ,{<<"TWN">>, {<<"TW">>, <<"Taiwan, Province of China">>}}
               ,{<<"TZA">>, {<<"TZ">>, <<"Tanzania, United Republic of">>}}
               ,{<<"UGA">>, {<<"UG">>, <<"Uganda">>}}
               ,{<<"UKR">>, {<<"UA">>, <<"Ukraine">>}}
               ,{<<"UMI">>, {<<"UM">>, <<"United States Minor Outlying Islands">>}}
               ,{<<"URY">>, {<<"UY">>, <<"Uruguay">>}}
               ,{<<"USA">>, {<<"US">>, <<"United States of America">>}}
               ,{<<"UZB">>, {<<"UZ">>, <<"Uzbekistan">>}}
               ,{<<"VAT">>, {<<"VA">>, <<"Holy See">>}}
               ,{<<"VCT">>, {<<"VC">>, <<"Saint Vincent and the Grenadines">>}}
               ,{<<"VEN">>, {<<"VE">>, <<"Venezuela (Bolivarian Republic of)">>}}
               ,{<<"VGB">>, {<<"VG">>, <<"Virgin Islands (British)">>}}
               ,{<<"VIR">>, {<<"VI">>, <<"Virgin Islands (U.S.)">>}}
               ,{<<"VNM">>, {<<"VN">>, <<"Viet Nam">>}}
               ,{<<"VUT">>, {<<"VU">>, <<"Vanuatu">>}}
               ,{<<"WLF">>, {<<"WF">>, <<"Wallis and Futuna">>}}
               ,{<<"WSM">>, {<<"WS">>, <<"Samoa">>}}
               ,{<<"YEM">>, {<<"YE">>, <<"Yemen">>}}
               ,{<<"ZAF">>, {<<"ZA">>, <<"South Africa">>}}
               ,{<<"ZMB">>, {<<"ZM">>, <<"Zambia">>}}
               ,{<<"ZWE">>, {<<"ZW">>, <<"Zimbabwe">>}}
               ]
       ).
