%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_tool_variables).

-include_lib("lineman/src/lineman.hrl").

-export([set_parameter/2]).
-export([prepare/2]).
-export([execute/2]).

-spec set_parameter/2 :: (string(), #xmlElement{}) -> 'ok'.
set_parameter(_Name, Parameter) -> 
    set_dynamic_vars(Parameter),
    ok.

-spec prepare/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
prepare(Xml, Workorder) ->
    DynamicVars = xmerl_xpath:string("*", Xml),
    set_dynamic_vars(DynamicVars),
    Workorder.

-spec execute/2 :: (xml(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
execute(_Xml, Workorder) ->
    Workorder.

-spec set_dynamic_vars/1 :: (xml()) -> 'ok'.
set_dynamic_vars([DynamicVar|DynamicVars]) ->
    Name = lineman_util:xml_attribute("name", DynamicVar),
    case lineman_util:xml_attribute("generate", DynamicVar) of
        undefined ->
            Value = lineman_util:xml_content(DynamicVar),
            lineman_util:add_dynamic_var(Name, Value);
        Type ->
            generate_dynamic_var(Type, Name, DynamicVar)
    end,
    set_dynamic_vars(DynamicVars);
set_dynamic_vars([]) -> ok.

-spec generate_dynamic_var/3 :: (ne_binary(), ne_binary(), xml()) -> 'ok'.
generate_dynamic_var(<<"chars">>, Name, Xml) ->
    Length = lineman_util:xml_integer_attribute("length", Xml, 16),
    UUID = wh_util:rand_hex_binary(Length),
    lineman_util:add_dynamic_var(Name, UUID);
generate_dynamic_var(<<"uuid">>, Name, _) ->
    UUID = wh_util:rand_hex_binary(16),
    lineman_util:add_dynamic_var(Name, UUID);
generate_dynamic_var(<<"username">>, Name, _) ->
    Rnd = wh_util:rand_hex_binary(3),
    lineman_util:add_dynamic_var(Name, <<"user_", Rnd/binary>>);
generate_dynamic_var(<<"password">>, Name, _) ->
    Password = wh_util:rand_hex_binary(6),
    lineman_util:add_dynamic_var(Name, Password);
generate_dynamic_var(<<"ip_v4">>, Name, _) ->
    IP = <<(wh_util:to_binary(crypto:rand_uniform(1, 255)))/binary, "."
           ,(wh_util:to_binary(crypto:rand_uniform(1, 255)))/binary, "."
           ,(wh_util:to_binary(crypto:rand_uniform(1, 255)))/binary, "."
           ,(wh_util:to_binary(crypto:rand_uniform(1, 255)))/binary>>,
    lineman_util:add_dynamic_var(Name, IP);
generate_dynamic_var(<<"word">>, Name, _) ->
    Word = random_word(crypto:rand_uniform(1, 225)),
    lineman_util:add_dynamic_var(Name, Word);
generate_dynamic_var(<<"domain">>, Name, _) ->
    Domain = <<(random_word(crypto:rand_uniform(1, 225)))/binary, ".",
               (random_word(crypto:rand_uniform(1, 225)))/binary, ".",
               (random_word(crypto:rand_uniform(1, 225)))/binary,
               (random_word(crypto:rand_uniform(1, 225)))/binary, ".",
               (random_top_level(crypto:rand_uniform(1, 312)))/binary>>,      
    lineman_util:add_dynamic_var(Name, Domain).

-spec random_word/1 :: (1..225) -> ne_binary().
random_word(1) -> <<"towel">>;
random_word(2) -> <<"monkey">>;
random_word(3) -> <<"other">>;
random_word(4) -> <<"these">>;
random_word(5) -> <<"would">>;
random_word(6) -> <<"write">>;
random_word(7) -> <<"could">>;
random_word(8) -> <<"people">>;
random_word(9) -> <<"water">>;
random_word(10) -> <<"little">>;
random_word(11) -> <<"place">>;
random_word(12) -> <<"sentence">>;
random_word(13) -> <<"great">>;
random_word(14) -> <<"where">>;
random_word(15) -> <<"through">>;
random_word(16) -> <<"before">>;
random_word(17) -> <<"follow">>;
random_word(18) -> <<"another">>;
random_word(19) -> <<"large">>;
random_word(20) -> <<"because">>;
random_word(21) -> <<"picture">>;
random_word(22) -> <<"again">>;
random_word(23) -> <<"change">>;
random_word(24) -> <<"answer">>;
random_word(25) -> <<"learn">>;
random_word(26) -> <<"should">>;
random_word(27) -> <<"world">>;
random_word(28) -> <<"below">>;
random_word(29) -> <<"country">>;
random_word(30) -> <<"school">>;
random_word(31) -> <<"father">>;
random_word(32) -> <<"earth">>;
random_word(33) -> <<"thought">>;
random_word(34) -> <<"dont">>;
random_word(35) -> <<"while">>;
random_word(36) -> <<"chose">>;
random_word(37) -> <<"something">>;
random_word(38) -> <<"example">>;
random_word(39) -> <<"begin">>;
random_word(40) -> <<"those">>;
random_word(41) -> <<"paper">>;
random_word(42) -> <<"together">>;
random_word(43) -> <<"group">>;
random_word(44) -> <<"white">>;
random_word(45) -> <<"began">>;
random_word(46) -> <<"state">>;
random_word(47) -> <<"second">>;
random_word(48) -> <<"enough">>;
random_word(49) -> <<"watch">>;
random_word(50) -> <<"really">>;
random_word(51) -> <<"almost">>;
random_word(52) -> <<"above">>;
random_word(53) -> <<"sometimes">>;
random_word(54) -> <<"young">>;
random_word(55) -> <<"being">>;
random_word(56) -> <<"brown">>;
random_word(57) -> <<"eight">>;
random_word(58) -> <<"laugh">>;
random_word(59) -> <<"myself">>;
random_word(60) -> <<"please">>;
random_word(61) -> <<"pretty">>;
random_word(62) -> <<"today">>;
random_word(63) -> <<"yellow">>;
random_word(64) -> <<"bride">>;
random_word(65) -> <<"broke">>;
random_word(66) -> <<"brute">>;
random_word(67) -> <<"close">>;
random_word(68) -> <<"crime">>;
random_word(69) -> <<"drove">>;
random_word(70) -> <<"frame">>;
random_word(71) -> <<"froze">>;
random_word(72) -> <<"globe">>;
random_word(73) -> <<"grade">>;
random_word(74) -> <<"grape">>;
random_word(75) -> <<"gripe">>;
random_word(76) -> <<"plane">>;
random_word(77) -> <<"plate">>;
random_word(78) -> <<"pride">>;
random_word(79) -> <<"prime">>;
random_word(80) -> <<"prize">>;
random_word(81) -> <<"scale">>;
random_word(82) -> <<"scare">>;
random_word(83) -> <<"scope">>;
random_word(84) -> <<"skate">>;
random_word(85) -> <<"slate">>;
random_word(86) -> <<"slave">>;
random_word(87) -> <<"slide">>;
random_word(88) -> <<"slope">>;
random_word(89) -> <<"smile">>;
random_word(90) -> <<"smoke">>;
random_word(91) -> <<"snake">>;
random_word(92) -> <<"snipe">>;
random_word(93) -> <<"spine">>;
random_word(94) -> <<"spite">>;
random_word(95) -> <<"spoke">>;
random_word(96) -> <<"stale">>;
random_word(97) -> <<"stone">>;
random_word(98) -> <<"stove">>;
random_word(99) -> <<"stripe">>;
random_word(100) -> <<"trade">>;
random_word(101) -> <<"brave">>;
random_word(102) -> <<"flute">>;
random_word(103) -> <<"quake">>;
random_word(104) -> <<"shine">>;
random_word(105) -> <<"steve">>;
random_word(106) -> <<"throne">>;
random_word(107) -> <<"called">>;
random_word(108) -> <<"circle">>;
random_word(109) -> <<"friend">>;
random_word(110) -> <<"chuckle">>;
random_word(111) -> <<"climb">>;
random_word(112) -> <<"clown">>;
random_word(113) -> <<"diver">>;
random_word(114) -> <<"fellow">>;
random_word(115) -> <<"fiddle">>;
random_word(116) -> <<"front">>;
random_word(117) -> <<"gobble">>;
random_word(118) -> <<"grandpa">>;
random_word(119) -> <<"grumble">>;
random_word(120) -> <<"jiggle">>;
random_word(121) -> <<"jumble">>;
random_word(122) -> <<"library">>;
random_word(123) -> <<"lived">>;
random_word(124) -> <<"lives">>;
random_word(125) -> <<"nibble">>;
random_word(126) -> <<"owner">>;
random_word(127) -> <<"reward">>;
random_word(128) -> <<"rivera">>;
random_word(129) -> <<"served">>;
random_word(130) -> <<"snuggle">>;
random_word(131) -> <<"sparkle">>;
random_word(132) -> <<"sprinkle">>;
random_word(133) -> <<"surprise">>;
random_word(134) -> <<"tadpole">>;
random_word(135) -> <<"tickle">>;
random_word(136) -> <<"twinkle">>;
random_word(137) -> <<"uncle">>;
random_word(138) -> <<"whistle">>;
random_word(139) -> <<"wiggle">>;
random_word(140) -> <<"window">>;
random_word(141) -> <<"milkshake">>;
random_word(142) -> <<"writing">>;
random_word(143) -> <<"ahead">>;
random_word(144) -> <<"anything">>;
random_word(145) -> <<"anyway">>;
random_word(146) -> <<"anywhere">>;
random_word(147) -> <<"awesome">>;
random_word(148) -> <<"babble">>;
random_word(149) -> <<"baffle">>;
random_word(150) -> <<"baseball">>;
random_word(151) -> <<"basin">>;
random_word(152) -> <<"battle">>;
random_word(153) -> <<"beanstalk">>;
random_word(154) -> <<"bedtime">>;
random_word(155) -> <<"beehive">>;
random_word(156) -> <<"behind">>;
random_word(157) -> <<"blind">>;
random_word(158) -> <<"blown">>;
random_word(159) -> <<"bluebird">>;
random_word(160) -> <<"blueprint">>;
random_word(161) -> <<"bonus">>;
random_word(162) -> <<"bottle">>;
random_word(163) -> <<"brace">>;
random_word(164) -> <<"brake">>;
random_word(165) -> <<"bread">>;
random_word(166) -> <<"breakfast">>;
random_word(167) -> <<"breath">>;
random_word(168) -> <<"bruise">>;
random_word(169) -> <<"bubble">>;
random_word(170) -> <<"built">>;
random_word(171) -> <<"bundle">>;
random_word(172) -> <<"butterfly">>;
random_word(173) -> <<"button">>;
random_word(174) -> <<"candle">>;
random_word(175) -> <<"careful">>;
random_word(176) -> <<"cattle">>;
random_word(177) -> <<"chalk">>;
random_word(178) -> <<"charge">>;
random_word(179) -> <<"chase">>;
random_word(180) -> <<"child">>;
random_word(181) -> <<"chime">>;
random_word(182) -> <<"choice">>;
random_word(183) -> <<"classmate">>;
random_word(184) -> <<"colorful">>;
random_word(185) -> <<"cowboy">>;
random_word(186) -> <<"crate">>;
random_word(187) -> <<"crinkle">>;
random_word(188) -> <<"cruise">>;
random_word(189) -> <<"cuddle">>;
random_word(190) -> <<"cupcake">>;
random_word(191) -> <<"dangle">>;
random_word(192) -> <<"dealt">>;
random_word(193) -> <<"decent">>;
random_word(194) -> <<"dimple">>;
random_word(195) -> <<"double">>;
random_word(196) -> <<"dragonfly">>;
random_word(197) -> <<"dread">>;
random_word(198) -> <<"driveway">>;
random_word(199) -> <<"elbow">>;
random_word(200) -> <<"everyone">>;
random_word(201) -> <<"everywhere">>;
random_word(202) -> <<"fence">>;
random_word(203) -> <<"firefighter">>;
random_word(204) -> <<"flagpole">>;
random_word(205) -> <<"refrigerator">>;
random_word(206) -> <<"fried">>;
random_word(207) -> <<"frozen">>;
random_word(208) -> <<"fruit">>;
random_word(209) -> <<"giggle">>;
random_word(210) -> <<"ginger">>;
random_word(211) -> <<"giraffe">>;
random_word(212) -> <<"gluestick">>;
random_word(213) -> <<"going">>;
random_word(214) -> <<"grace">>;
random_word(215) -> <<"graceful">>;
random_word(216) -> <<"grapefruit">>;
random_word(217) -> <<"grateful">>;
random_word(218) -> <<"grown">>;
random_word(219) -> <<"grown-up">>;
random_word(220) -> <<"growth">>;
random_word(221) -> <<"handwriting">>;
random_word(222) -> <<"health">>;
random_word(223) -> <<"heavy">>;
random_word(224) -> <<"hello">>;
random_word(225) -> <<"pizza">>.

-spec random_top_level/1 :: (1..312) -> ne_binary().
random_top_level(1) -> <<"ac">>;
random_top_level(2) -> <<"ad">>;
random_top_level(3) -> <<"ae">>;
random_top_level(4) -> <<"aero">>;
random_top_level(5) -> <<"af">>;
random_top_level(6) -> <<"ag">>;
random_top_level(7) -> <<"ai">>;
random_top_level(8) -> <<"al">>;
random_top_level(9) -> <<"am">>;
random_top_level(10) -> <<"an">>;
random_top_level(11) -> <<"ao">>;
random_top_level(12) -> <<"aq">>;
random_top_level(13) -> <<"ar">>;
random_top_level(14) -> <<"arpa">>;
random_top_level(15) -> <<"as">>;
random_top_level(16) -> <<"asia">>;
random_top_level(17) -> <<"at">>;
random_top_level(18) -> <<"au">>;
random_top_level(19) -> <<"aw">>;
random_top_level(20) -> <<"ax">>;
random_top_level(21) -> <<"az">>;
random_top_level(22) -> <<"ba">>;
random_top_level(23) -> <<"bb">>;
random_top_level(24) -> <<"bd">>;
random_top_level(25) -> <<"be">>;
random_top_level(26) -> <<"bf">>;
random_top_level(27) -> <<"bg">>;
random_top_level(28) -> <<"bh">>;
random_top_level(29) -> <<"bi">>;
random_top_level(30) -> <<"biz">>;
random_top_level(31) -> <<"bj">>;
random_top_level(32) -> <<"bm">>;
random_top_level(33) -> <<"bn">>;
random_top_level(34) -> <<"bo">>;
random_top_level(35) -> <<"br">>;
random_top_level(36) -> <<"bs">>;
random_top_level(37) -> <<"bt">>;
random_top_level(38) -> <<"bv">>;
random_top_level(39) -> <<"bw">>;
random_top_level(40) -> <<"by">>;
random_top_level(41) -> <<"bz">>;
random_top_level(42) -> <<"ca">>;
random_top_level(43) -> <<"cat">>;
random_top_level(44) -> <<"cc">>;
random_top_level(45) -> <<"cd">>;
random_top_level(46) -> <<"cf">>;
random_top_level(47) -> <<"cg">>;
random_top_level(48) -> <<"ch">>;
random_top_level(49) -> <<"ci">>;
random_top_level(50) -> <<"ck">>;
random_top_level(51) -> <<"cl">>;
random_top_level(52) -> <<"cm">>;
random_top_level(53) -> <<"cn">>;
random_top_level(54) -> <<"co">>;
random_top_level(55) -> <<"com">>;
random_top_level(56) -> <<"coop">>;
random_top_level(57) -> <<"cr">>;
random_top_level(58) -> <<"cu">>;
random_top_level(59) -> <<"cv">>;
random_top_level(60) -> <<"cw">>;
random_top_level(61) -> <<"cx">>;
random_top_level(62) -> <<"cy">>;
random_top_level(63) -> <<"cz">>;
random_top_level(64) -> <<"de">>;
random_top_level(65) -> <<"dj">>;
random_top_level(66) -> <<"dk">>;
random_top_level(67) -> <<"dm">>;
random_top_level(68) -> <<"do">>;
random_top_level(69) -> <<"dz">>;
random_top_level(70) -> <<"ec">>;
random_top_level(71) -> <<"edu">>;
random_top_level(72) -> <<"ee">>;
random_top_level(73) -> <<"eg">>;
random_top_level(74) -> <<"er">>;
random_top_level(75) -> <<"es">>;
random_top_level(76) -> <<"et">>;
random_top_level(77) -> <<"eu">>;
random_top_level(78) -> <<"fi">>;
random_top_level(79) -> <<"fj">>;
random_top_level(80) -> <<"fk">>;
random_top_level(81) -> <<"fm">>;
random_top_level(82) -> <<"fo">>;
random_top_level(83) -> <<"fr">>;
random_top_level(84) -> <<"ga">>;
random_top_level(85) -> <<"gb">>;
random_top_level(86) -> <<"gd">>;
random_top_level(87) -> <<"ge">>;
random_top_level(88) -> <<"gf">>;
random_top_level(89) -> <<"gg">>;
random_top_level(90) -> <<"gh">>;
random_top_level(91) -> <<"gi">>;
random_top_level(92) -> <<"gl">>;
random_top_level(93) -> <<"gm">>;
random_top_level(94) -> <<"gn">>;
random_top_level(95) -> <<"gov">>;
random_top_level(96) -> <<"gp">>;
random_top_level(97) -> <<"gq">>;
random_top_level(98) -> <<"gr">>;
random_top_level(99) -> <<"gs">>;
random_top_level(100) -> <<"gt">>;
random_top_level(101) -> <<"gu">>;
random_top_level(102) -> <<"gw">>;
random_top_level(103) -> <<"gy">>;
random_top_level(104) -> <<"hk">>;
random_top_level(105) -> <<"hm">>;
random_top_level(106) -> <<"hn">>;
random_top_level(107) -> <<"hr">>;
random_top_level(108) -> <<"ht">>;
random_top_level(109) -> <<"hu">>;
random_top_level(110) -> <<"id">>;
random_top_level(111) -> <<"ie">>;
random_top_level(112) -> <<"il">>;
random_top_level(113) -> <<"im">>;
random_top_level(114) -> <<"in">>;
random_top_level(115) -> <<"info">>;
random_top_level(116) -> <<"int">>;
random_top_level(117) -> <<"io">>;
random_top_level(118) -> <<"iq">>;
random_top_level(119) -> <<"ir">>;
random_top_level(120) -> <<"is">>;
random_top_level(121) -> <<"it">>;
random_top_level(122) -> <<"je">>;
random_top_level(123) -> <<"jm">>;
random_top_level(124) -> <<"jo">>;
random_top_level(125) -> <<"jobs">>;
random_top_level(126) -> <<"jp">>;
random_top_level(127) -> <<"ke">>;
random_top_level(128) -> <<"kg">>;
random_top_level(129) -> <<"kh">>;
random_top_level(130) -> <<"ki">>;
random_top_level(131) -> <<"km">>;
random_top_level(132) -> <<"kn">>;
random_top_level(133) -> <<"kp">>;
random_top_level(134) -> <<"kr">>;
random_top_level(135) -> <<"kw">>;
random_top_level(136) -> <<"ky">>;
random_top_level(137) -> <<"kz">>;
random_top_level(138) -> <<"la">>;
random_top_level(139) -> <<"lb">>;
random_top_level(140) -> <<"lc">>;
random_top_level(141) -> <<"li">>;
random_top_level(142) -> <<"lk">>;
random_top_level(143) -> <<"lr">>;
random_top_level(144) -> <<"ls">>;
random_top_level(145) -> <<"lt">>;
random_top_level(146) -> <<"lu">>;
random_top_level(147) -> <<"lv">>;
random_top_level(148) -> <<"ly">>;
random_top_level(149) -> <<"ma">>;
random_top_level(150) -> <<"mc">>;
random_top_level(151) -> <<"md">>;
random_top_level(152) -> <<"me">>;
random_top_level(153) -> <<"mg">>;
random_top_level(154) -> <<"mh">>;
random_top_level(155) -> <<"mil">>;
random_top_level(156) -> <<"mk">>;
random_top_level(157) -> <<"ml">>;
random_top_level(158) -> <<"mm">>;
random_top_level(159) -> <<"mn">>;
random_top_level(160) -> <<"mo">>;
random_top_level(161) -> <<"mobi">>;
random_top_level(162) -> <<"mp">>;
random_top_level(163) -> <<"mq">>;
random_top_level(164) -> <<"mr">>;
random_top_level(165) -> <<"ms">>;
random_top_level(166) -> <<"mt">>;
random_top_level(167) -> <<"mu">>;
random_top_level(168) -> <<"museum">>;
random_top_level(169) -> <<"mv">>;
random_top_level(170) -> <<"mw">>;
random_top_level(171) -> <<"mx">>;
random_top_level(172) -> <<"my">>;
random_top_level(173) -> <<"mz">>;
random_top_level(174) -> <<"na">>;
random_top_level(175) -> <<"name">>;
random_top_level(176) -> <<"nc">>;
random_top_level(177) -> <<"ne">>;
random_top_level(178) -> <<"net">>;
random_top_level(179) -> <<"nf">>;
random_top_level(180) -> <<"ng">>;
random_top_level(181) -> <<"ni">>;
random_top_level(182) -> <<"nl">>;
random_top_level(183) -> <<"no">>;
random_top_level(184) -> <<"np">>;
random_top_level(185) -> <<"nr">>;
random_top_level(186) -> <<"nu">>;
random_top_level(187) -> <<"nz">>;
random_top_level(188) -> <<"om">>;
random_top_level(189) -> <<"org">>;
random_top_level(190) -> <<"pa">>;
random_top_level(191) -> <<"pe">>;
random_top_level(192) -> <<"pf">>;
random_top_level(193) -> <<"pg">>;
random_top_level(194) -> <<"ph">>;
random_top_level(195) -> <<"pk">>;
random_top_level(196) -> <<"pl">>;
random_top_level(197) -> <<"pm">>;
random_top_level(198) -> <<"pn">>;
random_top_level(199) -> <<"pr">>;
random_top_level(200) -> <<"pro">>;
random_top_level(201) -> <<"ps">>;
random_top_level(202) -> <<"pt">>;
random_top_level(203) -> <<"pw">>;
random_top_level(204) -> <<"py">>;
random_top_level(205) -> <<"qa">>;
random_top_level(206) -> <<"re">>;
random_top_level(207) -> <<"ro">>;
random_top_level(208) -> <<"rs">>;
random_top_level(209) -> <<"ru">>;
random_top_level(210) -> <<"rw">>;
random_top_level(211) -> <<"sa">>;
random_top_level(212) -> <<"sb">>;
random_top_level(213) -> <<"sc">>;
random_top_level(214) -> <<"sd">>;
random_top_level(215) -> <<"se">>;
random_top_level(216) -> <<"sg">>;
random_top_level(217) -> <<"sh">>;
random_top_level(218) -> <<"si">>;
random_top_level(219) -> <<"sj">>;
random_top_level(220) -> <<"sk">>;
random_top_level(221) -> <<"sl">>;
random_top_level(222) -> <<"sm">>;
random_top_level(223) -> <<"sn">>;
random_top_level(224) -> <<"so">>;
random_top_level(225) -> <<"sr">>;
random_top_level(226) -> <<"st">>;
random_top_level(227) -> <<"su">>;
random_top_level(228) -> <<"sv">>;
random_top_level(229) -> <<"sx">>;
random_top_level(230) -> <<"sy">>;
random_top_level(231) -> <<"sz">>;
random_top_level(232) -> <<"tc">>;
random_top_level(233) -> <<"td">>;
random_top_level(234) -> <<"tel">>;
random_top_level(235) -> <<"tf">>;
random_top_level(236) -> <<"tg">>;
random_top_level(237) -> <<"th">>;
random_top_level(238) -> <<"tj">>;
random_top_level(239) -> <<"tk">>;
random_top_level(240) -> <<"tl">>;
random_top_level(241) -> <<"tm">>;
random_top_level(242) -> <<"tn">>;
random_top_level(243) -> <<"to">>;
random_top_level(244) -> <<"tp">>;
random_top_level(245) -> <<"tr">>;
random_top_level(246) -> <<"travel">>;
random_top_level(247) -> <<"tt">>;
random_top_level(248) -> <<"tv">>;
random_top_level(249) -> <<"tw">>;
random_top_level(250) -> <<"tz">>;
random_top_level(251) -> <<"ua">>;
random_top_level(252) -> <<"ug">>;
random_top_level(253) -> <<"uk">>;
random_top_level(254) -> <<"us">>;
random_top_level(255) -> <<"uy">>;
random_top_level(256) -> <<"uz">>;
random_top_level(257) -> <<"va">>;
random_top_level(258) -> <<"vc">>;
random_top_level(259) -> <<"ve">>;
random_top_level(260) -> <<"vg">>;
random_top_level(261) -> <<"vi">>;
random_top_level(262) -> <<"vn">>;
random_top_level(263) -> <<"vu">>;
random_top_level(264) -> <<"wf">>;
random_top_level(265) -> <<"ws">>;
random_top_level(266) -> <<"xn--0zwm56d">>;
random_top_level(267) -> <<"xn--11b5bs3a9aj6g">>;
random_top_level(268) -> <<"xn--3e0b707e">>;
random_top_level(269) -> <<"xn--45brj9c">>;
random_top_level(270) -> <<"xn--80akhbyknj4f">>;
random_top_level(271) -> <<"xn--80ao21a">>;
random_top_level(272) -> <<"xn--90a3ac">>;
random_top_level(273) -> <<"xn--9t4b11yi5a">>;
random_top_level(274) -> <<"xn--clchc0ea0b2g2a9gcd">>;
random_top_level(275) -> <<"xn--deba0ad">>;
random_top_level(276) -> <<"xn--fiqs8s">>;
random_top_level(277) -> <<"xn--fiqz9s">>;
random_top_level(278) -> <<"xn--fpcrj9c3d">>;
random_top_level(279) -> <<"xn--fzc2c9e2c">>;
random_top_level(280) -> <<"xn--g6w251d">>;
random_top_level(281) -> <<"xn--gecrj9c">>;
random_top_level(282) -> <<"xn--h2brj9c">>;
random_top_level(283) -> <<"xn--hgbk6aj7f53bba">>;
random_top_level(284) -> <<"xn--hlcj6aya9esc7a">>;
random_top_level(285) -> <<"xn--j6w193g">>;
random_top_level(286) -> <<"xn--jxalpdlp">>;
random_top_level(287) -> <<"xn--kgbechtv">>;
random_top_level(288) -> <<"xn--kprw13d">>;
random_top_level(289) -> <<"xn--kpry57d">>;
random_top_level(290) -> <<"xn--lgbbat1ad8j">>;
random_top_level(291) -> <<"xn--mgbaam7a8h">>;
random_top_level(292) -> <<"xn--mgbayh7gpa">>;
random_top_level(293) -> <<"xn--mgbbh1a71e">>;
random_top_level(294) -> <<"xn--mgbc0a9azcg">>;
random_top_level(295) -> <<"xn--mgberp4a5d4ar">>;
random_top_level(296) -> <<"xn--o3cw4h">>;
random_top_level(297) -> <<"xn--ogbpf8fl">>;
random_top_level(298) -> <<"xn--p1ai">>;
random_top_level(299) -> <<"xn--pgbs0dh">>;
random_top_level(300) -> <<"xn--s9brj9c">>;
random_top_level(301) -> <<"xn--wgbh1c">>;
random_top_level(302) -> <<"xn--wgbl6a">>;
random_top_level(303) -> <<"xn--xkc2al3hye2a">>;
random_top_level(304) -> <<"xn--xkc2dl3a5ee0h">>;
random_top_level(305) -> <<"xn--yfro4i67o">>;
random_top_level(306) -> <<"xn--ygbi2ammx">>;
random_top_level(307) -> <<"xn--zckzah">>;
random_top_level(308) -> <<"xxx">>;
random_top_level(309) -> <<"ye">>;
random_top_level(310) -> <<"yt">>;
random_top_level(311) -> <<"za">>;
random_top_level(312) -> <<"zm">>.
