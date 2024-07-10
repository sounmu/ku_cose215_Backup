package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // Number of trials for `mustEqual`
  private val TRIAL = 1_000

  // Tests for `enfaToDFA`
  lazy val enfa1 = ENFA(2, "", "0", "2")
  check(enfa1.toDFA.mustEqual(enfa1.lang, TRIAL))
  lazy val enfa2 = ENFA(2, "", "2", "2")
  check(enfa2.toDFA.mustEqual(enfa2.lang, TRIAL))
  lazy val enfa3 = ENFA(2, "a", "8", "2")
  check(enfa3.toDFA.mustEqual(enfa3.lang, TRIAL))
  lazy val enfa4 = ENFA(6, "ab", "ost0183xsrlx05m", "w")
  check(enfa4.toDFA.mustEqual(enfa4.lang, TRIAL))
  lazy val enfa5 = ENFA(4, "ab", "fs8wsgw", "8")
  check(enfa5.toDFA.mustEqual(enfa5.lang, TRIAL))
  lazy val enfa6 = ENFA(4, "a", "4zthc", "8")
  check(enfa6.toDFA.mustEqual(enfa6.lang, TRIAL))
  lazy val enfa7 = ENFA(4, "0", "eebu", "8")
  check(enfa7.toDFA.mustEqual(enfa7.lang, TRIAL))
  lazy val enfa8 = ENFA(6, "01", "urgp7qvlhgmm39c", "w")
  check(enfa8.toDFA.mustEqual(enfa8.lang, TRIAL))
  lazy val enfa9 = ENFA(8, "ab", "24vjqoqg4suuhnhwz9vdfl6tws3mgy", "3k")
  check(enfa9.toDFA.mustEqual(enfa9.lang, TRIAL))
  lazy val enfa10 = ENFA(2, "a", "8", "2")
  check(enfa10.toDFA.mustEqual(enfa10.lang, TRIAL))
  lazy val enfa11 = ENFA(10, "a", "bti5bb20kvfjhngpzltlwvcxbo5ochxmq", "e8")
  check(enfa11.toDFA.mustEqual(enfa11.lang, TRIAL))
  lazy val enc12 =
    "32dxdhyw21y3uaq3qblv0ouxoy3hud5hacyfgmvh" +
    "g7min7t7e375ds27rf570u833hcdq6x3qq551zbf" +
    "vaiqxca26mr0y1sa5lxn0dmllysd4bwwdlknquoe" +
    "32raofv5brb074368uiq5nf7b138z8uqgnyw8ex7" +
    "2s0kkuxermre2uafpxmolke32cl4pu2v1eeyb64q" +
    "62rifuymfytepvdlbxszk"
  lazy val enfa12 = ENFA(20, "01", enc12, "b8jk")
  check(enfa12.toDFA.mustEqual(enfa12.lang, TRIAL))
  lazy val enfa13 = ENFA(8, "ab", "2mrj9zn3340jv76ffmly5f8gyqupsa", "3k")
  check(enfa13.toDFA.mustEqual(enfa13.lang, TRIAL))
  lazy val enc14 =
    "tosew3hd1ajwwqjs5jf5lky7xfcmri3g3gcpjhw6" +
    "a2iu3to73quoqco4cp36kzfdjmnasze824vmtzeg" +
    "nk8icpljzwm0tp8bncgxrsoi0nyyn4mlqtfhgh03" +
    "jhqkjnbmk6vbe"
  lazy val enfa14 = ENFA(16, "01", enc14, "pa8")
  check(enfa14.toDFA.mustEqual(enfa14.lang, TRIAL))
  lazy val enc15 =
    "416jp2u82pnl82jtr13yjmnuonwkx1wsj080lg42" +
    "x5f0jy8mej9w0yrbnpczpjn9aty07b6v31auffg3" +
    "4dqa4v16zrvrx1fuvw2h1kxovlutyzujab74ytv6" +
    "gshmu8adhkb6oxlqxgq4zn35y2xlddq4ikx8ks2w" +
    "mon1hkrch9sgovjhcuxctb037qmztphy28ir4kh1" +
    "thkrdehwwo3v14stydbmgdwbfie83eqxfkdn8kkc" +
    "plorhabaqn7b9j3erv5u"
  lazy val enfa15 = ENFA(22, "01", enc15, "18y68")
  check(enfa15.toDFA.mustEqual(enfa15.lang, TRIAL))
  lazy val enc16 =
    "s02dm9f8evtkljjn64gd77gvi3pue0p0963uwpu0" +
    "aksok0hz7hc2owdhkp7zzhs2ere6aef00kmoodkz" +
    "e0u1kuasm6sfr3q9qx01mfsvf2fravy3555mhel0" +
    "3lxdl6ibwvi2cpg1yp4n8lg5odzazaeas25ilj3w" +
    "19znmrt77dlcdgzha49x3fb79h51uyvl2prcg06s" +
    "qgbs396vw8u2p9kebfq7qftf01a1vqlc0xkzo7fa" +
    "r3kohslwi5ij8zfr1cuacu7605mbdl6k2h192sxa" +
    "ydtf9s6j2ols6itssds8si2uynreedemnrc094lu" +
    "gwdk40dey3z4s3ueu6x1yofwd9rvfxl82rk4ka1r" +
    "w3tik8y"
  lazy val enfa16 = ENFA(26, "ab", enc16, "jz6rk")
  check(enfa16.toDFA.mustEqual(enfa16.lang, TRIAL))
  lazy val enc17 =
    "p8gzixnwf5825sxrovpxx95qf5ut0chjv4xwv7m3" +
    "kuwiiy9ur161ofa7vse62qxugoplvcqeeym4mj3z" +
    "ciy1lhg44n246k6mrrbz1i478jp6vz5afef2w8qh" +
    "oyutlh2dv5c74"
  lazy val enfa17 = ENFA(16, "ab", enc17, "pa8")
  check(enfa17.toDFA.mustEqual(enfa17.lang, TRIAL))
  lazy val enc18 =
    "8cxidkag4o1r1w9nfuv42uht68khpxgfo0wiy23k" +
    "w1dv2oen6r8bdmzquu5tqofd57x2hf3wbhfs7f7c" +
    "a03nhq7jna9x1axv7fb72fv6hp0a87m9yu3knbc3" +
    "86grvie9fs8cr9e6fmdjlaxt043qc4d0x0f6fh48" +
    "gfga0nsvfbdua2yekb77s4p0dyfdrfwwrdl4spee" +
    "v67uzkvl7pkpu"
  lazy val enfa18 = ENFA(20, "ab", enc18, "b8jk")
  check(enfa18.toDFA.mustEqual(enfa18.lang, TRIAL))
  lazy val enc19 =
    "k0nunqrp6ykgrk5supye2vyaqoe3l7a5sz0uitzm" +
    "oamkluro2d237ufuvjgv2whhq6a5lp0s66p8s6nl" +
    "gkyasbacyc3ez869aqcdsf27zfqj9beicvo3a8rs" +
    "ccm3t30ulptbdmo17v8v8v24zttzwsuthxh8qqp2" +
    "4rkz93s31jv18licka7ophdkizxv6kz9gynrk1br" +
    "8o3ingw46fu1k0za1oe2qr2gghzw972a412uhlzr" +
    "2688ix4ci04i92qhan5tzhtaec34htn0tovyuugs" +
    "qi4ixo45l8oi9or1npai9fywjuv2y2y"
  lazy val enfa19 = ENFA(24, "01", enc19, "4zsow")
  check(enfa19.toDFA.mustEqual(enfa19.lang, TRIAL))
  lazy val enc20 =
    "7v8a26nyze80ut84yg16tuenjgm286k2hhynas7t" +
    "jledl3gg4bldm0rmrj3lus6fl6l3l7937ya2ugjg" +
    "76jmf94ngptpzhx7yddfk5g34vlwqse9sauloug1" +
    "vrb92oy0t1ob3sgn6akwpqlo4yh8jbdu36z3iecx" +
    "z41z6d7m2dbb8dfvol30o4gbxbc1o213gcfpj17i" +
    "glgotghg7zima"
  lazy val enfa20 = ENFA(20, "01", enc20, "b8jk")
  check(enfa20.toDFA.mustEqual(enfa20.lang, TRIAL))

  // Tests for `re2enfa`
  lazy val re1 = RE("</>")
  check(re1.toENFA.mustEqual(re1.lang, TRIAL))
  lazy val re2 = RE("<e>")
  check(re2.toENFA.mustEqual(re2.lang, TRIAL))
  lazy val re3 = RE("a")
  check(re3.toENFA.mustEqual(re3.lang, TRIAL))
  lazy val re4 = RE("a|b")
  check(re4.toENFA.mustEqual(re4.lang, TRIAL))
  lazy val re5 = RE("ab")
  check(re5.toENFA.mustEqual(re5.lang, TRIAL))
  lazy val re6 = RE("</>a")
  check(re6.toENFA.mustEqual(re6.lang, TRIAL))
  lazy val re7 = RE("0*")
  check(re7.toENFA.mustEqual(re7.lang, TRIAL))
  lazy val re8 = RE("10*")
  check(re8.toENFA.mustEqual(re8.lang, TRIAL))
  lazy val re9 = RE("(a|b)*")
  check(re9.toENFA.mustEqual(re9.lang, TRIAL))
  lazy val re10 = RE("((a))")
  check(re10.toENFA.mustEqual(re10.lang, TRIAL))
  lazy val re11 = RE("a|a|a")
  check(re11.toENFA.mustEqual(re11.lang, TRIAL))
  lazy val re12 = RE("0101010101")
  check(re12.toENFA.mustEqual(re12.lang, TRIAL))
  lazy val re13 = RE("a*b*")
  check(re13.toENFA.mustEqual(re13.lang, TRIAL))
  lazy val re14 = RE("1*01*01*")
  check(re14.toENFA.mustEqual(re14.lang, TRIAL))
  lazy val re15 = RE("(0|1)*000(0|1)*")
  check(re15.toENFA.mustEqual(re15.lang, TRIAL))
  lazy val re16 = RE("(a|<e>)(ba)*|(b|<e>)(ab)*")
  check(re16.toENFA.mustEqual(re16.lang, TRIAL))
  lazy val re17 = RE("aaaa*(bb)*")
  check(re17.toENFA.mustEqual(re17.lang, TRIAL))
  lazy val re18 = RE("(aa)*(ab|<e>)(bb)*")
  check(re18.toENFA.mustEqual(re18.lang, TRIAL))
  lazy val re19 = RE("1*(01*01*01*)*")
  check(re19.toENFA.mustEqual(re19.lang, TRIAL))
  lazy val re20 = RE("(0|1(01*0)*1)*")
  check(re20.toENFA.mustEqual(re20.lang, TRIAL))

  // Tests for `dfa2re`
  lazy val dfa1 = DFA(1, "01", "0", "1")
  check(dfa1.toRE.mustEqual(dfa1.lang, TRIAL))
  lazy val dfa2 = DFA(2, "01", "3", "2")
  check(dfa2.toRE.mustEqual(dfa2.lang, TRIAL))
  lazy val dfa3 = DFA(2, "ab", "5", "2")
  check(dfa3.toRE.mustEqual(dfa3.lang, TRIAL))
  lazy val dfa4 = DFA(2, "xy", "d", "2")
  check(dfa4.toRE.mustEqual(dfa4.lang, TRIAL))
  lazy val dfa5 = DFA(3, "01", "51", "4")
  check(dfa5.toRE.mustEqual(dfa5.lang, TRIAL))
  lazy val dfa6 = DFA(3, "ab", "k3", "2")
  check(dfa6.toRE.mustEqual(dfa6.lang, TRIAL))
  lazy val dfa7 = DFA(3, "10", "gc", "3")
  check(dfa7.toRE.mustEqual(dfa7.lang, TRIAL))
  lazy val dfa8 = DFA(3, "ab", "5p", "4")
  check(dfa8.toRE.mustEqual(dfa8.lang, TRIAL))
  lazy val dfa9 = DFA(3, "01", "17", "4")
  check(dfa9.toRE.mustEqual(dfa9.lang, TRIAL))
  lazy val dfa10 = DFA(3, "abcd", "4d0k", "1")
  check(dfa10.toRE.mustEqual(dfa10.lang, TRIAL))
  lazy val dfa11 = DFA(3, "xyz", "a0o", "1")
  check(dfa11.toRE.mustEqual(dfa11.lang, TRIAL))
  lazy val dfa12 = DFA(3, "ab", "co", "3")
  check(dfa12.toRE.mustEqual(dfa12.lang, TRIAL))
  lazy val dfa13 = DFA(4, "01", "1ea9", "8")
  check(dfa13.toRE.mustEqual(dfa13.lang, TRIAL))
  lazy val dfa14 = DFA(4, "01", "jpl", "1")
  check(dfa14.toRE.mustEqual(dfa14.lang, TRIAL))
  lazy val dfa15 = DFA(4, "ab", "14dy", "4")
  check(dfa15.toRE.mustEqual(dfa15.lang, TRIAL))
  lazy val dfa16 = DFA(4, "ab", "e2q", "6")
  check(dfa16.toRE.mustEqual(dfa16.lang, TRIAL))
  lazy val dfa17 = DFA(5, "ab", "oceb", "n")
  check(dfa17.toRE.mustEqual(dfa17.lang, TRIAL))
  lazy val dfa18 = DFA(5, "01", "1p2ja", "c")
  check(dfa18.toRE.mustEqual(dfa18.lang, TRIAL))
  lazy val dfa19 = DFA(6, "01", "mvo98", "9")
  check(dfa19.toRE.mustEqual(dfa19.lang, TRIAL))
  lazy val dfa20 = DFA(7, "01", "i2cik2", "p")
  check(dfa20.toRE.mustEqual(dfa20.lang, TRIAL))

  /* Write your own tests */
}
