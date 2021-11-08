#include <stdio.h>
#include <inttypes.h>
#include "values.h"

void print_char(val_char_t);
void print_codepoint(val_char_t);
void print_cons(val_cons_t *);
void print_vect(val_vect_t*);
void print_str(val_str_t*);
void print_str_char(val_char_t);
void print_result_interior(val_t);
int utf8_encode_char(val_char_t, char *);

void print_result(val_t x)
{
  switch (val_typeof(x)) {
  case T_INT:
    printf("%" PRId64, val_unwrap_int(x));
    break;
  case T_BOOL:
    printf(val_unwrap_bool(x) ? "#t" : "#f");
    break;
  case T_CHAR:
    print_char(val_unwrap_char(x));
    break;
  case T_EOF:
    printf("#<eof>");
    break;
  case T_VOID:
    break;
  case T_EMPTY:
  case T_BOX:
  case T_CONS:
  case T_VECT:    
    printf("'");
    print_result_interior(x);
    break;
  case T_STR:
    putchar('"');
    print_str(val_unwrap_str(x));
    putchar('"');
    break;
  case T_PROC:
    printf("#<procedure>");
    break;
  case T_INVALID:
    printf("internal error");
  }
}

void print_result_interior(val_t x)
{
  switch (val_typeof(x)) {
  case T_EMPTY:
    printf("()");
    break;
  case T_BOX:
    printf("#&");
    print_result_interior(val_unwrap_box(x)->val);
    break;
  case T_CONS:
    printf("(");
    print_cons(val_unwrap_cons(x));
    printf(")");
    break;
  case T_VECT:
    print_vect(val_unwrap_vect(x));
    break;    
  default:
    print_result(x);
  }
}

void print_vect(val_vect_t *v)
{
  uint64_t i;

  if (!v) { printf("#()"); return; }

  printf("#(");
  for (i = 0; i < v->len; ++i) {
    print_result_interior(v->elems[i]);

    if (i < v->len - 1)
      putchar(' ');
  }
  printf(")");
}

void print_cons(val_cons_t *cons)
{
  print_result_interior(cons->fst);

  switch (val_typeof(cons->snd)) {
  case T_EMPTY:
    // nothing
    break;
  case T_CONS:
    printf(" ");
    print_cons(val_unwrap_cons(cons->snd));
    break;
  default:
    printf(" . ");
    print_result_interior(cons->snd);
    break;
  }
}

void print_str(val_str_t* s)
{
  if (!s) return;
  uint64_t i;
  for (i = 0; i < s->len; ++i)
    print_str_char(s->codepoints[i]);
}

void print_str_char_u(val_char_t c)
{
  printf("\\u%04X", c);
}

void print_str_char_U(val_char_t c)
{
  printf("\\U%08X", c);
}

void print_str_char(val_char_t c)
{
  switch (c) {
  case 0 ... 6:
    print_str_char_u(c);
    break;
  case 7:
    printf("\\a");
    break;
  case 8:
    printf("\\b");
    break;
  case 9:
    printf("\\t");
    break;
  case 10:
    printf("\\n");
    break;
  case 11:
    printf("\\v");
    break;
  case 12:
    printf("\\f");
    break;
  case 13:
    printf("\\r");
    break;
  case 14 ... 26:
    print_str_char_u(c);
    break;
  case 27:
    printf("\\e");
    break;
  case 28 ... 31:
    print_str_char_u(c);
    break;
  case 34:
    printf("\\\"");
    break;
  case 39:
    printf("'");
    break;
  case 92:
    printf("\\\\");
    break;
  case 127 ... 159:
  case 173 ... 173:
  case 888 ... 889:
  case 896 ... 899:
  case 907 ... 907:
  case 909 ... 909:
  case 930 ... 930:
  case 1328 ... 1328:
  case 1367 ... 1368:
  case 1376 ... 1376:
  case 1416 ... 1416:
  case 1419 ... 1420:
  case 1424 ... 1424:
  case 1480 ... 1487:
  case 1515 ... 1519:
  case 1525 ... 1541:
  case 1564 ... 1565:
  case 1757 ... 1757:
  case 1806 ... 1807:
  case 1867 ... 1868:
  case 1970 ... 1983:
  case 2043 ... 2047:
  case 2094 ... 2095:
  case 2111 ... 2111:
  case 2140 ... 2141:
  case 2143 ... 2207:
  case 2227 ... 2275:
  case 2436 ... 2436:
  case 2445 ... 2446:
  case 2449 ... 2450:
  case 2473 ... 2473:
  case 2481 ... 2481:
  case 2483 ... 2485:
  case 2490 ... 2491:
  case 2501 ... 2502:
  case 2505 ... 2506:
  case 2511 ... 2518:
  case 2520 ... 2523:
  case 2526 ... 2526:
  case 2532 ... 2533:
  case 2556 ... 2560:
  case 2564 ... 2564:
  case 2571 ... 2574:
  case 2577 ... 2578:
  case 2601 ... 2601:
  case 2609 ... 2609:
  case 2612 ... 2612:
  case 2615 ... 2615:
  case 2618 ... 2619:
  case 2621 ... 2621:
  case 2627 ... 2630:
  case 2633 ... 2634:
  case 2638 ... 2640:
  case 2642 ... 2648:
  case 2653 ... 2653:
  case 2655 ... 2661:
  case 2678 ... 2688:
  case 2692 ... 2692:
  case 2702 ... 2702:
  case 2706 ... 2706:
  case 2729 ... 2729:
  case 2737 ... 2737:
  case 2740 ... 2740:
  case 2746 ... 2747:
  case 2758 ... 2758:
  case 2762 ... 2762:
  case 2766 ... 2767:
  case 2769 ... 2783:
  case 2788 ... 2789:
  case 2802 ... 2816:
  case 2820 ... 2820:
  case 2829 ... 2830:
  case 2833 ... 2834:
  case 2857 ... 2857:
  case 2865 ... 2865:
  case 2868 ... 2868:
  case 2874 ... 2875:
  case 2885 ... 2886:
  case 2889 ... 2890:
  case 2894 ... 2901:
  case 2904 ... 2907:
  case 2910 ... 2910:
  case 2916 ... 2917:
  case 2936 ... 2945:
  case 2948 ... 2948:
  case 2955 ... 2957:
  case 2961 ... 2961:
  case 2966 ... 2968:
  case 2971 ... 2971:
  case 2973 ... 2973:
  case 2976 ... 2978:
  case 2981 ... 2983:
  case 2987 ... 2989:
  case 3002 ... 3005:
  case 3011 ... 3013:
  case 3017 ... 3017:
  case 3022 ... 3023:
  case 3025 ... 3030:
  case 3032 ... 3045:
  case 3067 ... 3071:
  case 3076 ... 3076:
  case 3085 ... 3085:
  case 3089 ... 3089:
  case 3113 ... 3113:
  case 3130 ... 3132:
  case 3141 ... 3141:
  case 3145 ... 3145:
  case 3150 ... 3156:
  case 3159 ... 3159:
  case 3162 ... 3167:
  case 3172 ... 3173:
  case 3184 ... 3191:
  case 3200 ... 3200:
  case 3204 ... 3204:
  case 3213 ... 3213:
  case 3217 ... 3217:
  case 3241 ... 3241:
  case 3252 ... 3252:
  case 3258 ... 3259:
  case 3269 ... 3269:
  case 3273 ... 3273:
  case 3278 ... 3284:
  case 3287 ... 3293:
  case 3295 ... 3295:
  case 3300 ... 3301:
  case 3312 ... 3312:
  case 3315 ... 3328:
  case 3332 ... 3332:
  case 3341 ... 3341:
  case 3345 ... 3345:
  case 3387 ... 3388:
  case 3397 ... 3397:
  case 3401 ... 3401:
  case 3407 ... 3414:
  case 3416 ... 3423:
  case 3428 ... 3429:
  case 3446 ... 3448:
  case 3456 ... 3457:
  case 3460 ... 3460:
  case 3479 ... 3481:
  case 3506 ... 3506:
  case 3516 ... 3516:
  case 3518 ... 3519:
  case 3527 ... 3529:
  case 3531 ... 3534:
  case 3541 ... 3541:
  case 3543 ... 3543:
  case 3552 ... 3557:
  case 3568 ... 3569:
  case 3573 ... 3584:
  case 3643 ... 3646:
  case 3676 ... 3712:
  case 3715 ... 3715:
  case 3717 ... 3718:
  case 3721 ... 3721:
  case 3723 ... 3724:
  case 3726 ... 3731:
  case 3736 ... 3736:
  case 3744 ... 3744:
  case 3748 ... 3748:
  case 3750 ... 3750:
  case 3752 ... 3753:
  case 3756 ... 3756:
  case 3770 ... 3770:
  case 3774 ... 3775:
  case 3781 ... 3781:
  case 3783 ... 3783:
  case 3790 ... 3791:
  case 3802 ... 3803:
  case 3808 ... 3839:
  case 3912 ... 3912:
  case 3949 ... 3952:
  case 3992 ... 3992:
  case 4029 ... 4029:
  case 4045 ... 4045:
  case 4059 ... 4095:
  case 4294 ... 4294:
  case 4296 ... 4300:
  case 4302 ... 4303:
  case 4681 ... 4681:
  case 4686 ... 4687:
  case 4695 ... 4695:
  case 4697 ... 4697:
  case 4702 ... 4703:
  case 4745 ... 4745:
  case 4750 ... 4751:
  case 4785 ... 4785:
  case 4790 ... 4791:
  case 4799 ... 4799:
  case 4801 ... 4801:
  case 4806 ... 4807:
  case 4823 ... 4823:
  case 4881 ... 4881:
  case 4886 ... 4887:
  case 4955 ... 4956:
  case 4989 ... 4991:
  case 5018 ... 5023:
  case 5109 ... 5119:
  case 5789 ... 5791:
  case 5881 ... 5887:
  case 5901 ... 5901:
  case 5909 ... 5919:
  case 5943 ... 5951:
  case 5972 ... 5983:
  case 5997 ... 5997:
  case 6001 ... 6001:
  case 6004 ... 6015:
  case 6110 ... 6111:
  case 6122 ... 6127:
  case 6138 ... 6143:
  case 6158 ... 6159:
  case 6170 ... 6175:
  case 6264 ... 6271:
  case 6315 ... 6319:
  case 6390 ... 6399:
  case 6431 ... 6431:
  case 6444 ... 6447:
  case 6460 ... 6463:
  case 6465 ... 6467:
  case 6510 ... 6511:
  case 6517 ... 6527:
  case 6572 ... 6575:
  case 6602 ... 6607:
  case 6619 ... 6621:
  case 6684 ... 6685:
  case 6751 ... 6751:
  case 6781 ... 6782:
  case 6794 ... 6799:
  case 6810 ... 6815:
  case 6830 ... 6831:
  case 6847 ... 6911:
  case 6988 ... 6991:
  case 7037 ... 7039:
  case 7156 ... 7163:
  case 7224 ... 7226:
  case 7242 ... 7244:
  case 7296 ... 7359:
  case 7368 ... 7375:
  case 7415 ... 7415:
  case 7418 ... 7423:
  case 7670 ... 7675:
  case 7958 ... 7959:
  case 7966 ... 7967:
  case 8006 ... 8007:
  case 8014 ... 8015:
  case 8024 ... 8024:
  case 8026 ... 8026:
  case 8028 ... 8028:
  case 8030 ... 8030:
  case 8062 ... 8063:
  case 8117 ... 8117:
  case 8133 ... 8133:
  case 8148 ... 8149:
  case 8156 ... 8156:
  case 8176 ... 8177:
  case 8181 ... 8181:
  case 8191 ... 8191:
  case 8203 ... 8207:
  case 8232 ... 8238:
  case 8288 ... 8303:
  case 8306 ... 8307:
  case 8335 ... 8335:
  case 8349 ... 8351:
  case 8382 ... 8399:
  case 8433 ... 8447:
  case 8586 ... 8591:
  case 9211 ... 9215:
  case 9255 ... 9279:
  case 9291 ... 9311:
  case 11124 ... 11125:
  case 11158 ... 11159:
  case 11194 ... 11196:
  case 11209 ... 11209:
  case 11218 ... 11263:
  case 11311 ... 11311:
  case 11359 ... 11359:
  case 11508 ... 11512:
  case 11558 ... 11558:
  case 11560 ... 11564:
  case 11566 ... 11567:
  case 11624 ... 11630:
  case 11633 ... 11646:
  case 11671 ... 11679:
  case 11687 ... 11687:
  case 11695 ... 11695:
  case 11703 ... 11703:
  case 11711 ... 11711:
  case 11719 ... 11719:
  case 11727 ... 11727:
  case 11735 ... 11735:
  case 11743 ... 11743:
  case 11843 ... 11903:
  case 11930 ... 11930:
  case 12020 ... 12031:
  case 12246 ... 12271:
  case 12284 ... 12287:
  case 12352 ... 12352:
  case 12439 ... 12440:
  case 12544 ... 12548:
  case 12590 ... 12592:
  case 12687 ... 12687:
  case 12731 ... 12735:
  case 12772 ... 12783:
  case 12831 ... 12831:
  case 13055 ... 13055:
  case 19894 ... 19903:
  case 40909 ... 40959:
  case 42125 ... 42127:
  case 42183 ... 42191:
  case 42540 ... 42559:
  case 42654 ... 42654:
  case 42744 ... 42751:
  case 42895 ... 42895:
  case 42926 ... 42927:
  case 42930 ... 42998:
  case 43052 ... 43055:
  case 43066 ... 43071:
  case 43128 ... 43135:
  case 43205 ... 43213:
  case 43226 ... 43231:
  case 43260 ... 43263:
  case 43348 ... 43358:
  case 43389 ... 43391:
  case 43470 ... 43470:
  case 43482 ... 43485:
  case 43519 ... 43519:
  case 43575 ... 43583:
  case 43598 ... 43599:
  case 43610 ... 43611:
  case 43715 ... 43738:
  case 43767 ... 43776:
  case 43783 ... 43784:
  case 43791 ... 43792:
  case 43799 ... 43807:
  case 43815 ... 43815:
  case 43823 ... 43823:
  case 43872 ... 43875:
  case 43878 ... 43967:
  case 44014 ... 44015:
  case 44026 ... 44031:
  case 55204 ... 55215:
  case 55239 ... 55242:
  case 55292 ... 55295:
  case 57344 ... 63743:
  case 64110 ... 64111:
  case 64218 ... 64255:
  case 64263 ... 64274:
  case 64280 ... 64284:
  case 64311 ... 64311:
  case 64317 ... 64317:
  case 64319 ... 64319:
  case 64322 ... 64322:
  case 64325 ... 64325:
  case 64450 ... 64466:
  case 64832 ... 64847:
  case 64912 ... 64913:
  case 64968 ... 65007:
  case 65022 ... 65023:
  case 65050 ... 65055:
  case 65070 ... 65071:
  case 65107 ... 65107:
  case 65127 ... 65127:
  case 65132 ... 65135:
  case 65141 ... 65141:
  case 65277 ... 65280:
  case 65471 ... 65473:
  case 65480 ... 65481:
  case 65488 ... 65489:
  case 65496 ... 65497:
  case 65501 ... 65503:
  case 65511 ... 65511:
  case 65519 ... 65531:
  case 65534 ... 65535:
    print_str_char_u(c);
    break;
  case 65548 ... 65548:
  case 65575 ... 65575:
  case 65595 ... 65595:
  case 65598 ... 65598:
  case 65614 ... 65615:
  case 65630 ... 65663:
  case 65787 ... 65791:
  case 65795 ... 65798:
  case 65844 ... 65846:
  case 65933 ... 65935:
  case 65948 ... 65951:
  case 65953 ... 65999:
  case 66046 ... 66175:
  case 66205 ... 66207:
  case 66257 ... 66271:
  case 66300 ... 66303:
  case 66340 ... 66351:
  case 66379 ... 66383:
  case 66427 ... 66431:
  case 66462 ... 66462:
  case 66500 ... 66503:
  case 66518 ... 66559:
  case 66718 ... 66719:
  case 66730 ... 66815:
  case 66856 ... 66863:
  case 66916 ... 66926:
  case 66928 ... 67071:
  case 67383 ... 67391:
  case 67414 ... 67423:
  case 67432 ... 67583:
  case 67590 ... 67591:
  case 67593 ... 67593:
  case 67638 ... 67638:
  case 67641 ... 67643:
  case 67645 ... 67646:
  case 67670 ... 67670:
  case 67743 ... 67750:
  case 67760 ... 67839:
  case 67868 ... 67870:
  case 67898 ... 67902:
  case 67904 ... 67967:
  case 68024 ... 68029:
  case 68032 ... 68095:
  case 68100 ... 68100:
  case 68103 ... 68107:
  case 68116 ... 68116:
  case 68120 ... 68120:
  case 68148 ... 68151:
  case 68155 ... 68158:
  case 68168 ... 68175:
  case 68185 ... 68191:
  case 68256 ... 68287:
  case 68327 ... 68330:
  case 68343 ... 68351:
  case 68406 ... 68408:
  case 68438 ... 68439:
  case 68467 ... 68471:
  case 68498 ... 68504:
  case 68509 ... 68520:
  case 68528 ... 68607:
  case 68681 ... 69215:
  case 69247 ... 69631:
  case 69710 ... 69713:
  case 69744 ... 69758:
  case 69821 ... 69821:
  case 69826 ... 69839:
  case 69865 ... 69871:
  case 69882 ... 69887:
  case 69941 ... 69941:
  case 69956 ... 69967:
  case 70007 ... 70015:
  case 70089 ... 70092:
  case 70094 ... 70095:
  case 70107 ... 70112:
  case 70133 ... 70143:
  case 70162 ... 70162:
  case 70206 ... 70319:
  case 70379 ... 70383:
  case 70394 ... 70400:
  case 70404 ... 70404:
  case 70413 ... 70414:
  case 70417 ... 70418:
  case 70441 ... 70441:
  case 70449 ... 70449:
  case 70452 ... 70452:
  case 70458 ... 70459:
  case 70469 ... 70470:
  case 70473 ... 70474:
  case 70478 ... 70486:
  case 70488 ... 70492:
  case 70500 ... 70501:
  case 70509 ... 70511:
  case 70517 ... 70783:
  case 70856 ... 70863:
  case 70874 ... 71039:
  case 71094 ... 71095:
  case 71114 ... 71167:
  case 71237 ... 71247:
  case 71258 ... 71295:
  case 71352 ... 71359:
  case 71370 ... 71839:
  case 71923 ... 71934:
  case 71936 ... 72383:
  case 72441 ... 73727:
  case 74649 ... 74751:
  case 74863 ... 74863:
  case 74869 ... 77823:
  case 78895 ... 92159:
  case 92729 ... 92735:
  case 92767 ... 92767:
  case 92778 ... 92781:
  case 92784 ... 92879:
  case 92910 ... 92911:
  case 92918 ... 92927:
  case 92998 ... 93007:
  case 93018 ... 93018:
  case 93026 ... 93026:
  case 93048 ... 93052:
  case 93072 ... 93951:
  case 94021 ... 94031:
  case 94079 ... 94094:
  case 94112 ... 110591:
  case 110594 ... 113663:
  case 113771 ... 113775:
  case 113789 ... 113791:
  case 113801 ... 113807:
  case 113818 ... 113819:
  case 113824 ... 118783:
  case 119030 ... 119039:
  case 119079 ... 119080:
  case 119155 ... 119162:
  case 119262 ... 119295:
  case 119366 ... 119551:
  case 119639 ... 119647:
  case 119666 ... 119807:
  case 119893 ... 119893:
  case 119965 ... 119965:
  case 119968 ... 119969:
  case 119971 ... 119972:
  case 119975 ... 119976:
  case 119981 ... 119981:
  case 119994 ... 119994:
  case 119996 ... 119996:
  case 120004 ... 120004:
  case 120070 ... 120070:
  case 120075 ... 120076:
  case 120085 ... 120085:
  case 120093 ... 120093:
  case 120122 ... 120122:
  case 120127 ... 120127:
  case 120133 ... 120133:
  case 120135 ... 120137:
  case 120145 ... 120145:
  case 120486 ... 120487:
  case 120780 ... 120781:
  case 120832 ... 124927:
  case 125125 ... 125126:
  case 125143 ... 126463:
  case 126468 ... 126468:
  case 126496 ... 126496:
  case 126499 ... 126499:
  case 126501 ... 126502:
  case 126504 ... 126504:
  case 126515 ... 126515:
  case 126520 ... 126520:
  case 126522 ... 126522:
  case 126524 ... 126529:
  case 126531 ... 126534:
  case 126536 ... 126536:
  case 126538 ... 126538:
  case 126540 ... 126540:
  case 126544 ... 126544:
  case 126547 ... 126547:
  case 126549 ... 126550:
  case 126552 ... 126552:
  case 126554 ... 126554:
  case 126556 ... 126556:
  case 126558 ... 126558:
  case 126560 ... 126560:
  case 126563 ... 126563:
  case 126565 ... 126566:
  case 126571 ... 126571:
  case 126579 ... 126579:
  case 126584 ... 126584:
  case 126589 ... 126589:
  case 126591 ... 126591:
  case 126602 ... 126602:
  case 126620 ... 126624:
  case 126628 ... 126628:
  case 126634 ... 126634:
  case 126652 ... 126703:
  case 126706 ... 126975:
  case 127020 ... 127023:
  case 127124 ... 127135:
  case 127151 ... 127152:
  case 127168 ... 127168:
  case 127184 ... 127184:
  case 127222 ... 127231:
  case 127245 ... 127247:
  case 127279 ... 127279:
  case 127340 ... 127343:
  case 127387 ... 127461:
  case 127491 ... 127503:
  case 127547 ... 127551:
  case 127561 ... 127567:
  case 127570 ... 127743:
  case 127789 ... 127791:
  case 127870 ... 127871:
  case 127951 ... 127955:
  case 127992 ... 127999:
  case 128255 ... 128255:
  case 128331 ... 128335:
  case 128378 ... 128378:
  case 128420 ... 128420:
  case 128579 ... 128580:
  case 128720 ... 128735:
  case 128749 ... 128751:
  case 128756 ... 128767:
  case 128884 ... 128895:
  case 128981 ... 129023:
  case 129036 ... 129039:
  case 129096 ... 129103:
  case 129114 ... 129119:
  case 129160 ... 129167:
  case 129198 ... 131071:
  case 173783 ... 173823:
  case 177973 ... 177983:
  case 178206 ... 194559:
  case 195102 ... 917759:
  case 918000 ... 1114110:
    print_str_char_U(c);
    break;
  default:
    print_codepoint(c);
    break;
  }
}

void print_char(val_char_t c)
{
  printf("#\\");
  switch (c) {
  case 0:
    printf("nul"); break;
  case 8:
    printf("backspace"); break;
  case 9:
    printf("tab"); break;
  case 10:
    printf("newline"); break;
  case 11:
    printf("vtab"); break;
  case 12:
    printf("page"); break;
  case 13:
    printf("return"); break;
  case 32:
    printf("space"); break;
  case 127:
    printf("rubout"); break;
  default:
    print_codepoint(c);
  }
}

void print_codepoint(val_char_t c)
{
  char buffer[5] = {0};
  utf8_encode_char(c, buffer);
  printf("%s", buffer);
}

int utf8_encode_char(val_char_t c, char *buffer)
{
  // Output to buffer using UTF-8 encoding of codepoint
  // https://en.wikipedia.org/wiki/UTF-8
  if (c < 128) {
    buffer[0] = (char) c;
    return 1;
  } else if (c < 2048) {
    buffer[0] =  (char)(c >> 6)       | 192;
    buffer[1] = ((char)       c & 63) | 128;
    return 2;
  } else if (c < 65536) {
    buffer[0] =  (char)(c >> 12)      | 224;
    buffer[1] = ((char)(c >> 6) & 63) | 128;
    buffer[2] = ((char)       c & 63) | 128;
    return 3;
  } else {
    buffer[0] =  (char)(c >> 18)       | 240;
    buffer[1] = ((char)(c >> 12) & 63) | 128;
    buffer[2] = ((char)(c >>  6) & 63) | 128;
    buffer[3] = ((char)        c & 63) | 128;
    return 4;
  }
}
