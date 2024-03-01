FIRRTL version 1.2.0
circuit MatMulSC :
  module MatMulSC :
    input clock : Clock
    input reset : UInt<1>
    output io_in_ready : UInt<1> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_valid : UInt<1> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_0_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_0_1 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_0_2 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_0_3 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_1_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_1_1 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_1_2 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_1_3 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_2_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_2_1 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_2_2 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_2_3 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_3_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_3_1 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_3_2 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_a_3_3 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_b_0_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_b_1_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_b_2_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    input io_in_bits_b_3_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    output io_out_valid : UInt<1> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    output io_out_bits_0_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    output io_out_bits_1_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    output io_out_bits_2_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]
    output io_out_bits_3_0 : SInt<32> @[\\src\\main\\scala\\hw4\\MatMulSC.scala 43:14]

    reg aReg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_0_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_0_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_0_3) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_1_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_1_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_1_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_1_3) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_2_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_2_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_2_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_2_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_2_3) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_3_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_3_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_3_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_3_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg aReg_3_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), aReg_3_3) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 51:17]
    reg bReg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), bReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 52:17]
    reg bReg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), bReg_1_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 52:17]
    reg bReg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), bReg_2_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 52:17]
    reg bReg_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), bReg_3_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 52:17]
    reg cReg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:21]
    reg cReg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cReg_1_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:21]
    reg cReg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cReg_2_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:21]
    reg cReg_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cReg_3_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:21]
    reg computing : UInt<1>, clock with :
      reset => (UInt<1>("h0"), computing) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 55:26]
    reg rowCounter : UInt<3>, clock with :
      reset => (UInt<1>("h0"), rowCounter) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 56:27]
    reg colCounter : UInt<1>, clock with :
      reset => (UInt<1>("h0"), colCounter) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 57:27]
    reg kCounter : UInt<3>, clock with :
      reset => (UInt<1>("h0"), kCounter) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 58:25]
    reg computeDone : UInt<1>, clock with :
      reset => (UInt<1>("h0"), computeDone) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 59:28]
    node _io_in_ready_T = eq(computing, UInt<1>("h0")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 61:18]
    node _io_out_valid_T = eq(computing, UInt<1>("h0")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 62:34]
    node _io_out_valid_T_1 = and(computeDone, _io_out_valid_T) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 62:31]
    node _T = and(io_in_ready, io_in_valid) @[src/main/scala/chisel3/util/Decoupled.scala 52:35]
    node _GEN_0 = mux(_T, UInt<1>("h1"), computing) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 66:15 55:26]
    node _GEN_1 = mux(_T, UInt<1>("h0"), computeDone) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 67:17 59:28]
    node _GEN_2 = mux(_T, io_in_bits_a_0_0, aReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_3 = mux(_T, io_in_bits_a_0_1, aReg_0_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_4 = mux(_T, io_in_bits_a_0_2, aReg_0_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_5 = mux(_T, io_in_bits_a_0_3, aReg_0_3) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_6 = mux(_T, io_in_bits_a_1_0, aReg_1_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_7 = mux(_T, io_in_bits_a_1_1, aReg_1_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_8 = mux(_T, io_in_bits_a_1_2, aReg_1_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_9 = mux(_T, io_in_bits_a_1_3, aReg_1_3) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_10 = mux(_T, io_in_bits_a_2_0, aReg_2_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_11 = mux(_T, io_in_bits_a_2_1, aReg_2_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_12 = mux(_T, io_in_bits_a_2_2, aReg_2_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_13 = mux(_T, io_in_bits_a_2_3, aReg_2_3) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_14 = mux(_T, io_in_bits_a_3_0, aReg_3_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_15 = mux(_T, io_in_bits_a_3_1, aReg_3_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_16 = mux(_T, io_in_bits_a_3_2, aReg_3_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_17 = mux(_T, io_in_bits_a_3_3, aReg_3_3) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 68:10 51:17]
    node _GEN_18 = mux(_T, io_in_bits_b_0_0, bReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 69:10 52:17]
    node _GEN_19 = mux(_T, io_in_bits_b_1_0, bReg_1_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 69:10 52:17]
    node _GEN_20 = mux(_T, io_in_bits_b_2_0, bReg_2_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 69:10 52:17]
    node _GEN_21 = mux(_T, io_in_bits_b_3_0, bReg_3_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 69:10 52:17]
    node _GEN_22 = mux(_T, UInt<1>("h0"), rowCounter) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 70:16 56:27]
    node _GEN_23 = mux(_T, UInt<1>("h0"), colCounter) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 71:16 57:27]
    node _GEN_24 = mux(_T, UInt<1>("h0"), kCounter) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 72:14 58:25]
    node _GEN_25 = mux(_T, asSInt(UInt<1>("h0")), cReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 53:21 73:30]
    node _GEN_26 = mux(_T, asSInt(UInt<1>("h0")), cReg_1_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 53:21 73:30]
    node _GEN_27 = mux(_T, asSInt(UInt<1>("h0")), cReg_2_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 53:21 73:30]
    node _GEN_28 = mux(_T, asSInt(UInt<1>("h0")), cReg_3_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 65:20 53:21 73:30]
    node _colIndex_T = add(colCounter, UInt<1>("h0")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 79:33]
    node colIndex = tail(_colIndex_T, 1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 79:33]
    node _T_1 = lt(colIndex, UInt<1>("h1")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 80:21]
    node _product_T = bits(rowCounter, 1, 0)
    node _product_T_1 = bits(kCounter, 1, 0)
    node _product_T_2 = bits(kCounter, 1, 0)
    node _GEN_29 = validif(and(eq(UInt<1>("h0"), _product_T), eq(UInt<1>("h0"), _product_T_1)), aReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_30 = mux(and(eq(UInt<1>("h0"), _product_T), eq(UInt<1>("h1"), _product_T_1)), aReg_0_1, _GEN_29) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_31 = mux(and(eq(UInt<1>("h0"), _product_T), eq(UInt<2>("h2"), _product_T_1)), aReg_0_2, _GEN_30) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_32 = mux(and(eq(UInt<1>("h0"), _product_T), eq(UInt<2>("h3"), _product_T_1)), aReg_0_3, _GEN_31) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_33 = mux(and(eq(UInt<1>("h1"), _product_T), eq(UInt<1>("h0"), _product_T_1)), aReg_1_0, _GEN_32) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_34 = mux(and(eq(UInt<1>("h1"), _product_T), eq(UInt<1>("h1"), _product_T_1)), aReg_1_1, _GEN_33) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_35 = mux(and(eq(UInt<1>("h1"), _product_T), eq(UInt<2>("h2"), _product_T_1)), aReg_1_2, _GEN_34) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_36 = mux(and(eq(UInt<1>("h1"), _product_T), eq(UInt<2>("h3"), _product_T_1)), aReg_1_3, _GEN_35) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_37 = mux(and(eq(UInt<2>("h2"), _product_T), eq(UInt<1>("h0"), _product_T_1)), aReg_2_0, _GEN_36) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_38 = mux(and(eq(UInt<2>("h2"), _product_T), eq(UInt<1>("h1"), _product_T_1)), aReg_2_1, _GEN_37) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_39 = mux(and(eq(UInt<2>("h2"), _product_T), eq(UInt<2>("h2"), _product_T_1)), aReg_2_2, _GEN_38) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_40 = mux(and(eq(UInt<2>("h2"), _product_T), eq(UInt<2>("h3"), _product_T_1)), aReg_2_3, _GEN_39) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_41 = mux(and(eq(UInt<2>("h3"), _product_T), eq(UInt<1>("h0"), _product_T_1)), aReg_3_0, _GEN_40) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_42 = mux(and(eq(UInt<2>("h3"), _product_T), eq(UInt<1>("h1"), _product_T_1)), aReg_3_1, _GEN_41) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_43 = mux(and(eq(UInt<2>("h3"), _product_T), eq(UInt<2>("h2"), _product_T_1)), aReg_3_2, _GEN_42) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_44 = mux(and(eq(UInt<2>("h3"), _product_T), eq(UInt<2>("h3"), _product_T_1)), aReg_3_3, _GEN_43) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_45 = validif(eq(UInt<1>("h0"), _product_T_2), bReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_46 = mux(eq(UInt<1>("h1"), _product_T_2), bReg_1_0, _GEN_45) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_47 = mux(eq(UInt<2>("h2"), _product_T_2), bReg_2_0, _GEN_46) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _GEN_48 = mux(eq(UInt<2>("h3"), _product_T_2), bReg_3_0, _GEN_47) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:{50,50}]
    node _aReg_product_T_product_T_1 = _GEN_44 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:50]
    node _bReg_product_T_2_0 = _GEN_48 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:50]
    node product = mul(_aReg_product_T_product_T_1, _bReg_product_T_2_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 81:50]
    node _T_2 = bits(rowCounter, 1, 0)
    node _cReg_0_T = bits(rowCounter, 1, 0)
    node _GEN_49 = validif(eq(UInt<1>("h0"), _cReg_0_T), cReg_0_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{66,66}]
    node _GEN_50 = mux(eq(UInt<1>("h1"), _cReg_0_T), cReg_1_0, _GEN_49) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{66,66}]
    node _GEN_51 = mux(eq(UInt<2>("h2"), _cReg_0_T), cReg_2_0, _GEN_50) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{66,66}]
    node _GEN_52 = mux(eq(UInt<2>("h3"), _cReg_0_T), cReg_3_0, _GEN_51) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{66,66}]
    node _cReg_cReg_0_T_0 = _GEN_52 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:66]
    node _cReg_0_T_1 = add(_cReg_cReg_0_T_0, product) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:66]
    node _cReg_0_T_2 = tail(_cReg_0_T_1, 1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:66]
    node _cReg_0_T_3 = asSInt(_cReg_0_T_2) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:66]
    node _cReg_T_2_0 = asSInt(bits(_cReg_0_T_3, 31, 0)) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{36,36}]
    node _GEN_53 = mux(eq(UInt<1>("h0"), _T_2), _cReg_T_2_0, _GEN_25) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{36,36}]
    node _GEN_54 = mux(eq(UInt<1>("h1"), _T_2), _cReg_T_2_0, _GEN_26) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{36,36}]
    node _GEN_55 = mux(eq(UInt<2>("h2"), _T_2), _cReg_T_2_0, _GEN_27) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{36,36}]
    node _GEN_56 = mux(eq(UInt<2>("h3"), _T_2), _cReg_T_2_0, _GEN_28) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 82:{36,36}]
    node _GEN_57 = mux(_T_1, _GEN_53, _GEN_25) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 80:34]
    node _GEN_58 = mux(_T_1, _GEN_54, _GEN_26) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 80:34]
    node _GEN_59 = mux(_T_1, _GEN_55, _GEN_27) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 80:34]
    node _GEN_60 = mux(_T_1, _GEN_56, _GEN_28) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 80:34]
    node _T_3 = eq(kCounter, UInt<2>("h3")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 86:19]
    node _T_4 = add(colCounter, UInt<1>("h1")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 88:23]
    node _T_5 = tail(_T_4, 1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 88:23]
    node _T_6 = geq(_T_5, UInt<1>("h1")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 88:41]
    node _T_7 = eq(rowCounter, UInt<2>("h3")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 90:25]
    node _rowCounter_T = add(rowCounter, UInt<1>("h1")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 94:36]
    node _rowCounter_T_1 = tail(_rowCounter_T, 1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 94:36]
    node _GEN_61 = mux(_T_7, UInt<1>("h0"), _GEN_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 90:46 91:21]
    node _GEN_62 = mux(_T_7, UInt<1>("h1"), _GEN_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 90:46 92:23]
    node _GEN_63 = mux(_T_7, _GEN_22, _rowCounter_T_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 90:46 94:22]
    node _colCounter_T = add(colCounter, UInt<1>("h1")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 97:34]
    node _colCounter_T_1 = tail(_colCounter_T, 1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 97:34]
    node _GEN_64 = mux(_T_6, UInt<1>("h0"), _colCounter_T_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 88:55 89:20 97:20]
    node _GEN_65 = mux(_T_6, _GEN_61, _GEN_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 88:55]
    node _GEN_66 = mux(_T_6, _GEN_62, _GEN_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 88:55]
    node _GEN_67 = mux(_T_6, _GEN_63, _GEN_22) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 88:55]
    node _kCounter_T = add(kCounter, UInt<1>("h1")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 100:28]
    node _kCounter_T_1 = tail(_kCounter_T, 1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 100:28]
    node _GEN_68 = mux(_T_3, UInt<1>("h0"), _kCounter_T_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 100:16 86:40 87:16]
    node _GEN_69 = mux(_T_3, _GEN_64, _GEN_23) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 86:40]
    node _GEN_70 = mux(_T_3, _GEN_65, _GEN_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 86:40]
    node _GEN_71 = mux(_T_3, _GEN_66, _GEN_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 86:40]
    node _GEN_72 = mux(_T_3, _GEN_67, _GEN_22) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 86:40]
    node _GEN_73 = mux(computing, _GEN_57, _GEN_25) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _GEN_74 = mux(computing, _GEN_58, _GEN_26) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _GEN_75 = mux(computing, _GEN_59, _GEN_27) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _GEN_76 = mux(computing, _GEN_60, _GEN_28) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _GEN_77 = mux(computing, _GEN_68, _GEN_24) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _GEN_78 = mux(computing, _GEN_69, _GEN_23) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _GEN_79 = mux(computing, _GEN_70, _GEN_0) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _GEN_80 = mux(computing, _GEN_71, _GEN_1) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _GEN_81 = mux(computing, _GEN_72, _GEN_22) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 77:19]
    node _T_8 = and(io_in_ready, io_in_valid) @[src/main/scala/chisel3/util/Decoupled.scala 52:35]
    node _T_9 = and(computeDone, _T_8) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 104:20]
    node _GEN_82 = mux(_T_9, UInt<1>("h0"), _GEN_80) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 104:35 105:17]
    node _cReg_WIRE_0 = asSInt(UInt<32>("h0")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{55,55}]
    node _cReg_WIRE_1_0 = asSInt(UInt<32>("h0")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{55,55}]
    node _cReg_WIRE_2_0 = asSInt(UInt<32>("h0")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{55,55}]
    node _cReg_WIRE_3_0 = asSInt(UInt<32>("h0")) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{55,55}]
    node _cReg_WIRE_4_0_0 = _cReg_WIRE_0 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{29,29}]
    node _cReg_WIRE_4_1_0 = _cReg_WIRE_1_0 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{29,29}]
    node _cReg_WIRE_4_2_0 = _cReg_WIRE_2_0 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{29,29}]
    node _cReg_WIRE_4_3_0 = _cReg_WIRE_3_0 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{29,29}]
    io_in_ready <= _io_in_ready_T @[\\src\\main\\scala\\hw4\\MatMulSC.scala 61:15]
    io_out_valid <= _io_out_valid_T_1 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 62:16]
    io_out_bits_0_0 <= cReg_0_0 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 63:15]
    io_out_bits_1_0 <= cReg_1_0 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 63:15]
    io_out_bits_2_0 <= cReg_2_0 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 63:15]
    io_out_bits_3_0 <= cReg_3_0 @[\\src\\main\\scala\\hw4\\MatMulSC.scala 63:15]
    aReg_0_0 <= _GEN_2
    aReg_0_1 <= _GEN_3
    aReg_0_2 <= _GEN_4
    aReg_0_3 <= _GEN_5
    aReg_1_0 <= _GEN_6
    aReg_1_1 <= _GEN_7
    aReg_1_2 <= _GEN_8
    aReg_1_3 <= _GEN_9
    aReg_2_0 <= _GEN_10
    aReg_2_1 <= _GEN_11
    aReg_2_2 <= _GEN_12
    aReg_2_3 <= _GEN_13
    aReg_3_0 <= _GEN_14
    aReg_3_1 <= _GEN_15
    aReg_3_2 <= _GEN_16
    aReg_3_3 <= _GEN_17
    bReg_0_0 <= _GEN_18
    bReg_1_0 <= _GEN_19
    bReg_2_0 <= _GEN_20
    bReg_3_0 <= _GEN_21
    cReg_0_0 <= mux(reset, _cReg_WIRE_4_0_0, _GEN_73) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{21,21}]
    cReg_1_0 <= mux(reset, _cReg_WIRE_4_1_0, _GEN_74) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{21,21}]
    cReg_2_0 <= mux(reset, _cReg_WIRE_4_2_0, _GEN_75) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{21,21}]
    cReg_3_0 <= mux(reset, _cReg_WIRE_4_3_0, _GEN_76) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 53:{21,21}]
    computing <= mux(reset, UInt<1>("h0"), _GEN_79) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 55:{26,26}]
    rowCounter <= mux(reset, UInt<3>("h0"), _GEN_81) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 56:{27,27}]
    colCounter <= mux(reset, UInt<1>("h0"), _GEN_78) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 57:{27,27}]
    kCounter <= mux(reset, UInt<3>("h0"), _GEN_77) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 58:{25,25}]
    computeDone <= mux(reset, UInt<1>("h0"), _GEN_82) @[\\src\\main\\scala\\hw4\\MatMulSC.scala 59:{28,28}]
