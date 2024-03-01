package hw4

import chisel3._
import chisel3.util._

class MatMulMC(p: MatMulParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val aBlock = Vec(p.aElementsPerTransfer, SInt(p.w))
      val bBlock = Vec(p.bElementsPerTransfer, SInt(p.w))
    }))
    val outBlock = Valid(Vec(p.cElementsPerTransfer, SInt(p.w)))
  })

  val matrixAInternal = Reg(Vec(p.aRows, Vec(p.aCols, SInt(p.w))))
  val matrixBInternal = Reg(Vec(p.bRows, Vec(p.bCols, SInt(p.w))))
  val resultMatrix = RegInit(VecInit(Seq.fill(p.cRows)(VecInit(Seq.fill(p.cCols)(0.S(p.w))))))

  val numElemA: Int = (p.aRows * p.aCols)
  val numElemB: Int = (p.bRows * p.bCols)
  val numElemC: Int = (p.cRows * p.cCols)

  val loadCounterA = RegInit(0.U(log2Ceil(numElemA + 1).W))
  val loadCounterB = RegInit(0.U(log2Ceil(numElemB + 1).W))
  val loadCounterC = RegInit(0.U(log2Ceil(numElemC + 1).W))

  // State Declaration
  val stateIdle = RegInit(true.B)
  val stateLoadMatrices = RegInit(false.B)
  val stateMultiplying = RegInit(false.B)
  val stateOutputting = RegInit(false.B)

  val inputReady = RegInit(true.B)
  val outputValid = RegInit(false.B)

  val rowA = RegInit(0.U(log2Ceil(p.aRows + 1).W))
  val colA = RegInit(0.U(log2Ceil(p.aCols + 1).W))
  val rowB = RegInit(0.U(log2Ceil(p.bRows + 1).W))
  val colB = RegInit(0.U(log2Ceil(p.bCols + 1).W))
  val rowC = RegInit(0.U(log2Ceil(p.cRows + 1).W))
  val colC = RegInit(0.U(log2Ceil(p.cCols + 1).W))

  val rowCounter = RegInit(0.U(log2Ceil(p.aRows + 1).W))
  val colCounter = RegInit(0.U(log2Ceil(p.bCols + 1).W))
  val innerCounter = RegInit(0.U(log2Ceil(p.aCols + 1).W))

  io.outBlock.bits.foreach(_ := 0.S)

  when(stateIdle) {
    when(io.in.valid && io.in.ready) {
      rowA := 0.U
      colA := 0.U
      rowB := 0.U
      colB := 0.U
      stateIdle := false.B
      stateLoadMatrices := true.B
      stateMultiplying := false.B
      stateOutputting := false.B
    }
  }.elsewhen(stateLoadMatrices) {
    for (i <- 0 until p.aElementsPerTransfer) {
      val currIndA = loadCounterA + i.U
      val rowAIdx = currIndA / p.aCols.U
      val colAIdx = currIndA % p.aCols.U
      matrixAInternal(rowAIdx)(colAIdx) := io.in.bits.aBlock(i)
    }

    loadCounterA := loadCounterA + p.aElementsPerTransfer.U

    for (i <- 0 until p.bElementsPerTransfer) {
      val currIndB = loadCounterB + i.U
      val rowBIdx = currIndB / p.bCols.U
      val colBIdx = currIndB % p.bCols.U
      matrixBInternal(rowBIdx)(colBIdx) := io.in.bits.bBlock(i)
    }

    loadCounterB := loadCounterB + p.bElementsPerTransfer.U

    when(loadCounterA >= (numElemA - p.aElementsPerTransfer).U && loadCounterB >= (numElemB - p.bElementsPerTransfer).U) {
      rowCounter := 0.U
      colCounter := 0.U
      innerCounter := 0.U
      inputReady := false.B
      outputValid := false.B
      stateIdle := false.B
      stateLoadMatrices := false.B
      stateMultiplying := true.B
      stateOutputting := false.B
    }
  }.elsewhen(stateMultiplying) {
    when(innerCounter === (p.aCols - 1).U) {
      innerCounter := 0.U
      when(colCounter + p.parallelism.U >= p.bCols.U) {
        colCounter := 0.U
        when(rowCounter === (p.aRows - 1).U) {
          rowC := 0.U
          colC := 0.U
          outputValid := true.B
          stateIdle := false.B
          stateLoadMatrices := false.B
          stateMultiplying := false.B
          stateOutputting := true.B
        }.otherwise {
          rowCounter := rowCounter + 1.U
        }
      }.otherwise {
        colCounter := colCounter + p.parallelism.U
      }
    }.otherwise {
      innerCounter := innerCounter + 1.U
    }

    for (i <- 0 until p.parallelism) {
      val rowIdx = rowCounter
      val colIdx = colCounter + i.U
      when(colIdx < p.bCols.U) {
        val product = matrixAInternal(rowIdx)(innerCounter) * matrixBInternal(innerCounter)(colIdx)
        resultMatrix(rowIdx)(colIdx) := resultMatrix(rowIdx)(colIdx) + product
      }
    }
  }.elsewhen(stateOutputting) {
    for (i <- 0 until p.cElementsPerTransfer) {
      val currIndC = loadCounterC + i.U
      val rowCIdx = currIndC / p.cCols.U
      val colCIdx = currIndC % p.cCols.U
      io.outBlock.bits(i) := resultMatrix(rowCIdx)(colCIdx)
    }

    loadCounterC := loadCounterC + p.cElementsPerTransfer.U

    when(loadCounterC >= (numElemC - p.cElementsPerTransfer).U) {
      inputReady := true.B
      outputValid := false.B
      stateIdle := true.B
      stateLoadMatrices := false.B
      stateMultiplying := false.B
      stateOutputting := false.B
    }
  }

  io.in.ready := inputReady
  io.outBlock.valid := outputValid
}