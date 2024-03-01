package hw4

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._


case class MatMulParams(m: Int, k: Int, n: Int, parallelism: Int = 1, cyclesPerTransfer: Int = 1) {
  // A (m x k) X B (k x n) = C (m x n)
  val aRows: Int = m
  val aCols: Int = k
  val bRows: Int = k
  val bCols: Int = n
  val cRows: Int = m
  val cCols: Int = n
  // Implementation details
  val w: Width = 32.W
  // Only relevant for MatMulMC (multi-cycle transfer)
  require((aRows * aCols) % cyclesPerTransfer == 0)
  val aElementsPerTransfer: Int = (aRows * aCols) / cyclesPerTransfer
  if (cyclesPerTransfer != 1) {
    require(aElementsPerTransfer <= aCols)
    require(aCols % aElementsPerTransfer == 0)
  }
  require((bRows * bCols) % cyclesPerTransfer == 0)
  val bElementsPerTransfer: Int = (bRows * bCols) / cyclesPerTransfer
  if (cyclesPerTransfer != 1) {
    require(bElementsPerTransfer <= bCols)
    require(bCols % bElementsPerTransfer == 0)
  }
  if ((cRows * cCols) > cyclesPerTransfer)
    require((cRows * cCols) % cyclesPerTransfer == 0)
  val cElementsPerTransfer: Int = ((cRows * cCols) / cyclesPerTransfer).max(1)
  if (cyclesPerTransfer != 1) {
    require(cElementsPerTransfer <= cCols)
    require(cCols % cElementsPerTransfer == 0)
  }
  require(cCols >= parallelism)
  require(cCols % parallelism == 0)
}

class MatMulSC(p: MatMulParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val a = Vec(p.aRows, Vec(p.aCols, SInt(p.w))) // Assuming p.w is an Int specifying width
      val b = Vec(p.bRows, Vec(p.bCols, SInt(p.w)))
    }))
    val out = Valid(Vec(p.cRows, Vec(p.cCols, SInt(p.w))))
  })

  val aReg = Reg(Vec(p.aRows, Vec(p.aCols, SInt(p.w))))
  val bReg = Reg(Vec(p.bRows, Vec(p.bCols, SInt(p.w))))
  val cReg = RegInit(VecInit(Seq.fill(p.cRows)(VecInit(Seq.fill(p.cCols)(0.S(p.w))))))

  val computing = RegInit(false.B)
  val rowCounter = RegInit(0.U(log2Ceil(p.aRows + 1).W))
  val colCounter = RegInit(0.U(log2Ceil(p.bCols + 1).W))
  val kCounter = RegInit(0.U(log2Ceil(p.aCols + 1).W))
  val computeDone = RegInit(false.B)

  io.in.ready := !computing
  io.out.valid := computeDone && !computing
  io.out.bits := cReg

  when(io.in.fire) {
    computing := true.B
    computeDone := false.B
    aReg := io.in.bits.a
    bReg := io.in.bits.b
    rowCounter := 0.U
    colCounter := 0.U
    kCounter := 0.U
    cReg.foreach(_.foreach(_ := 0.S))
  }

  // Main computation loop adjusted for parallelism
  when(computing) {
    (0 until p.parallelism).foreach { i =>
      val colIndex = colCounter + i.U
      when(colIndex < p.bCols.U) {
        val product = aReg(rowCounter)(kCounter) * bReg(kCounter)(colIndex)
        cReg(rowCounter)(colIndex) := cReg(rowCounter)(colIndex) + product
      }
    }

    when(kCounter === (p.aCols - 1).U) {
      kCounter := 0.U
      when(colCounter + p.parallelism.U >= p.bCols.U) {
        colCounter := 0.U
        when(rowCounter === (p.aRows - 1).U) {
          computing := false.B
          computeDone := true.B
        }.otherwise {
          rowCounter := rowCounter + 1.U
        }
      }.otherwise {
        colCounter := colCounter + p.parallelism.U
      }
    }.otherwise {
      kCounter := kCounter + 1.U
    }
  }

  when(computeDone && io.in.fire) {
    computeDone := false.B
  }
}

