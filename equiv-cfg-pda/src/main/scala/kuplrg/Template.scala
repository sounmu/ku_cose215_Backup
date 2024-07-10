package kuplrg

trait Template {
  def pdafs2es(pda: PDA): PDA
  def pdaes2fs(pda: PDA): PDA
  def cfg2pdaes(cfg: CFG): PDA
  def pdaes2cfg(pda: PDA): CFG
}
