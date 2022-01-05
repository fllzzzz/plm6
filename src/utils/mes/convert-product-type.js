import {
  componentTypeEnum,
  projectComponentTypeEnum as typeEnum
} from '@enum-ms/mes'

// mes的基础产品类型 转换成计划的基础产品类型
export function convertProductType(productType, category) {
  let _type = productType
  if (productType & (componentTypeEnum.ARTIFACT.V | componentTypeEnum.ASSEMBLE.V | componentTypeEnum.MACHINE_PART.V)) {
    _type = typeEnum.ARTIFACT.V
  }
  if (productType & componentTypeEnum.ENCLOSURE.V) {
    _type = category || (typeEnum.PRESSED_PLATE.V | typeEnum.SANDWICH_BOARD.V | typeEnum.TRUSS_FLOOR_PLATE.V | typeEnum.PRESSED_FLOOR_PLATE.V | typeEnum.FOLDING_PIECE.V)
  }
  return _type
}
