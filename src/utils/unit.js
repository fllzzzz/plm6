import { MES_MATERIAL_UNIT, MES_MATERIAL_LIST_UNIT } from '@/settings/config'
import { componentTypeEnum, componentListTypeEnum, enclosureSettlementTypeEnum } from '@/utils/enum/modules/building-steel'
import { isNotBlank } from '@data-type'

/**
 * TODO:辅材单位待修改,每个科目有自己定义单位
 * 获取建钢材料类型单位
 * @param {number} type 材料类型（enum）
 * @returns
 */
export function getMaterialTypeUnit(type, enclosureSettlementType = enclosureSettlementTypeEnum.LENGTH.V) {
  if (!isNotBlank(type)) {
    return
  }
  if ([componentTypeEnum.AUXILIARY_MATERIAL.V, componentTypeEnum.STRUCTURE.V].includes(type)) {
    return MES_MATERIAL_UNIT[type].normal
  }
  if (type === componentTypeEnum.ENCLOSURE.V) {
    return MES_MATERIAL_UNIT[type][enclosureSettlementType].normal
  }
}

/**
 * TODO:辅材单位待修改,每个科目有自己定义单位
 * 获取建钢材料清单类型单位
 * @param  {number} type 材料清单类型（enum）
 * @returns
 */
export function getMaterialListTypeUnit(type, enclosureSettlementType = enclosureSettlementTypeEnum.LENGTH.V) {
  if (!isNotBlank(type)) {
    return
  }
  if (
    [
      componentListTypeEnum.AUXILIARY_MATERIAL.V,
      componentListTypeEnum.STRUCTURE.V,
      componentListTypeEnum.MACHINE_PART.V,
      componentListTypeEnum.ARTIFACT_TREE.V
    ].includes(type)
  ) {
    return MES_MATERIAL_LIST_UNIT[type].normal
  }
  if (type === componentListTypeEnum.ENCLOSURE.V) {
    return MES_MATERIAL_LIST_UNIT[type][enclosureSettlementType].normal
  }
}
