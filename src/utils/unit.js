import { MES_MATERIAL_UNIT, MES_MATERIAL_LIST_UNIT } from '@/settings/config'
import { materialTypeEnum, materialListTypeEnum, enclosureSettlementTypeEnum } from '@/utils/enum/modules/building-steel'
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
  if ([materialTypeEnum.AUXILIARY_MATERIAL.V, materialTypeEnum.STRUCTURE.V].includes(type)) {
    return MES_MATERIAL_UNIT[type].normal
  }
  if (type === materialTypeEnum.ENCLOSURE.V) {
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
      materialListTypeEnum.AUXILIARY_MATERIAL.V,
      materialListTypeEnum.STRUCTURE.V,
      materialListTypeEnum.MACHINE_PART.V,
      materialListTypeEnum.ARTIFACT_TREE.V
    ].includes(type)
  ) {
    return MES_MATERIAL_LIST_UNIT[type].normal
  }
  if (type === materialListTypeEnum.ENCLOSURE.V) {
    return MES_MATERIAL_LIST_UNIT[type][enclosureSettlementType].normal
  }
}
