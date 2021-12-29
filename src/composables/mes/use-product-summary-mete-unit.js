import { componentTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'

export default function useProductSummaryMeteUnit({
  productType,
  w_unit = 't',
  l_unit = 'm',
  isSingle = false
}) {
  const unitDp = {
    't': 'COM_WT__T',
    'kg': 'COM_WT__KG',
    'm': 'COM_L__M',
    'mm': 'COM_L__MM'
  }
  const labelObj = {
    [componentTypeEnum.ARTIFACT.V]: isSingle ? '单重' : '重量',
    [componentTypeEnum.MACHINE_PART.V]: isSingle ? '单重' : '重量',
    [componentTypeEnum.ASSEMBLE.V]: isSingle ? '单长' : '长度',
    [componentTypeEnum.ENCLOSURE.V]: isSingle ? '单长' : '长度'
  }
  const unitObj = {
    [componentTypeEnum.ARTIFACT.V]: w_unit,
    [componentTypeEnum.MACHINE_PART.V]: w_unit,
    [componentTypeEnum.ASSEMBLE.V]: l_unit,
    [componentTypeEnum.ENCLOSURE.V]: l_unit
  }
  const measureUnitObj = {
    [componentTypeEnum.ARTIFACT.V]: '件',
    [componentTypeEnum.MACHINE_PART.V]: '件',
    [componentTypeEnum.ASSEMBLE.V]: '件',
    [componentTypeEnum.ENCLOSURE.V]: '张'
  }
  const dpObj = {
    [componentTypeEnum.ARTIFACT.V]: unitDp[w_unit],
    [componentTypeEnum.MACHINE_PART.V]: unitDp[w_unit],
    [componentTypeEnum.ASSEMBLE.V]: unitDp[l_unit],
    [componentTypeEnum.ENCLOSURE.V]: unitDp[l_unit]
  }
  const unit = unitObj[productType]
  const measure = measureUnitObj[productType]
  const dp = dpObj[productType]
  const label = labelObj[productType]
  return {
    label,
    unit,
    measure,
    dp,
    DP: DP[dp]
  }
}
