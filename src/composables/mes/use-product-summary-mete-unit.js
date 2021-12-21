import { componentTypeEnum } from '@enum-ms/mes'

export default function useProductSummaryMeteUnit({
  productType
}) {
  const labelObj = {
    [componentTypeEnum.ARTIFACT.V]: '重量',
    [componentTypeEnum.MACHINE_PART.V]: '重量',
    [componentTypeEnum.ASSEMBLE.V]: '长度',
    [componentTypeEnum.ENCLOSURE.V]: '长度'
  }
  const unitObj = {
    [componentTypeEnum.ARTIFACT.V]: 't',
    [componentTypeEnum.MACHINE_PART.V]: 't',
    [componentTypeEnum.ASSEMBLE.V]: 'm',
    [componentTypeEnum.ENCLOSURE.V]: 'm'
  }
  const dpObj = {
    [componentTypeEnum.ARTIFACT.V]: 'COM_WT__T',
    [componentTypeEnum.MACHINE_PART.V]: 'COM_WT__T',
    [componentTypeEnum.ASSEMBLE.V]: 'MES_ARTIFACT_L__M',
    [componentTypeEnum.ENCLOSURE.V]: 'MES_ENCLOSURE_L__M'
  }
  const unit = unitObj[productType]
  const dp = dpObj[productType]
  const label = labelObj[productType]
  return {
    label,
    unit,
    dp
  }
}
