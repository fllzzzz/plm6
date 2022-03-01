import { dataSourceEnum, alignEnum, fieldTypeEnum as typeEnum } from '@/utils/print/enum'
import { componentTypeEnum } from '@enum-ms/mes'

const STRUCTURE_BASE_INFO_FIELDS = [
  { show: true, key: 'name', title: '名称', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.STRUCTURE_NAME.K },
  { show: true, key: 'serialNumber', title: '编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SERIAL_NUMBER.K },
  { show: true, key: 'specification', title: '规格', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SPECIFICATION.K },
  { show: true, key: 'material', title: '材质', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.MATERIAL.K }
]

const ARTIFACT_BASE_INFO_FIELDS = [
  { show: true, key: 'name', title: '名称', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.STRUCTURE_NAME.K },
  { show: true, key: 'serialNumber', title: '构件编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SERIAL_NUMBER.K },
  { show: true, key: 'specification', title: '规格', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SPECIFICATION.K },
  { show: true, key: 'material', title: '材质', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.MATERIAL.K }
]

const ASSEMBLE_BASE_INFO_FIELDS = [
  { show: true, key: 'serialNumber', title: '组立号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SERIAL_NUMBER.K },
  { show: true, key: 'specification', title: '规格', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SPECIFICATION.K },
  { show: true, key: 'material', title: '材质', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.MATERIAL.K }
]

const MACHINE_PART_BASE_INFO_FIELDS = [
  { show: true, key: 'serialNumber', title: '零件编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SERIAL_NUMBER.K },
  { show: true, key: 'specification', title: '规格', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SPECIFICATION.K },
  { show: true, key: 'material', title: '材质', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.MATERIAL.K }
]

const ENCLOSURE_PART_BASE_INFO_FIELDS = [
  { show: true, key: 'name', title: '名称', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.STRUCTURE_NAME.K },
  { show: true, key: 'serialNumber', title: '编号', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 20, type: typeEnum.SERIAL_NUMBER.K },
  { show: true, key: 'plate', title: '板型', source: dataSourceEnum.SYSTEM.V, align: alignEnum.LEFT.V, minWidth: 18, type: typeEnum.PLATE_TYPE.K }
]

export const productTypeBaseInfoFields = (productType) => {
  switch (productType) {
    case componentTypeEnum.ARTIFACT.V:
      return ARTIFACT_BASE_INFO_FIELDS
    case componentTypeEnum.ASSEMBLE.V:
      return ASSEMBLE_BASE_INFO_FIELDS
    case componentTypeEnum.MACHINE_PART.V:
      return MACHINE_PART_BASE_INFO_FIELDS
    case componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V | componentTypeEnum.ARTIFACT.V:
      return STRUCTURE_BASE_INFO_FIELDS
    case componentTypeEnum.ENCLOSURE.V:
      return ENCLOSURE_PART_BASE_INFO_FIELDS
    default:
      return []
  }
}
