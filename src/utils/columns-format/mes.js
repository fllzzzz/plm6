import { componentTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'

const A_FORMAT = [
  ['length', ['to-fixed-ck', 'MES_ARTIFACT_L__MM']],
  ['netWeight', ['to-fixed-ck', 'COM_WT__KG']],
  ['grossWeight', ['to-fixed-ck', 'COM_WT__KG']],
  ['totalNetWeight', ['to-fixed-ck', 'COM_WT__KG']],
  ['totalGrossWeight', ['to-fixed-ck', 'COM_WT__KG']],
  ['surfaceArea', ['to-fixed-ck', 'COM_AREA__M2']]
]

const E_FORMAT = [
  ['width', ['to-fixed-ck', 'MES_ENCLOSURE_W__MM']],
  ['thickness', ['to-fixed-ck', 'MES_ENCLOSURE_T__MM']],
  ['length', ['to-fixed-ck', 'MES_ENCLOSURE_L__MM']],
  ['totalArea', ['to-fixed-ck', 'COM_AREA__M2']],
  ['totalLength', ['to-fixed-ck', 'MES_ENCLOSURE_L__M']],
  ['weight', ['to-fixed-ck', 'COM_WT__KG']]
]

export const productFormat = {
  [componentTypeEnum.MACHINE_PART.V]: A_FORMAT,
  [componentTypeEnum.ASSEMBLE.V]: A_FORMAT,
  [componentTypeEnum.ARTIFACT.V]: A_FORMAT,
  [componentTypeEnum.ENCLOSURE.V]: E_FORMAT
}

export const belongingFormat = [
  ['project', 'parse-project']
]

export const enclosureTypeFormat = [
  ['category', ['parse-enum', mesEnclosureTypeEnum]]
]
