import { matClsEnum } from '@enum-ms/classification'
import { measureTypeEnum } from '@enum-ms/wms'

// 钢板
const getFinalMatClsByIdForSteelPlate = {
  url: RegExp('/api/config/classification/final-material/' + '[1][0][0-9]'),
  method: 'get',
  // timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data:
        {
          'id|1-100': 1,
          name: '中厚板',
          fullName: '中厚板',
          serialNumber: /[0-9]{10}/,
          measureUnit: '张', // 计量单位
          accountingUnit: '千克', // 核算单位
          accountingPrecision: 2, // 核算单位小数精度
          measurePrecision: 0, // 计量单位小数精度
          outboundUnitType: measureTypeEnum.MEASURE.V, // 出库方式
          basicClass: matClsEnum.STEEL_PLATE.V,
          specConfig: [
            {
              id: 1,
              name: '材质',
              list: [
                { code: '01', name: 'Q325B' },
                { code: '02', name: 'Q235B' },
                { code: '03', name: 'Q235A' },
                { code: '04', name: 'Q235C' }
              ]
            }
          ]
        }
    }
  }
}

// 型钢
const getFinalMatClsByIdForSectionSteel = {
  url: RegExp('/api/config/classification/final-material/' + '[1][1][0-9]'),
  method: 'get',
  // timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data:
        {
          'id|1-100': 1,
          serialNumber: /[0-9]{10}/,
          name: '工字钢',
          fullName: '工字钢',
          measureUnit: '根', // 计量单位
          accountingUnit: '千克', // 核算单位
          accountingPrecision: 2, // 核算单位小数精度
          measurePrecision: 0, // 计量单位小数精度
          outboundUnitType: measureTypeEnum.MEASURE.V, // 出库方式
          basicClass: matClsEnum.SECTION_STEEL.V,
          specConfig: [
            {
              id: 1,
              name: 'GB-06',
              list: [
                { code: '01', name: '10*10*200*500', 'unitWeight|10-30.1-2': 1 },
                { code: '02', name: '12*10*200*500', 'unitWeight|10-30.1-2': 1 },
                { code: '03', name: '13*10*200*500', 'unitWeight|10-30.1-2': 1 },
                { code: '04', name: '13*10*100*200', 'unitWeight|10-30.1-2': 1 },
                { code: '05', name: '15*15*110*300', 'unitWeight|10-30.1-2': 1 },
                { code: '06', name: '57*21*3*9', 'unitWeight|10-30.1-2': 1 }
              ]
            }
          ]
        }
    }
  }
}

// 钢卷
const getFinalMatClsByIdForSteelCoil = {
  url: RegExp('/api/config/classification/final-material/' + '[1][2][0-9]'),
  method: 'get',
  // timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data:
        {
          'id|1-100': 1,
          name: '镀锌彩卷',
          fullName: '镀锌彩卷',
          serialNumber: /[0-9]{10}/,
          measureUnit: '毫米', // 计量单位
          accountingUnit: '千克', // 核算单位
          accountingPrecision: 2, // 核算单位小数精度
          measurePrecision: 0, // 计量单位小数精度
          outboundUnitType: measureTypeEnum.MEASURE.V, // 出库方式
          basicClass: matClsEnum.STEEL_COIL.V,
          specConfig: [
            {
              id: 1,
              name: '材质',
              list: [
                { code: '01', name: 'DC51D+Z' },
                { code: '02', name: 'Q235B' },
                { code: '03', name: 'Q235A' },
                { code: '04', name: 'Q235C' }
              ]
            }
          ]
        }
    }
  }
}

// 气体
const getFinalMatClsByIdForGas = {
  url: RegExp('/api/config/classification/final-material/' + '[9][0-9][0-9]'),
  method: 'get',
  // timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data:
        {
          'id|1-100': 1,
          name: '丙烷',
          fullName: '快乐气体>丙烷',
          serialNumber: /[0-9]{10}/,
          measureUnit: '瓶', // 计量单位
          accountingUnit: '千克', // 核算单位
          accountingPrecision: 0, // 核算单位小数精度
          measurePrecision: 0, // 计量单位小数精度
          outboundUnitType: measureTypeEnum.MEASURE.V, // 出库方式
          basicClass: matClsEnum.GAS.V
        }
    }
  }
}

// 根据id, 获取末级物料分类
const getFinalMatClsById = {
  url: RegExp('/api/config/classification/final-material/' + '[1-9][0-9]*'),
  method: 'get',
  // timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data:
        {
          'id|1-100': 1,
          name: '大六角',
          fullName: '紧固件>高强螺栓>大六角',
          serialNumber: /[0-9]{10}/,
          // measureUnit: '套', // 计量单位
          accountingUnit: '套', // 核算单位
          accountingPrecision: 0, // 核算单位小数精度
          // measurePrecision: 0, // 计量单位小数精度
          outboundUnitType: measureTypeEnum.ACCOUNTING.V, // 出库方式
          basicClass: matClsEnum.MATERIAL.V,
          specConfig: [
            {
              id: 1,
              name: '直径',
              list: [
                { code: '01', name: 'M24' },
                { code: '02', name: 'M26' },
                { code: '03', name: 'M27' },
                { code: '04', name: 'M28' },
                { code: '05', name: 'M29' },
                { code: '06', name: 'M30' },
                { code: '07', name: 'M31' },
                { code: '08', name: 'M32' },
                { code: '09', name: 'M33' },
                { code: '10', name: 'M34' }
              ]
            },
            {
              id: 2,
              name: '长度',
              list: [
                { code: '01', name: '60' },
                { code: '02', name: '65' },
                { code: '03', name: '66' },
                { code: '04', name: '67' },
                { code: '05', name: '68' },
                { code: '06', name: '69' },
                { code: '07', name: '70' },
                { code: '08', name: '71' },
                { code: '09', name: '72' },
                { code: '10', name: '73' },
                { code: '11', name: '74' }
              ]
            }
          ]
        }
    }
  }
}

export default [
  getFinalMatClsByIdForSteelPlate,
  getFinalMatClsByIdForSectionSteel,
  getFinalMatClsByIdForSteelCoil,
  getFinalMatClsByIdForGas,
  getFinalMatClsById
]
