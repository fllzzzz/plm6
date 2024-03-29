import { rawMatClsEnum } from '@enum-ms/classification'
// 获取物料计量配置
const getMaterialMeasure = {
  url: '/api/config/classification/material/measure',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      'data': {
        content: [{
          'id': 103, // id
          'name': '中厚板', // 科目名称
          'measureUnit': '张', // 计量单位
          'accountingUnit': '千克', // 核算单位
          'accountingPrecision': 2, // 核算单位小数精度
          'measurePrecision': 0, // 计量单位小数精度
          'outboundUnitType': 1, // 出库方式
          'code': '1', // 编码
          'basicClass': rawMatClsEnum.STEEL_PLATE.V // 基础分类
        }, {
          'id': 104,
          'name': '开平板',
          'measureUnit': '张',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '2',
          'basicClass': rawMatClsEnum.STEEL_PLATE.V
        }, {
          'id': 105,
          'name': '镀锌板',
          'measureUnit': '张',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '3',
          'basicClass': rawMatClsEnum.STEEL_PLATE.V
        }, {
          'id': 106,
          'name': '不锈钢板',
          'measureUnit': '张',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '4',
          'basicClass': rawMatClsEnum.STEEL_PLATE.V
        }, {
          'id': 107,
          'name': '花纹钢板',
          'measureUnit': '张',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '5',
          'basicClass': rawMatClsEnum.STEEL_PLATE.V
        }, {
          'id': 108,
          'name': '工字钢',
          'measureUnit': '根',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '6',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 109,
          'name': '热轧H型材',
          'measureUnit': '根',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '7',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 110,
          'name': '角钢',
          'measureUnit': '根',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '8',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 111,
          'name': '槽钢',
          'measureUnit': '根',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '9',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 112,
          'name': '方管',
          'measureUnit': '根',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '10',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 113,
          'name': '矩形管',
          'measureUnit': '根',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '11',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 114,
          'name': '扁铁',
          'measureUnit': '根',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '12',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 115,
          'name': 'U肋',
          'measureUnit': '根',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '13',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 116,
          'name': '镀锌彩卷',
          'measureUnit': '毫米',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '14',
          'basicClass': 3
        }, {
          'id': 117,
          'name': '镀锌钢带',
          'measureUnit': '毫米',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '15',
          'basicClass': 3
        }, {
          'id': 118,
          'name': '热轧卷',
          'measureUnit': '毫米',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '16',
          'basicClass': 3
        }, {
          'id': 119,
          'name': '不锈钢热轧卷',
          'measureUnit': '毫米',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '17',
          'basicClass': 3
        }, {
          'id': 120,
          'name': '不锈钢冷轧卷',
          'measureUnit': '毫米',
          'accountingUnit': '千克',
          'accountingPrecision': 2,
          'measurePrecision': 0,
          'outboundUnitType': 1,
          'code': '18',
          'basicClass': 3
        },
        // {
        //   'id': 121,
        //   'name': '成品构件',
        //   'isLast': 0,
        //   'code': '19',
        //   'basicClass': rawMatClsEnum.MANUFACTURED.V,
        //   'children': [
        //     {
        //       'id': 1210,
        //       'name': '钢柱',
        //       'measureUnit': '根',
        //       'accountingUnit': '千克',
        //       'accountingPrecision': 2,
        //       'measurePrecision': 0,
        //       'outboundUnitType': 1,
        //       'code': '01',
        //       'level': 2
        //     },
        //     {
        //       'id': 1211,
        //       'name': '钢梁',
        //       'measureUnit': '根',
        //       'accountingUnit': '千克',
        //       'accountingPrecision': 2,
        //       'measurePrecision': 0,
        //       'outboundUnitType': 1,
        //       'code': '02',
        //       'level': 2
        //     },
        //     {
        //       'id': 1212,
        //       'name': '钢箱梁',
        //       'measureUnit': '根',
        //       'accountingUnit': '千克',
        //       'accountingPrecision': 2,
        //       'measurePrecision': 0,
        //       'outboundUnitType': 1,
        //       'code': '03',
        //       'level': 2
        //     }
        //   ]
        // },
        // {
        //   'id': 122,
        //   'name': '成品围护',
        //   'isLast': 0,
        //   'code': '20',
        //   'basicClass': rawMatClsEnum.MANUFACTURED.V,
        //   'children': [
        //     {
        //       'id': 1220,
        //       'name': '压型彩钢板',
        //       'measureUnit': '张',
        //       'accountingUnit': '毫米',
        //       'accountingPrecision': 2,
        //       'measurePrecision': 0,
        //       'code': '01',
        //       'level': 2
        //     },
        //     {
        //       'id': 1221,
        //       'name': '楼承板',
        //       'measureUnit': '张',
        //       'accountingUnit': '毫米',
        //       'accountingPrecision': 2,
        //       'measurePrecision': 0,
        //       'code': '02',
        //       'level': 2
        //     },
        //     {
        //       'id': 1222,
        //       'name': '采光带',
        //       'measureUnit': '条',
        //       'accountingUnit': '毫米',
        //       'accountingPrecision': 2,
        //       'measurePrecision': 0,
        //       'code': '03',
        //       'level': 2
        //     }
        //   ]
        // },
        {
          'id': 139,
          'name': '紧固件',
          'isLast': 0,
          'code': '21',
          'basicClass': rawMatClsEnum.MATERIAL.V,
          'children': [{
            'id': 196,
            'name': '高强螺栓',
            'isLast': 0,
            'code': '01',
            'children': [{
              'id': 204,
              'name': '大六角',
              'measureUnit': undefined,
              'accountingUnit': '套',
              'accountingPrecision': undefined,
              'measurePrecision': undefined,
              'code': '01'
            }, {
              'id': 205,
              'name': '扭剪型',
              'measureUnit': undefined,
              'accountingUnit': '套',
              'accountingPrecision': undefined,
              'measurePrecision': undefined,
              'code': '02'
            }]
          }, {
            'id': 197,
            'name': '普通螺栓',
            'code': '02'
          }, {
            'id': 198,
            'name': '化学螺栓',
            'code': '03'
          }, {
            'id': 199,
            'name': '膨胀螺栓',
            'code': '04'
          }, {
            'id': 200,
            'name': '预埋螺栓',
            'code': '05'
          }, {
            'id': 201,
            'name': '螺母',
            'code': '06'
          }, {
            'id': 202,
            'name': '栓钉',
            'code': '07'
          }, {
            'id': 203,
            'name': '花篮螺栓',
            'code': '08'
          }]
        }, {
          'id': 141,
          'name': '配套件',
          'isLast': 0,
          'code': '22',
          'basicClass': rawMatClsEnum.MATERIAL.V,
          'children': [{
            'id': 216,
            'name': '雨水管',
            'code': '01'
          }, {
            'id': 217,
            'name': '液压装卸平台',
            'code': '02'
          }, {
            'id': 218,
            'name': '风机',
            'code': '03'
          }, {
            'id': 219,
            'name': '板材配件',
            'code': '04'
          }]
        }, {
          'id': 131,
          'name': '油漆涂料',
          'isLast': 0,
          'code': '23',
          'basicClass': rawMatClsEnum.MATERIAL.V,
          'children': [{
            'id': 247,
            'name': '油漆',
            'code': '01'
          }, {
            'id': 248,
            'name': '防火涂料',
            'code': '02'
          }]
        }, {
          'id': 1131,
          'name': '气体',
          'isLast': 0,
          'code': '24',
          'basicClass': rawMatClsEnum.GAS.V,
          'children': [{
            'id': 2147,
            'name': '丙烷',
            'code': '01'
          }, {
            'id': 2148,
            'name': '液氧',
            'code': '02'
          }]
        }]
      }
    }
  }
}

// 保存物料计量配置
const saveMaterialMeasure = {
  url: '/api/config/classification/material/measure',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getMaterialMeasure,
  saveMaterialMeasure
]
