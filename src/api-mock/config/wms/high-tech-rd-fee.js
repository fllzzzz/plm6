import { rawMatClsEnum } from '@enum-ms/classification'
// 获取物料计量配置
const get = {
  url: '/api/wms/config/material/high-tech-rd-fee',
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
          'code': '1', // 编码
          'basicClass': rawMatClsEnum.STEEL_PLATE.V, // 基础分类
          'rdRate': 10.50 // 研发比例
        }, {
          'id': 104,
          'name': '开平板',
          'code': '2',
          'basicClass': rawMatClsEnum.STEEL_PLATE.V,
          'rdRate': 7.50 // 研发比例

        }, {
          'id': 105,
          'name': '镀锌板',
          'code': '3',
          'basicClass': rawMatClsEnum.STEEL_PLATE.V,
          'rdRate': 8.20 // 研发比例
        }, {
          'id': 106,
          'name': '不锈钢板',
          'code': '4',
          'basicClass': rawMatClsEnum.STEEL_PLATE.V
        }, {
          'id': 107,
          'name': '花纹钢板',
          'code': '5',
          'basicClass': rawMatClsEnum.STEEL_PLATE.V
        }, {
          'id': 108,
          'name': '工字钢',
          'code': '6',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 109,
          'name': '热轧H型材',
          'code': '7',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 110,
          'name': '角钢',
          'code': '8',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 111,
          'name': '槽钢',
          'code': '9',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 112,
          'name': '方管',
          'code': '10',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 113,
          'name': '矩形管',
          'code': '11',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 114,
          'name': '扁铁',
          'code': '12',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 115,
          'name': 'U肋',
          'code': '13',
          'basicClass': rawMatClsEnum.SECTION_STEEL.V
        }, {
          'id': 116,
          'name': '镀锌彩卷',
          'code': '14',
          'basicClass': 3
        }, {
          'id': 117,
          'name': '镀锌钢带',
          'code': '15',
          'basicClass': 3
        }, {
          'id': 118,
          'name': '热轧卷',
          'code': '16',
          'basicClass': 3
        }, {
          'id': 119,
          'name': '不锈钢热轧卷',
          'code': '17',
          'basicClass': 3
        }, {
          'id': 120,
          'name': '不锈钢冷轧卷',
          'code': '18',
          'basicClass': 3
        },
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
              'code': '01'
            }, {
              'id': 205,
              'name': '扭剪型',
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
const save = {
  url: '/api/wms/config/material/high-tech-rd-fee',
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
  get,
  save
]
