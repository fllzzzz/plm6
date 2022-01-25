// 获取分类树
const getClassificationTree = {
  url: '/api/config/v1/classification/tree',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      'data': [{
        'id': 103,
        'name': '中厚板',
        'code': '1',
        'basicClass': 1
      }, {
        'id': 104,
        'name': '开平板',
        'isNormalPart': 1,
        'code': '2',
        'basicClass': 1
      }, {
        'id': 105,
        'name': '镀锌板',
        'isNormalPart': 1,
        'code': '3',
        'basicClass': 1
      }, {
        'id': 106,
        'name': '不锈钢板',
        'isNormalPart': 1,
        'code': '4',
        'basicClass': 1
      }, {
        'id': 107,
        'name': '花纹钢板',
        'isNormalPart': 1,
        'code': '5',
        'basicClass': 1
      }, {
        'id': 108,
        'name': '工字钢',
        'isNormalPart': 1,
        'code': '6',
        'basicClass': 2
      }, {
        'id': 109,
        'name': '热轧H型材',
        'isNormalPart': 1,
        'code': '7',
        'basicClass': 2
      }, {
        'id': 110,
        'name': '角钢',
        'isNormalPart': 1,
        'code': '8',
        'basicClass': 2
      }, {
        'id': 111,
        'name': '槽钢',
        'isNormalPart': 1,
        'code': '9',
        'basicClass': 2
      }, {
        'id': 112,
        'name': '方管',
        'isNormalPart': 1,
        'code': '10',
        'basicClass': 2
      }, {
        'id': 113,
        'name': '矩形管',
        'isNormalPart': 1,
        'code': '11',
        'basicClass': 2
      }, {
        'id': 114,
        'name': '扁铁',
        'isNormalPart': 1,
        'code': '12',
        'basicClass': 2
      }, {
        'id': 115,
        'name': 'U肋',
        'isNormalPart': 1,
        'code': '13',
        'basicClass': 2
      }, {
        'id': 116,
        'name': '镀锌彩卷',
        'isNormalPart': 1,
        'code': '14',
        'basicClass': 4
      }, {
        'id': 117,
        'name': '镀锌钢带',
        'isNormalPart': 1,
        'code': '15',
        'basicClass': 4
      }, {
        'id': 118,
        'name': '热轧卷',
        'isNormalPart': 1,
        'code': '16',
        'basicClass': 4
      }, {
        'id': 119,
        'name': '不锈钢热轧卷',
        'isNormalPart': 1,
        'code': '17',
        'basicClass': 4
      }, {
        'id': 120,
        'name': '不锈钢冷轧卷',
        'isNormalPart': 1,
        'code': '18',
        'basicClass': 4
      },
      //  {
      //   'id': 121,
      //   'name': '成品构件',
      //   'isLast': 0,
      //   'isNormalPart': 1,
      //   'code': '19',
      //   'basicClass': 8,
      //   'children': [
      //     {
      //       'id': 1210,
      //       'name': '钢柱',
      //       'code': '01'
      //     },
      //     {
      //       'id': 1211,
      //       'name': '钢梁',
      //       'code': '02'
      //     },
      //     {
      //       'id': 1212,
      //       'name': '钢箱梁',
      //       'code': '03'
      //     }
      //   ]
      // },
      // {
      //   'id': 122,
      //   'name': '成品围护',
      //   'isLast': 0,
      //   'isNormalPart': 1,
      //   'code': '20',
      //   'basicClass': 8,
      //   'children': [
      //     {
      //       'id': 1220,
      //       'name': '压型彩钢板',
      //       'code': '01'
      //     },
      //     {
      //       'id': 1221,
      //       'name': '楼承板',
      //       'code': '02'
      //     },
      //     {
      //       'id': 1222,
      //       'name': '采光带',
      //       'code': '03'
      //     }
      //   ]
      // },
      {
        'id': 139,
        'name': '紧固件',
        'isLast': 0,
        'isNormalPart': 1,
        'code': '21',
        'basicClass': 8,
        'children': [{
          'id': 196,
          'name': '高强螺栓',
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
        'code': '22',
        'basicClass': 8,
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
        'isNormalPart': 1,
        'code': '23',
        'basicClass': 8,
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
        'code': '24',
        'basicClass': 16,
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

export default [
  getClassificationTree
]
