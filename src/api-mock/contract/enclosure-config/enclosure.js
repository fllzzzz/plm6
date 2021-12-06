import { TechnologyTypeEnum } from '@enum-ms/contract'

const getEnclosure = {
  url: '/api/enclosure/dictionaries/dict',
  method: 'get',
  timeout: 1000,
  response: (res) => {
    switch (Number(res.query.type)) {
      case TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V:
        return {
          'code': 20000,
          'message': '操作成功',
          'data': {
            'totalElements': 3,
            'content': [{
              'code': '编号1',
              'status': true
            }, {
              'code': '编号2',
              'status': true
            }, {
              'code': '1',
              'status': true
            }]
          }
        }
      default:
        return {
          'code': 20000,
          'message': '操作成功',
          'data': {
            'totalElements': 23,
            'content': [{
              'name': 'brand',
              'remark': '品牌',
              'dictDetails': [{
                'id': 1,
                'name': 'brand',
                'remark': '品牌',
                'label': '中国',
                'sort': 1,
                'createTime': 1623915095645
              }, {
                'id': 226,
                'name': 'brand',
                'remark': '品牌',
                'label': '名牌',
                'sort': 6,
                'createTime': 1632478805118
              }]
            }, {
              'name': 'thickness',
              'remark': '厚度',
              'dictDetails': [{
                'id': 44,
                'name': 'thickness',
                'remark': '厚度',
                'label': '10',
                'sort': 2,
                'createTime': 1623918527047
              }, {
                'id': 177,
                'name': 'thickness',
                'remark': '厚度',
                'label': '2',
                'sort': 2,
                'createTime': 1624010032494
              }]
            }, {
              'name': 'plateType',
              'remark': '板型',
              'dictDetails': [{
                'id': 227,
                'name': 'plateType',
                'remark': '板型',
                'label': '长',
                'sort': 1,
                'createTime': 1632621315391
              }, {
                'id': 228,
                'name': 'plateType',
                'remark': '板型',
                'label': '正',
                'sort': 2,
                'createTime': 1632621330165
              }]
            }, {
              'name': 'effective_width',
              'remark': '有效宽度',
              'dictDetails': [{
                'id': 46,
                'name': 'effective_width',
                'remark': '有效宽度',
                'label': '10',
                'sort': 2,
                'createTime': 1623918527049
              }, {
                'id': 61,
                'name': 'effective_width',
                'remark': '有效宽度',
                'label': '5',
                'sort': 2,
                'createTime': 1623918527066
              }, {
                'id': 125,
                'name': 'effective_width',
                'remark': '有效宽度',
                'label': '10000',
                'sort': 4,
                'createTime': 1623980123234
              }, {
                'id': 183,
                'name': 'effective_width',
                'remark': '有效宽度',
                'label': '66',
                'sort': 5,
                'createTime': 1624409651319
              }, {
                'id': 197,
                'name': 'effective_width',
                'remark': '有效宽度',
                'label': '9',
                'sort': 6,
                'createTime': 1624410067146
              }, {
                'id': 212,
                'name': 'effective_width',
                'remark': '有效宽度',
                'label': '4',
                'sort': 7,
                'createTime': 1624410286766
              }]
            }, {
              'name': 'out_material',
              'remark': '外板钢板材质',
              'dictDetails': null
            }, {
              'name': 'out_coating',
              'remark': '外板钢板涂层',
              'dictDetails': null
            }, {
              'name': 'out_plating',
              'remark': '外板钢板镀层',
              'dictDetails': null
            }, {
              'name': 'out_steel_plate_brand',
              'remark': '外板钢板品牌',
              'dictDetails': null
            }, {
              'name': 'out_thickness',
              'remark': '外板钢板厚度',
              'dictDetails': null
            }, {
              'name': 'out_colour',
              'remark': '外板钢板颜色',
              'dictDetails': null
            }, {
              'name': 'out_effective_width',
              'remark': '外板钢板宽度',
              'dictDetails': null
            }, {
              'name': 'out_plate_shape',
              'remark': '外板钢板形状',
              'dictDetails': null
            }, {
              'name': 'in_material',
              'remark': '内板钢板材质',
              'dictDetails': null
            }, {
              'name': 'in_coating',
              'remark': '内板钢板涂层',
              'dictDetails': null
            }, {
              'name': 'in_plating',
              'remark': '内板钢板镀层',
              'dictDetails': null
            }, {
              'name': 'in_Steel_Plate_brand',
              'remark': '内板钢板品牌',
              'dictDetails': null
            }, {
              'name': 'in_thickness',
              'remark': '内板钢板厚度',
              'dictDetails': null
            }, {
              'name': 'in_effective_width',
              'remark': '内板钢板宽度',
              'dictDetails': null
            }, {
              'name': 'in_plate_shape',
              'remark': '内板钢板形状',
              'dictDetails': null
            }, {
              'name': 'in_colour',
              'remark': '内板钢板颜色',
              'dictDetails': null
            }, {
              'name': 'kind_core',
              'remark': '芯材种类',
              'dictDetails': [{
                'id': 54,
                'name': 'kind_core',
                'remark': '芯材种类',
                'label': '符合木',
                'sort': 2,
                'createTime': 1623918527061
              }, {
                'id': 67,
                'name': 'kind_core',
                'remark': '芯材种类',
                'label': '杉木',
                'sort': 2,
                'createTime': 1623918527070
              }, {
                'id': 158,
                'name': 'kind_core',
                'remark': '芯材种类',
                'label': '测试二十字测试二十字',
                'sort': 3,
                'createTime': 1623982196411
              }, {
                'id': 192,
                'name': 'kind_core',
                'remark': '芯材种类',
                'label': '测试保存',
                'sort': 4,
                'createTime': 1624409651331
              }, {
                'id': 206,
                'name': 'kind_core',
                'remark': '芯材种类',
                'label': '9',
                'sort': 5,
                'createTime': 1624410067161
              }, {
                'id': 221,
                'name': 'kind_core',
                'remark': '芯材种类',
                'label': '4',
                'sort': 6,
                'createTime': 1624410286783
              }]
            }, {
              'name': 'brand_core',
              'remark': '芯材品牌',
              'dictDetails': [{
                'id': 55,
                'name': 'brand_core',
                'remark': '芯材品牌',
                'label': '兔宝宝芯',
                'sort': 2,
                'createTime': 1623918527062
              }, {
                'id': 68,
                'name': 'brand_core',
                'remark': '芯材品牌',
                'label': '莫干山芯',
                'sort': 2,
                'createTime': 1623918527071
              }, {
                'id': 159,
                'name': 'brand_core',
                'remark': '芯材品牌',
                'label': '测试二十字测试二十字测试二十字测试二十字',
                'sort': 3,
                'createTime': 1623982196412
              }, {
                'id': 193,
                'name': 'brand_core',
                'remark': '芯材品牌',
                'label': '测试保存',
                'sort': 4,
                'createTime': 1624409651332
              }, {
                'id': 207,
                'name': 'brand_core',
                'remark': '芯材品牌',
                'label': '9',
                'sort': 5,
                'createTime': 1624410067162
              }, {
                'id': 222,
                'name': 'brand_core',
                'remark': '芯材品牌',
                'label': '4',
                'sort': 6,
                'createTime': 1624410286787
              }]
            }, {
              'name': 'unit_weight_core',
              'remark': '芯材容重',
              'dictDetails': [{
                'id': 136,
                'name': 'unit_weight_core',
                'remark': '芯材容重',
                'label': '1512155',
                'sort': 4,
                'createTime': 1623980123246
              }, {
                'id': 160,
                'name': 'unit_weight_core',
                'remark': '芯材容重',
                'label': '9999',
                'sort': 5,
                'createTime': 1623985346816
              }, {
                'id': 163,
                'name': 'unit_weight_core',
                'remark': '芯材容重',
                'label': '8888',
                'sort': 6,
                'createTime': 1623985952146
              }, {
                'id': 169,
                'name': 'unit_weight_core',
                'remark': '芯材容重',
                'label': '888',
                'sort': 7,
                'createTime': 1624007163957
              }, {
                'id': 179,
                'name': 'unit_weight_core',
                'remark': '芯材容重',
                'label': '65',
                'sort': 8,
                'createTime': 1624362547480
              }, {
                'id': 194,
                'name': 'unit_weight_core',
                'remark': '芯材容重',
                'label': '66',
                'sort': 9,
                'createTime': 1624409651333
              }, {
                'id': 208,
                'name': 'unit_weight_core',
                'remark': '芯材容重',
                'label': '9',
                'sort': 10,
                'createTime': 1624410067163
              }, {
                'id': 223,
                'name': 'unit_weight_core',
                'remark': '芯材容重',
                'label': '4',
                'sort': 11,
                'createTime': 1624410286788
              }]
            }]
          }
        }
    }
  }
}

const addEnclosure = {
  url: `/api/enclosure/dictionaries/dictDetailSave/type/${TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V}`,
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': true
    }
  }
}

const delEnclosure = {
  url: '/api/enclosure/dictionaries/dict',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editEnclosureStatus = {
  url: RegExp('/api/enclosure/dictionaries/dictDetailSave/type/' + '.*'),
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': true
    }
  }
}

export default [
  getEnclosure,
  addEnclosure,
  delEnclosure,
  editEnclosureStatus
]
