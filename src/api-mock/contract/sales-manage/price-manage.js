// 查询可签证项目信息（签证用）
const getUserVisaProjects = {
  url: '/api/user/project/visa/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 5,
        'content': [
          {
            'id': 5,
            'name': '深圳华云声信息技术',
            'shortName': '华云声',
            'serialNumber': 'AAA',
            'isSubmitSettle': true
          },
          {
            'id': 4,
            'name': '杭州体育馆',
            'shortName': '杭州体育馆',
            'serialNumber': 'HZTY220506-1',
            'isSubmitSettle': false
          },
          {
            'id': 3,
            'name': '格力电器',
            'shortName': '格力电器',
            'serialNumber': '111',
            'isSubmitSettle': false
          }
        ]
      }
    }
  }
}

// 保存商务价格
const businessSave = {
  url: '/api/business',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 项目及单体造价
const cost = {
  url: '/api/business/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'projectPrice|100-10000': 1000,
        'monomerPrice|100-10000': 100,
        'shipPrice|100-10000': 100
      }
    }
  }
}

// 商务价格审核列表
const checkList = {
  url: '/api/business/check',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 2,
        'content': [
          {
            'id': 2,
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': 'CMJK220502'
            },
            'monomer': {
              'id': 1,
              'name': '射击馆'
            },
            'type': 2,
            'status': 2,
            'createUserName': '超级管理员',
            'createTime': 1651731872000,
            'checkUserName': '超级管理员',
            'checkTime': 1651731884000,
            'remark': null
          },
          {
            'id': 1,
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': 'CMJK220502'
            },
            'monomer': {
              'id': 1,
              'name': '射击馆'
            },
            'type': 2,
            'status': 2,
            'createUserName': '超级管理员',
            'createTime': 1651544423000,
            'checkUserName': '超级管理员',
            'checkTime': 1651544428000,
            'remark': null
          }
        ]
      }
    }
  }
}

// 商务价格审核
const check = {
  url: '/api/business/check',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取商务待未审核数量
const checkCount = {
  url: '/api/business/check/count',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      'data|0-100': 10
    }
  }
}

// 商务价格审核详情列表
const checkDetail = {
  url: RegExp('/api/business/check/' + '\\d'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'totalElements': 1,
        'content': [
          {
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': null
            },
            'monomer': {
              'id': 1,
              'name': '射击馆'
            },
            'type': 2,
            'category': null,
            'name': '钢梁1',
            'plate': null,
            'color': null,
            'material': 'Q355B',
            'thickness': null,
            'length': null,
            'classifyFullName': null,
            'classifyName': null,
            'serialNumber': null,
            'specification': null,
            'oldUnitPrice': null,
            'newUnitPrice': 9000
          }
        ]
      }
    }
  }
}

// 获取结构商务列表
const getArtifactList = {
  url: '/api/business/artifact',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'content|3': [{
          'id|+1': 1,
          'createTime': '@datetime(T)',
          'updateTime': '@datetime(T)',
          'projectId': 4,
          'monomerId': 4,
          'boolMergeEnum': true,
          'name|+1': ['钢柱', '钢梁', '箱型柱'],
          'material': 'Q355B',
          'totalQuantity|1-100': 10,
          'totalWeight|100-10000': 1000,
          'unitPrice|1-1000': 100
        }],
        'totalElements': 3
      }
    }
  }
}

// 获取结构商务汇总
const getArtifactCost = {
  url: '/api/business/artifact/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'quantity|1-100': 10,
        'mete|100-10000': 100,
        'price|100-10000': 100
      }
    }
  }
}

// 获取结构商务汇总
const artifactDetail = {
  url: RegExp('/api/business/artifact/' + '\\d'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 2,
        'content': [
          {
            'createTime': 1651808371000,
            'updateTime': 1651808371000,
            'oldArtifactId': null,
            'oldArtifactSerialNumber': null,
            'id': 172,
            'projectId': 4,
            'monomerId': 4,
            'areaId': 6,
            'boolStatusEnum': true,
            'boolAbnormalEnum': false,
            'name': '钢柱',
            'serialNumber': '7-1GKZ-1',
            'specification': 'BOX400*400*16*16',
            'length': 8125,
            'material': 'Q355B',
            'quantity': 1,
            'netWeight': 2639.82,
            'totalNetWeight': 2639.82,
            'grossWeight': 2667.92,
            'totalGrossWeight': 2667.92,
            'surfaceArea': 0,
            'drawingNumber': null,
            'remark': null,
            'userId': 1,
            'businessId': 10,
            'producedQuantity': 0,
            'areaType': 0,
            'specPrefix': 'BOX'
          },
          {
            'createTime': 1651808371000,
            'updateTime': 1651808371000,
            'oldArtifactId': null,
            'oldArtifactSerialNumber': null,
            'id': 173,
            'projectId': 4,
            'monomerId': 4,
            'areaId': 6,
            'boolStatusEnum': true,
            'boolAbnormalEnum': false,
            'name': '钢柱',
            'serialNumber': '7-1GKZ-2',
            'specification': 'BOX400*400*16*16',
            'length': 8125,
            'material': 'Q355B',
            'quantity': 1,
            'netWeight': 2213.79,
            'totalNetWeight': 2213.79,
            'grossWeight': 2243.54,
            'totalGrossWeight': 2243.54,
            'surfaceArea': 0,
            'drawingNumber': null,
            'remark': null,
            'userId': 1,
            'businessId': 10,
            'producedQuantity': 0,
            'areaType': 0,
            'specPrefix': 'BOX'
          }
        ]
      }
    }
  }
}

// 商务绑定
const businessBind = {
  url: '/api/business/bind',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 构件商务下拉框
const businessList = {
  url: '/api/business/artifact/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 3,
        'content': [
          {
            'id': 1,
            'name': '钢梁',
            'material': 'Q355B'
          },
          {
            'id': 2,
            'name': '钢梁1',
            'material': 'Q355B'
          },
          {
            'id': 13,
            'name': '钢柱',
            'material': 'Q355B'
          }
        ]
      }
    }
  }
}

// 获取围护商务列表
const getEnclosureList = {
  url: '/api/business/enclosure',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'content|3': [{
          'id|+1': 1,
          'createTime': '@datetime(T)',
          'updateTime': '@datetime(T)',
          'projectId': 4,
          'monomerId': 4,
          'category': 2,
          'plate': 'HV-820',
          'color': '豆绿',
          'boolMergeEnum': true,
          'name|+1': ['隔墙板', '墙面板', '隔离板'],
          'material': 'null',
          'thickness|0.1-2': 0.5,
          'length|1-100': 10,
          'totalQuantity|1-100': 10,
          'totalArea|1-100': 10,
          'totalLength|100-10000': 1000,
          'unitPrice|1-1000': 100
        }],
        'totalElements': 3
      }
    }
  }
}

// 获取围护商务汇总
const getEnclosureCost = {
  url: '/api/business/enclosure/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalQuantity': 30,
        'totalLength': 265300,
        'totalArea': 265.3,
        'totalLengthPrice': 0,
        'totalAreaPrice': 0
      }
    }
  }
}

// 获取配套件商务列表
const getAuxiliaryMaterialList = {
  url: '/api/business/auxiliary-material',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'content|3': [{
          'id|+1': 1,
          'createTime': '@datetime(T)',
          'updateTime': '@datetime(T)',
          'projectId': 4,
          'monomerId': 4,
          'classifyId': 277,
          'classifyFullName': '消防安保 > 消防用品 > 灭火器',
          'serialNumber': 'F9011-1',
          'specification': '干粉-1kg',
          'color': '红',
          'classifyName|+1': ['隔墙板', '墙面板', '隔离板'],
          'accountingUnit': '瓶',
          'accountingPrecision': 0,
          'mete|0-200': 0,
          'unitPrice|1-1000': 100
        }],
        'totalElements': 3
      }
    }
  }
}

// 获取配套件商务汇总
const getAuxiliaryMaterialCost = {
  url: '/api/business/auxiliary-material/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalMete': 251,
        'totalPrice': 0
      }
    }
  }
}

export default [
  getUserVisaProjects,
  businessSave,
  checkList,
  cost,
  check,
  checkCount,
  checkDetail,
  getArtifactList,
  getArtifactCost,
  artifactDetail,
  businessBind,
  businessList,
  businessList,
  getEnclosureList,
  getEnclosureCost,
  getAuxiliaryMaterialList,
  getAuxiliaryMaterialCost
]

