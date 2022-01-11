import { businessTypeEnum } from '@enum-ms/contract'

const getProject = {
  url: '/api/project/listAllProject',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 7,
        'content': [{
          'id': 43,
          'serialNumber': 'dsas541565',
          'name': '高科技的干活呢',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,檩条加工,天沟加工,夹芯板加工',
          'contractAmount': 1111111.00,
          'allDays': null,
          'alreadyDays': 32,
          'projectManagerName': '张磊',
          'signerDeptName': '公司',
          'signerName': null,
          'signingDate': null,
          'createDate': 1635436800000,
          'status': 0,
          'settlementStatus': 0
        }, {
          'id': 42,
          'serialNumber': 'tech测试',
          'name': 'tech测试',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,檩条加工,天沟加工,夹芯板加工,桁架楼承板加工,压型楼承板加工,压型彩板加工',
          'contractAmount': 100000.00,
          'allDays': null,
          'alreadyDays': 33,
          'projectManagerName': null,
          'signerDeptName': null,
          'signerName': null,
          'signingDate': null,
          'createDate': 1635350400000,
          'status': 0,
          'settlementStatus': 0
        }, {
          'id': 41,
          'serialNumber': 'aaaatest',
          'name': 'aaaatest',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,檩条加工,天沟加工,夹芯板加工,桁架楼承板加工,压型彩板加工,压型楼承板加工',
          'contractAmount': 100000.00,
          'allDays': null,
          'alreadyDays': 39,
          'projectManagerName': null,
          'signerDeptName': null,
          'signerName': null,
          'signingDate': null,
          'createDate': 1634832000000,
          'status': 0,
          'settlementStatus': 0
        }, {
          'id': 40,
          'serialNumber': 'ssstest',
          'name': 'ssstest',
          'businessType': 2,
          'projectType': 1,
          'projectContent': '围护工程,结构工程',
          'contractAmount': 100.00,
          'allDays': null,
          'alreadyDays': 39,
          'projectManagerName': null,
          'signerDeptName': null,
          'signerName': null,
          'signingDate': null,
          'createDate': 1634832000000,
          'status': 0,
          'settlementStatus': 0
        }, {
          'id': 39,
          'serialNumber': 'content测试',
          'name': 'content测试',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,天沟加工,桁架楼承板加工,夹芯板加工,压型彩板加工,压型楼承板加工',
          'contractAmount': 1000000.00,
          'allDays': null,
          'alreadyDays': 50,
          'projectManagerName': null,
          'signerDeptName': null,
          'signerName': null,
          'signingDate': null,
          'createDate': 1633881600000,
          'status': 0,
          'settlementStatus': 0
        }, {
          'id': 38,
          'serialNumber': 'hggggggg',
          'name': 'hgg',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,桁架楼承板加工,压型楼承板加工,压型彩板加工',
          'contractAmount': 999999999999.00,
          'allDays': null,
          'alreadyDays': 50,
          'projectManagerName': null,
          'signerDeptName': null,
          'signerName': null,
          'signingDate': null,
          'createDate': 1633881600000,
          'status': 0,
          'settlementStatus': 0
        }, {
          'id': 37,
          'serialNumber': 'fsdfsfsfsfsf',
          'name': 'fsfsfsfs',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,檩条加工,桁架楼承板加工,压型楼承板加工',
          'contractAmount': 999999999999.00,
          'allDays': 81,
          'alreadyDays': 50,
          'projectManagerName': null,
          'signerDeptName': null,
          'signerName': null,
          'signingDate': null,
          'createDate': 1633881600000,
          'status': 0,
          'settlementStatus': 0
        }]
      }
    }
  }
}

const addProject ={
  url: '/api/project',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delProject = {
  url: '/api/project',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editProject = {
  url: '/api/project/updateProject',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editProjectStatus = {
  url: RegExp('/api/project/' + '[1-9][0-9]*' + '/status/' + '[1-9][0-9]*'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const getProjectContentInfo ={
  url: '/api/project/getAllType',
  method: 'get',
  timeout: 1000,
  response: (res) => {
    switch (Number(res.query.businessType)) {
      case  businessTypeEnum.ENUM.MACHINING.V:
        return {
          'code': 20000,
          'message': '成功',
          'data': {
            'businessTypeVOList': [{
              'name': '加工承揽',
              'type': 1
            }, {
              'name': '项目承包',
              'type': 2
            }],
            'projectTypeVOList': [{
              'name': '建钢',
              'type': 1
            }, {
              'name': '桥梁',
              'type': 2
            }, {
              'name': '立体停车库',
              'type': 3
            }, {
              'name': '围护',
              'type': 4
            }],
            'projectContentVOList': [{
              'type': 'STRUCTURE',
              'label': '结构',
              'contentList': [{
                'id': 1,
                'name': '主结构加工',
                'no': '5'
              }, {
                'id': 2,
                'name': '檩条加工',
                'no': '6'
              }, {
                'id': 3,
                'name': '天沟加工',
                'no': '7'
              }]
            }, {
              'type': 'ENCLOSURE',
              'label': '围护',
              'contentList': [{
                'id': 4,
                'name': '夹芯板加工',
                'no': '1'
              }, {
                'id': 5,
                'name': '桁架楼承板加工',
                'no': '3'
              }, {
                'id': 6,
                'name': '压型楼承板加工',
                'no': '4'
              }, {
                'id': 7,
                'name': '压型彩板加工',
                'no': '2'
              }]
            }]
          }
        }
      default:
        return {
          'code': 20000,
          'message': '成功',
          'data': {
            'businessTypeVOList': [{
              'name': '加工承揽',
              'type': 1
            }, {
              'name': '项目承包',
              'type': 2
            }],
            'projectTypeVOList': [{
              'name': '建钢',
              'type': 1
            }, {
              'name': '桥梁',
              'type': 2
            }, {
              'name': '立体停车库',
              'type': 3
            }, {
              'name': '围护',
              'type': 4
            }],
            'projectContentVOList': [{
              'id': 8,
              'name': '结构工程',
              'alias': 'STRUCTURE'
            }, {
              'id': 9,
              'name': '围护工程',
              'alias': 'ENCLOSURE'
            }, {
              'id': 10,
              'name': '门窗工程',
              'alias': null
            }, {
              'id': 11,
              'name': '消防工程',
              'alias': null
            }, {
              'id': 12,
              'name': '幕墙工程',
              'alias': null
            }]
          }
        }
      }
  }
}

const getUserAllSimpleByProject = {
  url: RegExp('/api/user/project/' + '[1-9][0-9]*' + '/all/simple'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 6,
        'content': [{
          'id': 1,
          'name': '超级管理员',
          'deptId': 1,
          'deptName': '公司'
        }, {
          'id': 2,
          'name': '张磊',
          'deptId': 1,
          'deptName': '公司'
        }, {
          'id': 42,
          'name': '张晓',
          'deptId': 1,
          'deptName': '公司'
        }, {
          'id': 51,
          'name': '张飞波',
          'deptId': 1,
          'deptName': '公司'
        }, {
          'id': 58,
          'name': '杨娟',
          'deptId': 1,
          'deptName': '公司'
        }, {
          'id': 59,
          'name': '王子豪',
          'deptId': 1,
          'deptName': '公司'
        }]
      }
    }
  }
}

const getEnclosureDictList = {
  url: RegExp('/api/project/listByType/' + '.*' ),
  method: 'get',
  timeout: 1000,
  response: (res) => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 23,
        'content': [{
          'name': 'brand',
          'remark': '品牌',
          'labels': ['名牌', '2']
        }, {
          'name': 'thickness',
          'remark': '厚度',
          'labels': ['10', '2']
        }, {
          'name': 'plateType',
          'remark': '板型',
          'labels': ['长', '正']
        }, {
          'name': 'effective_width',
          'remark': '有效宽度',
          'labels': ['10', '5', '10000', '66', '9', '4']
        }, {
          'name': 'out_material',
          'remark': '外板钢板材质',
          'labels': []
        }, {
          'name': 'out_coating',
          'remark': '外板钢板涂层',
          'labels': []
        }, {
          'name': 'out_plating',
          'remark': '外板钢板镀层',
          'labels': []
        }, {
          'name': 'out_steel_plate_brand',
          'remark': '外板钢板品牌',
          'labels': []
        }, {
          'name': 'out_thickness',
          'remark': '外板钢板厚度',
          'labels': []
        }, {
          'name': 'out_colour',
          'remark': '外板钢板颜色',
          'labels': []
        }, {
          'name': 'out_effective_width',
          'remark': '外板钢板宽度',
          'labels': []
        }, {
          'name': 'out_plate_shape',
          'remark': '外板钢板形状',
          'labels': []
        }, {
          'name': 'in_material',
          'remark': '内板钢板材质',
          'labels': []
        }, {
          'name': 'in_coating',
          'remark': '内板钢板涂层',
          'labels': []
        }, {
          'name': 'in_plating',
          'remark': '内板钢板镀层',
          'labels': []
        }, {
          'name': 'in_Steel_Plate_brand',
          'remark': '内板钢板品牌',
          'labels': []
        }, {
          'name': 'in_thickness',
          'remark': '内板钢板厚度',
          'labels': []
        }, {
          'name': 'in_effective_width',
          'remark': '内板钢板宽度',
          'labels': []
        }, {
          'name': 'in_plate_shape',
          'remark': '内板钢板形状',
          'labels': []
        }, {
          'name': 'in_colour',
          'remark': '内板钢板颜色',
          'labels': []
        }, {
          'name': 'kind_core',
          'remark': '芯材种类',
          'labels': ['符合木', '杉木', '测试二十字测试二十字', '测试保存', '9', '4']
        }, {
          'name': 'brand_core',
          'remark': '芯材品牌',
          'labels': ['兔宝宝芯', '莫干山芯', '测试二十字测试二十字测试二十字测试二十字', '测试保存', '9', '4']
        }, {
          'name': 'unit_weight_core',
          'remark': '芯材容重',
          'labels': ['1512155', '9999', '8888', '888', '65', '66', '9', '4']
        }]
      }
    }
  }
}


const getContractBase = {
  url: RegExp('/api/project/' + '[1-9][0-9]*' + '/base'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'id': 42,
        'serialNumber': 'tech测试',
        'name': 'tech测试',
        'shortName': 'tech测试',
        'countryId': null,
        'provinceId': null,
        'cityId': null,
        'regionId': null,
        'regionalFullName': '',
        'address': null,
        'startDate': 1635350400000,
        'endDate': null,
        'contractAmount': 100000.00,
        'prepayments': null,
        'managementFeeRate': 0.00,
        'marginAmount': null,
        'marginType': null,
        'currencyType': null,
        'projectManagerId': null,
        'projectManagerFullName': null,
        'businessLeaderId': null,
        'businessLeaderFullName': null,
        'businessLeaderTwoId': null,
        'businessLeaderTwoFullName': null,
        'attachments': null
      }
    }
  }
}
const getContractBusiness = {
  url: RegExp('/api/project/' + '[1-9][0-9]*' + '/business'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'id': 42,
        'signingMainBodyId': null,
        'signingMainBodyName': '河南六建建筑集团有限公司',
        'businessType': 1,
        'businessTypeDesc': '加工承揽',
        'projectType': 1,
        'projectTypeDesc': '建钢',
        'projectContentList': [{
          'no': 5,
          'name': '主结构加工',
          'id': 1
        }, {
          'no': 6,
          'name': '檩条加工',
          'id': 2
        }, {
          'no': 7,
          'name': '天沟加工',
          'id': 3
        }, {
          'no': 1,
          'name': '夹芯板加工',
          'id': 4
        }, {
          'no': 3,
          'name': '桁架楼承板加工',
          'id': 5
        }, {
          'no': 4,
          'name': '压型楼承板加工',
          'id': 6
        }, {
          'no': 2,
          'name': '压型彩板加工',
          'id': 7
        }],
        'transportMode': null,
        'structureMeasureMode': null,
        'enclosureMeasureMode': null,
        'signingAddress': null,
        'signingDate': null,
        'signerId': null,
        'signerName': null,
        'isTax': 0,
        'invoiceType': 1,
        'payType': null,
        'payTypeDesc': null
      }
    }
  }
}
const getContractCustomer = {
  url: RegExp('/api/project/' + '[1-9][0-9]*' + '/customer'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'id': 42,
        'customerUnit': null,
        'socialCode': null,
        'customerUnitPhone': null,
        'customerProvinceId': null,
        'customerProvinceName': null,
        'customerCityId': null,
        'customerCityName': null,
        'customerRegionId': null,
        'customerRegionName': null,
        'customerCountryId': null,
        'customerCountryName': '',
        'customerAddress': null,
        'customerBankUserName': null,
        'customerBankCode': null,
        'customerBankName': null,
        'customerManagerOne': null,
        'customerManagerOnePhone': null,
        'customerManagerTwo': null,
        'customerManagerTwoPhone': null,
        'customerEmail': null
      }
    }
  }
}

const getContractTechInfo = {
  url: RegExp('/api/project/techDisclosure/' + '.*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'structureList': [{
          'createTime': 1633942445000,
          'id': 9,
          'project_id': 39,
          'type': null,
          'techDesc': null,
          'status': 1,
          'userId': 1
        }],
        'profiledPlateList': [{
          'createTime': 1633942445364,
          'id': 4,
          'project_id': 39,
          'coating': '白色',
          'colour': '红绿',
          'plateType': '压型钢板',
          'plating': '绿色',
          'status': 1,
          'thickness': 4.0,
          'brand': '名牌',
          'usePart': null,
          'userId': 1
        }],
        'trussFloorPlateList': [],
        'pressureBearingPlateList': [{
          'createTime': 1633942445000,
          'id': 4,
          'project_id': 39,
          'mode': '1',
          'plateType': '1',
          'thickness': 1.0,
          'brand': '1',
          'plating': '1',
          'usePart': null,
          'userId': 1,
          'status': 1
        }],
        'sandwichBoardList': []
      }
    }
  }
}

const getBranchCompanyAllSimple = {
  url: '/api/branchCompany/all/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 3,
        'content': [{
          'id': 2,
          'name': '河南六建建筑集团有限公司',
          'sort': 1
        }, {
          'id': 1,
          'name': '河南六建重工有限公司',
          'sort': 2
        }, {
          'id': 3,
          'name': '河南六建集团钢结构分公司',
          'sort': 3
        }]
      }
    }
  }
}

export default [
  getProject,
  addProject,
  delProject,
  editProjectStatus,
  getEnclosureDictList,
  getProjectContentInfo,
  getUserAllSimpleByProject,
  getContractBase,
  getContractBusiness,
  getContractCustomer,
  getContractTechInfo,
  getBranchCompanyAllSimple
]
