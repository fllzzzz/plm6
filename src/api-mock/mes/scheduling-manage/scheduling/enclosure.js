import {
  mesEnclosureTypeEnum
} from '@enum-ms/mes'

const getEnclosure = {
  url: '/api/mes/building/enclosure/scheduling/page',
  method: 'get',
  timeout: 1000,
  response: (res) => {
    switch (Number(res.query.category)) {
      case mesEnclosureTypeEnum.PRESSED_FLOOR_PLATE.V:
        return {
          'code': 20000,
          'message': '成功',
          'data': {
            'hasPreviousPage': false,
            'hasNextPage': false,
            'totalElements': 1,
            'content': [{
              'createTime': null,
              'id': 1,
              'projectId': 39,
              'monomerId': 5,
              'districtId': 8,
              'boolStatusEnum': true,
              'category': 4,
              'name': '开口式楼层板',
              'serialNumber': 'WB1',
              'plate': 'HV-820',
              'color': null,
              'material': 'PE+AZ150',
              'weight': 111.00000000,
              'thickness': 0.53000000,
              'width': 820.00000000,
              'length': 12270.00000000,
              'quantity': 147,
              'totalLength': 1803.69000000,
              'totalArea': 1479.02580000,
              'remark': 'remark1',
              'businessId': null,
              'brand': null,
              'type': null,
              'capacity': null,
              'producedQuantity': 0,
              'totalSchedulingQuantity': 0,
              'totalTaskQuantity': 0,
              'schedulingProductionLineDTOS': [],
              'projectName': 'content测试',
              'monomerName': '单体1',
              'districtName': '区域1'
            }]
          }
        }
      case mesEnclosureTypeEnum.SANDWICH_BOARD.V:
        return {
          'code': 20000, 'message': '成功', 'data': {
            'hasPreviousPage': false,
            'hasNextPage': false,
            'totalElements': 1,
            'content': [{
              'createTime': null,
              'id': 3,
              'projectId': 39,
              'monomerId': 5,
              'districtId': 8,
              'boolStatusEnum': true,
              'category': 1,
              'name': '韩德板',
              'serialNumber': null,
              'plate': 'HV-1000',
              'color': null,
              'material': null,
              'weight': null,
              'thickness': 100.00000000,
              'width': 850.00000000,
              'length': 1000.00000000,
              'quantity': 10,
              'totalLength': 10.00000000,
              'totalArea': 8.50000000,
              'remark': null,
              'businessId': null,
              'brand': '中华',
              'type': '玻璃丝棉',
              'capacity': '120.00000000',
              'producedQuantity': 0,
              'totalSchedulingQuantity': 0,
              'totalTaskQuantity': 0,
              'schedulingProductionLineDTOS': [],
              'projectName': 'content测试',
              'monomerName': '单体1',
              'districtName': '区域1'
            }]
          }
        }
      case mesEnclosureTypeEnum.PRESSED_PLATE.V:
        return {
          'code': 20000, 'message': '成功', 'data': {
            'hasPreviousPage': false,
            'hasNextPage': false,
            'totalElements': 1,
            'content': [{
              'createTime': null,
              'id': 4,
              'projectId': 39,
              'monomerId': 5,
              'districtId': 8,
              'boolStatusEnum': true,
              'category': 2,
              'name': '屋面板',
              'serialNumber': 'WB1',
              'plate': 'HV-820',
              'color': '天蓝',
              'material': 'PE+AZ150',
              'weight': 6153.48684090,
              'thickness': 0.53000000,
              'width': 820.00000000,
              'length': 12270.00000000,
              'quantity': 147,
              'totalLength': 1803.69000000,
              'totalArea': 1479.02580000,
              'remark': 'remark11',
              'businessId': null,
              'brand': null,
              'type': null,
              'capacity': null,
              'producedQuantity': 0,
              'totalSchedulingQuantity': 0,
              'totalTaskQuantity': 0,
              'schedulingProductionLineDTOS': [],
              'projectName': 'content测试',
              'monomerName': '单体1',
              'districtName': '区域1'
            }]
          }
        }
      case mesEnclosureTypeEnum.FOLDING_PIECE.V:
        return {
          'code': 20000, 'message': '成功', 'data': {
            'hasPreviousPage': false,
            'hasNextPage': false,
            'totalElements': 1,
            'content': [{
              'createTime': null,
              'id': 6,
              'projectId': 39,
              'monomerId': 5,
              'districtId': 8,
              'boolStatusEnum': true,
              'category': 6,
              'name': '窗台度',
              'serialNumber': 'WB1',
              'plate': null,
              'color': '天蓝',
              'material': 'PE+AZ150',
              'weight': 1000.00000000,
              'thickness': 0.53000000,
              'width': 820.00000000,
              'length': 12270.00000000,
              'quantity': 147,
              'totalLength': 1803.69000000,
              'totalArea': 1479.02580000,
              'remark': 'remark11',
              'businessId': null,
              'brand': null,
              'type': null,
              'capacity': null,
              'producedQuantity': 0,
              'totalSchedulingQuantity': 0,
              'totalTaskQuantity': 0,
              'schedulingProductionLineDTOS': [],
              'projectName': 'content测试',
              'monomerName': '单体1',
              'districtName': '区域1'
            }]
          }
        }
      case mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V:
        return {
          'code': 20000, 'message': '成功', 'data': {
            'hasPreviousPage': false,
            'hasNextPage': false,
            'totalElements': 1,
            'content': [{
              'createTime': null,
              'id': 8,
              'projectId': 39,
              'monomerId': 5,
              'districtId': 8,
              'boolStatusEnum': true,
              'category': 3,
              'name': '桁架式楼承板',
              'serialNumber': 'B1',
              'plate': 'TD-75-1',
              'color': null,
              'material': null,
              'weight': null,
              'thickness': null,
              'width': 700.00000000,
              'length': 12270.00000000,
              'quantity': 147,
              'totalLength': 1803.69000000,
              'totalArea': 1262.58300000,
              'remark': null,
              'businessId': null,
              'brand': null,
              'type': null,
              'capacity': null,
              'producedQuantity': 0,
              'totalSchedulingQuantity': 0,
              'totalTaskQuantity': 0,
              'schedulingProductionLineDTOS': [],
              'projectName': 'content测试',
              'monomerName': '单体1',
              'districtName': '区域1'
            }]
          }
        }
      default:
        return {
          'code': 500,
          'message': '无此类型围护'
        }
    }
  }
}

export default [
  getEnclosure
]
