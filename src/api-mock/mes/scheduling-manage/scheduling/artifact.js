import { artifactListInfo } from '../../common-mock-data/product-type-data'

const getArtifact = {
  url: '/api/mes/building/scheduling/artifact/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 2,
        'content': [{
          'createTime': null,
          'id': 5,
          'projectId': 39,
          'monomerId': 5,
          'areaId': 8,
          'boolStatusEnum': true,
          'name': '焊接H钢柱',
          'serialNumber': 'GZ1-3',
          'specification': 'BH600*600*14*28',
          'length': 10919.00000000,
          'material': 'Q355B',
          'quantity': 10,
          'netWeight': 4135.82000000,
          'totalNetWeight': 4135.82000000,
          'grossWeight': 4135.82000000,
          'totalGrossWeight': 4135.82000000,
          'surfaceArea': 12.00000000,
          'drawingNumber': null,
          'remark': null,
          'businessId': null,
          'assembleId': null,
          'assembleSerialNumber': null,
          'producedQuantity': 0,
          'totalSchedulingQuantity': 0,
          'totalTaskQuantity': 0,
          'schedulingProductionLineDTOS': [],
          'projectName': 'content测试',
          'monomerName': '单体1',
          'areaName': '区域1',
          'packageQuantity': null,
          'unPackageQuantity': null
        }, {
          'createTime': null,
          'id': 6,
          'projectId': 39,
          'monomerId': 5,
          'areaId': 8,
          'boolStatusEnum': true,
          'name': '焊接H钢柱',
          'serialNumber': 'GZ1-4',
          'specification': 'BH600*600*14*28',
          'length': 9770.00000000,
          'material': 'Q355B',
          'quantity': 10,
          'netWeight': 3968.98000000,
          'totalNetWeight': 3968.98000000,
          'grossWeight': 3968.98000000,
          'totalGrossWeight': 3968.98000000,
          'surfaceArea': 12.00000000,
          'drawingNumber': null,
          'remark': null,
          'businessId': null,
          'assembleId': null,
          'assembleSerialNumber': null,
          'producedQuantity': 0,
          'totalSchedulingQuantity': 0,
          'totalTaskQuantity': 0,
          'schedulingProductionLineDTOS': [],
          'projectName': 'content测试',
          'monomerName': '单体1',
          'areaName': '区域1',
          'packageQuantity': null,
          'unPackageQuantity': null
        }, {
          ...artifactListInfo,
          'boolStatusEnum': true,
          'schedulingProductionLineDTOS': [],
          'totalSchedulingQuantity|1-100': 0,
          'totalTaskQuantity|1-100': 0,
          'packageQuantity|1-100': 1,
          'unPackageQuantity|1-100': 1
        }]
      }
    }
  }
}

export default [
  getArtifact
]
