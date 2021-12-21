const get = {
  url: '/api/mes/building/analysis/production_summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'artifactAnalysis': {
          'surplusTaskQuantity|1-100': 0,
          'surplusNetWeight|1-1000.1-8': 0,
          'surplusGrossWeight|1-1000.1-8': 0,
          'taskQuantity|1-100': 0,
          'taskNetWeight|1-1000.1-8': 0,
          'taskGrossWeight|1-1000.1-8': 0,
          'inProductionQuantity|1-100': 0,
          'inProductionNetWeight|1-1000.1-8': 0,
          'inProductionGrossWeight|1-1000.1-8': 0,
          'completeQuantity|1-100': 0,
          'completeNetWeight|1-1000.1-8': 0,
          'completeGrossWeight|1-1000.1-8': 0
        },
        'enclosureAnalysis': {
          'completeArea|1-1000.1-8': 0,
          'completeLength|1-1000.1-8': 0,
          'completeQuantity|1-100': 0,
          'inProductionArea|1-1000.1-8': 0,
          'inProductionLength|1-1000.1-8': 0,
          'inProductionQuantity|1-100': 0,
          'surplusArea|1-1000.1-8': 0,
          'surplusLength|1-1000.1-8': 0,
          'surplusTaskQuantity|1-100': 0,
          'taskArea|1-1000.1-8': 0,
          'taskLength|1-1000.1-8': 0,
          'taskQuantity|1-100': 0
        }
      }
    }
  }
}

const getByGroup = {
  url: '/api/mes/building/analysis/production_summary/group',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'enclosureAnalysisList': [{
          'project': {
            'id': null,
            'name': 'hgg',
            'contractNo': 'hggggggg',
            'shortName': 'hghghgh',
            'type': null
          },
          'monomer': {
            'id': null,
            'name': '单体#1'
          },
          'areaDetail': {
            'id': null,
            'name': '区域'
          },
          'name': '桁架式楼承板',
          'material': null,
          'surplusTaskQuantity': 0,
          'surplusLength': 0,
          'surplusArea': 0,
          'taskQuantity': 20,
          'taskLength': null,
          'taskArea': 171780000,
          'inProductionQuantity': 7,
          'inProductionLength': null,
          'inProductionArea': 60123000,
          'completeQuantity': 4,
          'completeLength': null,
          'completeArea': 34356000
        }],
        'artifactAnalysisDTOList': [{
          'areaDetail': {
            'id': null,
            'name': '区域'
          },
          'completeGrossWeight': 20000,
          'completeNetWeight': 4135,
          'completeQuantity': 1,
          'inProductionGrossWeight': 140000,
          'inProductionNetWeight': 28950,
          'inProductionQuantity': 7,
          'material': 'Q355B',
          'monomer': {
            'id': null,
            'name': '单体#1'
          },
          'name': '焊接H钢柱',
          'project': {
            'contractNo': 'hggggggg',
            'id': null,
            'name': 'hgg',
            'shortName': 'hghghgh',
            'type': null
          },
          'surplusGrossWeight': 0,
          'surplusNetWeight': 0,
          'surplusTaskQuantity': 0,
          'taskGrossWeight': 1000000,
          'taskNetWeight': 206791,
          'taskQuantity': 50
        }]

      }
    }
  }
}

const getDetail = {
  url: '/api/mes/building/analysis/production_summary/details',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'enclosureDetailsAnalysisList': [{
          'project': {
            'id': null,
            'name': 'hgg',
            'contractNo': 'hggggggg',
            'shortName': 'hghghgh',
            'type': null
          },
          'monomer': {
            'id': null,
            'name': '单体#1'
          },
          'areaDetail': {
            'id': null,
            'name': '区域'
          },
          'id': 17,
          'name': '桁架式楼承板',
          'material': null,
          'serialNumber': 'B1',
          'surplusTaskQuantity': 0,
          'surplusLength': 0,
          'surplusArea': 0,
          'taskQuantity': 20,
          'taskLength': null,
          'taskArea': 171780000,
          'inProductionQuantity': 7,
          'inProductionLength': null,
          'inProductionArea': 60123000,
          'completeQuantity': 4,
          'completeLength': null,
          'completeArea': 34356000,
          'processSummaryList': [{
            'name': '围护工序1',
            'completeQuantity': 4,
            'inspectionQuantity': null
          }]
        }],
        'artifactDetailsAnalysisDTOList': [{
          'areaDetail': {
            'id': null,
            'name': '区域'
          },
          'completeGrossWeight': 20000,
          'completeNetWeight': 4135,
          'completeQuantity': 1,
          'id': 1,
          'inProductionGrossWeight': 140000,
          'inProductionNetWeight': 28950,
          'inProductionQuantity': 7,
          'material': 'Q355B',
          'monomer': {
            'id': null,
            'name': '单体#1'
          },
          'name': '焊接H钢柱',
          'processSummaryList': [{
            'completeQuantity': 2,
            'inspectionQuantity': null,
            'name': '构件1'
          },
          {
            'completeQuantity': 1,
            'inspectionQuantity': null,
            'name': '构件2'
          }
          ],
          'project': {
            'contractNo': 'hggggggg',
            'id': null,
            'name': 'hgg',
            'shortName': 'hghghgh',
            'type': null
          },
          'serialNumber': 'GZ1-3',
          'surplusGrossWeight': 0,
          'surplusNetWeight': 0,
          'surplusTaskQuantity': 0,
          'taskGrossWeight': 1000000,
          'taskNetWeight': 206791,
          'taskQuantity': 50
        }]
      }
    }
  }
}

export default [
  get,
  getByGroup,
  getDetail
]
