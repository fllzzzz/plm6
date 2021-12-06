const getSteelPlate = {
  url: '/api/wms/material-inventory/steel-plate',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            serialNumber: /([0-9]{8})/,
            classifyId: 103,
            basicClass: 1,
            specification: 'Q325B',
            quantity: 10,
            frozenMete: 5000, // 冻结量
            frozenQuantity: 5, // 冻结数量
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            mete: 800000,
            // project: {
            //   'id|+1': 1,
            //   'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
            //   'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
            //   serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            // },
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ],
        totalElements: 2
      }
    }
  }
}

const getSectionSteel = {
  url: '/api/wms/material-inventory/section-steel',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 4,
            classifyId: 110,
            basicClass: 2,
            specification: '10*10*200*500',
            quantity: 10,
            frozenQuantity: 5, // 冻结数量
            mete: 10000,
            frozenMete: 5000, // 冻结量
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            length: 3907.62,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 4,
              name: '668号仓库'
            }
          }
        ],
        totalElements: 1
      }
    }
  }
}

const getSteelCoil = {
  url: '/api/wms/material-inventory/steel-coil',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 5,
            classifyId: 120,
            basicClass: 4,
            specification: 'DC51D+Z',
            quantity: 3907,
            frozenMete: 5000, // 冻结量
            frozenQuantity: 2000, // 冻结数量
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 10000,
            project: {
              id: 1,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '你脸红个泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂',
              shortName: '一工'
            },
            warehouse: {
              id: 4,
              name: '622号仓库'
            }
          }
        ],
        totalElements: 1
      }
    }
  }
}

export default [getSteelPlate, getSectionSteel, getSteelCoil]
