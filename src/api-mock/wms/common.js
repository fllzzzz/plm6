import { measureTypeEnum } from '@enum-ms/wms'

// 保存库存预警通知配置
const fetchInventoryNotify = {
  url: '/api/wms/material/inventory/reminder',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      data: {
        content: [
          {
            inventory: 0.0,
            minimumInventory: 1000.0,
            unitType: measureTypeEnum.MEASURE.V,
            classifyId: 1,
            specification: /([A-Z0-9]{2,3}\*){1,3}[A-Z0-9]{2,3}/,
            factoryId: 3
          }
        ],
        hasNextPage: false,
        totalElements: 1
      },
      message: '操作成功'
    }
  }
}

export default [fetchInventoryNotify]
