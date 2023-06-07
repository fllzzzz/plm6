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
            classifyId: 204,
            specification: 'M26 * 65',
            workshopId: 3
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
