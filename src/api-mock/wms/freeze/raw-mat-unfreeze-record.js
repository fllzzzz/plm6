import { materialFreezeTypeEnum } from '@/utils/enum/modules/wms'

const get = {
  url: '/api/wms/freeze/raw-material/unfreeze',
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
            freezeType: materialFreezeTypeEnum.REQUISITIONS.V,
            remark: 'fffff', // 解冻备注
            document: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            material: {
              id: 1,
              boolPartyA: false, // 是否甲供材料
              serialNumber: /([0-9]{8})/,
              classifyId: 103,
              basicClass: 1,
              specification: 'Q325B',
              mete: 800000, // 解冻核算量
              quantity: 10, // 解冻数量
              thickness: 10,
              length: 1000,
              width: 1000,
              brand: '嘻嘻',
              heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
              project: {
                id: 1,
                name: '长安街666666号辅路',
                shortName: '长安街',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              factory: {
                id: 1,
                name: '一号工厂',
                shortName: '一工'
              },
              warehouse: {
                id: 1,
                name: '666号仓库'
              }
            },
            founderName: '@cname', // 操作人
            createTime: '@datetime(T)' // 创建日期
          },
          {
            id: 2,
            freezeType: materialFreezeTypeEnum.OUTBOUND.V,
            remark: '666666', // 解冻备注
            document: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            material: {
              id: 1,
              boolPartyA: false, // 是否甲供材料
              serialNumber: /([0-9]{8})/,
              classifyId: 103,
              basicClass: 1,
              specification: 'Q325B',
              mete: 200000, // 解冻核算量
              quantity: 2, // 解冻数量
              thickness: 10,
              length: 1000,
              width: 1500,
              brand: '嘻嘻',
              heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
              project: {
                id: 1,
                name: '长安街666666号辅路',
                shortName: '长安街',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              factory: {
                id: 2,
                name: '二号工厂',
                shortName: '二工'
              },
              warehouse: {
                id: 1,
                name: '667号仓库'
              }
            },
            founderName: '@cname', // 操作人
            createTime: '@datetime(T)' // 创建日期
          },
          {
            id: 3,
            freezeType: materialFreezeTypeEnum.TRANSFER.V,
            remark: '666666', // 解冻备注
            document: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            material: {
              id: 1,
              boolPartyA: false, // 是否甲供材料
              serialNumber: /([0-9]{8})/,
              classifyId: 103,
              basicClass: 1,
              specification: 'Q325B',
              mete: 210000, // 解冻核算量
              quantity: 3, // 解冻数量
              thickness: 10,
              length: 2000,
              width: 1500,
              brand: '嘻嘻',
              heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,5}/,
              project: {
                id: 1,
                name: '长安街666666号辅路',
                shortName: '长安街',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              factory: {
                id: 2,
                name: '二号工厂',
                shortName: '二工'
              },
              warehouse: {
                id: 1,
                name: '667号仓库'
              }
            },
            founderName: '@cname', // 操作人
            createTime: '@datetime(T)' // 创建日期
          }
        ],
        totalElements: 3
      }
    }
  }
}

export default [get]
