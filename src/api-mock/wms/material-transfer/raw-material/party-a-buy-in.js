import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'

// 甲供买入记录
const get = {
  url: '/api/wms/transfer/party-a-buy-in',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            classifyId: 103,
            basicClass: rawMatClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000,
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            invoiceType: invoiceTypeEnum.SPECIAL.V,
            taxRate: 13,
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            transfer: {
              id: 1, // 调拨id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 借用调拨单号
            },
            applicantName: '@cname', // 调拨人
            reviewTime: '@datetime(T)' // 调拨时间
          },
          {
            id: 2,
            classifyId: 103,
            basicClass: rawMatClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            mete: 800000,
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1200,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            invoiceType: invoiceTypeEnum.SPECIAL.V,
            taxRate: 13,
            unitPrice: 0.01,
            amount: 10000,
            amountExcludingVAT: 8563.26,
            inputVAT: 1436.74,
            remark: '66666',
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            transfer: {
              id: 1, // 调拨id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 借用调拨单号
            },
            applicantName: '@cname', // 调拨人
            reviewTime: '@datetime(T)' // 调拨时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

// 甲供材料买入记录excel导出
const exportExcel = {
  url: '/api/wms/transfer/party-a-buy-in/excel',
  method: 'get',
  timeout: 500,
  rawResponse: async (req, res) => {
    let result = ''
    res.setHeader('Content-Type', 'application/vnd.ms-excel;charset=UTF-8')
    res.setHeader('Content-Disposition', 'attachment;filename=%E7%94%B2%E4%BE%9B%E6%9D%90%E6%96%99%E4%B9%B0%E5%85%A5%E8%AE%B0%E5%BD%95.xlsx')
    if (Math.random() > 0.5) {
      result = 'code=20000;message='
    } else {
      result =
        'code=40000;message=%E7%94%B2%E4%BE%9B%E6%9D%90%E6%96%99%E4%B9%B0%E5%85%A5%E8%AE%B0%E5%BD%95excel%E8%A1%A8%E6%A0%BC%E5%AF%BC%E5%87%BA%E6%97%B6%E9%97%B4%E8%8C%83%E5%9B%B4%E4%B8%8D%E5%8F%AF%E8%B6%85%E8%BF%87%E4%B8%80%E5%B9%B4'
    }
    res.setHeader('Result', result)
    res.statusCode = 200
    res.end(`甲供材料买入记录excel导出`)
  }
}

export default [get, exportExcel]
