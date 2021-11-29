
// 钢材入库申请
const steelInboundApplication = {
  url: '/api/wms/inbound/application/steel',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [steelInboundApplication]
