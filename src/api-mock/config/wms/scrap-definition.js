// 获取入库基础配置
const getSteelScrapDefinitionConf = {
  url: '/api/wms/config/steel/scrap-definition',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        // 钢板最短边长度
        steelPlateShortestSideMinLength: 1000,
        // 型材最小长度
        sectionSteelMinLength: 1000,
        // 钢卷最小长度
        steelCoilMinLength: 1000
      }
    }
  }
}

// 设置入库基础配置
const setSteelScrapDefinitionConf = {
  url: '/api/wms/config/steel/scrap-definition',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getSteelScrapDefinitionConf,
  setSteelScrapDefinitionConf
]
