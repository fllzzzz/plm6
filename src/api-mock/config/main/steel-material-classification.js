// 获取钢材对应材料配置的简要信息
const getSteelMaterialClassifyBrief = {
  url: '/api/config/steel-material/classification/all/brief',
  timeout: 500,
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            name: '钢板',
            boundSubjectIds: [103, 104, 105, 106, 107],
            links: [
              { specIndex: 1, keyword: 'PL' },
              { specIndex: 1, keyword: 'P' }
            ]
          },
          { id: 2, name: '钢管', boundSubjectIds: [115], links: [{ specIndex: 0, keyword: 'RHS' }] },
          { id: 3, name: '角钢', boundSubjectIds: [112], links: [{ specIndex: 0, keyword: 'L' }] },
          { id: 4, name: '工字钢', boundSubjectIds: [110], links: [{ specIndex: 0, keyword: 'I' }] }
        ],
        totalElements: 4
      }
    }
  }
}

export default [getSteelMaterialClassifyBrief]
