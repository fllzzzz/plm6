import { rawMatClsEnum } from '@/utils/enum/modules/classification'

// 获取钢材对应材料配置的简要信息
const getSteelClassifyConfBrief = {
  url: '/api/config/structure-steel/classification/all/brief',
  timeout: 500,
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1, // id
            name: '钢板', // 名称
            basicClass: rawMatClsEnum.STEEL_PLATE.V, // 主科目分类
            boundFinalClassifyIds: [103, 104, 105, 106, 107], // 所绑定的科目的所有末级科目id集合
            links: [
              // 关联
              { specIndex: 1, keyword: 'PL' },
              { specIndex: 1, keyword: 'P' }
            ]
          },
          {
            id: 2,
            name: '钢管',
            basicClass: rawMatClsEnum.SECTION_STEEL.V, // 主科目分类,
            boundFinalClassifyIds: [115],
            links: [{ specIndex: 0, keyword: 'RHS' }]
          },
          {
            id: 3,
            name: '角钢',
            basicClass: rawMatClsEnum.SECTION_STEEL.V, // 主科目分类,
            boundFinalClassifyIds: [112],
            links: [{ specIndex: 0, keyword: 'L' }]
          },
          {
            id: 4,
            name: '工字钢',
            basicClass: rawMatClsEnum.SECTION_STEEL.V, // 主科目分类,
            boundFinalClassifyIds: [110],
            links: [{ specIndex: 0, keyword: 'I' }]
          }
        ],
        totalElements: 4
      }
    }
  }
}

export default [getSteelClassifyConfBrief]
