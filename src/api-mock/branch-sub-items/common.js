import { componentTypeEnum } from '@enum-ms/building-steel'

// 获取外包区域树(构件及围护)
const getAreaOutsourcingTree = {
  url: RegExp('/api/plan/branch-sub-items/area/tree/outsourcing?' + '.*'),
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: [
        {
          id: 1, // 项目id
          name: '老仙师项目', // 项目简称
          children: [
            {
              id: 1,
              name: '1号楼',
              children: [
                { id: 1, name: 'A区', type: componentTypeEnum.STRUCTURE.V },
                { id: 2, name: 'A区', type: componentTypeEnum.ENCLOSURE.V }
              ]
            },
            {
              id: 2,
              name: '2号楼',
              children: [
                { id: 3, name: 'C区', type: componentTypeEnum.STRUCTURE.V },
                { id: 4, name: 'D区', type: componentTypeEnum.STRUCTURE.V }
              ]
            }
          ]
        },
        {
          id: 2, // 项目id
          name: '有点骚项目', // 项目简称
          children: [
            {
              id: 3,
              name: '游泳馆',
              children: [
                { id: 5, name: '1区', type: componentTypeEnum.STRUCTURE.V }
              ]
            }
          ]
        }
      ]
    }
  }
}

export default [
  getAreaOutsourcingTree
]
