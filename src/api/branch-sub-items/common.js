import request from '@/utils/request'

/**
 * 获取外包区域树(构件及围护)（项目-单体区域-树）
 * @param {array} projectIds
 * @param {}
 * @returns
 */
export function getAreaOutsourcingTree(params) {
  return request({
    module: 'plan',
    url: `branch-sub-items/area/tree/outsourcing`,
    method: 'get',
    params: params
  })
}

