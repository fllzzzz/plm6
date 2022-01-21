import request from '@/utils/request'

/**
 * 合同台账（合同登记表）
 * @param {number} year 年份
 */
export function contractLedger({ year }) {
  return request({
    module: 'contract',
    url: 'project/listAllProject/print',
    method: 'get',
    params: { year }
  })
}

/**
 * 我的项目
 * @param {number} year 年份
 */
export function myProject({ year }) {
  return request({
    module: 'contract',
    url: 'project/listMy/print',
    method: 'get',
    params: { year }
  })
}

export default {
  contractLedger, // 合同台账（合同登记表）
  myProject // 我的项目
}
