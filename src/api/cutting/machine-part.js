import request from '@/utils/request'

// 查看切割报表
export function get(params) {
  return request({
    url: `/api/cut/getMachinePartList`,
    method: 'get',
    params
  })
}

// 设备查看生产零件详情
export function getPartListByMac({ mac }) {
  return request({
    url: `/api/cut/getPartListByMac/${mac}`,
    method: 'get'
  })
}

export function exportByProject(params) {
  return request({
    url: `/api/cut/exportByProject`,
    method: 'get',
    params
  })
}

export default {
  get,
  exportByProject,
  getPartListByMac
}
