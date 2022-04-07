import request from '@/utils/request'

// 项目数据页面查询
export function get(params) {
  return request({
    url: '/api/cut/getPlant',
    method: 'get',
    params
  })
}

// 机器列表
export function getMachine(params) {
  return request({
    url: '/api/cut/getMachine',
    method: 'get',
    params
  })
}

// 零件
export function getCutPart(params) {
  return request({
    url: `/api/cut/getCutPart/${params}`,
    method: 'get'

  })
}

// 余料
export function getCutSurplus(params) {
  return request({
    url: `/api/cut/getCutSurplus/${params}`,
    method: 'get'
  })
}

// 暂停任务
export function suspendTask(data) {
  return request({
    url: `/api/cut/suspendTask`,
    method: 'post',
    data
  })
}

// 继续任务
export function continueTask(data) {
  return request({
    url: `/api/cut/continueTask`,
    method: 'post',
    data
  })
}

export default {
  get,
  getCutPart,
  continueTask,
  getCutSurplus,
  suspendTask,
  getMachine
}
