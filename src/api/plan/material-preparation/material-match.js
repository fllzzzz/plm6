
import request from '@/utils/request'

/**
 * 加载钢材匹配列表
 * @param {*} params
 * @returns
 */
export function getMatchSteelPlateList(params) {
  return request({
    module: 'wms',
    url: 'material-preparation/match/steel-plate',
    method: 'get',
    params
  })
}
