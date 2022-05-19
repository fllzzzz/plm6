import request from '@/utils/request'

/**
 * 进存销分析
 * @param {string} dataTime
 * @returns
 */
export function getInventoryAnalysis({ tax, type, dateTime }) {
  return request({
    url: `/api/operational/analysis/inventory/summary`,
    method: 'get',
    params: { tax, type, dateTime }
  })
}

/**
 * 进销存分析-库存
 * @param {string} dataTime
 * @returns
 */
export function getInventoryStock({ tax, type, dateTime }) {
  return request({
    url: `/api/operational/analysis/inventory/stock`,
    method: 'get',
    params: { tax, type, dateTime }
  })
}

/**
 * 进销存分析-钢材库存
 * @param {string} dataTime
 * @returns
 */
export function getInventorySteelStock({ tax, type, dateTime }) {
  return request({
    url: `/api/operational/analysis/inventory/stock/steel`,
    method: 'get',
    params: { tax, type, dateTime }
  })
}
