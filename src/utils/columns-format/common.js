// 基础时间
export const baseTimeColumns = [
  ['createTime', 'parse-time'], // 创建时间
  ['updateTime', 'parse-time'] // 发生修改时间
]

// 相对于“基础时间”多了“用户修改时间”
export const recordTimeColumns = [
  ...baseTimeColumns,
  ['userUpdateTime', 'parse-time'] // 用户修改时间
]

// 相对于“基础时间”多了“用户修改时间”与“审核时间”
export const reviewTimeColumns = [
  ...recordTimeColumns,
  ['reviewTime', 'parse-time'] // 审核时间
]
