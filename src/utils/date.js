/**
 * 日期及时间处理工具类
 */
import { prefixZero } from '@/utils/data-type/number'

/**
 * 时间快捷选择：日
 * 时间选择器快捷选项（element pickerOption）
 */
export const elmShortcutsDay = [
  {
    text: '最近一周',
    onClick(picker) {
      const end = new Date()
      const start = new Date()
      start.setTime(start.getTime() - 3600 * 1000 * 24 * 7)
      picker.$emit('pick', [start, end])
    }
  },
  {
    text: '当前月份',
    onClick(picker) {
      const end = new Date()
      const start = new Date()
      start.setDate(1)
      start.setHours(0)
      start.setSeconds(0)
      start.setMinutes(0)
      picker.$emit('pick', [start, end])
    }
  },
  {
    text: '最近一个月',
    onClick(picker) {
      const end = new Date()
      const start = new Date()
      start.setTime(start.getTime() - 3600 * 1000 * 24 * 30)
      picker.$emit('pick', [start, end])
    }
  },
  {
    text: '最近三个月',
    onClick(picker) {
      const end = new Date()
      const start = new Date()
      start.setTime(start.getTime() - 3600 * 1000 * 24 * 90)
      picker.$emit('pick', [start, end])
    }
  },
  {
    text: '最近六个月',
    onClick(picker) {
      const end = new Date()
      const start = new Date()
      start.setMonth(start.getMonth() - 6)
      picker.$emit('pick', [start, end])
    }
  },
  {
    text: '今年至今',
    onClick(picker) {
      const end = new Date()
      const start = new Date(new Date().getFullYear(), 0)
      picker.$emit('pick', [start, end])
    }
  }
]

/**
 * 时间快捷选择：月
 * 时间选择器快捷选项（element pickerOption）
 */
export const elmShortcutsMonth = [
  {
    text: '本月',
    onClick(picker) {
      picker.$emit('pick', [new Date(), new Date()])
    }
  },
  {
    text: '今年至今',
    onClick(picker) {
      const end = new Date()
      const start = new Date(new Date().getFullYear(), 0)
      picker.$emit('pick', [start, end])
    }
  },
  {
    text: '最近六个月',
    onClick(picker) {
      const end = new Date()
      const start = new Date()
      start.setMonth(start.getMonth() - 6)
      picker.$emit('pick', [start, end])
    }
  }
]

/**
 * 时间格式化
 * @param {(Object|string|number)} time
 * @param {string} cFormat
 * @returns {string | null}
 */
export function parseTime(time, cFormat) {
  if (!time) {
    return null
  }
  if (arguments.length === 0) {
    return null
  }
  const format = cFormat || '{y}-{m}-{d} {h}:{i}:{s}'
  let date
  if (typeof time === 'object') {
    date = time
  } else {
    if (typeof time === 'string' && /^[0-9]+$/.test(time)) {
      time = parseInt(time)
    }
    if (typeof time === 'number' && time.toString().length === 10) {
      time = time * 1000
    }
    date = new Date(time)
  }
  const formatObj = {
    y: date.getFullYear(),
    m: date.getMonth() + 1,
    d: date.getDate(),
    h: date.getHours(),
    i: date.getMinutes(),
    s: date.getSeconds(),
    a: date.getDay()
  }
  const time_str = format.replace(/{([ymdhisa])+}/g, (result, key) => {
    const value = formatObj[key]
    // Note: getDay() returns 0 on Sunday
    if (key === 'a') {
      return ['日', '一', '二', '三', '四', '五', '六'][value]
    }
    return value.toString().padStart(2, '0')
  })
  return time_str
}

/**
 * 时间格式化
 * @param {number} time
 * @param {string} option
 * @returns {string}
 */
export function formatTime(time, option) {
  if (('' + time).length === 10) {
    time = parseInt(time) * 1000
  } else {
    time = +time
  }
  const d = new Date(time)
  const now = Date.now()

  const diff = (now - d) / 1000

  if (diff < 30) {
    return '刚刚'
  } else if (diff < 3600) {
    // less 1 hour
    return Math.ceil(diff / 60) + '分钟前'
  } else if (diff < 3600 * 24) {
    return Math.ceil(diff / 3600) + '小时前'
  } else if (diff < 3600 * 24 * 2) {
    return '1天前'
  }
  if (option) {
    return parseTime(time, option)
  } else {
    return d.getMonth() + 1 + '月' + d.getDate() + '日' + d.getHours() + '时' + d.getMinutes() + '分'
  }
}

/**
 * 计算时间差（天）
 * TODO:可改造为，根据传入单位来计算
 * @export
 * @param {*} sDate1
 * @param {*} sDate2
 * @returns
 */
export function dateDifference(sDate1, sDate2) {
  const oneDay = 24 * 60 * 60 * 1000
  sDate1 = new Date(+sDate1)
  sDate2 = new Date(+sDate2)
  sDate1 = `${sDate1.getFullYear()}-${prefixZero(sDate1.getMonth() + 1)}-${prefixZero(sDate1.getDate())}` // ie 下需要补0才能转换
  sDate2 = `${sDate2.getFullYear()}-${prefixZero(sDate2.getMonth() + 1)}-${prefixZero(sDate2.getDate())}`
  sDate1 = Date.parse(sDate1)
  sDate2 = Date.parse(sDate2)
  let dateSpan = sDate2 - sDate1
  dateSpan = Math.abs(dateSpan) + oneDay
  const iDays = Math.floor(dateSpan / oneDay)
  return iDays
}

/**
 * 格式化excel中的日期
 * @export
 * @param {*} numb
 * @returns
 */
export function formatExcelDate(numb) {
  const time = new Date((numb - 1) * 24 * 3600000 + 1)
  time.setYear(time.getFullYear() - 70)
  return time.getTime()
}
