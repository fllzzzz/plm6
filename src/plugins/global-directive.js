import checkPermission from '@/directive/permission'
import parseTime from '@/directive/parse-time'
import parseEnum from '@/directive/parse-enum'
import parseProject from '@/directive/parse-project'
import emptyText from '@/directive/empty-text'
import empty from '@/directive/empty'
import prefix from '@/directive/prefix'
import suffix from '@/directive/suffix'
import split from '@/directive/split'
import toFixed from '@/directive/to-fixed'
import toThousand from '@/directive/to-thousand'
import convertWeight from '@/directive/convert-weight'
import showThousand from '@/directive/show-thousand'

const directive = new Map([
  ['permission', checkPermission], // 校验权限
  ['parse-time', parseTime], // 格式转换：时间
  ['parse-enum', parseEnum], // 值转换：枚举
  ['parse-project', parseProject], // 格式转换：项目
  ['empty-text', emptyText], // 空值显示
  ['empty', empty], // 空值显示
  ['prefix', prefix], // 添加前缀
  ['suffix', suffix], // 添加后缀
  ['split', split], // 数组切割
  ['to-fixed', toFixed], // 保留小数精度
  ['thousand', toThousand], // 1000 => 1,000
  ['convert-weight', convertWeight],
  ['show-thousand', showThousand]
])

export default (app) => {
  directive.forEach((dir, name) => {
    app.directive(name, dir)
  })
}
