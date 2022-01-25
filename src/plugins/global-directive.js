import checkPermission from '@/directive/permission'
import parseTime from '@/directive/parse-time'
import parseEnum from '@/directive/parse-enum'
import parseProject from '@/directive/parse-project'
import emptyText from '@/directive/empty-text'
import empty from '@/directive/empty'
import suffix from '@/directive/suffix'
import split from '@/directive/split'
import toFixed from '@/directive/to-fixed'
import toThousand from '@/directive/to-thousand'
import convertWeight from '@/directive/convert-weight'
import showThousand from '@/directive/show-thousand'

const directive = new Map([
  ['permission', checkPermission],
  ['parse-time', parseTime],
  ['parse-enum', parseEnum],
  ['parse-project', parseProject],
  ['empty-text', emptyText],
  ['empty', empty],
  ['suffix', suffix],
  ['split', split],
  ['to-fixed', toFixed],
  ['thousand', toThousand],
  ['convert-weight', convertWeight],
  ['show-thousand', showThousand]
])

export default (app) => {
  directive.forEach((dir, name) => {
    app.directive(name, dir)
  })
}
