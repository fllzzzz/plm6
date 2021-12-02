import checkPermission from '@/directive/permission'
import parseTime from '@/directive/parse-time'
import parseEnum from '@/directive/parse-enum'
import parseProject from '@/directive/parse-project'
import emptyText from '@/directive/empty-text'
import suffix from '@/directive/suffix'
import arrJoin from '@/directive/arr-join'
import toFixed from '@/directive/to-fixed'
import toThousand from '@/directive/to-thousand'
import convertWeight from '@/directive/convert-weight'

const directive = new Map([
  ['permission', checkPermission],
  ['parse-time', parseTime],
  ['parse-enum', parseEnum],
  ['parse-project', parseProject],
  ['empty-text', emptyText],
  ['suffix', suffix],
  ['arr-join', arrJoin],
  ['to-fixed', toFixed],
  ['thousand', toThousand],
  ['convert-weight', convertWeight]

])

export default (app) => {
  directive.forEach((dir, name) => {
    app.directive(name, dir)
  })
}
