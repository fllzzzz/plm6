import checkPermission from '@/directive/permission'
import parseTime from '@/directive/parse-time'
import parseEnum from '@/directive/parse-enum'
import emptyText from '@/directive/empty-text'
import arrJoin from '@/directive/arr-join'
import toFixed from '@/directive/to-fixed'

const directive = new Map([
  ['permission', checkPermission],
  ['parse-time', parseTime],
  ['parse-enum', parseEnum],
  ['empty-text', emptyText],
  ['arr-join', arrJoin],
  ['to-fixed', toFixed]
])

export default (app) => {
  directive.forEach((dir, name) => {
    app.directive(name, dir)
  })
}
