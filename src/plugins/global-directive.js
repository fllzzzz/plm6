import checkPermission from '@/directive/permission'
import parseTime from '@/directive/parse-time'
import emptyText from '@/directive/empty-text'

const directive = new Map([
  ['permission', checkPermission],
  ['parse-time', parseTime],
  ['empty-text', emptyText]
])

export default (app) => {
  directive.forEach((dir, name) => {
    app.directive(name, dir)
  })
}
