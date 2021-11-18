import checkPermission from '@/directive/permission'
import parseTime from '@/directive/parse-time'

const directive = new Map([
  ['permission', checkPermission],
  ['parse-time', parseTime]
])

export default (app) => {
  directive.forEach((dir, name) => {
    app.directive(name, dir)
  })
}
