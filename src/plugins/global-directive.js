import checkPermission from '@/directive/permission'

const directive = new Map([
  ['permission', checkPermission]
])

export default (app) => {
  directive.forEach((dir, name) => {
    app.directive(name, dir)
  })
}
